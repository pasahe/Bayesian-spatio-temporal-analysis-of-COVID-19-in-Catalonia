#File for estimating the different models and save the results
#Compile to generate the data that will go in the 2_Analysis folder. These data is not found in github because of the high memory weight of these files. Only dat.R is found, which contains the necessary information to perform the models.
rm(list=ls())

library(dplyr)
library(tidyr)
library(INLA)
library(spdep)
library(lubridate)
library(stringr)
library(purrr)
library(zoo)
library(tibble)
library(tsibble)
library(Matrix) 

#---Load data----
load(file = "Data/1_Processed/pob_abs.Rda")
load(file = "Data/1_Processed/dat_cas.Rda")
load(file = "Data/1_Processed/dat_edat.Rda")
load(file = "Data/1_Processed/dat_hosp.Rda")
load(file = "Data/shapefileT.Rda")

#Load covariates
load(file = "Data/1_Processed/dat_covar.Rda")
#Les dades de mobilitat no les carregarem perquè són mensuals (no tenim suficient variabilitat...)
#Les dades de restriccions les hauríem de processar abans d'incorporar-les com a covariants

#Load vacccination data
load(file = "Data/1_Processed/dat_vac.Rda")

#Population in the total of Catalonia by sex and age (to calculate the standarized incidence of cases). Because the age aggrupation changes we have to calculate the population by each age aggrupation
Tpob_cas <- pob_abs %>% 
  group_by(any, sexe, edat_cas) %>% 
  summarise(NT = sum(N)) %>% 
  rename(edat = edat_cas)

Tpob_hosp <- pob_abs %>% 
  group_by(any, sexe, edat_hosp) %>% 
  summarise(NT = sum(N)) %>% 
  rename(edat = edat_hosp)

Tpob_vac <- pob_abs %>%
  group_by(any, sexe, edat_vac) %>%
  summarise(NT = sum(N)) %>%
  rename(edat = edat_vac)

sf::sf_use_s2(FALSE)

#----------Spatial models-------------
#The outcome will be the SIR to estimate RR. We won't calculate smooth taxes because I think is most relevant to take in account the age and sex. In the end, our goal is not to compare the different waves in terms of incidence and rates (and they're even uncomparable) but to estimate areas with higher risks than others across the pandemic, and to try to explain these differences. In the end, RR are comparable between waves case incidence values no. I think that incidence and rates are nice to have but we don't need to smooth them, what is the point.
#page 107 of the Modelling Spatial and Spatial-Temporal Data book uses a Poisson with the total population as an offset and explains the motivation of using small area estimation in that case.

#Furthermore, I think that we would have to calculate the RR only for the cases and hospitalization as there is very few variability on fully vaccination percentages and thus it's not necessary to calculate the RR. 

#Model: y_i ~ Poiss(n_i*theta_i); log(theta_i) ~ beta0 + b_i
# The random effect b_i follows a BYM model that includes an ICAR (Intrinsic Conditional Auto-Regressive model) component for spatial auto-correlation and an ordinary random-effect component for non-spatial heterogenity:
#(Specific formulation from https://sci-hub.st/10.1002/sim.4780142111)
# b_i = u_i + v_i, where:
#u_i|u ~ N(W_u, 1/theta_u*n_i), where W_u is the weighted sum of the u_j of the adjacent neighbors
#v_i ~ N(0, 1/theta_v)

#We choose prior distributions for the logarithm of the hyperparameters of the distribution, this is log(theta_u) and log(theta_v). By default they are a loggamma distribution, that we can see in the inla documentation of the bym model (loggamma(1, 0.0005)) https://inla.r-inla-download.org/r-inla.org/doc/latent/bym.pdf
#But this distribution (the inverse-gamma, i.e gamma for the 1/X) is not recommended (Andrew Gelman, http://www.stat.columbia.edu/~gelman/research/published/taumain.pdf), because when the parameter is estimated to be closed to zero the resulting inferences will be sensitive. It's recommended a standard uniform distribution that we can define, by:
# sdunif = "expression: logdens=log(0.5)-log_precision/2; return(logdens);"
sdunif = "expression: logdens=-log_precision/2; return(logdens);"
#Aritz Adin, https://academica-e.unavarra.es/bitstream/handle/2454/27572/Adin%20Urtasun%20Tesis%20MA.pdf?sequence=1&isAllowed=y

#BYM2: https://arxiv.org/pdf/1601.01180.pdf, implementation also in https://www.paulamoraga.com/book-geospatial/sec-arealdatatheory.html
# In the classical BYM (Besag, York and Mollié) model, the spatially structured component cannot be seen independently from the unstructured component. This makes prior definitions for the hyperparameters of the two random effects challenging. BYM2 leads to improved parameter control as the hyperparameters can be seen independently from each other, defining a model that depends on two hyperparameters tau_b (pure overdispersion) and phi (spatially structured correlation - proportion of the marginal variance explained by the structured effect-) that can be interpretable.
# In INLA the prior is defined on log(tau) & log(phi/(1-phi)). A reasonable choice for the prior of phi is the conservative one that assumes that the unstructured random effect accounts for more of the variability than the spatially structured effect so that P(phi < 0.5) = 2/3. For the prior of tau it depends on the marginal standard deviation that we define. We can define for example a 0.5 of marginal standard deviation upper bound that corresponds to P((1/square(tau)) > (0.5/0.31)) = 0.01. 
pc_prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)
  ),
  phi = list(
    prior = "pc",
    param = c(0.5, 2/3)
  )
)

#We will calculate both estimates using a bym model specification with standard uniform prior distribution for the hyperparameters and also using a bym2 model specification with the previous noted prior hyperparamaters distribution.

#First, we have to build the adjancency matrix:
nb <- poly2nb(shapefileT)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

#Let's define an index for every poligon
shapefileT$idarea <- 1:nrow(shapefileT@data)

#Define the function that calculates the observed, expected, sir and the estimated SAE results for the 7-days window defined by the date x of each one of the outcomes
#We will consider the cases, hospitalization and vaccination incidence over the total number of people in the ABS being considered the population at risk
#We would like to consider the hospitalization/cases ratio but we don't have the age distribution of the cases by ABS... We only can do it grouping it by sex.
#Let's incorporate the age to calculate only for the ages older than 70 years (we will use it in the vaccination models)
res_out <- function(x, inla = FALSE, edat = FALSE){
  
  print(x)
  
  if(edat) {
    
    spob_abs <- pob_abs %>% 
      filter(edat_cas %in% c("70-79", "80-89", "90+")) %>% 
      mutate(
        edat_cas = droplevels(edat_cas),
        edat_hosp = droplevels(edat_hosp)
      )
    
    sdat_hosp <- dat_hosp %>% 
      filter(edat %in% c("70 a 79", "80 o més")) %>% 
      mutate(
        edat = droplevels(edat)
      )
    
    spob_hosp <- Tpob_hosp %>% 
      filter(edat %in% c("70 a 79", "80 o més")) %>% 
      mutate(
        edat = droplevels(edat)
      )
    
    sdat_vac <- dat_vac %>% 
      filter(edat %in% c("70 a 74", "75 a 79", "80 o més")) %>% 
      mutate(
        edat = droplevels(edat)
      )
    
    spob_vac <- Tpob_vac %>% 
      filter(edat %in% c("70 a 74", "75 a 79", "80 o més")) %>% 
      mutate(
        edat = droplevels(edat)
      )
    
  } else {
    
    spob_abs <- pob_abs
    sdat_hosp <- dat_hosp
    spob_hosp <- Tpob_hosp
    sdat_vac <- dat_vac
    spob_vac <- Tpob_vac
    
  }
  
  
  if(!edat) {
    ##Age-sex distribution of the covid cases, hospitalization, vaccination in the total of Catalonia in the 7-days window. Let's add also the population in the total of Catalonia.
    Tedat_cas <- dat_edat %>% 
      filter(data == x) %>% 
      #We consider the year given by x
      full_join(Tpob_cas %>% filter(any == year(x)), by = c("sexe", "edat")) %>% 
      mutate(
        n = ifelse(is.na(n), 0, n),
        ratio = n/NT
      ) %>% 
      dplyr::select(sexe, edat, ratio)
    
    #Calculate the reference rates by each sex-age group for the whole period
    Tedat_cas_total <- dat_edat %>% 
      mutate(
        any = year(data)
      ) %>% 
      left_join(Tpob_cas, by = c("any", "sexe", "edat")) %>% 
      group_by(sexe, edat) %>% 
      summarise(n = sum(n),
                NT = sum(NT)) %>% 
      mutate(
        Tratio = n/NT
      ) %>% 
      dplyr::select(sexe, edat, Tratio)
    
    exp_cas <- pob_abs %>% 
      filter(any == year(x)) %>% 
      rename(edat = edat_cas) %>% 
      group_by(codi_abs, sexe, edat) %>% 
      summarise(N = sum(N)) %>% 
      left_join(Tedat_cas, by = c("sexe", "edat")) %>% 
      left_join(Tedat_cas_total, by = c("sexe", "edat")) %>% 
      mutate(
        exp = N*ratio,
        Texp = N*Tratio
      ) %>% 
      group_by(codi_abs) %>% 
      summarise(
        N = sum(N),
        exp_cas = sum(exp),
        Texp_cas = sum(Texp)
      )
  }
  
  Tedat_hosp <- sdat_hosp %>% 
    filter(data == x) %>% 
    group_by(sexe, edat) %>% 
    summarise(n = sum(n)) %>% 
    full_join(spob_hosp %>% filter(any == year(x)), by = c("sexe", "edat")) %>% 
    mutate(
      n = ifelse(is.na(n), 0, n),
      ratio = n/NT
    ) %>% 
    dplyr::select(sexe, edat, ratio)
  
  # Calculate the reference rates by each sex-age group for the whole period
  Tedat_hosp_total <- sdat_hosp %>% 
    mutate(
      any = year(data)
    ) %>% 
    #We have to group it previously to aggregate all areas
    group_by(data, any, sexe, edat) %>% 
    summarise(n = sum(n)) %>% 
    left_join(spob_hosp, by = c("any", "sexe", "edat")) %>% 
    group_by(sexe, edat) %>% 
    summarise(n = sum(n),
              NT = sum(NT)) %>% 
    mutate(
      Tratio = n/NT
    ) %>% 
    dplyr::select(sexe, edat, Tratio)
  
  if(edat) {
    exp_hosp <- spob_abs %>% 
      filter(any == year(x)) %>% 
      rename(edat = edat_hosp) %>% 
      group_by(codi_abs, sexe, edat) %>% 
      summarise(N = sum(N)) %>% 
      left_join(Tedat_hosp, by = c("sexe", "edat")) %>% 
      left_join(Tedat_hosp_total, by = c("sexe", "edat")) %>% 
      mutate(
        exp = N*ratio,
        Texp = N*Tratio
      ) %>% 
      group_by(codi_abs) %>% 
      summarise(
        N = sum(N),
        exp_hosp = sum(exp),
        Texp_hosp = sum(Texp)
      )
  } else {
    exp_hosp <- spob_abs %>% 
      filter(any == year(x)) %>% 
      rename(edat = edat_hosp) %>% 
      group_by(codi_abs, sexe, edat) %>% 
      summarise(N = sum(N)) %>% 
      left_join(Tedat_hosp, by = c("sexe", "edat")) %>% 
      left_join(Tedat_hosp_total, by = c("sexe", "edat")) %>% 
      mutate(
        exp = N*ratio,
        Texp = N*Tratio
      ) %>% 
      group_by(codi_abs) %>% 
      summarise(
        exp_hosp = sum(exp),
        Texp_hosp = sum(Texp)
      )
  }
  
  #We will use the indirect standardization as we don't have the age distribution of cases for every ABS. The population of interest is every ABS and the standard population is the global of Catalonia. Article for direct/indirect methods: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3406211/
  #Indirect standardization: multiply the number of people in each group of the population of interest by the age-sex specific incidence rate in the comparable group of the reference population, giving the expected number of cases. 
  
  ##Expected cases, hospitalization by ABS in function of the population in each ABS and age & sex group and the calculated ratio of cases:
  if(edat) {
    #If edat=TRUE we will not calculate cases as we don't have the cases of the ABS by age
    exp_abs <- exp_hosp 
    
    #Observed hospitalization by ABS
    hosp_abs <- sdat_hosp %>% 
      filter(data == x) %>% 
      group_by(codi_abs) %>% 
      summarise(n_hosp = sum(n))
    
    #Observed vaccination by ABS
    vac_abs <- sdat_vac %>%
      filter(data <= x) %>%
      group_by(codi_abs) %>%
      summarise(n_vac = sum(n))
    
    #Group the observed and the expected incidence by ABS and calculate the rates and SIR
    inc_abs <- exp_abs %>% 
      left_join(hosp_abs, by = "codi_abs") %>% 
      left_join(vac_abs, by = "codi_abs") %>%
      mutate_at(c("n_hosp", "n_vac"), ~ifelse(is.na(.x), 0, .x)) %>% 
      mutate(
        #Rates
        rate_hosp = n_hosp/N,
        rate_vac = n_vac/N,
        #SIRs
        sir_hosp = n_hosp/exp_hosp
      )
    
  } else {
    
    exp_abs <- exp_cas %>% 
      left_join(exp_hosp, by = "codi_abs")
    
    #Observed cases by ABS
    cas_abs <- dat_cas %>% 
      filter(data == x) %>% 
      group_by(codi_abs) %>% 
      summarise(n_cas = sum(n))
    
    #Observed hospitalization by ABS
    hosp_abs <- sdat_hosp %>% 
      filter(data == x) %>% 
      group_by(codi_abs) %>% 
      summarise(n_hosp = sum(n))
    
    #Observed vaccination by ABS
    vac_abs <- sdat_vac %>%
      filter(data <= x) %>%
      group_by(codi_abs) %>%
      summarise(n_vac = sum(n))
    
    #Group the observed and the expected incidence by ABS and calculate the rates and SIR
    inc_abs <- exp_abs %>% 
      left_join(cas_abs, by = "codi_abs") %>% 
      left_join(hosp_abs, by = "codi_abs") %>% 
      left_join(vac_abs, by = "codi_abs") %>%
      mutate_at(c("n_cas", "n_hosp", "n_vac"), ~ifelse(is.na(.x), 0, .x)) %>% 
      mutate(
        #Rates
        rate_cas = n_cas/N,
        rate_hosp = n_hosp/N,
        rate_vac = n_vac/N,
        #SIRs
        sir_cas = n_cas/exp_cas,
        sir_hosp = n_hosp/exp_hosp
      )
    
  }
  
  
  #Hospitalization & vaccination data has days without any event because their time series start after
  if(all(inc_abs$n_hosp == 0)) {
    inc_abs$sir_hosp <- NA
  } 
  
  #Run the INLA models and save them:
  map<-shapefileT
  
  map@data <- map@data %>% 
    full_join(inc_abs, by = "codi_abs") 
  
  if(inla) {
    #Run inla models for every one of the outcomes:
    inla_res <- tibble(outcomes = c("cas", "hosp")) %>% 
      mutate(
        res_sir_bym = map(outcomes, ~inla_mod(map, .x, model = "bym")),
        res_sir_bym2 = map(outcomes, ~inla_mod(map, .x, model = "bym2"))
      )
    
    return(inla_res)
    
  } else {
    #We want to return only the observed SIR for the date (without the estimated models)
    return(map@data)
    
  }
  
}

#Define the function to perform the INLA models with the model specified
inla_mod <- function(sf, outcome = "cas", model = "bym"){
  
  if(outcome == "cas") {
    print(model)
  }
  
  sf@data <- sf@data %>% 
    rename("n" = str_glue("n_{outcome}"), "exp" = str_glue("exp_{outcome}"))
  
  #If there are no cases (the hospitalization doesn't begin at the initial cases date)
  if(!all(sf$n == 0)){
    
    #Define the two different formulations depending on the model used:
    if(model == "bym") {
      formula = n ~ f(idarea, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)))
    } else if(model == "bym2") {
      formula <- n ~ f(idarea, model = "bym2", graph = g, hyper = pc_prior)
    }
    
    #INLA model for the smooth ratio observed/expected (SIR)
    set.seed(342)
    mod <- inla(formula, family="poisson", data=sf@data, E=exp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
    
  }else{
    mod <- NULL
  }
  
  return(mod)
    
}

ptm <- proc.time()
#The full period will be the minimum sunday of all data (+ 7 days) and the last sunday (in the hospitalization data the date is always in sunday. Also the cases and vac date ends on a sunday). We will take the period given in the cases data as it's the most common period.
#(It lasts 30 min aprox)
range_data <- range(dat_cas$data)
ndat_sae1 <- tibble(data = seq(range_data[1], range_data[2], by = 1)) %>%
  mutate(wday = wday(data)) %>% 
  #Filter sundays:
  filter(wday == 1) %>% 
  #Exclude the first one as we don't have cumulative data:
  slice(-1) %>% 
  dplyr::select(-wday) %>%
  #We divide the period by 2 because the inla program might crash
  filter(data <= ymd("2021-05-09")) %>% 
  mutate(
    res = map(data, ~res_out(.x, inla = TRUE))
  )

ndat_sae2 <- tibble(data = seq(range_data[1], range_data[2], by = 1)) %>%
  mutate(wday = wday(data)) %>% 
  #Filter mondays:
  filter(wday == 1) %>% 
  #Exclude the first one as we don't have cumulative data:
  slice(-1) %>% 
  dplyr::select(-wday) %>%
  #We divide the period by 2 because the inla program might crash
  filter(data > ymd("2021-05-09")) %>% 
  mutate(
    res = map(data, ~res_out(.x, inla = TRUE))
  )

ndat_sae <- rbind(ndat_sae1, ndat_sae2)

#Unnest the results in ndat_sae
ndat_sae <- ndat_sae %>% 
  unnest(res)

print(proc.time()-ptm)

#For comparison of the two models, we wil calculate DIC and WAIC of the weekly models (https://academica-e.unavarra.es/bitstream/handle/2454/43973/Urdangarin_Space-timeInteractions_1662460190262_41560.pdf?sequence=2&isAllowed=y; code in https://github.com/spatialstatisticsupna/Comparing-R-INLA-and-NIMBLE/blob/main/R). The mean and standard deviation of the hyperparameters using each model can't be compared as in table 3 because the hyperparameters are different. We can compare the estimated relative risks and see if there're differences. 
#Deviance Information Criterion (DIC) is a combination of the posterior mean deviance (that is directly related to the likelihood of the model) penalized by the number of effective parameters, similar to what AIC is. 
#Watanabe-Akaike Information Criterion (WAIC) also is a combination of two quantities, the pointwise posterior predictive density and a correction on the effective number of parameters to adjust for overfitting. It's recommended by Gelman, 2014 (https://link.springer.com/article/10.1007/s11222-013-9416-2) over the DIC criterium.

#We expect not to get optimal DIC and WAIC with BYM2 but similar ones, and choose bym2 as we can interpret the parameters (page 2 in https://arxiv.org/pdf/1601.01180.pdf). We can interpret the results of the posterior hyperparameters of the bym2 model: how spatial influence and pure overdispersion changes over time. Also, it would be interesting to see how the variability of the spatial effect changes when adjusting for more variables (to see its importance over other possible effects)

#Calculate DIC and WAIC
sp_dic_waic <- ndat_sae %>% 
  mutate(
    dic_bym = map_dbl(res_sir_bym, ~ifelse(is.null(.x), NA, .x$dic$dic)),
    waic_bym = map_dbl(res_sir_bym, ~ifelse(is.null(.x), NA, .x$waic$waic)),
    dic_bym2 = map_dbl(res_sir_bym2, ~ifelse(is.null(.x), NA, .x$dic$dic)),
    waic_bym2 = map_dbl(res_sir_bym2, ~ifelse(is.null(.x), NA, .x$waic$waic))
  ) %>% 
  dplyr::select(data, outcomes, dic_bym, waic_bym, dic_bym2, waic_bym2) %>% 
  pivot_longer(dic_bym:waic_bym2, names_to = c(".value", "model"), names_pattern = "(.*)(bym.*$)") %>% 
  rename_all(
    ~gsub("\\_$", "", .x)
  )

#Get estimated hyperparameters (phi and precision) from BYM2 models

bym2_hp <- ndat_sae %>% 
  mutate(
    hp = pmap(list(data, outcomes, res_sir_bym2), function(x, y, z) {
      if(!is.null(z) & !(x == ymd("2020-08-23") & y == "hosp")) {
        tibble("SD" = 1/sqrt(z$summary.hyperpar$mean[1]), "Phi" = z$summary.hyperpar$mean[2])
      } else {
        NULL
      }
    })
  ) %>% 
  dplyr::select(data, outcomes, hp) %>% 
  unnest(hp)

#Get estimated results from smooth RR from the BYM and BYM2 models
res_sp <- ndat_sae %>% 
  mutate(
    res_sir_bym = map(res_sir_bym, function(x) {
      if(!is.null(x)) {
        x$summary.fitted.values %>% 
          dplyr::select("rr_bym" = "mean", "rr_lci_bym" = "0.025quant", "rr_uci_bym" = "0.975quant", "p_bym" = contains("cdf"))
      } else {
        NULL
      }
    }
    ),
    res_sir_bym2 = map(res_sir_bym2, function(x) {
      if(!is.null(x)) {
        x$summary.fitted.values %>% 
          dplyr::select("rr_bym2" = "mean", "rr_lci_bym2" = "0.025quant", "rr_uci_bym2" = "0.975quant", "p_bym2" = contains("cdf"))
      } else {
        NULL
      }
    }
    ),
    res = pmap(list(res_sir_bym, res_sir_bym2), cbind),
    #Add the ABS
    res = map(res, function(x) {
      if(!is.null(x)) {
        x %>%
          tibble::add_column(abs = shapefileT$abs,
                     .before = "rr_bym")
      } else {
        NULL
      }
    }
    )
  ) %>% 
  dplyr::select(data, outcomes, res) %>% 
  unnest(res) %>% 
  pivot_longer(rr_bym:p_bym2, names_to = c(".value", "model"), names_pattern = "(.*)(bym.*$)") %>% 
  rename_all(
    ~gsub("\\_$", "", .x)
  )

save(sp_dic_waic, file = "Data/2_Analysed/Spatial/sp_dic_waic.Rda")
save(bym2_hp, file = "Data/2_Analysed/Spatial/bym2_hp.Rda")
save(res_sp, file = "Data/2_Analysed/Spatial/res_sp.Rda")

#----------- Spatio-temporal models --------------
# https://sci-hub.st/10.1002/1097-0258%2820000915/30%2919%3A17/18%3C2555%3A%3AAID-SIM587%3E3.0.CO%3B2-%23
# https://www.uv.es/famarmu/doc/Euroheis2-report.pdf
# Other specifications appart from the Knorr Held (b-splines, p-splines for the temporal effect, etc):
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5334700/
# https://academica-e.unavarra.es/bitstream/handle/2454/27572/Adin%20Urtasun%20Tesis%20MA.pdf?sequence=1&isAllowed=y

#There're different ways to take in account the time dependency of the effect.
#Linear trend is discarded, so we can follow Knorr-Held specification, for example:
#log(theta_ij) = alpha + u_i + v_i + gamma_j + phi_j + delta_ij

#gamma_j + phi_j is a temporal random effect: phi_j is an unstructred temporal effect and gamma_j can follow a random walk in time of first order (RW1):
#gamma_j | gamma_{j-1} ~ N(gamma_{j-1}, sigma^2_{gamma})
#or a random walk in time of second order (RW2):
#gamma_j | gamma_{j-1},gamma_{j-2} ~ N(2gamma_{j-1} - gamma_j-2, sigma^2_{gamma})

#delta_ij is the interaction term between space and time and can be specified in many different ways. Knorr-Held proposes four types of interactions between (u_i, gamma_j), (u_i, phi_j), (v_i, gamma_j) and (v_i, phi_j)

#For the only spatial dependency we will take BYM2 specification as it's the same as BYM, but interpetable as we have seen

#We can follow the implementation on https://www.paulamoraga.com/book-geospatial/sec-arealdatatheory.html or the one in Adin thesis and implemented in:
#https://github.com/spatialstatisticsupna/Identifiability_Constraints_article/blob/main/R/CARmodels_INLA.R
#The two versions are equivalent (well explained in the book https://sci-hub.st/10.1002/9781118950203), one is simplified in code but can't allow constraints and the other is more complicated but allows constraints programming it from scratch. The thing is that the second one is computationally expensive and gives an error.

#We will consider again standard uniform distribution for the set of hyperprior distributions for all hyperparameters as in the article (except for bym2 ones that have they own PC prior distributions)

# https://sci-hub.st/10.4081/gh.2014.3
#We can calculate the reference rates to calculate the expected cases in both different ways: one is to use the average rates for the entire study period, while another, is to apply the average rates for each period included. The first option allow us to model the temporal trend whereas for the second option we would have a flat temporal trend as it is intrinsically included in the calculation of the expected cases.  It is advisable to use reference rates corresponding to each study period when checking spatial patterns, since the use of average reference rates for a very broad time window may mask the geographical pattern with increasing distance from the reference rates. Thus, we will calculate the expected values using every time period as the first option and we will also include in some occasions the model taking in account the expected cases with the entire period as it was like if we were modeling the rates but taking in account the age and sex group distributions. The use of a different reference time for each estimate will allow you to identify trends over time of the areas beyond the overall Catalonia trend over time. At the end, areas that in the sixth wave have higher incidences will be teh ones that have more of spatial relative risk. At the end, we're comparing different waves that might be uncomparable at least on cases.

#For modeling temporal effects we have to take the dataset with all the areas and dates available (we will build them only for cases and hospitalization):
range_data <- range(dat_cas$data)

#Calculate the observed incidence and sir for every date
dat <- tibble(data = seq(range_data[1], range_data[2], by = 1)) %>%
  mutate(wday = wday(data)) %>% 
  #Filter sundays:
  filter(wday == 1) %>% 
  dplyr::select(-wday) %>%
  mutate(
    obs = map(data, res_out)
  ) %>% 
  unnest(obs) %>% 
  dplyr::select(data, codi_abs, abs, idarea, N:sir_hosp)

#Lag vaccination data
dat <- dat %>% 
  group_by(codi_abs) %>% 
  mutate(
    lag_rate_vac1 = lag(rate_vac),
    lag_rate_vac2 = lag(rate_vac, 2)
  ) %>% 
  ungroup()

#Add covariates to the data:

dat <- dat %>% 
  left_join(dat_covar, by = "codi_abs") %>% 
  dplyr::select(-N.y) %>% 
  rename(N = N.x)

#Save data
save(dat, file = "Data/dat.Rda")

# https://sci-hub.st/10.1002/1097-0258%2820000915/30%2919%3A17/18%3C2555%3A%3AAID-SIM587%3E3.0.CO%3B2-%23
# In practice, temporal trends are typically strong for most diseases so the unstructured temporal effect can be neglected. 
# The main effects model already imposes an identifiability problem, because the intercept can be absorbed by both the spatial or the temporal effect. Thus, we have to recenter the effects or to impose some constraints into the effects. We will impose some constraints on the effects (use constr = TRUE to impose sum to zero constraint by default) specified in table 1.2 of https://academica-e.unavarra.es/bitstream/handle/2454/27572/Adin%20Urtasun%20Tesis%20MA.pdf?sequence=1&isAllowed=y
#  Second, there is a problem of identifiability with the interaction term overlaps with the main spatial and temporal effects. This overlap depends on the type of interaction (https://academica-e.unavarra.es/bitstream/handle/2454/43973/Urdangarin_Space-timeInteractions_1662460190262_41560.pdf?sequence=2&isAllowed=y).


#Define the function to run the INLA spatio-temporal models:
inla_mod_st <- function(df, outcome = "cas", model = "bym2", iid = FALSE, rw = "rw1", interaction = "no", effect = "sir") {
  
  if(effect == "sir") {
    print(interaction)
    print(model)
    print(outcome)
  }
  
  if(outcome == "hosp") {
    df <- df %>% 
      filter(data >= ymd("2020-05-03"))
  } 
  
  #Let's define an index for every time and an index for every area-time:
  df <- df %>% 
    mutate(
      idarea1 = idarea,
      idtime = as.numeric(factor(data)),
      idtime1 = idtime,
      idareatime = 1:nrow(df)
    ) %>% 
    rename("n" = str_glue("n_{outcome}"), "exp" = str_glue("exp_{outcome}"), "Texp" = str_glue("Texp_{outcome}"))
  
  #Define the variables that we will need to define the constraints and the structure matrix of the interaction effect
  
  #For defining the constraints we will need to define the following variables:
  s <- length(unique(df$idarea))
  t <- length(unique(df$idtime))
  
  #Define the temporal structure matrix of a RW1
  D1 <- diff(diag(t), differences = 1)
  Rt <- t(D1) %*% D1
  
  #Define the spatial structure matrix
  Rs <- matrix(0, g$n, g$n)
  for (i in 1:g$n) {
    Rs[i, i] = g$nnbs[[i]]
    Rs[i, g$nbs[[i]]] = -1
  }
      
    if (interaction == "no") {
      
      #The combination of BYM2 with no interaction gives me an error, so we'll compile this model with BYM specification
      formula <-
        n ~ 
        f(
          idarea,
          model = "bym",
          graph = g,
          hyper = list(
            prec.unstruct = list(prior = sdunif),
            prec.spatial = list(prior = sdunif)
          ),
          constr = TRUE
        ) +
        # f(
        #   idarea,
        #   model = "bym2",
        #   graph = g,
        #   hyper = pc_prior,
        #   constr = TRUE
        # ) +
        f(
          idtime,
          model = rw,
          hyper = list(prec = list(prior = sdunif)),
          constr = TRUE
        )
      
    } else if (interaction == "I") {
      
      #This will be the base model that we will compare different strategies (BYM2 vs BYM, iid vs no iid)
      if(model == "bym2") {
        
        if(!iid) {
          
          formula <-
            n ~ f(
              idarea,
              model = "bym2",
              graph = g,
              hyper = pc_prior,
              constr = TRUE
            ) +
            f(
              idtime,
              model = rw,
              hyper = list(prec = list(prior = sdunif)),
              constr = TRUE
            ) +
            f(
              idareatime,
              model = "iid",
              hyper = list(prec = list(prior = sdunif)),
              constr = TRUE
            )
          
        } else {
          
          formula <-
            n ~ f(
              idarea,
              model = "bym2",
              graph = g,
              hyper = pc_prior,
              constr = TRUE
            ) +
            f(
              idtime,
              model = rw,
              hyper = list(prec = list(prior = sdunif)),
              constr = TRUE
            ) +
            f(
              idtime1,
              model = "iid",
              hyper = list(prec = list(prior = sdunif)),
              constr = TRUE
            ) +
            f(
              idareatime,
              model = "iid",
              hyper = list(prec = list(prior = sdunif)),
              constr = TRUE
            )
        }
        
      } else {
        #The base model will be compiled with type I interaction
        formula <-
          n ~ f(
            idarea,
            model = "bym",
            graph = g,
            hyper = list(
              prec.unstruct = list(prior = sdunif),
              prec.spatial = list(prior = sdunif)
            ),
            constr = TRUE
          ) +
          f(
            idtime,
            model = rw,
            hyper = list(prec = list(prior = sdunif)),
            constr = TRUE
          ) +
          f(
            idareatime,
            model = "iid",
            hyper = list(prec = list(prior = sdunif)),
            constr = TRUE
          )
      }
      
    } else if (interaction == "II") {
      #Define the structure matrix of this type of interaction effect
      R <- kronecker(Rt, Diagonal(s))
      r <- s
      #Define the constraints
      A <- kronecker(matrix(1, 1, t), diag(s))
      A <- A[-1, ]
      e <- rep(0, s - 1)
      
      formula <-
        n ~ f(
          idarea,
          model = "bym2",
          graph = g,
          hyper = pc_prior,
          constr = TRUE
        ) +
        f(
          idtime,
          model = rw,
          hyper = list(prec = list(prior = sdunif)),
          constr = TRUE
        ) +
        f(idareatime,
          model = "generic0", Cmatrix=R, rankdef=r,
          hyper=list(prec=list(prior=sdunif)),
          constr = TRUE, extraconstr=list(A=A, e=e)
          )
        # f(
        #   idarea1,
        #   model = "iid",
        #   group = idtime1,
        #   control.group = list(model = rw),
        #   hyper = list(prec = list(prior = sdunif))
        # )
      
    } else if (interaction == "III") {
      #Define the structure matrix of this type of interaction effect
      R <- kronecker(Diagonal(t), Rs)
      r <- t
      #Define the constraints
      A <- kronecker(Diagonal(t),matrix(1,1,s))
      A <- A[-1,]
      e <- rep(0,t-1)
      
      formula <-
        n ~ f(
          idarea,
          model = "bym2",
          graph = g,
          hyper = pc_prior,
          constr = TRUE
        ) +
        f(
          idtime,
          model = rw,
          hyper = list(prec = list(prior = sdunif)),
          constr = TRUE
        ) +
        f(idareatime,
          model = "generic0", Cmatrix=R, rankdef=r,
          hyper=list(prec=list(prior=sdunif)),
          constr = TRUE, extraconstr=list(A=A, e=e)
        )
        # f(
        #   idtime1,
        #   model = "iid",
        #   group = idarea1,
        #   control.group = list(model = "besag", graph = g),
        #   hyper = list(prec = list(prior = sdunif))
        # )
      
    } else {
      
      #Type IV
      #Define the structure matrix of this type of interaction effect
      R <- kronecker(Rt, Rs)
      r <- s+t-1
      #Define the constraints
      A1 <- kronecker(matrix(1,1,t),Diagonal(s))
      A2 <- kronecker(Diagonal(t),matrix(1,1,s))
      A <- rbind(A1[-1,], A2[-1,])
      e <- rep(0, s+t-2)
      
      formula <-
        n ~ f(
          idarea,
          model = "bym2",
          graph = g,
          hyper = pc_prior,
          constr = TRUE
        ) +
        f(
          idtime,
          model = rw,
          hyper = list(prec = list(prior = sdunif)),
          constr = TRUE
        ) +
        f(idareatime,
          model = "generic0", Cmatrix=R, rankdef=r,
          hyper=list(prec=list(prior=sdunif)),
          constr = TRUE, extraconstr=list(A=A, e=e)
        )
        # f(
        #   idarea1,
        #   model = "besag",
        #   graph = g,
        #   group = idtime1,
        #   control.group = list(model = rw),
        #   hyper = list(prec = list(prior = sdunif))
        # )
    }
      
    #INLA model for the smooth ratio observed/expected (SIR)
    set.seed(342)
    if(effect == "sir") {
      mod <- inla(formula, family="poisson", data=df, E=exp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
    } else {
      mod <- inla(formula, family="poisson", data=df, E=Texp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
    }
    
  mod  
}

#We will compare different base models to see which model specify in the effect type "I":
t0 <- Sys.time()
#Comparison between BYM and BYM2 models:

ndat_sae_st_bym <- tibble(expand.grid(outcomes = c("cas", "hosp"), model = c("bym2", "bym"), effect = c("sir", "sir2"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, model, effect), function(x, y, z) {
      inla_mod_st(df = dat, outcome = x, model = y, effect = z, interaction = "I")
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )
#DIC and AIC are nearly the same for BYM2 and BYM. We will take BYM2 for interpetable reasons

#Comparison between considering iid unstructure dtemporal effect or not:
ndat_sae_st_iid <- tibble(expand.grid(outcomes = c("cas", "hosp"), iid = c(FALSE, TRUE), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = pmap(list(outcomes, iid, effect), function(x, y, z) {
      inla_mod_st(df = dat, outcome = x, iid = y, effect = z, interaction = "I")
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )
#We have seen that are very similar in DIC and WAIC. We will take the model without an iid temporal effect for simplicity reasons

#Comparison between random walks:
ndat_sae_st_rw <- tibble(expand.grid(outcomes = c("cas", "hosp"), rw = c("rw1", "rw2"), effect = c("sir", "sir2"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, rw, effect), function(x, y, z) {
      inla_mod_st(df = dat, outcome = x, rw = y, effect = z, interaction = "I")
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )
#We have seen that RW1 and RW2 are very similar in DIC and WAIC. We will take RW1 for simplicity reasons

ndat_sae_st_base <- rbind(ndat_sae_st_bym %>% rename(base = model), ndat_sae_st_iid %>% rename(base = iid), ndat_sae_st_rw %>% rename(base = rw))


#Let's get the DIC & WAIC of these models 

st_dic_waic_base <- ndat_sae_st_base %>% 
  dplyr::select(outcomes, base, effect, dic, waic) %>% 
  pivot_wider(names_from = outcomes, values_from = c(dic, waic)) %>% 
  mutate(
    base = case_when(
      base == "TRUE" ~ "Temporal unstructured",
      base == "FALSE" ~ "No temporal unstructured",
      TRUE ~ toupper(base)
    )
  ) %>% 
  dplyr::select(base, effect, dic_cas, waic_cas, dic_hosp, waic_hosp)

#Save the results with the hyperparameters of the different base model specifications
save(st_dic_waic_base, file = "Data/2_Analysed/SpatioTemporal/st_dic_waic_base.Rda")

#SIR2 

#Now that we know the base models (BYM2, no iid, RW1), we will estimate all the models with all types of interaction effect:
t0 <- Sys.time()
ndat_sae_st0 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("no"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y, effect = "sir2")
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

ndat_sae_st1 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("I"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y, effect = "sir2")
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

ndat_sae_st2 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("II"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y, effect = "sir2")
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

ndat_sae_st3 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("III"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y, effect = "sir2")
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

ndat_sae_st4 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("IV"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y, effect = "sir2")
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

Sys.time() - t0

ndat_sae_st_sir2 <- rbind(ndat_sae_st1, ndat_sae_st2, ndat_sae_st3, ndat_sae_st4)

#Let's get the DIC & WAIC of all models 

st_dic_waic_sir2 <- ndat_sae_st_sir2 %>% 
  dplyr::select(outcomes, interaction, dic, waic) %>% 
  pivot_wider(names_from = outcomes, values_from = c(dic, waic)) %>% 
  dplyr::select(interaction, dic_cas, waic_cas, dic_hosp, waic_hosp)

#Type II interaction is the best model for the cases. For the hospitalization, the type IV is the best model but it doesn't improve a lot the II one, so we will stick with this one as well for simplicity and coherence with the other outcome. With this interaction, we're assuming a random walk of order 1 across time for each area independently from all the other areas (every area has its own random walk). So, we're saying that temporal trends are different from area to area but there is not any structure in space (type IV). The same type II interaction as the best one chosen from the Toronto covid-19 data (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9172088/).

#Save the results with the hyperparameters
save(st_dic_waic_sir2, file = "Data/2_Analysed/SpatioTemporal/st_dic_waic_sir2.Rda")

#Let's do the same for the SIR calculated in each week
Sys.time() - t0
#Now that we know the base models (BYM2, no iid, RW1), we will estimate all the models with all types of interaction effect:
t0 <- Sys.time()
ndat_sae_st0 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("no"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y)
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

ndat_sae_st1 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("I"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y)
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

ndat_sae_st2 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("II"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y)
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

ndat_sae_st3 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("III"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y)
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

ndat_sae_st4 <- tibble(expand.grid(outcomes = c("cas", "hosp"), interaction = c("IV"))) %>% 
  mutate_all(as.character) %>% 
  mutate(
    res = pmap(list(outcomes, interaction), function(x, y) {
      inla_mod_st(df = dat, outcome = x, interaction = y)
    }),
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic)
  )

Sys.time() - t0

ndat_sae_st <- rbind(ndat_sae_st1, ndat_sae_st2, ndat_sae_st3, ndat_sae_st4)

#Let's get the DIC & WAIC of all models 

st_dic_waic <- ndat_sae_st %>% 
  dplyr::select(outcomes, interaction, dic, waic) %>% 
  pivot_wider(names_from = outcomes, values_from = c(dic, waic)) %>% 
  dplyr::select(interaction, dic_cas, waic_cas, dic_hosp, waic_hosp)

#Type II interaction is the best model for the cases. For the hospitalization, the type IV is the best model but it doesn't improve a lot the II one, so we will stick with this one as well for simplicity and coherence with the other outcome. With this interaction, we're assuming a random walk of order 1 across time for each area independently from all the other areas (every area has its own random walk). So, we're saying that temporal trends are different from area to area but there is not any structure in space (type IV). The same type II interaction as the best one chosen from the Toronto covid-19 data (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9172088/).

#Save the results with the hyperparameters
save(st_dic_waic, file = "Data/2_Analysed/SpatioTemporal/st_dic_waic.Rda")

#Let's fit the type II model without constraints to see the change
#We will pass the defined parameters to the function directly
inla_mod_II <- function(df, outcome = "cas", effect = "sir", constraints = TRUE, temp = TRUE) {

  print(outcome)
  print(effect)

  if(outcome == "hosp") {
    df <- df %>%
      filter(data >= ymd("2020-05-03"))
  }

  #Let's define an index for every time and an index for every area-time:
  df <- df %>%
    mutate(
      idarea1 = idarea,
      idtime = as.numeric(factor(data)),
      idtime1 = idtime,
      idareatime = 1:nrow(df)
    ) %>%
    rename("n" = str_glue("n_{outcome}"), "exp" = str_glue("exp_{outcome}"), "Texp" = str_glue("Texp_{outcome}"))

  # For defining the structure matrix and the constraints we will need to define the following variables:
  s <- length(unique(df$idarea))
  t <- length(unique(df$idtime))

  #Define the temporal structure matrix of a RW1
  D1 <- diff(diag(t),differences=1)
  Rt <- t(D1)%*%D1

  #Defining the diagonal matrix as an sparse matrix we can store it in an efficient way (if not, we can't not even store it in a system with 16GB RAM). We can talk about it in the TFM.
  R <- kronecker(Rt,Diagonal(s)) #Sparse kronecker matrix
  r <- s
  #Define the constraints
  A <- kronecker(matrix(1,1,t), diag(s))
  A <- A[-1,]
  e <- rep(0,s-1)

  if(constraints) {

    if(temp) {
      formula <- n ~ f(idarea, model = "bym2", graph = g, hyper = pc_prior, constr = TRUE) +
        f(idtime, model = "rw1", hyper=list(prec=list(prior=sdunif)), constr = TRUE) +
        f(idareatime,
          model = "generic0", Cmatrix=R, rankdef=r,
          hyper=list(prec=list(prior=sdunif)),
          constr = TRUE, extraconstr=list(A=A, e=e)
        )
    } else {
      formula <- n ~ f(idarea, model = "bym2", graph = g, hyper = pc_prior, constr = TRUE) +
        f(idareatime,
          model = "generic0", Cmatrix=R, rankdef=r,
          hyper=list(prec=list(prior=sdunif)),
          constr = TRUE, extraconstr=list(A=A, e=e)
        )
    }


  } else{

    formula <- n ~ f(idarea, model = "bym2", graph = g, hyper = pc_prior) +
      f(idtime, model = "rw1", hyper=list(prec=list(prior=sdunif))) +
      f(idareatime,
        model = "generic0", Cmatrix=R, rankdef=r,
        hyper=list(prec=list(prior=sdunif))
      )
      # f(idarea1,
      #   model = "iid",
      #   group = idtime1, control.group = list(model = "rw1"),
      #   hyper=list(prec=list(prior=sdunif))
      # )
  }

  set.seed(342)

  if(effect == "sir") {
    mod <- inla(formula, family="poisson", data=df, E=exp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
  } else {
    mod <- inla(formula, family="poisson", data=df, E=Texp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))

  }

  return(mod)

}

#For the expected cases for the whole period
ndat_sae_II <- tibble(expand.grid(outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(
    res = map2(outcomes, effect, ~inla_mod_II(dat, outcome = .x, effect = .y))
  )

#Save the models
save(ndat_sae_II, file = "Data/2_Analysed/SpatioTemporal/ndat_sae_II.Rda")

#-------Include spatial covariates in the spatiotemporal model-------------

#Treballs que utilitzen spatiotemporal models amb covariables en el fixed effect: 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9324591/
# Spatiotemporal studies by Wang et al. [160], Briz-Redón et al. [173] and Jaya et al., 2021 [175] did not incorporate any covariates to measure the spatiotemporal relative risk of COVID-19. Bermudi et al. [171], Ngwira et al., 2021 [177] and Paul et al., 2021 [178] incorporated socioeconomic covariates, while Johnson et al., 2021 [176] included 6 Social vulnerability and 7 environmental variables as fixed effects.
# 

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7709594/
# utilitza SE covariable en el fixed effect i també fa els diferents models espacials per a cada week mirant com canvia l'efecte del SE

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7912604/
#Common spatiotemporal model including the spatial covariates as a fixed effect

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8335698/
#Well explained the background for including sociodemographic variables to model covid-19 outcomes
#covid-19 case study very similar to what we are doing !!

#We will pass the defined parameters to the function directly
inla_mod_covar <- function(df, outcome = "cas", effect = "sir", var = "si") {
  
  print(outcome)
  print(effect)
  
  if(outcome == "hosp") {
    df <- df %>% 
      filter(data >= ymd("2020-05-03"))
  } 
  
  #Let's define an index for every time and an index for every area-time:
  df <- df %>% 
    mutate(
      idarea1 = idarea,
      idtime = as.numeric(factor(data)),
      idtime1 = idtime,
      idareatime = 1:nrow(df)
    ) %>% 
    rename("n" = str_glue("n_{outcome}"), "exp" = str_glue("exp_{outcome}"), "Texp" = str_glue("Texp_{outcome}"))
  
  # For defining the structure matrix and the constraints we will need to define the following variables:
  s <- length(unique(df$idarea))
  t <- length(unique(df$idtime))
  
  #Define the temporal structure matrix of a RW1
  D1 <- diff(diag(t),differences=1)
  Rt <- t(D1)%*%D1
  
  #Defining the diagonal matrix as an sparse matrix we can store it in an efficient way (if not, we can't not even store it in a system with 16GB RAM). We can talk about it in the TFM.
  R <- kronecker(Rt,Diagonal(s)) #Sparse kronecker matrix
  r <- s
  #Define the constraints
  A <- kronecker(matrix(1,1,t), diag(s))
  A <- A[-1,]
  e <- rep(0,s-1)
  
  #Scale the covariates
  df <- df %>% 
    mutate_at(c("isc", "poblacio_exempta_de_copagament_farmaceutic", "poblacio_amb_rendes_inferiors_a_18_000_euros", "poblacio_amb_nivell_dinstruccio_insuficient", "taxa_de_mortalitat_prematura", "hospitalitzacions_evitables"), scale)
  
  var_mod <- case_when(
    var == "urban" ~ "urban +",
    var == "si" ~ "urban + isc +",
    var == "int" ~ "urban/isc +",
    var == "si_comp" ~ "urban + poblacio_exempta_de_copagament_farmaceutic + poblacio_amb_rendes_inferiors_a_18_000_euros + poblacio_amb_nivell_dinstruccio_insuficient + taxa_de_mortalitat_prematura + hospitalitzacions_evitables +",
    var == "si2" ~ "urban + isc + I(isc^2) +"
  )
  
  
  formula <- as.formula(str_glue('n ~ {var_mod}
    f(idarea, model = "bym2", graph = g, hyper = pc_prior, constr = TRUE) +
    f(idtime, model = "rw1", hyper=list(prec=list(prior=sdunif)), constr = TRUE) +
    f(idareatime,
      model = "generic0", Cmatrix=R, rankdef=r,
      hyper=list(prec=list(prior=sdunif)),
      constr = TRUE, extraconstr=list(A=A, e=e))'))
  
  print(formula)
  
  set.seed(342)
  
  if(effect == "sir") {
    mod <- inla(formula, family="poisson", data=df, E=exp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
  } else {
    mod <- inla(formula, family="poisson", data=df, E=Texp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
    
  }
  
  return(mod)
  
}


res_sae_raw <- ndat_sae_II %>% 
  mutate(
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic),
    hp = map(res, function(x) {
      
      est <- summary(x)$fixed %>% 
        as.data.frame() %>% 
        mutate(est = str_glue("{round(exp(mean), 2)} ({round(exp(`0.025quant`), 2)}, {round(exp(`0.975quant`), 2)})")) %>%
        rownames_to_column(var = "var") %>% 
        dplyr::select(var, est)
      
      hp <- x$summary.hyperpar %>% 
        as.data.frame() %>% 
        mutate(
          id = row_number(),
          est = case_when(
            id == 2 ~ str_glue("{round(mean, 2)} ({round(`0.025quant`, 2)}, {round(`0.975quant`, 2)})"),
            TRUE ~ str_glue("{round(1/sqrt(mean), 2)} ({round(1/sqrt(`0.025quant`), 2)}, {round(1/sqrt(`0.975quant`), 2)})")
          )
        ) %>% 
        dplyr::select(est) %>% 
        tibble::rownames_to_column(var = "var")
      
      rbind(est, hp)
    }
    ),
    var = map(res, ~as.data.frame(inla.hyperpar.sample(10000, .x)) %>% 
                dplyr::select(contains("Precision")) %>% 
                mutate_all(~1/.x) %>% 
                mutate(tvar = rowSums(.),
                       `Precision for idarea` = `Precision for idarea`/tvar,
                       `Precision for idtime` = `Precision for idtime`/tvar,
                       `Precision for idareatime` = `Precision for idareatime`/tvar
                ) %>% 
                summarise(across(-tvar, ~round(mean(.x)*100, 2)))
    )
  ) %>% 
  dplyr::select(-res)


ndat_sae_urban <- tibble(expand.grid(outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = map2(outcomes, effect, ~inla_mod_covar(dat, outcome = .x, effect = .y, var = "urban"))
  )

res_sae_urban <- ndat_sae_urban %>% 
  mutate(
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic),
    hp = map(res, function(x) {
      
      est <- summary(x)$fixed %>% 
        as.data.frame() %>% 
        mutate(est = str_glue("{round(exp(mean), 2)} ({round(exp(`0.025quant`), 2)}, {round(exp(`0.975quant`), 2)})")) %>%
        rownames_to_column(var = "var") %>% 
        dplyr::select(var, est)
      
      hp <- x$summary.hyperpar %>% 
        as.data.frame() %>% 
        mutate(
          id = row_number(),
          est = case_when(
            id == 2 ~ str_glue("{round(mean, 2)} ({round(`0.025quant`, 2)}, {round(`0.975quant`, 2)})"),
            TRUE ~ str_glue("{round(1/sqrt(mean), 2)} ({round(1/sqrt(`0.025quant`), 2)}, {round(1/sqrt(`0.975quant`), 2)})")
          )
        ) %>% 
        dplyr::select(est) %>% 
        tibble::rownames_to_column(var = "var")
      
      rbind(est, hp)
    }
    ),
    var = map(res, ~as.data.frame(inla.hyperpar.sample(10000, .x)) %>% 
                dplyr::select(contains("Precision")) %>% 
                mutate_all(~1/.x) %>% 
                mutate(tvar = rowSums(.),
                       `Precision for idarea` = `Precision for idarea`/tvar,
                       `Precision for idtime` = `Precision for idtime`/tvar,
                       `Precision for idareatime` = `Precision for idareatime`/tvar
                ) %>% 
                summarise(across(-tvar, ~round(mean(.x)*100, 2)))
    )
  ) %>% 
  dplyr::select(-res)

ndat_sae_si <- tibble(expand.grid(outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = map2(outcomes, effect, ~inla_mod_covar(dat, outcome = .x, effect = .y, var = "si"))
  )

res_sae_si <- ndat_sae_si %>% 
  mutate(
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic),
    hp = map(res, function(x) {
      
      est <- summary(x)$fixed %>% 
        as.data.frame() %>% 
        mutate(est = str_glue("{round(exp(mean), 2)} ({round(exp(`0.025quant`), 2)}, {round(exp(`0.975quant`), 2)})")) %>%
        tibble::rownames_to_column(var = "var") %>% 
        dplyr::select(var, est)
      
      hp <- x$summary.hyperpar %>% 
        as.data.frame() %>% 
        mutate(
          id = row_number(),
          est = case_when(
            id == 2 ~ str_glue("{round(mean, 2)} ({round(`0.025quant`, 2)}, {round(`0.975quant`, 2)})"),
            TRUE ~ str_glue("{round(1/sqrt(mean), 2)} ({round(1/sqrt(`0.025quant`), 2)}, {round(1/sqrt(`0.975quant`), 2)})")
          )
        ) %>% 
        dplyr::select(est) %>% 
        tibble::rownames_to_column(var = "var")
      
      rbind(est, hp)
    }
    ),
    var = map(res, ~as.data.frame(inla.hyperpar.sample(10000, .x)) %>% 
                dplyr::select(contains("Precision")) %>% 
                mutate_all(~1/.x) %>% 
                mutate(tvar = rowSums(.),
                       `Precision for idarea` = `Precision for idarea`/tvar,
                       `Precision for idtime` = `Precision for idtime`/tvar,
                       `Precision for idareatime` = `Precision for idareatime`/tvar
                ) %>% 
                summarise(across(-tvar, ~round(mean(.x)*100, 2)))
    )
  ) %>% 
  dplyr::select(-res)

ndat_sae_si <- tibble(expand.grid(outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = map2(outcomes, effect, ~inla_mod_covar(dat, outcome = .x, effect = .y, var = "si"))
  )

ndat_sae_int <- tibble(expand.grid(outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = map2(outcomes, effect, ~inla_mod_covar(dat, outcome = .x, effect = .y, var = "int"))
  )

res_sae_int <- ndat_sae_int %>% 
  mutate(
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic),
    hp = map(res, function(x) {
      
      est <- summary(x)$fixed %>% 
        as.data.frame() %>% 
        mutate(est = str_glue("{round(exp(mean), 2)} ({round(exp(`0.025quant`), 2)}, {round(exp(`0.975quant`), 2)})")) %>%
        tibble::rownames_to_column(var = "var") %>% 
        dplyr::select(var, est)
      
      hp <- x$summary.hyperpar %>% 
        as.data.frame() %>% 
        mutate(
          id = row_number(),
          est = case_when(
            id == 2 ~ str_glue("{round(mean, 2)} ({round(`0.025quant`, 2)}, {round(`0.975quant`, 2)})"),
            TRUE ~ str_glue("{round(1/sqrt(mean), 2)} ({round(1/sqrt(`0.025quant`), 2)}, {round(1/sqrt(`0.975quant`), 2)})")
          )
        ) %>% 
        dplyr::select(est) %>% 
        tibble::rownames_to_column(var = "var")
      
      rbind(est, hp)
    }
    ),
    var = map(res, ~as.data.frame(inla.hyperpar.sample(10000, .x)) %>% 
                dplyr::select(contains("Precision")) %>% 
                mutate_all(~1/.x) %>% 
                mutate(tvar = rowSums(.),
                       `Precision for idarea` = `Precision for idarea`/tvar,
                       `Precision for idtime` = `Precision for idtime`/tvar,
                       `Precision for idareatime` = `Precision for idareatime`/tvar
                ) %>% 
                summarise(across(-tvar, ~round(mean(.x)*100, 2)))
    )
  ) %>% 
  dplyr::select(-res)

ndat_sae_si_comp <- tibble(expand.grid(outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = map2(outcomes, effect, ~inla_mod_covar(dat, outcome = .x, effect = .y, var = "si_comp"))
  )

res_sae_si_comp <- ndat_sae_si_comp %>% 
  mutate(
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic),
    hp = map(res, function(x) {
      
      est <- summary(x)$fixed %>% 
        as.data.frame() %>% 
        mutate(est = str_glue("{round(exp(mean), 2)} ({round(exp(`0.025quant`), 2)}, {round(exp(`0.975quant`), 2)})")) %>%
        tibble::rownames_to_column(var = "var") %>% 
        dplyr::select(var, est)
      
      hp <- x$summary.hyperpar %>% 
        as.data.frame() %>% 
        mutate(
          id = row_number(),
          est = case_when(
            id == 2 ~ str_glue("{round(mean, 2)} ({round(`0.025quant`, 2)}, {round(`0.975quant`, 2)})"),
            TRUE ~ str_glue("{round(1/sqrt(mean), 2)} ({round(1/sqrt(`0.025quant`), 2)}, {round(1/sqrt(`0.975quant`), 2)})")
          )
        ) %>% 
        dplyr::select(est) %>% 
        tibble::rownames_to_column(var = "var")
      
      rbind(est, hp)
    }
    ),
    var = map(res, ~as.data.frame(inla.hyperpar.sample(10000, .x)) %>% 
                dplyr::select(contains("Precision")) %>% 
                mutate_all(~1/.x) %>% 
                mutate(tvar = rowSums(.),
                       `Precision for idarea` = `Precision for idarea`/tvar,
                       `Precision for idtime` = `Precision for idtime`/tvar,
                       `Precision for idareatime` = `Precision for idareatime`/tvar
                ) %>% 
                summarise(across(-tvar, ~round(mean(.x)*100, 2)))
    )
  ) %>% 
  dplyr::select(-res)

save(res_sae_raw, file = "Data/2_Analysed/Covariates/res_sae_raw.Rda")

save(ndat_sae_urban, file = "Data/2_Analysed/Covariates/ndat_sae_urban.Rda")
save(res_sae_urban, file = "Data/2_Analysed/Covariates/res_sae_urban.Rda")

save(ndat_sae_si, file = "Data/2_Analysed/Covariates/ndat_sae_si.Rda")
save(res_sae_si, file = "Data/2_Analysed/Covariates/res_sae_si.Rda")

save(ndat_sae_int, file = "Data/2_Analysed/Covariates/ndat_sae_int.Rda")
save(res_sae_int, file = "Data/2_Analysed/Covariates/res_sae_int.Rda")

save(ndat_sae_si_comp, file = "Data/2_Analysed/Covariates/ndat_sae_si_comp.Rda")
save(res_sae_si_comp, file = "Data/2_Analysed/Covariates/res_sae_si_comp.Rda")

#Quadratic effect of the Socio-economic index on cases
ndat_sae_si_quad <- tibble(expand.grid(outcomes = c("cas"), effect = c("sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = map2(outcomes, effect, ~inla_mod_covar(dat, outcome = .x, effect = .y, var = "si2"))
  )

res_sae_si_quad <- ndat_sae_si_quad %>% 
  mutate(
    dic = map_dbl(res, ~.x$dic$dic),
    waic = map_dbl(res, ~.x$waic$waic),
    hp = map(res, function(x) {
      
      est <- summary(x)$fixed %>% 
        as.data.frame() %>% 
        mutate(est = str_glue("{round(exp(mean), 2)} ({round(exp(`0.025quant`), 2)}, {round(exp(`0.975quant`), 2)})")) %>%
        tibble::rownames_to_column(var = "var") %>% 
        dplyr::select(var, est)
      
      hp <- x$summary.hyperpar %>% 
        as.data.frame() %>% 
        mutate(
          id = row_number(),
          est = case_when(
            id == 2 ~ str_glue("{round(mean, 2)} ({round(`0.025quant`, 2)}, {round(`0.975quant`, 2)})"),
            TRUE ~ str_glue("{round(1/sqrt(mean), 2)} ({round(1/sqrt(`0.025quant`), 2)}, {round(1/sqrt(`0.975quant`), 2)})")
          )
        ) %>% 
        dplyr::select(est) %>% 
        tibble::rownames_to_column(var = "var")
      
      rbind(est, hp)
    }
    ),
    var = map(res, ~as.data.frame(inla.hyperpar.sample(10000, .x)) %>% 
                dplyr::select(contains("Precision")) %>% 
                mutate_all(~1/.x) %>% 
                mutate(tvar = rowSums(.),
                       `Precision for idarea` = `Precision for idarea`/tvar,
                       `Precision for idtime` = `Precision for idtime`/tvar,
                       `Precision for idareatime` = `Precision for idareatime`/tvar
                ) %>% 
                summarise(across(-tvar, ~round(mean(.x)*100, 2)))
    )
  ) %>% 
  dplyr::select(-res)

save(ndat_sae_si_quad, file = "Data/2_Analysed/Covariates/ndat_sae_si_quad.Rda")
save(res_sae_si_quad, file = "Data/2_Analysed/Covariates/res_sae_si_quad.Rda")

#--------Effect of vaccination---------
#We include the cumulative fully vaccination percentage because of the argument given in https://stats.stackexchange.com/questions/425055/time-varying-covariates-in-longitudinal-mixed-effect-models

# if you just include smoking as a time-varying covariate in your mixed model, then you have a type of cross-sectional relationship, namely, you say that the cognition at time 𝑡is only associated with smoking at the same time point 𝑡. But it could be that the cognition at 𝑡 is also associated with smoking at previous time points. For example, cognition at 𝑡 depends not only on whether you smoke at time 𝑡 but rather on how much you have smoked up to 𝑡. In this case, you will need to construct a new time-varying covariate which is the cumulative smoking.

#We will pass the defined parameters to the function directly
inla_mod_vac <- function(df, outcome = "cas", effect = "sir", lag = 1, vac = TRUE, covar = TRUE) {

  print(outcome)
  print(effect)
  
  #Let's define an index for every time and an index for every area-time:
  df <- df %>% 
    mutate(
      idarea1 = idarea,
      idtime = as.numeric(factor(data)),
      idtime1 = idtime,
      idareatime = 1:nrow(df)
    ) %>% 
    rename("n" = str_glue("n_{outcome}"), "exp" = str_glue("exp_{outcome}"), "Texp" = str_glue("Texp_{outcome}"))
  
  # For defining the structure matrix and the constraints we will need to define the following variables:
  s <- length(unique(df$idarea))
  t <- length(unique(df$idtime))
  
  #Define the temporal structure matrix of a RW1
  D1 <- diff(diag(t),differences=1)
  Rt <- t(D1)%*%D1
  
  #Defining the diagonal matrix as an sparse matrix we can store it in an efficient way (if not, we can't not even store it in a system with 16GB RAM). We can talk about it in the TFM.
  R <- kronecker(Rt,Diagonal(s)) #Sparse kronecker matrix
  r <- s
  #Define the constraints
  A <- kronecker(matrix(1,1,t), diag(s))
  A <- A[-1,]
  e <- rep(0,s-1)
  
  #Scale the covariates
  df <- df %>% 
    mutate_at(c("isc", "rate_vac", "lag_rate_vac1", "lag_rate_vac2"), scale)
  
  covar_mod <- case_when(
    covar ~ str_glue("urban + isc +"),
    TRUE ~  ""
  )
  
  vac_mod <- case_when(
    vac ~ str_glue("lag_rate_vac{lag} +"),
    TRUE ~ ""
  )
  
  formula <- as.formula(str_glue('n ~ {covar_mod} {vac_mod} 
    f(idarea, model = "bym2", graph = g, hyper = pc_prior, constr = TRUE) +
    f(idtime, model = "rw1", hyper=list(prec=list(prior=sdunif)), constr = TRUE) +
    f(idareatime,
      model = "generic0", Cmatrix=R, rankdef=r,
      hyper=list(prec=list(prior=sdunif)),
      constr = TRUE, extraconstr=list(A=A, e=e))'))
  
  print(formula)
  set.seed(342)
  
  if(effect == "sir") {
    #config=TRUE to sample from the posterior distribution
    mod <- inla(formula, family="poisson", data=df, E=exp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
  } else {
    mod <- inla(formula, family="poisson", data=df, E=Texp, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
    
  }
  
  return(mod)
  
}

#All observed vaccinated data (for shiny app COVIDCAT_EVO)
ndat_vac <- tibble(data_min = ymd("2021-01-31"),
               data_max = ymd("2022-07-24")) %>% 
  mutate(
    df = map2(data_min, data_max, function(x, y) {
      range_data <- seq(x, y, by = 1)
      
      sdat <- tibble(data = range_data) %>% 
        mutate(
          wday = wday(data)
        ) %>% 
        #Filter sundays
        filter(wday == 1) %>% 
        dplyr::select(-wday) %>% 
        mutate(
          obs = map(data, ~res_out(.x))
        ) %>% 
        unnest(obs)
    })
  ) %>% 
  dplyr::select(df) %>% 
  unnest(df) %>% 
  mutate(
    rate_vac = rate_vac*100
  ) %>% 
  dplyr::select(data, abs, n_vac, N, rate_vac) 

save(ndat_vac, file = "Data/2_Analysed/Vaccination/ndat_vac.Rda")

#We have fully vaccinated data since 2020-28-12 but is not until the week that ends in 2021-01-24 in which there are more than 5000 individuals. We have to take one week more to incorporate the lag (we start in the week that ends in the 2021-01-31). the 3th and 4th wave will end in the sunday that begins the 5th wave (2021-06-13). Also for the 5th wave we will let one week pass so because it starts in 2021-06-13 we will start in the week that starts in 2021-06-20 and ends in 2021-06-27

ndat <- rbind(
  tibble(
    data_min = ymd(c("2021-01-31", "2021-01-31", "2021-06-27")),
    data_max = ymd(c("2021-11-02", "2021-06-14", "2021-11-02")),
    wave = c("All", "3rd-4th wave", "5th wave"),
    edat = FALSE
  ),
  tibble(
    data_min = ymd(c("2021-01-31")),
    data_max = ymd(c("2021-06-14")),
    wave = c("3rd-4th wave"),
    edat = TRUE
  )
) %>% 
  mutate(
    df = pmap(list(data_min, data_max, edat), function(x, y, z) {
      
      #We take one week less to be able to calculate the lagged vaccination and one day less to take in account that it's < data_max
      range_data <- seq(x - 7, y - 1, by = 1)
      
      sdat <- tibble(data = range_data) %>% 
        mutate(
          wday = wday(data)
        ) %>% 
        #Filter sundays
        filter(wday == 1) %>% 
        dplyr::select(-wday) %>% 
        mutate(
          obs = map(data, ~res_out(.x, edat = z))
        ) %>% 
        unnest(obs) %>% 
        dplyr::select(data, codi_abs, abs, idarea, N:sir_hosp)
      
      #Lag vaccination data
      sdat <- sdat %>% 
        group_by(codi_abs) %>% 
        mutate(
          lag_rate_vac1 = lag(rate_vac),
          lag_rate_vac2 = lag(rate_vac, 2)
        ) %>% 
        ungroup() %>% 
        #remove first week:
        filter(data > data[1])
      
      #Add covariates to the data:
      
      sdat <- sdat %>% 
        left_join(dat_covar, by = "codi_abs") %>% 
        dplyr::select(-N.y) %>% 
        rename(N = N.x)
      
      sdat
      
    })
  )

#Save the data of the population older than 70 years for all the period
dat_70 <- ndat %>% 
  filter(wave == "3rd-4th wave", edat) %>% 
  pull(df)
dat_70 <- dat_70[[1]]

save(dat_70, file = "Data/dat_70.Rda")

ndat_sae_vac <- tibble(expand.grid(period = c("All", "3rd-4th wave", "5th wave"), outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = pmap(list(period, outcomes, effect), function(x, y, z) {
      
      sdf <- ndat %>% 
        filter(wave == x, !edat)
      
      inla_mod_vac(df = sdf$df[[1]], outcome = y, effect = z)
      
    })
  )

#Save the model
save(ndat_sae_vac, file = "Data/2_Analysed/Vaccination/ndat_sae_vac.Rda")

#Let's filter >= 70 years old. We will only calculate it for the 3rd-4th wave that is when the majority of the aged >=70 years old are fully vaccinated.
ndat_sae_70 <- tibble(expand.grid(period = c("3rd-4th wave"), outcomes = c("hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = pmap(list(period, outcomes, effect), function(x, y, z) {
      
      sdf <- ndat %>% 
        filter(wave == x, edat)
      
      inla_mod_vac(df = sdf$df[[1]], outcome = y, effect = z)
      
    })
  )

ndat_sae_70_raw <- tibble(expand.grid(period = c("3rd-4th wave"), outcomes = c("hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = pmap(list(period, outcomes, effect), function(x, y, z) {
      
      sdf <- ndat %>% 
        filter(wave == x, edat)
      
      inla_mod_vac(df = sdf$df[[1]], outcome = y, effect = z, vac=FALSE, covar=FALSE)
      
    })
  )

#Save the model
save(ndat_sae_70, file = "Data/2_Analysed/Vaccination/ndat_sae_70.Rda")
save(ndat_sae_70_raw, file = "Data/2_Analysed/Vaccination/ndat_sae_70_raw.Rda")

#Population aged over 70 with 2-weeks lags
ndat_sae_70_lag2 <- tibble(expand.grid(period = c("3rd-4th wave"), outcomes = c("hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = pmap(list(period, outcomes, effect), function(x, y, z) {
      
      sdf <- ndat %>% 
        filter(wave == x, edat)
      
      inla_mod_vac(df = sdf$df[[1]], outcome = y, effect = z, lag = 2)
      
    })
  )

save(ndat_sae_70_lag2, file = "Data/2_Analysed/Vaccination/ndat_sae_70_lag2.Rda")

#Let's estimate the raw model:
ndat_sae_raw <- tibble(expand.grid(period = c("All", "3rd-4th wave", "5th wave"), outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = pmap(list(period, outcomes, effect), function(x, y, z) {
      
      sdf <- ndat %>% 
        filter(wave == x, !edat)
      
      inla_mod_vac(df = sdf$df[[1]], outcome = y, effect = z, vac=FALSE, covar=FALSE)
      
    })
  )

#Save the model
save(ndat_sae_raw, file = "Data/2_Analysed/Vaccination/ndat_sae_raw.Rda")

ndat_sae_vac_nocovar <- tibble(expand.grid(period = c("All", "3rd-4th wave", "5th wave"), outcomes = c("hosp"), effect = c("sir", "sir2"))) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(
    res = pmap(list(period, outcomes, effect), function(x, y, z) {

      sdf <- ndat %>%
        filter(wave == x, !edat)

      inla_mod_vac(df = sdf$df[[1]], outcome = y, effect = z, covar=FALSE)

    })
  )

#Save the model
save(ndat_sae_vac_nocovar, file = "Data/2_Analysed/Vaccination/ndat_sae_vac_nocovar.Rda")


ndat_sae_vac_lag2 <- tibble(expand.grid(period = c("All", "3rd-4th wave", "5th wave"), outcomes = c("cas", "hosp"), effect = c("sir", "sir2"))) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    res = map2(period, outcomes, function(x, y) {
      
      sdf <- ndat %>% 
        filter(wave == x, !edat)
      
      inla_mod_vac(df = sdf$df[[1]], outcome = y, effect = "sir", lag = 2)
      
    })
  )

save(ndat_sae_vac_lag2, file = "Data/2_Analysed/Vaccination/ndat_sae_vac_lag2.Rda")