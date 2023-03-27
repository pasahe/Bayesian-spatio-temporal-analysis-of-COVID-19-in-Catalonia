#SAE with INLA tutorial
# http://www.paulamoraga.com/tutorial-areal-data-es/
#ECDC country dashboard
# https://www.ecdc.europa.eu/en/covid-19/country-overviews
#ECDC guidelines for thresholds
# https://www.ecdc.europa.eu/sites/default/files/documents/RRA-15th-update-June%202021.pdf
#WHO guidelines (covid-19 indicator thresholds)
# https://www.who.int/publications/i/item/considerations-in-adjusting-public-health-and-social-measures-in-the-context-of-covid-19-interim-guidance
#UK dashboard
# https://coronavirus.data.gov.uk/
#CAT dashboard
# https://dadescovid.cat/

#We will take a period of 7 days for the cumulative window period as we would like to show the evolution in every week. 7 days is the minimum we can take as for the hospitalization it's already given in a weekly basis. Also presenting the results weekly we have more random fluctuation so the need of a sae estimation method gains sense

#For the cases we will use the case incidence measured as new confirmed cases per 100k population per week
#For the hospitalization we will use the hospitalization rate measured as New COVID- 19 hospitalizations per 100k population per week

#WHO indicated about these indicators:
#Consider averaging over a two-week period to minimize the effect of random fluctuations.
#Considering the smooth SAE we will guarantee that the effect of random fluctuations is not present.

#For the vaccination we will use the cumulated total covid-19 vaccine doses administred per 100 people updated every week.


rm(list=ls())

library(dplyr)
library(tidyr)
library(INLA)
library(spdep)
library(lubridate)
library(stringr)
library(purrr)
library(zoo)
library(tsibble)
setwd('P:/TFM')

dades <- "./2_Dades/1_Originals"
dades_ana <- "./2_Dades/2_Analisi"
codi <- "./3_CodiR"

#---Load data----
load(file = file.path(dades_ana, "pob_abs.Rda"))
load(file = file.path(dades_ana, "Tdat_cas.Rda"))
load(file = file.path(dades_ana, "dat_cas.Rda"))
load(file = file.path(dades_ana, "dat_edat.Rda"))
load(file = file.path(dades_ana, "Tdat_hosp.Rda"))
load(file = file.path(dades_ana, "dat_hosp.Rda"))
load(file = file.path(dades_ana, "Tdat_vac.Rda"))
load(file = file.path(dades_ana, "dat_vac.Rda"))
load(file = file.path(dades_ana, "shapefileT.Rda"))

#Population in the total of Catalonia by sex and age (to calculate the standarized incidence of cases)
Tpob_abs <- pob_abs %>% 
  group_by(sexe, edat) %>% 
  summarise(NT = sum(N))

#Incidence in the total of Catalonia by sex and age (to calculate the standarized incidence of hospitalization)
Tdat_edat <- dat_edat %>% 
  group_by(sexe, edat) %>% 
  filter(!is.na(sexe)) %>% 
  summarise(NT = sum(n))


sf::sf_use_s2(FALSE)

#Parameters to make INLA work
nb <- poly2nb(shapefileT)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
shapefileT$re_u <- 1:nrow(shapefileT@data)
sdunif = "expression: logdens=log(0.5)-log_precision/2; return(logdens);"

#INLA models
inla_mod <- function(sf, outcome = "cas"){
  
  sf@data <- sf@data %>% 
    rename("n" = str_glue("n_{outcome}"), "exp" = str_glue("exp_{outcome}"))
  
  #If there are no cases (the hospitalization and vaccination doesn't begin at the initial cases date)
  if(!all(sf$n == 0)){
    #INLA model for the smooth ratio observed/expected (SIR)
    formula = n ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)), cdf=c(log(1)))
    
    set.seed(342)
    res <- inla(formula, family="poisson", data=sf@data, E=exp, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))), control.fixed = list(prec.intercept = 0.01))
    
    res_sir <- res$summary.fitted.values %>% 
      dplyr::select("rr" = "mean", "rr_lci" = "0.025quant", "rr_uci" = "0.975quant", "p" = contains("cdf"))
    
    #INLA model for the smooth incidence tax
    if(outcome == "cas"){
      
      formula = n ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)), cdf=c(log(0.01),log(0.005)))
      
      set.seed(342)
      res <- inla(formula, family="poisson", data=sf@data, E=N, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE, cdf=c(log(0.005),log(0.0025))), control.fixed = list(prec.intercept = 0.01))
      res_srate <- res$summary.fitted.values %>% 
        dplyr::select("srate" = "mean", "srate_lci" = "0.025quant", "srate_uci" = "0.975quant", contains("cdf"))
      names(res_srate)[grepl("cdf", names(res_srate))] <- c("p2", "p3")
      
    }else{
      
      formula = n ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)))
      
      set.seed(342)
      res <- inla(formula, family="poisson", data=sf@data, E=N, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE), control.fixed = list(prec.intercept = 0.01))
      res_srate <- res$summary.fitted.values %>% 
        dplyr::select("srate" = "mean", "srate_lci" = "0.025quant", "srate_uci" = "0.975quant")
      
    }

    res <- cbind("codi_abs" = sf$codi_abs, res_sir, res_srate)
    
  }else{
    
    if(outcome == "cas"){
      res <- sf@data %>% 
        dplyr::select(codi_abs) %>% 
        mutate(rr = 0, rr_lci = 0, rr_uci = 0, p = 0, srate = 0, srate_lci = 0, srate_uci = 0, p2 = 0, p3 = 0)
    }else{
      res <- sf@data %>% 
        dplyr::select(codi_abs) %>% 
        mutate(rr = 0, rr_lci = 0, rr_uci = 0, p = 0, srate = 0, srate_lci = 0, srate_uci = 0)
    }
      
  }
  
  if(outcome != "cas"){
    res <- res %>% 
      dplyr::select(-codi_abs)
  }
    
  rownames(res) <- NULL
  
  return(res %>% 
            rename_all(function(x){
              ifelse(!x %in% c("codi_abs", "p2", "p3"), str_glue("{x}_{outcome}"), x)
            })
        )
    
}

#Function that calculates the observed, expected, sir and the estimated SAE results for the 7-days window defined by the date x of each one of the outcomes
#We will consider the cases, hospitalization and vaccination incidence over the total number of people in the ABS being considered the population at risk
#We would like to consider the hospitalization/cases ratio but we don't have the age distribution of the cases by ABS... We only can do it grouping it by sex.
sae_inc <- function(x){
  
  print(x)
  
  ##Age-sex distribution of the covid cases, hospitalization, vaccination in the total of Catalonia in the 7-days window. Let's add also the population in the total of Catalonia.
  Tedat_cas <- dat_edat %>% 
    filter(data <= x, data  > (x - 7)) %>% 
    group_by(sexe, edat) %>% 
    summarise(n_cas = sum(n))
  
  Tedat_hosp <- dat_hosp %>% 
    filter(data == x) %>% 
    group_by(sexe, edat) %>% 
    summarise(n_hosp = sum(n))
  
  #7-day rate of percentage of second doses administred:
  # Tedat_vac <- dat_vac %>% 
  #   filter(data <= x, data  > (x - 7)) %>% 
  #   group_by(sexe, edat) %>% 
  #   summarise(n_vac = sum(n))
  #total rate:
  Tedat_vac <- dat_vac %>% 
    filter(data <= x) %>% 
    group_by(sexe, edat) %>% 
    summarise(n_vac = sum(n))
  
  Tedat <- Tedat_cas %>% 
    full_join(Tedat_hosp, by = c("sexe", "edat")) %>% 
    full_join(Tedat_vac, by = c("sexe", "edat")) %>% 
    full_join(Tpob_abs, by = c("sexe", "edat")) %>% 
    mutate_at(c("n_cas", "n_hosp", "n_vac"), ~ifelse(is.na(.x), 0, .x)) %>% 
    mutate(ratio_cas = n_cas/NT,
           ratio_hosp = n_hosp/NT,
           ratio_vac = n_vac/NT)
  
  #We will use the indirect standardization as we don't have the age distribution of cases for every ABS. The population of interest is every ABS and the standard population is the global of Catalonia. Article for direct/indirect methods: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3406211/
  #Indirect standardization: multiply the number of people in each group of the population of interest by the age-sex specific incidence rate in the comparable group of the reference population, giving the expected number of cases. 
  
  ##Expected cases, hospitalization and vaccination by ABS in function of the population in each ABS and age & sex group and the calculated ratio of cases:
  exp_abs <- pob_abs %>% 
    left_join(Tedat, by = c("sexe", "edat")) %>% 
    mutate(exp_cas = N*ratio_cas,
           exp_hosp = N*ratio_hosp,
           exp_vac = N*ratio_vac
           ) %>% 
    #We group them by abs as we will only take in account the ABS
    group_by(codi_abs) %>% 
    summarise_at(c("N", "exp_cas", "exp_hosp", "exp_vac"), sum)
  
  
  ##Observed cases by ABS
  cas_abs <- dat_cas %>% 
    filter(data <= x, data > (x - 7)) %>% 
    group_by(codi_abs) %>% 
    summarise(n_cas = sum(n))
  
  #Observed hospitalization by ABS
  hosp_abs <- dat_hosp %>% 
    filter(data == x) %>% 
    group_by(codi_abs) %>% 
    summarise(n_hosp = sum(n))
  
  #Observed vaccination by ABS
  vac_abs <- dat_vac %>% 
    # filter(data <= x, data > (x - 7)) %>% 
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
      sir_cas = ifelse(exp_cas == 0, 0, n_cas/exp_cas),
      sir_hosp = ifelse(exp_hosp == 0, 0, n_hosp/exp_hosp),
      sir_vac = ifelse(exp_vac == 0, 0, n_vac/exp_vac)
    )
  
  #Add the results for each ABS to the geographical information
  map<-shapefileT
  
  map@data <- map@data %>% 
    full_join(inc_abs, by = "codi_abs") 
  
  #Run inla models for every one of the outcomes:
  inla_res <- tibble(outcomes = c("cas", "hosp", "vac")) %>% 
    mutate(
      res = map(outcomes, ~inla_mod(map, .x))
    )
  
  add_res <- do.call(cbind, inla_res$res)
  
  inc_abs <- inc_abs %>% 
    left_join(add_res, by = "codi_abs")
  
  return(inc_abs)
  
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
  filter(data <= ymd("2021-05-13")) %>% 
  mutate(
    df = map(data, sae_inc)
  )
  
ndat_sae2 <- tibble(data = seq(range_data[1], range_data[2], by = 1)) %>%
  mutate(wday = wday(data)) %>% 
  #Filter mondays:
  filter(wday == 1) %>% 
  #Exclude the first one as we don't have cumulative data:
  slice(-1) %>% 
  dplyr::select(-wday) %>%
  #We divide the period by 2 because the inla program might crash
  filter(data > ymd("2021-05-13")) %>% 
  mutate(
    df = map(data, sae_inc)
  )
  
ndat_sae <- rbind(ndat_sae1, ndat_sae2)

print(proc.time()-ptm)

#Unnest and transform the data:
#We will multiply by 100k cases and hospitalization rates. We will multiply by 100 the second dose vaccination rate to have the percentage. 
#We will create a categoric version of the smooth rates in order to paint the maps. We will look at the quantiles that the variables take and also on literature.
#For the 7-day case rate the UK government takes the following breaks: 0-10, 10-50, 50-100, 100-200, 200-400, 400-800, 800-1600, +1600. We will use the same ones. In the CAT dashboard the following thresholds are used:0-15, 15-25, 25-50, 50-75, 75-100, 100-200, 200-300, 300-400, >400
#For the 7-day hospitalization rate we take a mix of the ecdc and who thresholds (ecdc <10, 10 - <25, 25 - <50, ≥50; who <5, 5 - <10, 10 - <30, 30+) that define in the guidelines: 0-5, 5-10, 10-30, 30-50, >50. 10-30 will be break into 10-15, 15-20, 20-25, 25-30 because our data demands it.
#For the percentage of second doses we can use a simple categorization of a percentage similar to the one used in ecdc (0-25, 25-50, 50-60, 60-70, 70-80, 80-90, >90) that is: 0-10, 10-25, 25-50, 50-60, 60-70, 70-80,80-90, >90
#For the total vaccine doses x 100 the world in data uses the following breaks: 0-50, 50-100, 100-150, 150-200, 200-250, 250-300, 300-350, 350-400 (https://ourworldindata.org/grapher/covid-vaccination-doses-per-capita?tab=map&time=2021-08-28). They have more doses because the period is longer.
#The WHO uses the following braks for total vaccine doses x 100 (https://covid19.who.int/): 0-20, 20-40, 40-60, 60-70, 70-100, +100
#We will use a combination of the two: 0-20, 20-40, 40-60, 60-80, 80-100, 100-150, 150-200, +200
dat_sae <- ndat_sae %>%
  unnest(df) %>% 
  mutate_at(vars(contains("rate_cas"), contains("rate_hosp")), ~.x*100000) %>% 
  mutate_at(vars(contains("rate_vac")), ~.x*100) %>% 
  mutate(
    srate_cas_cat = case_when(
      srate_cas < 10 ~ 0,
      srate_cas < 50 ~ 1,
      srate_cas < 100 ~ 2,
      srate_cas < 200 ~ 3,
      srate_cas < 400 ~ 4,
      srate_cas < 800 ~ 5,
      srate_cas < 1600 ~ 6,
      TRUE ~ 7
    ),
    srate_cas_cat = factor(srate_cas_cat, levels = 0:7, labels = c("0-10", "10-50", "50-100", "100-200", "200-400", "400-800", "800-1600", "+1600")),
    srate_hosp_cat = case_when(
      srate_hosp < 5 ~ 0,
      srate_hosp < 10 ~ 1,
      srate_hosp < 15 ~ 2,
      srate_hosp < 20 ~ 3,
      srate_hosp < 25 ~ 4,
      srate_hosp < 30 ~ 5,
      srate_hosp < 50 ~ 6,
      TRUE ~ 7
    ),
    srate_hosp_cat = factor(srate_hosp_cat, levels = 0:7, labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", ">50")),
    srate_vac_cat = case_when(
      srate_vac < 10 ~ 0,
      srate_vac < 25 ~ 1,
      srate_vac < 50 ~ 2,
      srate_vac < 60 ~ 3,
      srate_vac < 70 ~ 4,
      srate_vac < 80 ~ 5,
      srate_vac < 90 ~ 6,
      TRUE ~ 7
    ),
    srate_vac_cat = factor(srate_vac_cat, levels = 0:7, labels = c("0-10", "10-25", "25-50", "50-60", "60-70", "70-80", "80-90", "+90"))
  )


#Total of CAT

#Total population
N <- pob_abs %>% 
  summarise(N = sum(N)) %>% 
  pull(N)

#Take the period of the cases data
dat_cat <- Tdat_cas %>%
  left_join(Tdat_hosp, by = "data") %>% 
  left_join(Tdat_vac, by = "data") %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.x), 0, .x)) %>% 
  rename("n_cas" = n.x, "n_hosp" = n.y, "n_vac" = n) %>% 
  mutate(N = N,
         #7-day case incidence
         rate_cas = c(rep(NA, 6), rollapply(n_cas, 7, sum)),
         rate_cas = rate_cas*100000/N,
         #7-day hosp rate
         rate_hosp = c(rep(NA, 6), rollapply(n_hosp, 7, sum)),
         rate_hosp = rate_hosp*100000/N,
         #Total vac doses
         rate_vac = cumsum(n_vac),
         rate_vac = rate_vac*100/N
         ) %>% 
  #Remove the first 7 days
  filter(!is.na(rate_cas))
  


#### Save data ####

save(ndat_sae, file = file.path(dades_ana, "ndat_sae.Rda"))
save(dat_sae, file = file.path(dades_ana, "dat_sae.Rda"))
save(dat_sae, file = "5_Productes/COVIDCAT_Evo/dat_sae.Rda")
save(dat_cat, file = file.path(dades_ana, "dat_cat.Rda"))
save(dat_cat, file = "5_Productes/COVIDCAT_Evo/dat_cat.Rda")

