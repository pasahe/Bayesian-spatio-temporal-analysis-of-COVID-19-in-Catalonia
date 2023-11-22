#File to prepare data for the shiny app COVIDCAT_Evo
rm(list=ls())
########################
#Packages 
########################
library(dplyr) 
library(tidyr)
library(lubridate)
library(stringr)
library(RSocrata)
library(tibble)
library(purrr)
library(forcats)
library(janitor)
library(raster) 
library(maptools)
library(rmapshaper)
library(readxl)
########################

########################

load(file = "Data/1_Processed/dat_covar.Rda")
load(file = "Data/dat.Rda")
#We have to compile SAE.R in order to have the following data:
load(file = "Data/2_Analysed/Covariates/ndat_sae_si_comp.Rda")
load(file = "Data/2_Analysed/Covariates/res_sae_si_comp.Rda")

#Model results
res_model <- res_sae_si_comp %>% 
  filter(effect == "sir2") %>% 
  dplyr::select(outcomes, hp, var)

#spatial effect
dat_spatial <- dat %>% 
  distinct(idarea)
  
dat_rr_add <- ndat_sae_si_comp %>% 
  filter(effect == "sir2") %>% 
  mutate(
    rr = map(res, ~.x$summary.random$idarea[unique(dat$idarea),])
  ) %>% 
  dplyr::select(outcomes, rr) %>%
  unnest(rr) %>% 
  group_by(outcomes) %>% 
  mutate(
    idarea = row_number()
  ) %>% 
  ungroup() %>% 
  dplyr::select(outcomes, idarea, rr = mean, rr_lci = "0.025quant", rr_uci = "0.975quant") %>% 
  mutate_at(vars(starts_with("rr")), ~exp(.x))

dat_spatial <- dat_spatial %>% 
  full_join(dat_rr_add, by = "idarea") %>% 
  relocate(outcomes) %>% 
  arrange(outcomes, idarea)

#temporal effect
d_range_cas <- dat %>% 
  filter(!is.na(sir_cas)) %>% 
  distinct(data) %>% 
  pull(data)

d_range_hosp <- dat %>% 
  filter(!is.na(sir_hosp)) %>% 
  distinct(data) %>% 
  pull(data)

dat_temp <- ndat_sae_si_comp %>% 
  filter(effect == "sir2") %>% 
  mutate(
    rr = map(res, ~.x$summary.random$idtime)
  ) %>% 
  dplyr::select(outcomes, rr) %>% 
  unnest(rr) %>% 
  mutate(
    data = case_when(
      outcomes == "cas" ~ d_range_cas[ID],
      outcomes == "hosp" ~ d_range_hosp[ID]
    )
  ) %>% 
  dplyr::select(outcomes, data, rr = mean, rr_lci = "0.025quant", rr_uci = "0.975quant") %>% 
  mutate_if(is.numeric, ~exp(.x))


#spatio-temporal effect
dat_sptemp <- rbind(
  tibble(expand.grid(outcomes = "cas", idarea = unique(dat$idarea), data = d_range_cas)) %>% 
    mutate(ID = row_number()),
  tibble(expand.grid(outcomes = "hosp", idarea = unique(dat$idarea), data = d_range_hosp)) %>% 
    mutate(ID = row_number())
)
  
dat_rr_add <- ndat_sae_si_comp %>% 
  filter(effect == "sir2") %>% 
  mutate(
    rr = map(res, ~.x$summary.random$idareatime)
  ) %>% 
  dplyr::select(outcomes, rr) %>% 
  unnest(rr) %>% 
  dplyr::select(outcomes, ID, rr = mean, rr_lci = "0.025quant", rr_uci = "0.975quant") %>% 
  mutate_at(vars(starts_with("rr")), ~exp(.x)) 

dat_sptemp <- dat_sptemp %>% 
  full_join(dat_rr_add, by = c("outcomes", "ID")) %>% 
  relocate(outcomes) %>% 
  arrange(outcomes, ID)


#Estimated total RR
dat_rr <- rbind(
  tibble(expand.grid(outcomes = "cas", idarea = unique(dat$idarea), data = d_range_cas)) %>% 
    mutate(ID = row_number()),
  tibble(expand.grid(outcomes = "hosp", idarea = unique(dat$idarea), data = d_range_hosp)) %>% 
    mutate(ID = row_number())
)

dat_rr_add <- ndat_sae_si_comp %>% 
  filter(effect == "sir2") %>% 
  mutate(
    rr = map(res, ~.x$summary.fitted.values)
  ) %>% 
  unnest(rr) %>% 
  group_by(outcomes) %>% 
  mutate(ID = row_number()) %>%
  ungroup() %>% 
  dplyr::select(outcomes, ID, rr = mean, rr_lci = "0.025quant", rr_uci = "0.975quant") 
  
dat_rr <- dat_rr %>% 
  full_join(dat_rr_add, by = c("outcomes", "ID")) %>% 
  relocate(outcomes) %>% 
  arrange(outcomes, ID)


#Data for covariables
n_quintile <- function(x) {
  if(cur_column() == "poblacio_amb_rendes_superiors_a_100_000_euros") {
    x <- -x
  } 
  
  perc <- map_dbl(x, ~ecdf(x)(.x))
    
  case_when(
    perc < 0.2 ~ 1,
    perc < 0.4 ~ 2,
    perc < 0.6 ~ 3,
    perc < 0.8 ~ 4,
    TRUE ~ 5
  )

  
}

dat_covar <- dat_covar %>% 
  left_join(dat %>% distinct(codi_abs, abs), by = "codi_abs") %>% 
  dplyr::select(-codi_abs) %>% 
  relocate(abs) %>% 
  #Calculate privation quantile of every component
  mutate(across(poblacio_exempta_de_copagament_farmaceutic:hospitalitzacions_evitables, n_quintile, .names = "q_{.col}"))

#Save data:
save(res_model, file = "COVIDCAT_Evo/res_model.Rda")
save(dat_spatial, file = "COVIDCAT_Evo/dat_spatial.Rda")
save(dat_temp, file = "COVIDCAT_Evo/dat_temp.Rda")
save(dat_sptemp, file = "COVIDCAT_Evo/dat_sptemp.Rda")
save(dat_rr, file = "COVIDCAT_Evo/dat_rr.Rda")
save(dat_covar, file = "COVIDCAT_Evo/dat_covar.Rda")