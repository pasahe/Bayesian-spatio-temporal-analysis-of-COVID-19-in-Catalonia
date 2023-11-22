#File to load data from the opendata portal and process it
rm(list=ls())
########################
#Load packages 
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

#Read data from portal dades obertes (one first time)
# https://governobert.gencat.cat/ca/dades_obertes/inici/

#To read the data one has to follow the instructions in the API documentation of the database (f.ex.: https://dev.socrata.com/foundry/analisi.transparenciacatalunya.cat/jj6z-iyrp) to read the data with the SOCRATA API. 

#Cases data by ABS (https://analisi.transparenciacatalunya.cat/Salut/Registre-de-casos-de-COVID-19-a-Catalunya-per-rea-/xuwf-dxjd)
#No more updates at 2022-07-26
#Initial date: 2020-03-01
#Final date: 2022-07-25
dat_cas <- read.socrata(
  "https://analisi.transparenciacatalunya.cat/resource/xuwf-dxjd.json",
  email     = "youremail",
  password  = "yourpasswd"
) %>%
  #Variables format transformation
  mutate(
    data = ymd(data),
    numcasos = as.integer(numcasos)
  ) %>%
  as_tibble()

#Filter by the study period (we won't consider the 2022-07-25 as it's monday and we take weeks so we take data until 2022-07-24)
dat_cas <- dat_cas %>% 
  filter(data <= ymd("2022-07-24"))

#Calculate the total for Catalonia first
Tdat_cas <- dat_cas %>% 
  group_by(data) %>% 
  summarise(n = sum(numcasos))

#-We will exclude rows with missing ABS
#-We will consider every type of case as a covid case not only PCR
dat_cas <- dat_cas %>% 
  filter(absdescripcio != "No classificat") %>% 
  mutate(sexedescripcio = factor(sexedescripcio, levels = c("Dona", "Home"))
  )

#Group data by: sex, abs
dat_cas <- dat_cas %>% 
  group_by(data, abscodi, absdescripcio, sexedescripcio) %>% 
  summarise(numcasos = sum(numcasos)) %>% 
  ungroup() %>% 
  dplyr::select(data, codi_abs = abscodi, abs = absdescripcio, sexe = sexedescripcio, n = numcasos)

#Group by week as it is the time unit of the study period
dat_cas <- dat_cas %>%
  mutate(data = factor(as.character(data), levels = as.character(seq(min(data), max(data), 1))),
         codi_abs = factor(codi_abs)) %>% 
  complete(data, codi_abs, sexe) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         data = ymd(as.character(data)),
         codi_abs = as.character(codi_abs)) %>% 
  group_by(codi_abs) %>% 
  fill(abs, .direction = "downup") %>% 
  group_by(codi_abs, sexe) %>% 
  mutate(
    n = c(rep(NA, 6), zoo::rollapply(n, 7, sum))
  ) %>% 
  mutate(wday = wday(data)) %>% 
  #Filter sundays:
  filter(wday == 1) %>% 
  #Exclude the first one as we don't have cumulative data:
  filter(data > ymd("2020-03-01")) %>% 
  dplyr::select(-wday) %>% 
  ungroup()

#Let's take only the code & name of the abs for the correspondence
code_name <- dat_cas %>% 
  distinct(codi_abs, abs) %>% 
  arrange(codi_abs)

#Cases data by age group (https://analisi.transparenciacatalunya.cat/resource/qwj8-xpvk)
#No more updates at 2022-07-29
#Initial date: 2020-03-01
#Final date: 2022-07-25
#We will use this file to get the covid cases for each age group in the global of Catalonia
dat_edat <- read.socrata(
  "https://analisi.transparenciacatalunya.cat/resource/qwj8-xpvk.json",
  email     = "youremail",
  password  = "yourpasswd"
) %>%
  mutate(
    data = ymd(data),
    numcasos = as.integer(numcasos)
  ) %>%
  as_tibble()

#-As we're interested in the total of the cases we won't exclude missing regions. 
#-We have to exclude missing age & sex groups as this data is used for the standardization of the covid cases
dat_edat <- dat_edat %>% 
  filter(edatrang != "No classificat", sexedescripcio != "No classificat") %>% 
  mutate(sexedescripcio = factor(sexedescripcio, levels = c("Dona", "Home")))

#Group data by: sex, age
dat_edat <- dat_edat %>% 
  group_by(data, sexedescripcio, edatrang) %>% 
  summarise(numcasos = sum(numcasos)) %>% 
  ungroup() %>% 
  dplyr::select(data, sexe = sexedescripcio, edat = edatrang, n = numcasos) %>% 
  mutate(
    edat = factor(edat, levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"))
  )

#Group by week as it is the time unit of the study period
dat_edat <- dat_edat %>%
  mutate(data = factor(as.character(data), levels = as.character(seq(min(data), max(data), 1)))) %>% 
  complete(data, sexe, edat) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         data = ymd(as.character(data))) %>% 
  group_by(sexe, edat) %>% 
  mutate(
    n = c(rep(NA, 6), zoo::rollapply(n, 7, sum))
  ) %>% 
  mutate(wday = wday(data)) %>% 
  #Filter sundays:
  filter(wday == 1) %>% 
  #Exclude the first one as we don't have cumulative data:
  filter(data > ymd("2020-03-01")) %>% 
  dplyr::select(-wday)

#Hospitalization data by ABS (https://analisi.transparenciacatalunya.cat/Salut/COVID-19-Persones-hospitalitzades/hzw2-sfyd)
#Keeps updating weekly (at which time?)
#Initial date: 2020-04-27
#Final date: 2023-01-29
dat_hosp <- read.socrata(
  "https://analisi.transparenciacatalunya.cat/resource/hzw2-sfyd.json",
  email     = "youremail",
  password  = "yourpasswd"
) %>%
  #Variables format transformation
  mutate_at(c("data_inici", "data_final"), ymd) %>%
  mutate_at(c("setmana_epidemiologica", "any", "casos", "poblacio"), as.integer) %>%
  as_tibble()

#Filter for the study period
dat_hosp <- dat_hosp %>% 
  #Filter for the study period (the final date of the cases and vaccination data)
  filter(data_final <= ymd("2022-07-24"))

#Calculate the total for Catalonia first
Tdat_hosp <- dat_hosp %>% 
  group_by(data_final) %>% 
  summarise(n = sum(casos)) %>% 
  rename(data = data_final)

#-We will exclude rows with missing ABS. 
#-All ABS found in dat_cas are in dat_hosp. There is only one ABS (404: TERRASSA H) that is not found in the incidence database.
#-The names of the ABS are different from the incidence database. Some are cut... We will only keep the code and will put the name found in the incidence database. But first we have to put TERRASSA H to TERRASSA E (https://emap.terrassa.cat/mapserver/ambits/img_ambits/divisio_sanitaria.pdf)
#-We will transform the groups of age in dat_vac and the groups of age in dat_hosp to be homogeneous
dat_hosp <- dat_hosp %>% 
  filter(nom_abs != "No disponible") %>% 
  mutate(grup_edat = factor(grup_edat, levels = c("0", "1 i 2", "3 i 4", "5 a 14", "15 a 44", "45 a 59", "60 a 69", "70 a 79", "80 o més")),
         #Group low ages (we don't need such a small group)
         grup_edat = fct_collapse(grup_edat, "0 a 14" = c("0", "1 i 2", "3 i 4", "5 a 14")),
         sexe = factor(sexe, levels = c("Dona", "Home")),
         index_socioeconomic = factor(index_socioeconomic),
         #TERRASSA H to TERRASSA E
         codi_abs = ifelse(codi_abs == "404", "251", codi_abs)
  ) 

#Group data by: sex, age, abs. The socioeconomic index is the same for each of the ABS so we won't group because we can add it later
#We discard the population column (we will get the population with another file)
#The difference between data_inicial and data_final is always of 6 days so there is no need on keeping data_inicial. Keeping data_final is like having the cumulative incidence for 7 days. We don't have also to know the epidemiologic week
dat_hosp <- dat_hosp %>% 
  group_by(data_final, codi_abs, sexe, grup_edat) %>% 
  summarise(casos = sum(casos)) %>% 
  ungroup() %>% 
  dplyr::select(data = data_final, codi_abs, sexe, edat = grup_edat, n = casos)

#Complete data by date, sex and age
dat_hosp <- dat_hosp %>% 
  mutate(data = factor(as.character(data), levels = as.character(seq(min(data), max(data), 7))),
         codi_abs = factor(codi_abs)) %>% 
  complete(data, codi_abs, sexe, edat) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         data = ymd(as.character(data)),
         codi_abs = as.character(codi_abs)) 

#Input the name of the ABS for each code:
dat_hosp <- dat_hosp %>% 
  left_join(code_name, by = "codi_abs") %>% 
  relocate(abs, .after = codi_abs)

#Vaccination data by ABS. The number of the dose is specified (https://analisi.transparenciacatalunya.cat/Salut/Vacunaci-per-al-COVID-19-dosis-administrades-per-r/tp23-dey4)
#Takes 30 min aprox (> 4M registers. For each group of the 16 age groups and each brand...)
#No more updates at 2022-07-26
#Initial date: 2020-12-27
#Final date: 2022-07-25
dat_vac <- read.socrata(
  "https://analisi.transparenciacatalunya.cat/resource/tp23-dey4.json",
  email     = "youremail",
  password  = "yourpasswd"
) %>%
  #Variables format transformation
mutate(
  data = ymd(data),
  dosi = as.integer(dosi),
  recompte = as.integer(recompte)
) %>%
as_tibble()

#Filter by the study period (we won't consider the 2022-07-25 as it's monday and we take weeks so we take data until 2022-07-24)
dat_vac <- dat_vac %>% 
  filter(data <= ymd("2022-07-24"))

#Calculate the total for Catalonia first for the second dose
Tdat_vac <- dat_vac %>%
  filter(is.na(no_vacunat), dosi == 2 | fabricant == "J&J / Janssen") %>% 
  group_by(data) %>% 
  summarise(n = sum(recompte))

#-We will only keep the second dose or the J&J shots
#-We will exclude rows with missing ABS. 
#-The information about the ABS is the same as the incidence database. We will take both the code and the name
#-We will exclude rows with no vaccination?¿
#-We will transform the groups of age in dat_vac and the groups of age in dat_hosp to be homogeneous
# Groups of age: 0-14, 15-44, 45-59, 60-69, 70-79, 80+

dat_vac <- dat_vac %>% 
  filter(abs != "No classificat", is.na(no_vacunat), dosi == 2 | fabricant == "J&J / Janssen") %>% 
  mutate(edat = factor(edat, levels = c("0 a 11", "12 a 14", paste(seq(15, 75, by = 5), seq(19, 79, by = 5), sep = " a "), "80 o més")),
         sexe = factor(sexe, levels = c("Dona", "Home"))
  )

#Group data by: sex, age, abs
#We have to take in account the order of the grouping variables according to the desired arrangement
dat_vac <- dat_vac %>% 
  group_by(data, abs_codi, abs, sexe, edat) %>% 
  summarise(recompte = sum(recompte)) %>% 
  ungroup() %>% 
  dplyr::select(data, codi_abs = abs_codi, abs, sexe, edat, n = recompte)

#Group by week as it is the time unit of the study period
dat_vac <- dat_vac %>%
  mutate(data = factor(as.character(data), levels = as.character(seq(min(data), max(data), 1))),
         codi_abs = factor(codi_abs)) %>% 
  complete(data, codi_abs, sexe, edat) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         data = ymd(as.character(data)),
         codi_abs = as.character(codi_abs)
  ) %>% 
  group_by(codi_abs) %>% 
  fill(abs, .direction = "downup") %>% 
  group_by(codi_abs, sexe, edat) %>% 
  mutate(
    n = c(rep(NA, 6), zoo::rollapply(n, 7, sum))
  ) %>% 
  mutate(wday = wday(data)) %>% 
  #Filter sundays:
  filter(wday == 1) %>% 
  #Exclude the first one as we don't have sufficient cumulative data (we will take the first complete week that starts at 2021-01-11):
  filter(data > ymd("2021-01-10")) %>% 
  dplyr::select(-wday) %>% 
  ungroup()

#Population by ABS 
#We read first the population by ABS
#https://analisi.transparenciacatalunya.cat/en/Salut/Registre-central-de-poblaci-del-CatSalut-poblaci-p/ftq4-h9vk
#-Filter by 2020 (the age of the pandemic)
#-There is one ABS (CASTELLBISBAL) in the data that is not found in the year 2020 but it's found in the 2021. We add it to the 2020 data as it seems that this ABS is missing not because this ABS was included in some other ABS (martorell abs aproximately the same number of population in 2020 than in 2021) but because of missing information.
#We have to group the age considering every age group for cases, hospitalization and vaccination
pob_abs <- read.socrata(
  "https://analisi.transparenciacatalunya.cat/resource/ftq4-h9vk.json",
  email     = "youremail",
  password  = "yourpasswd"
) 

pob_cast <- pob_abs %>% 
  filter(any == 2021, abs_nom == "CASTELLBISBAL") %>% 
  mutate(any = 2020)

pob_abs <- rbind(pob_abs, pob_cast) %>% 
  filter(any %in% 2020:2022) %>% 
  dplyr::select(any, codi_abs = abs_codi, sexe="genere", edat , N="poblacio_oficial") %>% 
  mutate(any = as.numeric(any), 
         sexe = factor(sexe, levels = c("Dona", "Home")),
         edat = as.numeric(edat),
         #Group age by cases age agrupation
         edat_cas = cut(edat, breaks = c(seq(0, 90, by = 10), max(edat)), labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"), right = FALSE, include.lowest = TRUE),
         #Group age by hospitalization age aggrupation
         edat_hosp = cut(edat, breaks = c(0, 15, 45, 60, 70, 80, max(edat)), labels = c("0 a 14", "15 a 44", "45 a 59", "60 a 69", "70 a 79", "80 o més"), right = FALSE, include.lowest = TRUE),
         #Group age by vaccination age agrupation
         edat_vac = cut(edat, breaks = c(0, 12, seq(15, 80, by = 5), max(edat)), labels = c("0 a 11", "12 a 14", paste(seq(15, 75, by = 5), seq(19, 79, by = 5), sep = " a "), "80 o més"), right = FALSE, include.lowest = TRUE),
         N = as.numeric(N)
  ) %>% 
  arrange(any, codi_abs, sexe, edat)

#Group by: any, abs, sexe, age
pob_abs <- pob_abs %>% 
  group_by(any, codi_abs, sexe, edat_cas, edat_hosp, edat_vac) %>% 
  summarise(N = sum(N)) %>% 
  ungroup()

#In RUBÍ-2 there is a dropdown of population from 2012-2020 to 2021 and on. This dropdown of population doesn't translate in the neighbouring areas.
# x <- pob_abs %>% filter(abs_codi %in% c("309", "310", "378", "389", "390")) %>% group_by(abs_codi, any) %>% summarise(N = sum(as.numeric(poblacio_oficial)))

#Load the shapefile with the geographical spatial information about ABS. It has been downloaded from 
#2022 ABS have some discordances with the ABS in the data, we will not use it
#The 2018 ABS has few discordances. Only Montcada i Reixac - 1 and Montcada i Reixac - 2 are in the shapefileT and not in the incidence database, because they were MONTCADA I REIXAC one unique ABS.
shapefileT <- rgdal::readOGR("Data/0_Raw/ABS_2018/ABS_2018.shp", stringsAsFactors = FALSE, encoding = "UTF-8", use_iconv = T)

#Name of àrea column:
shapefileT@data <- shapefileT@data %>% 
  rename("area"="NA.")

#Read the correspondence between the name of the region and the code:
code_name_rs <- read_excel("Data/0_Raw/code_name_rs.xlsx")

shapefileT@data <- shapefileT@data %>% 
  dplyr::select(-NOMRS) %>% 
  left_join(code_name_rs, by = "CODIRS")

#Transform the shapefileT (unite Montcada i Reixac - 1 and 2)

#Unite it in the data:
shapefileT_data <- shapefileT@data %>%
  #Remove Montcada i Reixac - 2
  subset(CODIABS != "382") %>%
  #Put Montcada i Reixac - 1 as Montcada i Reixac
  mutate(CODIABS = ifelse(CODIABS == "381", "302", CODIABS))

#Put it in the last row and recode OBJECT-ID
shapefileT_data <- rbind(shapefileT_data[shapefileT_data$CODIABS != "302", ], shapefileT_data[shapefileT_data$CODIABS == "302", ])
shapefileT_data$OBJECTID <- 1:dim(shapefileT_data)[1]
rownames(shapefileT_data) <- shapefileT_data$OBJECTID

#Unite it in the geographical information:
shapefileT_mont <- raster::aggregate(shapefileT[grep("Montcada i Reixac",shapefileT$NOMABS),], dissolve=T)
shapefileT <- shapefileT[!grepl("Montcada i Reixac",shapefileT$NOMABS),]
shapefileT <- spChFIDs(shapefileT, as.character(1:dim(shapefileT@data)[1]))
shapefileT_mont <- spChFIDs(shapefileT_mont, as.character(dim(shapefileT@data)[1]+1))
shapefileT <- spRbind(shapefileT,shapefileT_mont)

#Redefine the shapefile with the data and the geographical information transformed
shapefileT <- SpatialPolygonsDataFrame(shapefileT, shapefileT_data)

#Put the names of the ABS:
shapefileT@data <- shapefileT@data %>% 
  dplyr::select(-NOMABS) %>% 
  rename(codi_abs = CODIABS) %>% 
  left_join(code_name, by = "codi_abs") %>% 
  relocate(abs, .after = codi_abs) %>% 
  #Arrange shapefileT by OBJECTID (it's important because its the order of the geographical information)
  arrange(OBJECTID)

#Coordenates to be read with leaflet:
shapefileT <- spTransform(shapefileT, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
shapefileT@data[c("Coord_X", "Coord_Y")] <- coordinates(shapefileT)

#Coordenates regions sanitàries (google maps)
dat_coord_rs <- tibble(NOMRS = c("Alt Pirineu i Aran", "Barcelona", "Camp de Tarragona", "Catalunya Central", "Girona", "Lleida", "Terres de l'Ebre"),
                       RS_Coord_X = c(1.0590430746862542, 2.1686826624984032, 1.2437547185899736, 1.8171257308528628, 2.82142419452458, 0.6193317361640661, 0.5904418049666795),
                       RS_Coord_Y = c(42.38909981282361, 41.38749532401533, 41.11937617162244, 41.72416580711079, 41.97988965443885, 41.61795750939373, 40.730342147982014)
)

#Add them to the shapefile:

shapefileT@data <- shapefileT@data %>% 
  left_join(dat_coord_rs, by = c("NOMRS"))

#Simplify shapefileT in order to take less to compile
shapefileT <- shapefileT %>% ms_simplify(keep=0.01,keep_shapes=TRUE)

#Socioeconomic index & components (has been downloaded from https://observatorisalut.gencat.cat/ca/observatori-desigualtats-salut/dades_obertes/):
dat_se_comp <- readxl::read_excel("Data/0_Raw/Dades_indicador_socioeconomic_components_2017_xls.xlsx", skip = 1) %>%
  rename(codi_abs = idabs) %>%
  mutate(
    codi_abs = case_when(
      (floor(log10(codi_abs)) + 1) == 1 ~ str_glue("00{codi_abs}"),
      (floor(log10(codi_abs)) + 1) == 2 ~ str_glue("0{codi_abs}"),
      TRUE ~ str_glue("{codi_abs}")
    )
  ) %>% mutate(
    codi_abs = case_when(
      codi_abs == "057" ~ "059",
      codi_abs == "058" ~ "402",
      codi_abs == "060" ~ "403",
      codi_abs == "061" ~ "403",
      codi_abs == "318" ~ "394",
      TRUE ~ codi_abs
    )
  ) 

dat_se_comp_add <- dat_se_comp %>% 
  filter(codi_abs %in% c("394", "266", "036", "149")) %>% 
  mutate(codi_abs = case_when(
    codi_abs == "394" ~ "393",
    codi_abs == "266" ~ "401",
    codi_abs == "036" ~ "400",
    codi_abs == "149" ~ "399"
  ))

dat_se_comp <- rbind(dat_se_comp, dat_se_comp_add) %>% 
  group_by(codi_abs) %>% 
  dplyr::select(-ABS) %>% 
  summarise_all(mean) %>% 
  clean_names() 

#Percentage of population >65 years and population density per square kilometre of land area in each ABS
# I didn't find any treshold levels for population density nor percentage of >65 years population so we will use rounded beautified quantile points to categorize them
dat_demo <- pob_abs %>% 
  filter(any == 2020) %>%
  mutate(
    N_women = case_when(
      sexe == "Dona" ~ N,
      TRUE  ~ 0
    ),
    N_70 = case_when(
      edat_cas %in% c("70-79", "80-89", "90+") ~ N,
      TRUE  ~ 0
    )
  ) %>% 
  group_by(codi_abs) %>% 
  summarise(N = sum(N),
            N_70 = sum(N_70),
            N_women = sum(N_women)) %>% 
  full_join(
    shapefileT@data[,c("codi_abs", "area")],
    by = "codi_abs"
  ) %>% 
  mutate(
    #% women
    perc_women = N_women*100/N,
    #% 65 population:
    perc_70 = N_70*100/N,
    perc_70_cat = case_when(
      perc_70 < 12 ~ 1,
      perc_70 < 14 ~ 2,
      perc_70 < 16 ~ 3,
      TRUE ~ 4
    ),
    perc_70_cat = factor(perc_70_cat, levels = 1:4, labels = c("<12", "12-14", "14-16", ">16")),
    #Density:
    #m2 to km2
    area = area/1000000,
    dens = N/area,
    dens_cat = case_when(
      dens < 165 ~ 1,
      dens < 1720 ~ 2,
      dens < 15100 ~ 3,
      TRUE ~ 4
    ),
    dens_cat = factor(dens_cat, levels = 1:4, labels = c("<165", "165-1720", "1720-15100",">15100"))
  )

#Urban/rural ABS
#We will define an urban ABS if there're at least three ABS in the same city
dat_urban <- code_name %>% 
  mutate(
    ciutat = gsub("\\d.*", "", abs),
    ciutat = gsub("TERRASSA.*", "TERRASSA", ciutat),
    ciutat = gsub("GELTRU", "GELTRÚ", ciutat),
    ciutat = trimws(gsub("-$", "", ciutat))
  ) %>% 
  group_by(ciutat) %>% 
  mutate(n = length(abs),
         urban = case_when(
           n >= 3 ~ 1,
           TRUE ~ 0
         )
  ) %>% 
  ungroup() %>% 
  mutate(
    #Edit some areas that have 2 ABS to urban looking at its density (at least one area bigger or close to 10000)
    urban = case_when(
      ciutat %in% c("CASTELLDEFELS", "ESPLUGUES DE LLOBREGAT", "SANT ADRIÀ DEL BESÒS", "IGUALADA", "RIPOLLET", "SANT JOAN DESPÍ") ~ 1,
      TRUE ~ urban
    ),
    urban = factor(urban, levels = 0:1, labels = c("Rural", "Urban"))
  ) %>% 
  dplyr::select(codi_abs, urban)

dat_demo <- dat_demo %>% 
  left_join(dat_urban, by = "codi_abs")

#Merge covariates dat_demo & dat_se_comp
dat_covar <- dat_demo %>% 
  full_join(dat_se_comp, by = "codi_abs")

#########Process data##########
#Total of CAT

#Total population
N <- pob_abs %>% 
  group_by(any) %>% 
  summarise(N = sum(N))

#Take the period of the cases data (left_join)
dat_cat <- Tdat_cas %>%
  left_join(Tdat_hosp, by = "data") %>% 
  left_join(Tdat_vac, by = "data") %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.x), 0, .x)) %>% 
  mutate(
    any = year(data),
    wday = wday(data)
  ) %>% 
  left_join(N, by = "any") %>% 
  rename("n_cas" = n.x, "n_hosp" = n.y, "n_vac" = n) %>% 
  mutate(N = N,
         #7-day case incidence
         rate_cas = c(rep(NA, 6), zoo::rollapply(n_cas, 7, sum)),
         rate_cas = rate_cas*100000/N,
         #7-day hosp rate
         rate_hosp = c(rep(NA, 6), zoo::rollapply(n_hosp, 7, sum)),
         rate_hosp = rate_hosp*100000/N,
         #7-day vac rate
         rate_vac = c(rep(NA, 6), zoo::rollapply(n_vac, 7, sum)),
         rate_vac = rate_vac*100000/N,
         #Total vac doses
         Trate_vac = cumsum(n_vac),
         Trate_vac = Trate_vac*100/N
  ) %>% 
  filter(wday == 1) %>% 
  #Exclude the first one as we don't have cumulative data:
  slice(-1) %>% 
  dplyr::select(-any, -wday)

#Outcomes by sanitary regions:
#Similar calculation than by CAT but by every sanitary region

abs_rs <- shapefileT@data %>% 
  distinct(codi_abs, NOMRS)

#Total population by RS
N_rs <- pob_abs %>%
  left_join(abs_rs, by = "codi_abs") %>% 
  group_by(any, NOMRS) %>% 
  summarise(N = sum(N))

#Summary cases data by RS 
dat_cas_rs <- dat_cas %>% 
  left_join(abs_rs, by = "codi_abs") %>% 
  group_by(data, NOMRS) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup() %>%   
  mutate(NOMRS = factor(NOMRS)) %>% 
  complete(data, NOMRS, fill = list(n = 0))

#Summary hospitalization data by RS 
dat_hosp_rs <- dat_hosp %>% 
  left_join(abs_rs, by = "codi_abs") %>% 
  group_by(data, NOMRS) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup() %>%   
  mutate(NOMRS = factor(NOMRS)) %>% 
  complete(data, NOMRS, fill = list(n = 0))

#Summary vaccination data by RS 
dat_vac_rs <- dat_vac %>% 
  left_join(abs_rs, by = "codi_abs") %>% 
  group_by(data, NOMRS) %>% 
  summarise(
    n = sum(n)
  ) %>% 
  ungroup() %>%   
  mutate(NOMRS = factor(NOMRS)) %>% 
  complete(data, NOMRS, fill = list(n = 0))

#Take the period of the cases data
dat_rs <- dat_cas_rs %>%
  left_join(dat_hosp_rs, by = c("data", "NOMRS")) %>% 
  left_join(dat_vac_rs, by = c("data", "NOMRS")) %>% 
  mutate(
    any = year(data)
  ) %>% 
  left_join(N_rs, by = c("any", "NOMRS")) %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.x), 0, .x)) %>% 
  rename("n_cas" = n.x, "n_hosp" = n.y, "n_vac" = n) %>% 
  group_by(NOMRS) %>% 
  mutate(#7-day case incidence
         rate_cas = c(rep(NA, 6), zoo::rollapply(n_cas, 7, sum)),
         rate_cas = rate_cas*100000/N,
         #7-day hosp rate
         rate_hosp = c(rep(NA, 6), zoo::rollapply(n_hosp, 7, sum)),
         rate_hosp = rate_hosp*100000/N,
         #Total vac doses
         rate_vac = cumsum(n_vac),
         rate_vac = rate_vac*100/N
  ) %>% 
  #Remove the first 7 days
  filter(!is.na(rate_cas)) %>% 
  #Calculate weekday and filter only sundays
  mutate(wday = wday(data)) %>% 
  #Filter sundays:
  filter(wday == 1) %>% 
  dplyr::select(-any)

############Save data###############
save(pob_abs, "Data/1_Processed/pob_abs.Rda")

save(Tdat_cas, file = "Data/1_Processed/Tdat_cas.Rda")
save(dat_cas, file = "Data/1_Processed/dat_cas.Rda")

save(dat_edat, file = "Data/1_Processed/dat_edat.Rda")

save(Tdat_hosp, file = "Data/1_Processed/Tdat_hosp.Rda")
save(dat_hosp, file = "Data/1_Processed/dat_hosp.Rda")

save(Tdat_vac, file = "Data/1_Processed/Tdat_vac.Rda")
save(dat_vac, file = "Data/1_Processed/dat_vac.Rda")

#Save data for the total of Catalonia 
save(dat_cat, file = "Data/1_Processed/dat_cat.Rda")

#Save data by sanitary region
save(dat_rs, file = "Data/1_Processed/dat_rs.Rda")

#geography
save(shapefileT, file = "Data/shapefileT.Rda")

#Covariates
save(dat_se_comp, file = "Data/1_Processed/dat_se_comp.Rda")

save(dat_demo, file = "Data/1_Processed/dat_demo.Rda")

save(dat_covar, file = "Data/1_Processed/dat_covar.Rda")
