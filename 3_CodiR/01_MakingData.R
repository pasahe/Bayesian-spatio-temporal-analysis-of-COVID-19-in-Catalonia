rm(list=ls())
########################
#Llibreries 
########################
library(dplyr) # per extreure un subset de un data frame (p.e. mtcars)
library(tidyr)
library(lubridate)
library(stringr)
library(RSocrata)
library(tibble)
library(purrr)
library(forcats)
library(janitor)
library(raster) #Funció aggregate
library(maptools) #Funció spRbind
library(rmapshaper)
library(readxl)
########################

setwd('P:/TFM')
########################

## Directoris
########################
dades<-"./2_Dades/1_Originals"
dades_ana<-"./2_Dades/2_Analisi"
codi<-"./3_CodiR"

# #Read data from portal dades obertes (one first time)
# # https://governobert.gencat.cat/ca/dades_obertes/inici/
# 
# #Cases data by ABS (https://analisi.transparenciacatalunya.cat/Salut/Registre-de-casos-de-COVID-19-a-Catalunya-per-rea-/xuwf-dxjd)
# #No more updates at 2022-07-26
# #Initial date: 2020-03-01
# #Final date: 2022-07-25
# dat_cas <- read.socrata(
#   "https://analisi.transparenciacatalunya.cat/resource/xuwf-dxjd.json",
#   email     = "psatorra@idibell.cat",
#   password  = "Pau.satorra24"
# ) %>% 
#   #Variables format transformation
#   mutate(
#     data = ymd(data),
#     numcasos = as.integer(numcasos)
#   ) %>% 
#   as_tibble()
# 
# #Cases data by age group (https://analisi.transparenciacatalunya.cat/resource/qwj8-xpvk)
# #No more updates at 2022-07-29
# #Initial date: 2020-03-01
# #Final date: 2022-07-25
# #We will use this file to get the covid cases for each age group in the global of Catalonia
# dat_edat <- read.socrata(
#   "https://analisi.transparenciacatalunya.cat/resource/qwj8-xpvk.json",
#   email     = "psatorra@idibell.cat",
#   password  = "Pau.satorra24"
# ) %>% 
#   mutate(
#     data = ymd(data),
#     numcasos = as.integer(numcasos)
#   ) %>% 
#   as_tibble()
# 
# #Hospitalization data by ABS (https://analisi.transparenciacatalunya.cat/Salut/COVID-19-Persones-hospitalitzades/hzw2-sfyd)
# #Keeps updating weekly (at which time?)
# #Initial date: 2020-04-27
# #Final date: 2023-01-29
# dat_hosp <- read.socrata(
#   "https://analisi.transparenciacatalunya.cat/resource/hzw2-sfyd.json",
#   email     = "psatorra@idibell.cat",
#   password  = "Pau.satorra24"
# ) %>% 
#   #Variables format transformation
#   mutate_at(c("data_inici", "data_final"), ymd) %>% 
#   mutate_at(c("setmana_epidemiologica", "any", "casos", "poblacio"), as.integer) %>% 
#   as_tibble()
# 
# #Vaccination data by ABS. The number of the dose is specified (https://analisi.transparenciacatalunya.cat/Salut/Vacunaci-per-al-COVID-19-dosis-administrades-per-r/tp23-dey4)
# #Takes 30 min aprox (> 4M registers. For each group of the 16 age groups and each brand...)
# #No more updates at 2022-07-26
# #Initial date: 2020-12-27
# #Final date: 2022-07-25
# dat_vac <- read.socrata(
#   "https://analisi.transparenciacatalunya.cat/resource/tp23-dey4.json",
#   email     = "psatorra@idibell.cat",
#   password  = "Pau.satorra24"
# ) %>% 
#   #Variables format transformation
# mutate(
#   data = ymd(data),
#   dosi = as.integer(dosi),
#   recompte = as.integer(recompte)
# ) %>%
# as_tibble()
# 
# #No entenc quin sentit té que surti no vacunats (persones que al final no es vacunen però van a la cita?)

# #Extra data:
# 
# #Vaccination at one fixed time (https://analisi.transparenciacatalunya.cat/Salut/Vacunaci-per-al-COVID-19-persones-vacunades-per-re/wmsw-k3nu)
# #No more updates at 2022-07-26
# #Only one date: 2022-07-25
# dat_vac2 <- read.socrata(
#   "https://analisi.transparenciacatalunya.cat/resource/wmsw-k3nu.json",
#   email     = "psatorra@idibell.cat",
#   password  = "Pau.satorra24"
# ) %>% 
#   #Variables format transformation
#   mutate(
#     data = ymd(data)
#   ) %>% 
#   mutate_at(c("poblacio_diana", "vacunacio_iniciada", "vacunacio_completada"), as.integer)
# 
# 
# #Data linking covid outcomes and vaccination (https://analisi.transparenciacatalunya.cat/Salut/Impacte-del-COVID-19-per-estat-de-vacunaci-/6izj-g3sb)
# #No more updates at 2022-07-26
# #Initial date: 2021-01-01
# #Final date: 2022-07-25
# dat_out <- read.socrata(
#   "https://analisi.transparenciacatalunya.cat/resource/6izj-g3sb.json",
#   email     = "psatorra@idibell.cat",
#   password  = "Pau.satorra24"
# ) %>% 
#   #Variables format transformation
#   mutate(
#     data = ymd(data),
#     recompte = as.integer(recompte)
#   ) %>% 
#   as_tibble()


# #Save the data
# 
# save(dat_cas,file=file.path(dades, "dat_cas.Rda"))
# save(dat_edat,file=file.path(dades, "dat_edat.Rda"))
# save(dat_hosp,file=file.path(dades, "dat_hosp.Rda"))
# save(dat_vac,file=file.path(dades, "dat_vac.Rda"))
# 
# #Extra:
# save(dat_out,file=file.path(dades, "Extra/dat_out.Rda"))
# save(dat_vac2,file=file.path(dades, "Extra/dat_vac2.Rda"))

#Load the data and process it
load(file.path(dades, "dat_cas.Rda"))
load(file.path(dades, "dat_edat.Rda"))
load(file.path(dades, "dat_hosp.Rda"))
load(file.path(dades, "dat_vac.Rda"))

#Population by ABS 
#We read first the population by ABS
#I don't know where did I get the data. There is one file in the open data portal with the population updated to 2023 but there're some abs that are not found in the data (https://analisi.transparenciacatalunya.cat/en/Salut/Registre-central-de-poblaci-del-CatSalut-poblaci-p/ftq4-h9vk). In the data we read here only one ABS in the data is not found in the register
#-Filter by 2020 (the age of the pandemic)
#-There is one ABS in the data that is not found here (CASTELLBISBAL)
pob_abs <- read.csv(file.path(dades, "Registre_central_de_poblaci__del_CatSalut.csv"), encoding = "UTF-8") %>%
  subset(any==2020) %>%
  dplyr::select(codi_abs = "codi.Àrea.Bàsica.de.Saut", sexe="gènere", edat , N="població.oficial") %>% 
  mutate(sexe = factor(sexe, levels = c("Dona", "Home")),
         edat = cut(edat, breaks = c(min(edat), seq(10, 90, 10), max(edat)), labels = sort(unique(dat_edat$edat)), right = FALSE, include.lowest = TRUE),
         edat = as.character(edat)
  )

#Group by: abs, sexe, age
pob_abs <- pob_abs %>% 
  group_by(codi_abs, sexe, edat) %>% 
  summarise(N = sum(N)) %>% 
  ungroup()

#Transform data

#Cases data:
#Calculate the total for Catalonia first
Tdat_cas <- dat_cas %>% 
  group_by(data) %>% 
  summarise(n = sum(numcasos))

#-We will exclude rows with missing ABS
#-ABS in CASTELLBISBAL have to be assigned to MARTORELL because CASTELLBISBAL is not found in the population data
#-We will consider every type of case as a covid case not only PCR
dat_cas <- dat_cas %>% 
  filter(absdescripcio != "No classificat") %>% 
  mutate(sexedescripcio = factor(sexedescripcio, levels = c("Dona", "Home")),
         abscodi = ifelse(abscodi == "399", "149", abscodi),
         absdescripcio = ifelse(absdescripcio == "CASTELLBISBAL", "MARTORELL", absdescripcio)
         )

#Group data by: sex, abs
dat_cas <- dat_cas %>% 
  group_by(data, abscodi, absdescripcio, sexedescripcio) %>% 
  summarise(numcasos = sum(numcasos)) %>% 
  ungroup() %>% 
  dplyr::select(data, codi_abs = abscodi, abs = absdescripcio, sexe = sexedescripcio, n = numcasos)

#Let's take only the code & name of the abs for the correspondence
code_name <- dat_cas %>% 
  distinct(codi_abs, abs) %>% 
  arrange(codi_abs)

#Cases data by age group:
#-As we're interested in the total of the cases we won't exclude missing regions. 
#-We have to exclude missing age groups for the standardization of the covid cases
dat_edat <- dat_edat %>% 
  filter(edatrang != "No classificat") %>% 
  mutate(sexedescripcio = factor(sexedescripcio, levels = c("Dona", "Home")))

#Group data by: sex, age
dat_edat <- dat_edat %>% 
  group_by(data, sexedescripcio, edatrang) %>% 
  summarise(numcasos = sum(numcasos)) %>% 
  ungroup() %>% 
  dplyr::select(data, sexe = sexedescripcio, edat = edatrang, n = numcasos)

#Hospitalization data
#Calculate the total for Catalonia first
Tdat_hosp <- dat_hosp %>% 
  group_by(data_final) %>% 
  summarise(n = sum(casos)) %>% 
  rename(data = data_final)

#-We will exclude rows with missing ABS. 
#-All ABS found in dat_cas are in dat_hosp. There is only one ABS (404: TERRASSA H) that is not found in the incidence database.
#-ABS in CASTELLBISBAL have to be assigned to MARTORELL because CASTELLBISBAL is not found in the population data
#-The names of the ABS are different from the incidence database. Some are cut... We will only keep the code and will put the name found in the incidence database. But first we have to put TERRASSA H to TERRASSA E (https://emap.terrassa.cat/mapserver/ambits/img_ambits/divisio_sanitaria.pdf)
#-We will transform the groups of age in dat_vac and the groups of age in dat_hosp to be homogeneous
# Groups of age: 0-14, 15-44, 45-59, 60-69, 70-79, 80+
dat_hosp <- dat_hosp %>% 
  filter(nom_abs != "No disponible") %>% 
  mutate(grup_edat = factor(grup_edat, exclude = "No disponible"),
         grup_edat = fct_collapse(grup_edat,
                                  "0-14" = c("0", "1 i 2", "3 i 4", "5 a 14"),
                                  "15-44" = "15 a 44",
                                  "45-59" = "45 a 59",
                                  "60-69" = "60 a 69",
                                  "70-79" = "70 a 79",
                                  "80+" = "80 o més"
         ),
         sexe = factor(sexe, levels = c("Dona", "Home")),
         index_socioeconomic = factor(index_socioeconomic),
         #TERRASSA H to TERRASSA E
         codi_abs = ifelse(codi_abs == "404", "251", codi_abs),
         #CASTELLBISBAL to MARTORELL
         codi_abs = ifelse(codi_abs == "399", "149", codi_abs)
  )


#Group data by: sex, age, abs. The socioeconomic index is the same for each of the ABS so we won't group because we can add it later
#We discard the population column (we will get the population with another file)
#The difference between data_inicial and data_final is always of 6 days so there is no need on keeping data_inicial. Keeping data_final is like having the cumulative incidence for 7 days. We don't have also to know the epidemiologic week
dat_hosp <- dat_hosp %>% 
  group_by(data_final, codi_abs, sexe, grup_edat) %>% 
  summarise(casos = sum(casos)) %>% 
  ungroup() %>% 
  dplyr::select(data = data_final, codi_abs, sexe, edat = grup_edat, n = casos)

#Input the name of the ABS for each code:
dat_hosp <- dat_hosp %>% 
  left_join(code_name, by = "codi_abs") %>% 
  relocate(abs, .after = codi_abs)

#Vaccination data:
#Calculate the total for Catalonia first for the second dose
Tdat_vac <- dat_vac %>%
  filter(is.na(no_vacunat), dosi == 2 | fabricant == "J&J / Janssen") %>% 
  group_by(data) %>% 
  summarise(n = sum(recompte))


#-We will only keep the second dose or the J&J shots
#-We will exclude rows with missing ABS. 
#-ABS in CASTELLBISBAL have to be assigned to MARTORELL because CASTELLBISBAL is not found in the population data
#-The information about the ABS is the same as the incidence database. We will take both the code and the name
#-We will exclude rows with no vaccination?¿
#-We will transform the groups of age in dat_vac and the groups of age in dat_hosp to be homogeneous
# Groups of age: 0-14, 15-44, 45-59, 60-69, 70-79, 80+

dat_vac <- dat_vac %>% 
  filter(abs != "No classificat", is.na(no_vacunat), dosi == 2 | fabricant == "J&J / Janssen") %>% 
  mutate(edat = factor(edat, exclude = "No classificat"),
         edat = fct_collapse(edat,
                             "0-14" = c("0 a 11", "12 a 14"),
                             "15-44" = c("15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44"),
                             "45-59" = c("45 a 49", "50 a 54", "55 a 59"),
                             "60-69"= c("60 a 64", "65 a 69"),
                             "70-79" = c("70 a 74", "75 a 79"),
                             "80+" = "80 o més"
         ),
         sexe = factor(sexe, levels = c("Dona", "Home")),
         abs_codi = ifelse(abs_codi == "399", "149", abs_codi),
         abs = ifelse(abs == "CASTELLBISBAL", "MARTORELL", abs)
  )

#Group data by: sex, age, abs
#We have to take in account the order of the grouping variables according to the desired arrangement
dat_vac <- dat_vac %>% 
  group_by(data, abs_codi, abs, sexe, edat) %>% 
  summarise(recompte = sum(recompte)) %>% 
  ungroup() %>% 
  dplyr::select(data, codi_abs = abs_codi, abs, sexe, edat, n = recompte)


#Load the shapefile with the geographical spatial information about ABS.
#2022 ABS have some discordances with the ABS in the data, we will not use it
#The 2018 ABS has few discordances. Only Montcada i Reixac - 1 and Montcada i Reixac - 2 are in the shapefileT and not in the incidence database, because they were MONTCADA I REIXAC one unique ABS.
shapefileT <- rgdal::readOGR(file.path(dades,"ABS_2018/ABS_2018.shp"), stringsAsFactors = FALSE, encoding = "UTF-8",use_iconv = T)

#Name of àrea column:
shapefileT@data <- shapefileT@data %>% 
  rename("area"="NA.")

#Read it without encoding to keep the name of the health region:
NOMRS <- rgdal::readOGR(file.path(dades,"ABS_2018/ABS_2018.shp"), stringsAsFactors = FALSE)@data %>% 
  mutate(
    NOMRS = gsub(".*Sanitària ", "", NOMRS)
  ) %>% 
  pull(NOMRS)

shapefileT$NOMRS <- NOMRS

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

#Add CASTELLBISBAL to MARTORELL (population data doesn't have CASTELLBISBAL ABS)
shapefileT_data<-shapefileT@data %>%
  subset(CODIABS!=399)

shapefileT_data<-rbind(shapefileT_data[shapefileT_data$CODIABS!=149,],shapefileT_data[shapefileT_data$CODIABS==149,])

shapefileT_data$OBJECTID <- 1:dim(shapefileT_data)[1]

rownames(shapefileT_data)<-shapefileT_data$OBJECTID

shapefileT_mart<-raster::aggregate(shapefileT[shapefileT$CODIABS%in%c(399,149),],dissolve=T)
shapefileT<-shapefileT[!shapefileT$CODIABS%in%c(399,149),]
shapefileT <- spChFIDs(shapefileT, as.character(1:dim(shapefileT@data)[1]))
shapefileT_mart <- spChFIDs(shapefileT_mart, as.character(dim(shapefileT@data)[1]+1))
shapefileT<-spRbind(shapefileT,shapefileT_mart)

shapefileT<-SpatialPolygonsDataFrame(shapefileT,shapefileT_data)

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
dat_rs <- tibble(NOMRS = c("Alt Pirineu i Aran", "Barcelona", "Camp de Tarragona", "Catalunya Central", "Girona", "Lleida", "Terres de l'Ebre"),
                 RS_Coord_X = c(1.0590430746862542, 2.1686826624984032, 1.2437547185899736, 1.8171257308528628, 2.82142419452458, 0.6193317361640661, 0.5904418049666795),
                 RS_Coord_Y = c(42.38909981282361, 41.38749532401533, 41.11937617162244, 41.72416580711079, 41.97988965443885, 41.61795750939373, 40.730342147982014)
)

#Add them to the shapefile:

shapefileT@data <- shapefileT@data %>% 
  left_join(dat_rs, by = c("NOMRS"))

#Save data for analysis
save(pob_abs, file = file.path(dades_ana, "pob_abs.Rda"))

save(Tdat_cas, file = file.path(dades_ana, "Tdat_cas.Rda"))
save(dat_cas, file = file.path(dades_ana, "dat_cas.Rda"))

save(dat_edat, file = file.path(dades_ana, "dat_edat.Rda"))

save(Tdat_hosp, file = file.path(dades_ana, "Tdat_hosp.Rda"))
save(dat_hosp, file = file.path(dades_ana, "dat_hosp.Rda"))

save(Tdat_vac, file = file.path(dades_ana, "Tdat_vac.Rda"))
save(dat_vac, file = file.path(dades_ana, "dat_vac.Rda"))

#Simplify shapefileT in order to take less to compile
shapefileT <- shapefileT %>% ms_simplify(keep=0.01,keep_shapes=TRUE)
save(shapefileT, file = file.path(dades_ana, "shapefileT.Rda"))
save(shapefileT, file = "5_Productes/COVIDCAT_Evo/shapefileT.Rda")

#Extra data for analysis:

#Socieconomic data by ABS (http://observatorisalut.gencat.cat/ca/observatori-desigualtats-salut/indicadors_comunitaria/)
#We have to categorize the synthetic score to be able to interpet it. We will follow the described categories in the page 18 of https://observatorisalut.gencat.cat/web/.content/minisite/observatorisalut/observatori_desigualtats/comunitaria/guia_informe_salut_abs_indicadors_octubre2021.pdf:
# "Per a l’anàlisi de resultats s’han definit 6 categories de nivell socioeconòmic segons
# el valor de l’IST: molt baix (menor de 75), baix (de 75 a 90), mitjà baix (de 90 a 100), mitjà alt
# (de 100 a 110), alt (de 110 a 125) i molt alt (major de 125)."

#- Year: 2018
#- We have several indicators by ABS. We are interested only in the socieconomic index

dat_se <- read_excel(file.path(dades, "Extra/Indicadors_ABS_format-pla_2018.xlsx")) %>%
  filter(Indicador == "Índex socioeconòmic territorial 2018 (ABS)") %>%
  dplyr::select("codi_abs" = "Codi ABS", "index_socioeconomic" = "ABS total") %>%
  mutate(index_socioeconomic = as.numeric(index_socioeconomic),
         codi_abs = case_when(
           codi_abs < 10 ~ str_glue("00{codi_abs}"),
           codi_abs < 100 ~ str_glue("0{codi_abs}"),
           TRUE ~ str_glue("{codi_abs}")
         ),
         #Put Montcada i Reixac - 1 and Montcada i Reixac - 2 as Montcada i Reixac
         codi_abs = ifelse(codi_abs %in% c("381", "382"), "302", codi_abs),
         #Put Castellbisbal to Martorell
         codi_abs = ifelse(codi_abs %in% "399", "149", codi_abs),
  ) %>%
  #There're some codi_abs that is not from 2018 and we don't have the se. Also, there is one ABS that we don't have the shapefile (057 - Barcelona 8A) and also the total of Catalonia
  filter(!is.na(index_socioeconomic), !is.na(codi_abs), codi_abs != "057") %>%
  group_by(codi_abs) %>%
  #Mean of the ABS that have been unified (Montcada i Reixac and Castellbisbal with Martorell)
  summarise(
    index_socioeconomic = mean(index_socioeconomic, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  #Categorize it
  mutate(
    index_socioeconomic_cat = case_when(
      index_socioeconomic < 75 ~ 1,
      index_socioeconomic < 90 ~ 2,
      index_socioeconomic < 100 ~ 3,
      index_socioeconomic < 110 ~ 4,
      index_socioeconomic < 125 ~ 5,
      TRUE ~ 6
    ),
    index_socioeconomic_cat = factor(index_socioeconomic_cat, levels = 1:6, labels = c("Very low", "Low", "Middle low", "Middle high", "High", "Very high"))
  )

save(dat_se, file = file.path(dades_ana, "dat_se.Rda"))
save(dat_se, file = "5_Productes/COVIDCAT_Evo/dat_se.Rda")

# #Mobility data by ABS for each month from March-November 2020 (https://flowmap.blue/1BORBjX0JHOycm5MeflAkTU6ZT2maS7X_ilXdgVCxF9E/5098cd4?v=41.389359%2C2.146799%2C10.93%2C0%2C0&a=1&as=1&b=1&bo=75&c=1&ca=1&d=1&fe=1&lt=1&lfm=ALL&col=DarkMint&f=50)
# ndat_mobi <- tibble(mes = 3:11) %>% 
#   mutate(df = map(mes, ~read_excel(file.path(dades, "Extra/FlowMap Catalonia Public Mobility.xlsx"), sheet = .x) %>% 
#                     mutate_at(c("origin", "dest"), 
#                               ~case_when(
#                                 .x < 10 ~ str_glue("00{.x}"),
#                                 .x < 100 ~ str_glue("0{.x}"),
#                                 #Put Montcada i Reixac - 1 and Montcada i Reixac - 2 as Montcada i Reixac
#                                 .x %in% c(381, 382) ~ "302",
#                                 #Put Castellbisbal to Martorell
#                                 .x == 399 ~ "149",
#                                 TRUE ~ str_glue("{.x}")
#                                 )
#                     )
#                   )
#          )
# save(ndat_mobi, file = file.path(dades_ana, "ndat_mobi.Rda"))

# Creation of the file of all the relevant restrictions taking place in Catalonia with the initial date and final date. Also, I add some puntual events that might be relevant to know. I have to add the date of the detection of the first case of each variant. 
# I get the data from:
# Very complete chronology but only of the year 2020 (https://es.wikipedia.org/wiki/Anexo:Cronolog%C3%ADa_de_la_pandemia_de_COVID-19_en_Espa%C3%B1a)
# Good chronology of Catalonia to complement the last one. Specially since the final of 2020. In 2021 it begins to have less and less information (https://ca.wikipedia.org/wiki/Pand%C3%A8mia_de_COVID-19_a_Catalunya)
# I complemented the missing information searching those missing information in google (references in the column of source)
# Also, for national imposed restrictions the data in the oxford github was useful (https://www.bsg.ox.ac.uk/research/covid-19-government-response-tracker). We get first the latest_responses file as we want each policy with the start and the end (https://github.com/OxCGRT/covid-policy-tracker/tree/master/data)
# dat_rest <- read.csv(file.path(dades, "Extra/OxCGRT_nat_latest_responses.csv"), encoding = "UTF-8") %>% 
#   as_tibble() %>% 
#   filter(CountryName == "Spain") %>% 
#   mutate(
#     StartDate = as_date(as.character(StartDate)),
#     EndDate = as_date(as.character(EndDate)),
#     #If missing put the final date
#     EndDate = case_when(
#       is.na(EndDate) ~ dmy("24/07/2022"),
#       TRUE ~ EndDate
#     )
#   ) %>% 
#   #Remove those policies that might not be relevant
#   filter(!PolicyType %in% c("H1: Public information campaigns"))

#Let's read the created file with all the restrictions to place in the app
dat_rest <- read_excel(file.path(dades, "Extra/restrictions_CAT.xlsx")) %>%
  #Filter those rows that at the moment we won't put
  filter(!grepl("\\?$", Description)) %>%
  mutate(
    #Transform to date
    start = as_date(start),
    end = as_date(end),
    #Put the next sunday in those rows that end is missing
    start_wday = wday(start),
    end = case_when(
      is.na(end) ~ start + (8-start_wday),
      TRUE ~ end
    ),
    #Put the icon for each type of restriction
    icon = case_when(
      type =="Certificate" ~ '<i class="fa-solid fa-file-signature"></i>',
      type =="Concert" ~ '<i class="fa-solid fa-guitars"></i>',
      type =="Diagnostics" ~ '<i class="fa-solid fa-vial-virus"></i>',
      type =="Elections" ~ '<i class="fa-solid fa-ballot"></i>',
      type =="Leisure" ~ '<i class="fa-solid fa-martini-glass-citrus"></i>',
      type =="Lockdown" ~ '<i class="fa-sharp fa-solid fa-lock"></i>',
      type =="Masks" ~ '<i class="fa-solid fa-mask-face"></i>',
      type =="Reunion" ~ '<i class="fa-solid fa-people-group"></i>',
      type =="School" ~ '<i class="fa-solid fa-school"></i>',
      type =="Vaccine" ~ '<i class="fa-solid fa-syringe"></i>'
    )
  ) %>%
  dplyr::select(-start_wday, -sources, -...6)

save(dat_rest, file = file.path(dades_ana, "dat_rest.Rda"))
save(dat_rest, file = "5_Productes/COVIDCAT_Evo/dat_rest.Rda")

#Percentage of population >65 years and population density per square kilometre of land area in each ABS
# I didn't find any treshold levels for population density nor percentage of >65 years population so we will use rounded beautified quantile points to categorize them
dat_demo <- read.csv(file.path(dades, "Registre_central_de_poblaci__del_CatSalut.csv"), encoding = "UTF-8") %>%
  subset(any==2020) %>%
  dplyr::select(codi_abs = "codi.Àrea.Bàsica.de.Saut", sexe="gènere", edat , N="població.oficial") %>% 
  mutate(N_65 = ifelse(edat >= 65, N, 0)) %>%
  group_by(codi_abs) %>% 
  summarise(N_65 = sum(N_65),
            N = sum(N)) %>% 
  full_join(
    shapefileT@data[,c("codi_abs", "area")],
    by = "codi_abs"
  ) %>% 
  mutate(
    #% 65 population:
    perc_65 = N_65*100/N,
    perc_65_cat = case_when(
      perc_65 < 17 ~ 1,
      perc_65 < 20 ~ 2,
      perc_65 < 22 ~ 3,
      TRUE ~ 4
    ),
    perc_65_cat = factor(perc_65_cat, levels = 1:4, labels = c("<17", "17-20", "20-22", ">22")),
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
save(dat_demo, file = file.path(dades_ana, "dat_demo.Rda"))
save(dat_demo, file = "5_Productes/COVIDCAT_Evo/dat_demo.Rda")
