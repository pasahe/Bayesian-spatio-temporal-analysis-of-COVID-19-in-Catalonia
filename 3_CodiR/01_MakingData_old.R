# http://www.paulamoraga.com/tutorial-areal-data-es/
rm(list=ls())
########################
#Llibreries 
########################
library(epiR)
library(devtools)
library(survminer)##Grafics amb la gent a risc
library(knitr) # per utilitzar "kable"
library(dplyr) # per extreure un subset de un data frame (p.e. mtcars)
library(tidyr)
library(pander) # per fer taules amb opcions de table-layout
library(compareGroups)
library(captioner) # enumera taules i figures
library(dummy) #Serveix per colapsar variables categoriques
library(Hmisc)
library(car)
library(lubridate)
library(varhandle)
library(chron)
library(openxlsx)
library(spdep)
library(INLA)
library(stringr)
library(RJSONIO)
library(vroom)
library(raster) #Funció aggregate
library(maptools) #Funció spRbind
library(RSocrata)
library(rsconnect)
library(rmapshaper)
library(EpiEstim)
library(tibble)
library(purrr)
########################
## Funcions
########################
RutinesLocals<-"S:/5_CodiR\\1_Funcions\\rutines"
source(file.path(RutinesLocals,"table2.R"))
Funcions<-"S:/5_CodiR\\1_Funcions"
source(file.path(Funcions,"no_accents.R"))
setwd('S:/6_Projectes/2020_26COVIDCAT')
########################
## Directoris
########################
dades<-"./2_Dades/1_Originals"
dades_ana<-"./2_Dades/2_Analisi"
codi<-"./3_CodiR"

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#--------------------
#
dat <- read.socrata(
  "https://analisi.transparenciacatalunya.cat/resource/xuwf-dxjd.json",
  email     = "psatorra@idibell.cat",
  password  = "Pau.satorra24"
) %>% 
  rename(ABS=absdescripcio,CODIABS=abscodi,SEXE=sexedescripcio,TIPUS=resultatcoviddescripcio) %>% 
  #Hi ha missings de ABS, els treiem pel mapa per ABS (si volguéssim calcular el total els hauríem d'incloure)
  mutate(data=as.Date(data,format="%d/%m/%Y")+1,
         numcasos=as.integer(numcasos),
         #Fiquem els casos de castellbisbal a martorell (no tenim la població per castellbisbal...)
         ABS=ifelse(ABS=="CASTELLBISBAL","MARTORELL",ABS),
         CODIABS=ifelse(CODIABS==399,149,CODIABS)) %>%
  as.data.frame()

dat_tot <- dat %>% 
  group_by(data,SEXE) %>% 
  summarise(numcasos=sum(numcasos)) %>% 
  as.data.frame() %>% 
  pivot_wider(names_from="SEXE",values_from="numcasos") %>%
  group_by(data) %>% 
  summarise(casos=sum(Dona,`No classificat`,Home,na.rm=T),casos_h=sum(Home,na.rm=T),casos_d=sum(Dona,na.rm=T)) %>% 
  as.data.frame()

dat <- dat %>% 
  subset(ABS!="No classificat")

load(file.path(dades_ana,"dat_old.Rda"))

dat_old2 <- dat_old

print(1)

if(max(dat$data)>max(dat_old$data)){
  
dat_old<-dat

#------CREAR MAPA (comentar perquè no s'ha d'actualitzar)-----
# 
# shapefileT <- rgdal::readOGR(file.path(dades_ana,"ABS_2018/ABS_2018.shp"),stringsAsFactors = FALSE,encoding = "UTF-8",use_iconv = T)
# 
# #Agreguem Montcada i Reixac 1 i 2:
# 
# #Modifiquem el @data perquè les files del shapefileT@data han de coincidir amb els ID's dels polígons
# shapefileT_data<-shapefileT@data %>%
#   subset(CODIABS!=382) %>%
#   mutate(CODIABS=ifelse(CODIABS==381,302,CODIABS))
# shapefileT_data<-rbind(shapefileT_data[shapefileT_data$CODIABS!=302,],shapefileT_data[shapefileT_data$CODIABS=="302",])
# shapefileT_data$OBJECTID<-1:dim(shapefileT_data)[1]
# rownames(shapefileT_data)<-shapefileT_data$OBJECTID
# 
# shapefileT_mont<-raster::aggregate(shapefileT[grep("Montcada i Reixac",shapefileT$NOMABS),],dissolve=T)
# shapefileT<-shapefileT[!grepl("Montcada i Reixac",shapefileT$NOMABS),]
# shapefileT <- spChFIDs(shapefileT, as.character(1:dim(shapefileT@data)[1]))
# shapefileT_mont <- spChFIDs(shapefileT_mont, as.character(dim(shapefileT@data)[1]+1))
# shapefileT<-spRbind(shapefileT,shapefileT_mont)
# 
# shapefileT<-SpatialPolygonsDataFrame(shapefileT,shapefileT_data)
# 
# #Agreguem Castellbisbal a Martorell
# 
# shapefileT_data<-shapefileT@data %>%
#   subset(CODIABS!=399)
# 
# shapefileT_data<-rbind(shapefileT_data[shapefileT_data$CODIABS!=149,],shapefileT_data[shapefileT_data$CODIABS==149,])
# shapefileT_data$OBJECTID<-1:dim(shapefileT_data)[1]
# rownames(shapefileT_data)<-shapefileT_data$OBJECTID
# 
# shapefileT_mart<-raster::aggregate(shapefileT[shapefileT$CODIABS%in%c(399,149),],dissolve=T)
# shapefileT<-shapefileT[!shapefileT$CODIABS%in%c(399,149),]
# shapefileT <- spChFIDs(shapefileT, as.character(1:dim(shapefileT@data)[1]))
# shapefileT_mart <- spChFIDs(shapefileT_mart, as.character(dim(shapefileT@data)[1]+1))
# shapefileT<-spRbind(shapefileT,shapefileT_mart)
# 
# shapefileT<-SpatialPolygonsDataFrame(shapefileT,shapefileT_data)
# 
# names(shapefileT@data)[ncol(shapefileT@data)-1]<-"ÀREA"
# 
# sum<-dat %>%
#   group_by(ABS) %>%
#   summarise(CODIABS=unique(CODIABS))
# 
# shapefileT@data<-merge(shapefileT@data %>% dplyr::select(-NOMABS),sum,by="CODIABS",all.x=TRUE)
# 
# #Per ficar les coordenades amb el sistema que pot llegir el leaflet:
# shapefileT<-spTransform(shapefileT, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# 
# #Abans de ficar les coordenades hem d'ordenar el shapefileT per OBJECT ID!! Si no fica a cada ABS li fica coordenades d'una altra ABS
# shapefileT@data<-shapefileT@data %>%
#   arrange(OBJECTID)
# 
# shapefileT@data[c("Coord_X","Coord_Y")]<-coordinates(shapefileT)
# 
#------------------------
load(file.path(dades_ana,"shapefileT.Rda"))

dat_edat <- read.socrata(
  "https://analisi.transparenciacatalunya.cat/resource/qwj8-xpvk.json",
  email     = "psatorra@idibell.cat",
  password  = "Pau.satorra24"
) %>% 
  rename(SEXE=sexedescripcio,edat=edatrang,TIPUS=resultatcoviddescripcio) %>% 
  #Hi ha missings de ABS, els treiem pel mapa per ABS (si volguéssim calcular el total els hauríem d'incloure)
  subset(TIPUS!="Sospitós") %>% 
  mutate(data=as.Date(data,format="%d/%m/%Y")+1,
         numcasos=as.integer(numcasos)) %>%
  as.data.frame()

#----- POPULATION and age (comentar perquè no s'ha d'actualitzar)-----
# pob_abs<-vroom(file.path(dades_ana,"Registre_central_de_poblaci__del_CatSalut.csv"),delim=",") %>%
#   subset(any==2020) %>% 
#   dplyr::select(CODIABS="codi Àrea Bàsica de Saut",SEXE="gènere",edat,total="població oficial") 
# pob_abs$edat<-cut(pob_abs$edat,breaks=c(min(pob_abs$edat),seq(10,90,10),max(pob_abs$edat)),labels=unique(dat_edat$edat),right=FALSE,include.lowest=TRUE)
# pob_abs<-pob_abs %>% 
#   group_by(SEXE,CODIABS,edat) %>% 
#   summarise(total=sum(total))
#-----------------------
load(file.path(dades_ana,"pob_abs.Rda"))

pob_abs_total <- pob_abs %>% 
  group_by(SEXE) %>% 
  summarise(total=sum(total)) %>% 
  pivot_wider(names_from="SEXE",values_from="total") %>% 
  mutate(total=Dona+Home,total_h=Home,total_d=Dona) %>% 
  dplyr::select(-Dona,-Home) %>% 
  as.data.frame()

sf::sf_use_s2(FALSE)
source(file.path(codi,"smr_rr_prob2.R"))

#Càlcul R0
r0_abs<-function(x,dat){
  sdat<-dat %>% subset(ABS==x)
  #Dona
  est<-estimate_R(incid=data.frame(dates=sdat$data,I=sdat$numcasos_d),
                  method = "parametric_si",
                  config = make_config(list(mean_si = 4.7, std_si = 2.9)))
  res<-sdat %>%
    as.data.frame() %>% 
    mutate(r0_d=c(rep(NA,7),est$R[,"Mean(R)"]),
           r0L_d=c(rep(NA,7),est$R[,"Quantile.0.025(R)"]),
           r0U_d=c(rep(NA,7),est$R[,"Quantile.0.75(R)"])
    )
  
  #Home
  sdat<-dat %>% subset(ABS==x) 
  est<-estimate_R(incid=data.frame(dates=sdat$data,I=sdat$numcasos_h),
                  method = "parametric_si",
                  config = make_config(list(mean_si = 4.7, std_si = 2.9)))
  res<-res %>%
    mutate(r0_h=c(rep(NA,7),est$R[,"Mean(R)"]),
           r0L_h=c(rep(NA,7),est$R[,"Quantile.0.025(R)"]),
           r0U_h=c(rep(NA,7),est$R[,"Quantile.0.75(R)"])
    )
  #Tots
  sdat<-dat %>% subset(ABS==x) 
  est<-estimate_R(incid=data.frame(dates=sdat$data,I=sdat$numcasos),
                  method = "parametric_si",
                  config = make_config(list(mean_si = 4.7, std_si = 2.9)))
  res<-res %>%
    mutate(r0=c(rep(NA,7),est$R[,"Mean(R)"]),
           r0L=c(rep(NA,7),est$R[,"Quantile.0.025(R)"]),
           r0U=c(rep(NA,7),est$R[,"Quantile.0.75(R)"])
    )
  
  for(i in 8:nrow(sdat)){
    if(sum(replace_na(sdat$numcasos[sdat$data%in%seq(sdat$data[i]-7,sdat$data[i],1)],0)==0)>=2){
      res$r0[i]<-NA
      res$r0L[i]<-NA
      res$r0U[i]<-NA
    }
    if(sum(replace_na(sdat$numcasos_d[sdat$data%in%seq(sdat$data[i]-7,sdat$data[i],1)],0)==0)>=2){
      res$r0_d[i]<-NA
      res$r0L_d[i]<-NA
      res$r0U_d[i]<-NA
    }
    if(sum(replace_na(sdat$numcasos_h[sdat$data%in%seq(sdat$data[i]-7,sdat$data[i],1)],0)==0)>=2){
      res$r0_h[i]<-NA
      res$r0L_h[i]<-NA
      res$r0U_h[i]<-NA
    }
  }
  
  res
}

sdat<-dat %>% 
  group_by(SEXE,ABS,data) %>% 
  summarise(numcasos=sum(numcasos)) %>% 
  pivot_wider(names_from = SEXE, values_from = numcasos) %>% 
  rename("numcasos_d"=Dona,"numcasos_h"=Home) %>% 
  as.data.frame() %>%
  rowwise() %>% 
  mutate(numcasos=sum(numcasos_d,numcasos_h,na.rm=T))

#Omplir forats de les dates amb zeros:
gaps<-data.frame(ABS=rep(unique(dat$ABS),each=length(seq(min(dat$data),max(dat$data),1))),data=rep(seq(min(dat$data),max(dat$data),1),length(unique(dat$ABS))),stringsAsFactors = FALSE) %>% arrange(ABS,data) %>% 
  mutate(zerocasos=0)

sdat<-merge(sdat,gaps,all=TRUE,by=c("ABS","data")) %>% 
  dplyr::select(-zerocasos) %>% 
  mutate_at(vars(contains("numcasos")),~replace_na(.x,0)) 

res<-lapply(unique(dat$ABS),r0_abs,dat=sdat)

dat_r0<-do.call(rbind,res)

data_inici <- dmy(c("25/02/2020","01/10/2020","07/12/2020","15/03/2021","13/06/2021","02/11/2021"))
#De moment fiquem 2030 per a l'última data perquè sempre la data actual sigui més petita:
data_fi <- dmy(c("04/07/2020","06/12/2020","14/03/2021","12/06/2021","01/11/2021","25/01/2030"))

dat_r0<-dat_r0 %>% 
  mutate(reactive=ifelse(data>=data_inici[1] & data<=data_fi[1],1,ifelse(data>=data_inici[2] & data<=data_fi[2],2,ifelse(data>=data_inici[3] & data<=data_fi[3],3,ifelse(data>=data_inici[4] & data<=data_fi[4],4,ifelse(data>=data_inici[5] & data<=data_fi[5],5,ifelse(data>=data_inici[6] & data<=data_fi[6],6,NA))))))) %>% 
  # mutate(reactive=ifelse(data<="2020-06-23",1,ifelse(data<="2020-12-08",2,3))) %>% 
  #Treiem els últims 4 dies perquè es consolidi la dada:
  subset(data<=(max(data)-4))

print(2)

#------------EVOLUTION-------------------
#per tot el període
dat<-dat %>% arrange(data)
# dates<-dates[7:length(dates)]

onada <- 1:6
ndat <- enframe(onada,name=NULL,value="onada")
ndat <- ndat %>% 
  mutate(data_inici = data_inici,
         data_fi = data_fi) %>% 
  #Quan haguem de compilar només la sisena onada:
  filter(onada==6) %>%
  mutate(
         sdat = map2(data_inici,data_fi,~ dat %>% 
                       filter(data>=.x,data<=.y)),
         dates = map(sdat, ~ sort(unique(.x$data))),
         #Total:
         dates = map(dates, ~ .x[14:length(.x)]),
         dat_evo = map2(dates, sdat, function(x, y) {
           map(x, function(x2) {
             sy <- y %>% filter(data <= x2)
             sdat_casos <- smr(sy)
             ssum_casos <- rr(sdat_casos)
             ssum_casos %>%
               tibble::add_column(data=x2,
                                  .before="ABS")
             
           })
         }),
         dat_evo = map(dat_evo, function(x){
           map_df(x, rbind)
         }),
         dat_evo = map2(dat_evo, onada, ~ .x %>% 
                         mutate(reactive = 1 + 3*(.y-1))),
         #Últims 7 dies:
         dates1 = map(dates, ~ .x[.x > (max(.x)-7)]),
         dat_evo1 = map2(dates1, sdat, function(x, y) {
           map(x, function(x2) {
             sy <- y %>% filter(data <= x2, data > (x2-7))
             sdat_casos <- smr(sy)
             ssum_casos <- rr(sdat_casos)
             ssum_casos %>%
               tibble::add_column(data=x2,
                                  .before="ABS")
           })
         }),
         dat_evo1 = map(dat_evo1, function(x){
           map_df(x, rbind)
         }),
         dat_evo1 = map2(dat_evo1, onada, ~ .x %>% 
                          mutate(reactive = 2 + 3*(.y-1))),
         #Últims 14 dies:
         dates2 = map(dates, ~ .x[.x > (max(.x)-14)]),
         dat_evo2 = map2(dates2, sdat, function(x, y) {
           map(x, function(x2) {
             sy <- y %>% filter(data <= x2, data > (x2-14))
             sdat_casos <- smr(sy)
             ssum_casos <- rr(sdat_casos)
             ssum_casos %>%
               tibble::add_column(data=x2,
                                  .before="ABS")
             
           })
         }),
         dat_evo2 = map(dat_evo2, function(x){
           map_df(x, rbind)
         }),
         dat_evo2 = map2(dat_evo2, onada, ~ .x %>% 
                           mutate(reactive = 3 + 3*(.y-1)))
         )
  
dat_evo0 <- do.call(rbind,ndat$dat_evo)
dat_evo1 <- do.call(rbind,ndat$dat_evo1)
dat_evo2 <- do.call(rbind,ndat$dat_evo2)

load(file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/dat_evo.Rda")

dat_evo <- dat_evo %>% 
  subset(reactive<=15)

dat_evo <- rbind(dat_evo,dat_evo0,dat_evo1,dat_evo2) %>% 
  arrange(reactive)

#Per si més endavant peta!
save(dat_evo, file="S:/6_Projectes/2020_26COVIDCAT/2_Dades/2_Analisi/dat_evo.Rda")

#Actualitzar només sisena onada:
# dat_evo_6 <- dat_evo
# load(file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/dat_evo.Rda")
# dat_evo <- dat_evo %>% 
#   subset(reactive<=15)
# dat_evo <- rbind(dat_evo,dat_evo_6)

#Ajuntar IA7 a dat_r0

sdat_evo<-dat_evo %>% 
  filter(reactive%in%unique(dat_evo1$reactive)) %>% 
  mutate(reactive=as.numeric(as.factor(reactive))) %>% 
  dplyr::select(data,ABS,casos,casos_h,casos_d,stax,staxL,staxU,stax_h,staxL_h,staxU_h,stax_d,staxL_d,staxU_d,reactive)

dat_r0<-merge(dat_r0,sdat_evo,by=c("data","ABS","reactive"),all.x=TRUE)

print(3)


#---------------Pel total de Catalunya----------------

sdat<-dat %>% 
  group_by(SEXE,data) %>% 
  summarise(numcasos=sum(numcasos)) %>% 
  pivot_wider(names_from = SEXE, values_from = numcasos) %>% 
  rename("numcasos_d"=Dona,"numcasos_h"=Home) %>% 
  as.data.frame() %>%
  rowwise() %>% 
  mutate(numcasos=sum(numcasos_d,numcasos_h,na.rm=T)) %>% 
  mutate(ABS="Total")

#Omplir forats de les dates amb zeros:
gaps<-data.frame(ABS=rep(unique(sdat$ABS),each=length(seq(min(sdat$data),max(sdat$data),1))),data=rep(seq(min(sdat$data),max(sdat$data),1),length(unique(sdat$ABS))),stringsAsFactors = FALSE) %>% arrange(ABS,data) %>% 
  mutate(zerocasos=0)

sdat<-merge(sdat,gaps,all=TRUE,by=c("ABS","data")) %>% 
  dplyr::select(-zerocasos) %>% 
  mutate_at(vars(contains("numcasos")),~replace_na(.x,0))

res<-lapply(unique(sdat$ABS),r0_abs,dat=sdat)

tot_r0<-do.call(rbind,res) 

tot_r0 <- tot_r0 %>% 
  mutate(reactive=ifelse(data>=data_inici[1] & data<=data_fi[1],1,ifelse(data>=data_inici[2] & data<=data_fi[2],2,ifelse(data>=data_inici[3] & data<=data_fi[3],3,ifelse(data>=data_inici[4] & data<=data_fi[4],4,ifelse(data>=data_inici[5] & data<=data_fi[5],5,ifelse(data>=data_inici[6] & data<=data_fi[6],6,NA)))))))%>% 
  #Treiem els últims 4 dies perquè es consolidi la dada:
  subset(data<=(max(data)-4))


onada <- 1:6
ndatT <- enframe(onada,name=NULL,value="onada")
ndatT <- ndatT %>% 
  mutate(data_inici = data_inici,
         data_fi = data_fi
         ) %>% 
  #Quan haguem de compilar només la sisena onada:
  filter(onada==6) %>%
  mutate(
         sdat = map2(data_inici,data_fi,~ dat_tot %>% 
                       filter(data>=.x,data<=.y)),
         dates = map(sdat, ~ unique(.x$data)),
         #Total:
         dates = map(dates, ~ .x[14:length(.x)]),
         dat_evo = map2(dates, sdat, function(x, y) {
           map(x, function(x2) {
             sy <- y %>% filter(data <= x2)
             ssum_casos <- sy %>%
               summarise(casos=sum(casos),casos_h=sum(casos_h),casos_d=sum(casos_d))
             ssum_casos %>%
               tibble::add_column(data=x2,
                                  .before="casos")
             
           })
         }),
         dat_evo = map(dat_evo, function(x){
           map_df(x, rbind)
         }),
         dat_evo = map2(dat_evo, onada, ~ .x %>% 
                          mutate(reactive = 1 + 3*(.y-1))),
         #Últims 7 dies:
         dates1 = map(dates, ~ .x[.x > (max(.x)-7)]),
         dat_evo1 = map2(dates1, sdat, function(x, y) {
           map(x, function(x2) {
             sy <- y %>% filter(data <= x2, data > (x2-7))
             ssum_casos <- sy %>%
               summarise(casos=sum(casos),casos_h=sum(casos_h),casos_d=sum(casos_d))
             ssum_casos %>%
               tibble::add_column(data=x2,
                                  .before="casos")
           })
         }),
         dat_evo1 = map(dat_evo1, function(x){
           map_df(x, rbind)
         }),
         dat_evo1 = map2(dat_evo1, onada, ~ .x %>% 
                           mutate(reactive = 2 + 3*(.y-1))),
         #Últims 14 dies:
         dates2 = map(dates, ~ .x[.x > (max(.x)-14)]),
         dat_evo2 = map2(dates2, sdat, function(x, y) {
           map(x, function(x2) {
             sy <- y %>% filter(data <= x2, data > (x2-14))
             ssum_casos <- sy %>%
               summarise(casos=sum(casos),casos_h=sum(casos_h),casos_d=sum(casos_d))
             ssum_casos %>%
               tibble::add_column(data=x2,
                                  .before="casos")
             
           })
         }),
         dat_evo2 = map(dat_evo2, function(x){
           map_df(x, rbind)
         }),
         dat_evo2 = map2(dat_evo2, onada, ~ .x %>% 
                           mutate(reactive = 3 + 3*(.y-1)))
  )

dat_tot_evo <- do.call(rbind,ndatT$dat_evo)
dat_tot_evo1 <- do.call(rbind,ndatT$dat_evo1)
dat_tot_evo2 <- do.call(rbind,ndatT$dat_evo2)

dat_tot_evo <- rbind(dat_tot_evo,dat_tot_evo1,dat_tot_evo2) %>% 
  arrange(reactive)

#Primer cop:
dat_tot_evo <- dat_tot_evo %>% 
  mutate(total=pob_abs_total$total,total_h=pob_abs_total$total_h,total_d=pob_abs_total$total_d)

load(file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/tot_evo.Rda")
tot_evo <- tot_evo %>% 
  subset(reactive<=15)

tot_evo<-rbind(tot_evo,as.data.frame(tot(dat_tot_evo)))

#Actualitzar sisena onada:
# load(file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/tot_evo.Rda")
# tot_evo <- tot_evo %>% 
#   subset(reactive<=15)
# 
# dat_tot_evo<-rbind(dat_tot_evo,dat_tot_evo1,dat_tot_evo2) %>% 
#   mutate(total=pob_abs_total$total,total_h=pob_abs_total$total_h,total_d=pob_abs_total$total_d)
# 
# tot_evo<-rbind(tot_evo,as.data.frame(tot(dat_tot_evo))) 

print(4)

#Evolution:

load(file="S:/6_Projectes/2020_26COVIDCAT/2_Dades/2_Analisi/dat_evo14.Rda")

dat_evo14_old <- dat_evo14
#per tot el període
# dat<-dat %>% arrange(data)
# dates <- unique(dat$data)
# dates<-dates[14:length(dates)]

#Actualitzem l'últim mes:
dat<-dat %>% arrange(data)
dates <- unique(dat$data)
dates<-dates[(length(dates)-30):length(dates)]

dat_evo14 <- NULL
for(i in 1:length(dates)){
  x <- dates[i]
  sdat <- dat %>% filter(data <= x, data > (x-14))
  sdat_casos <- smr(sdat)
  ssum_casos <- rr(sdat_casos, per_sexe = FALSE)
  dat_evo14 <- rbind(dat_evo14, ssum_casos %>%
                       tibble::add_column(data=x,
                                          .before="ABS"))
  
}

dat_evo14_old <- dat_evo14_old %>% 
  filter(!data %in% dat_evo14$data)

dat_evo14 <- rbind(dat_evo14_old, dat_evo14)

print(5)

#---------------------Save datasets----------------
#-------FIXOS----------
#No cal anar compilant:
#Llista amb els noms de les ABS:
# write.xlsx(x=data.frame(x=unique(dat$ABS),stringsAsFactors = FALSE),file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/llista_abs.xlsx")
#MAPA:
# save(shapefileT, file="S:/6_Projectes/2020_26COVIDCAT/2_Dades/2_Analisi/shapefileT.Rda")
# shapefileT<-shapefileT %>% ms_simplify(keep=0.05,keep_shapes=TRUE)
# save(shapefileT, file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/shapefileT.Rda")
#POPULATION:
# save(pob_abs, file="C:/Users/pausa/OneDrive - IDIBELL/6_Projectes/2020_COVIDCAT/2_Dades/2_Analisi/pob_abs.Rda")
#-------------------

#----------ACTUALITZAR---------------
#Original dataset
save(dat_old, file="S:/6_Projectes/2020_26COVIDCAT/2_Dades/2_Analisi/dat_old.Rda")
save(dat_old2, file="S:/6_Projectes/2020_26COVIDCAT/2_Dades/2_Analisi/dat_old2.Rda")

#EVOLUTION:
#BY ABS
# save(dat_evo, file="C:/Users/pausa/OneDrive - IDIBELL/6_Projectes/2020_COVIDCAT/2_Dades/2_Analisi/dat_evo.Rda")
save(dat_evo, file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/dat_evo.Rda")
save(dat_r0, file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/dat_r0.Rda")
#TOTAL
# save(tot_evo, file="C:/Users/pausa/OneDrive - IDIBELL/6_Projectes/2020_COVIDCAT/2_Dades/2_Analisi/tot_evo.Rda")
save(tot_evo, file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/tot_evo.Rda")
save(tot_r0, file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/tot_r0.Rda")

#EVOLUTION:
save(dat_evo14, file="S:/6_Projectes/2020_26COVIDCAT/2_Dades/2_Analisi/dat_evo14.Rda")
save(dat_evo14, file="S:/6_Projectes/2020_26COVIDCAT/5_Productes/COVIDCAT/dat_evo14.Rda")
#------------------

rsconnect::deployApp('S:\\6_Projectes\\2020_26COVIDCAT\\5_Productes\\COVIDCAT',account="ubidi",forceUpdate = T)

print(5)

}