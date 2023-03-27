# rm(list=ls())
library(shinydashboard)
library(dplyr) 
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(kableExtra)
library(tibble)
library(maptools)
library(RColorBrewer)
library(scales) #gradient color
library(leaflet) #map
library(leaflet.extras)
library(ggrepel)
library(Hmisc) #Per a la funció capitalize
library(plotly)
library(htmlwidgets)
library(shinycssloaders)
library(tidyr)
library(shinyFeedback)
library(stringr)
library(lubridate)

# setwd("P:/TFM/5_Productes/COVIDCAT_Evo")
# 
# rsconnect::deployApp('P:/TFM/5_Productes/COVIDCAT_Evo',account="ubidi",forceUpdate = T)

source("outcomeModule.R")

#### UI ####
ui <- tagList(
  useShinyFeedback(),
  #Load font-awesome icons:
  tags$script(src = "https://kit.fontawesome.com/c699d591dc.js"),
  #Load CSS styles with font specification, etc.
  includeCSS("HTML/styles.css"),
  #to put the logo of IDIBELL in the header
  tags$head(
    tags$script(type="text/javascript", src = "code.js")
  ), 
  navbarPage( 
    windowTitle = "COVIDCAT Evolution",
    title=div(
      tags$span(style="font-size:30px;margin-left:10px;margin-top:-10px","COVIDCAT"),
      img(
        src = "covid.png",
        height = 50,
        width = 70,
        style = "margin-top:-15px"
    )
    ),
    id="navbar",
    tabPanel("Cases", outcomeModuleUI("cas")),
    tabPanel("Hospitalization", outcomeModuleUI("hosp")),
    tabPanel("Vaccination", outcomeModuleUI("vac"))
)
)


#### Server ####

server <- function(input, output){
  
  #### Load data ####
  load("dat_sae.Rda")
  load("shapefileT.Rda")
  load("dat_cat.Rda")
  load("dat_rest.Rda")
  load("dat_se.Rda")
  
  outcome <- reactiveVal(NULL)
  
  observeEvent(input$navbar, {
    outcome(case_when(
      input$navbar == "Cases" ~ "cas",
      input$navbar == "Hospitalization" ~ "hosp",
      input$navbar == "Vaccination" ~ "vac"
    ))
  })
  
  callModule(outcomeModule, "cas", reactive({outcome()}), dat_sae, dat_cat, shapefileT, dat_rest, dat_se)
  callModule(outcomeModule, "hosp", reactive({outcome()}), dat_sae, dat_cat, shapefileT, dat_rest, dat_se)
  callModule(outcomeModule, "vac", reactive({outcome()}), dat_sae, dat_cat, shapefileT, dat_rest, dat_se)
  
    
}


shinyApp(ui, server)

