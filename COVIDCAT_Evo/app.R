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
library(purrr)
library(rcartocolor)
library(leaflegend)
library(fmsb)
library(scales)
library(rcartocolor)
library(DT)
library(shinyBS)
library(shinyjs)

source("VacModule.R")
source("outcomeModule.R")
source("modModule.R")

#### UI ####
ui <- tagList(
  useShinyFeedback(),
  #Load font-awesome icons:
  tags$script(src = "https://kit.fontawesome.com/c699d591dc.js"),
  #Load CSS styles with font specification, etc.
  includeCSS("HTML/styles.css"),
  tags$head(
    tags$script(type="text/javascript", src = "code.js")
  ), 
  navbarPage( 
    windowTitle = "COVIDCAT-Evolution",
    title=div(
      tags$span(style="font-size:30px;margin-left:10px;margin-top:-10px","COVIDCAT-Evolution"),
      img(
        src = "covid.png",
        height = 50,
        width = 70,
        style = "margin-top:-15px"
      )
    ),
    id="navbar",
    navbarMenu(
      "Cases",
      outcomeModuleUI("cas_sptemp"),
      outcomeModuleUI("cas_spatial"),
      outcomeModuleUI("cas")
    ),
    navbarMenu(
      "Hospitalisations",
      outcomeModuleUI("hosp_sptemp"),
      outcomeModuleUI("hosp_spatial"),
      outcomeModuleUI("hosp")
    ),
    tabPanel("Vaccination",
             VacModuleUI("vac")),
    #Help
    navbarMenu("About this web",
               tabPanel("Information",
                        wellPanel(
                          HTML("<p><i> COVIDCAT Evolution version Beta 1.0 (09/11/2023)</i> </p>
                                  <h4> Authors: </h4>
                                  Pau Satorra(1), Cristian Tebé(1),
                                  <ol>
                                  <li>Germans Trias i Pujol Research Institute and Hospital (IGTP), Badalona, Barcelona, Spain</li>
                                  </ol>
                                    <h4> Contact: </h4>
                                    <b> E-mail:</b> psatorra@igtp.cat
                                   "
                          ),
                          tags$br(),
                          tags$a(
                            href = "https://github.com/pasahe/Bayesian-spatio-temporal-analysis-of-COVID-19-in-Catalonia",
                            tags$img(
                              src = "github.png",
                              title = "Bayesian spatio-temporal analysis of COVID-19 in Catalonia",
                              width = 120,
                              style = "margin-top:2%; margin-bottom:5px; margin-left: -1%;"
                            )
                          )
                        )
               ),
               tabPanel("Methodology",
                        wellPanel(titlePanel("Data"),
                                  withMathJax(HTML("
                                  <p>All data came from the official open data catalogue of the Government of Catalonia publicly available online: <a href = 'https://analisi.transparenciacatalunya.cat/en/'>Dades obertes de Catalunya</a></p>
                                  <p>We analyzed reported COVID-19 cases and hospitalisations starting from 2020-03-01, the earliest date for which cases were available, and ending on 2022-07-24, the last week for which cases were available, comprising a total of 125 epidemiological weeks. In this period, six different waves of the COVID-19 pandemic were registered.</p>"))
                        ),
                        wellPanel(titlePanel("Analysis"),
                                  withMathJax(HTML(
                                    paste0(
                                      "<p>We used the Bayesian hierarchical spatio-temporal framework to model the weekly observed counts of COVID-19 cases/hospitalisations, \\(Y_{it}\\), as follows:</p>",
                                      r"(\begin{equation}
\begin{split}
Y_{it} \mid \theta_{it} & \sim Poisson(E_i \theta_{it}) \\
\log{\theta_{it}} &= \alpha + b_i + \gamma_t + w_t + \delta_{it}
\end{split}
\end{equation}
)",
"
<p>where \\(\\alpha\\) quantifies the global risk; \\(b_i\\) is the spatial effect; \\(\\gamma_t\\) and \\(w_t\\) are the temporally structured and unstructured random effects, respectively; and \\(\\delta_{it}\\) models the spatio-temporal interaction random effect. \\(E_i\\) is the expected number that the area would have if it behaved like the general population of all Catalonia in average for the whole period. The expected counts were calculated using indirect standardization, knowing the age and sex distribution of cases/hospitalisations in the general population.</p>

<p>With this formulation, the maximum likelihood (ML) estimator of \\(\\theta_{it}\\) is given by \\(\\hat{\\theta_{it}} = Y_{it}/E_i\\) corresponding to the Standardised Incidence Ratio (SIR). Thus, the estimated \\(\\hat{\\theta_{it}}\\) is a smooth estimate of the SIR and can be interpreted as the area and week specific relative risk (RR), with respect to the global territory of Catalonia for the whole period.</p>
<p>Different models with different specifications for each of the random effects were evaluated. The best model in terms of different model selection criteria was selected. We use the Deviance Information Criterion (DIC) and the Widely Applicable Information Criterion (WAIC) as model selection criteria. The best model was the spatial and temporal structured effect modelled with BYM2 and RW1 respectively, no unstructured temporal effect and the spatio-temporal interaction effect modelled with type II.</p>
<p>To study the association with the socio-demographic characteristics of the ABS, we included them in the previous model as fixed effects:</p>
\\begin{equation}
\\log{\\theta_{it}} = \\alpha + b_i + \\gamma_t + w_t + \\delta_{it} + \\sum\\limits_{k=1}^{K}\\beta_k x_{ki}
\\end{equation}
<p>where \\(\\{\\beta_1, ..., \\beta_K\\}\\) correspond to the coefficients related to the K$ spatial covariates \\(x_1\\), ..., \\(x_K\\) that we included in the model: urban/rural ABS indicator, the ABS socio-economic deprivation index measured as in 2018 and the components of this index (proportion of the population exempted from pharmaceutical co-payments, proportion of the population with an income of less than 18,000 euros, proportion of the population with an income of more than 100,000 euros, proportion of the population with manual occupations, proportion of the population with an inadequate level of education, premature mortality and avoidable hospitalisations). Finally, because of multicollinearity problems, we didn't include two of the components (proportion of the population with an income of more than 100,000 euros, proportion of the population with manual occupations).</p>
<p>For each random effect, we calculated the posterior estimates of the corresponding relative risk:</p>
<ul>
<li>Spatial relative risk: \\(\\text{RR}_{\\text{Spatial}} = \\exp(b_i)\\). It represents the lack/excess risk of an ABS compared to the general population on average over the whole period.</li>
<li>Temporal relative risk: \\(\\text{RR}_{\\text{Temporal}} = \\exp(\\gamma_t)\\). It represents the lack/excess risk for the general population of one week compared to the average for the whole period.</li>
<li>Spatio-temporal relative risk: \\(\\text{RR}_{\\text{Spatio-temporal}} = \\exp(\\delta_{it})\\). It represents the lack/excess risk of an ABS and week compared to the average for the general population over the whole period that remains unexplained after adjusting for the spatial and temporal effect alone.</li>
</ul>"))
                                  )),
wellPanel(titlePanel("Shiny app"),
          withMathJax(HTML("
                                  <p>The 'Cases' and 'Hospitalisations' menus visualise the results of the models descried above. The 'Spatio-temporal' sub-menu shows the evolution of the spatio-temporal relative risk (\\(RR_{\\text{Spatio-temporal}}\\)), together with the plot of the evolution of the temporal relative risk (\\(RR_{\\text{Temporal}}\\)). The 'Spatial' sub-menu shows the spatial relative risk (\\(RR_{\\text{Spatial}}\\)) in a map.</p>
                                   <p>There is an additional menu to visualize the vaccination data ('Vaccination'). It shows the evolution of the map of the cumulative percentage of fully vaccination by ABS, together with the plot of the evolution of the cumulative percentage of fully vaccination in all Catalonia. Fully vaccination is measured as second doses of any vaccine or single doses of the one-shot vaccine.</p>"))
),
wellPanel(titlePanel("Software"),
          HTML("
                                  All code was developed using the free statistical software <code>R</code> in the version 4.3.0. The main <code>R</code> package used was <code>R-INLA</code> to fit the models with INLA and <code>R-Shiny</code> for developing this web application. In addition, <code>dplyr</code> and <code>purrr</code> were used for data management and <code>leaflet</code> and <code>plotly</code> for graphical representations. 
                                  ")
),
wellPanel(titlePanel("References"),
          HTML("<ul>
                    <li> Moraga, Paula. (2019). <a href='https://www.paulamoraga.com/book-geospatial'><i> Geospatial Health Data: Modeling and Visualization with R-INLA and Shiny </i></a>. Chapman & Hall/CRC Biostatistics Series </li>
                    <li>Knorr-Held L. Bayesian modelling of inseparable space-time variation in disease risk. Stat Med. 2000;19(17-18):2555-2567. doi:10.1002/1097-0258(20000915/30)19:17/18<2555::aid-sim587>3.0.co;2-#</li>
                    <li>H. Rue, S. Martino, and N. Chopin. Approximate Bayesian inference for latent Gaussian models using integrated nested Laplace approximations (with discussion). Journal of the Royal Statistical Society, Series B, 71(2):319{392, 2009. </li>
                    <li> FRANCE | COVID <a href='https://guillaumepressiat.shinyapps.io/covidfrance/'>https://guillaumepressiat.shinyapps.io/covidfrance/</a> [Accessed at 05/10/2020]</li> 
                    <li>Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). _shiny: Web Application Framework for R_. R package version 1.7.4, <https://CRAN.R-project.org/package=shiny>.</li>
                    </ul>")
)


               )
    )
  )

)


#### Server ####

server <- function(input, output){
  
  #### Load data ####
  load("ndat_vac.Rda")
  load("dat_cat.Rda")
  load("shapefileT.Rda")
  load("dat_covar.Rda")
  
  #Estimated RR
  load("res_model.Rda")
  load("dat_rr.Rda")
  load("dat_spatial.Rda")
  load("dat_temp.Rda")
  load("dat_sptemp.Rda")

  tab <- reactiveVal(NULL)
  observeEvent(input$navbar, {
    if(input$navbar %in% c("Total RR", "Total RR ", "Spatio-temporal RR", "Spatio-temporal RR ", "Spatial RR", "Spatial RR ")) {
      tab(
        case_match(
          input$navbar,
          "Total RR" ~ "cas",
          "Total RR " ~ "hosp",
          "Spatio-temporal RR" ~ "cas_sptemp",
          "Spatio-temporal RR " ~ "hosp_sptemp",
          "Spatial RR" ~ "cas_spatial",
          "Spatial RR " ~ "hosp_spatial"
        )
      )
    }
  })
  
  outcome <- reactiveVal(NULL)
  vac <- reactiveVal(NULL)
  observeEvent(input$navbar, {
    if(input$navbar %in% c("Total RR", "Total RR ", "Spatio-temporal RR", "Spatio-temporal RR ", "Spatial RR", "Spatial RR ")) {
      x <- case_when(
        grepl(" $", input$navbar) ~ "hosp",
        TRUE ~ "cas"
      )
      outcome(x)
    } else if(input$navbar == "Vaccination") {
      vac("vac")
    }
  })
  
  
  callModule(outcomeModule, id = "cas", tab = reactive(tab()), outcome = reactive(outcome()) , dat_spatial = dat_spatial, dat_temp = dat_temp, dat_sptemp = dat_sptemp, dat_rr = dat_rr, shapefileT = shapefileT, dat_covar = dat_covar, res_model = res_model)
  callModule(outcomeModule, id = "cas_sptemp", tab = reactive(tab()), outcome = reactive(outcome()) , dat_spatial = dat_spatial, dat_temp = dat_temp, dat_sptemp = dat_sptemp, dat_rr = dat_rr, shapefileT = shapefileT, dat_covar = dat_covar, res_model = res_model)
  callModule(outcomeModule, id = "cas_spatial", tab = reactive(tab()), outcome = reactive(outcome()) , dat_spatial = dat_spatial, dat_temp = dat_temp, dat_sptemp = dat_sptemp, dat_rr = dat_rr, shapefileT = shapefileT, dat_covar = dat_covar, res_model = res_model)
  
  callModule(outcomeModule, id = "hosp", tab = reactive(tab()), outcome = reactive(outcome()) , dat_spatial = dat_spatial, dat_temp = dat_temp, dat_sptemp = dat_sptemp, dat_rr = dat_rr, shapefileT = shapefileT, dat_covar = dat_covar, res_model = res_model)
  callModule(outcomeModule, id = "hosp_sptemp", tab = reactive(tab()), outcome = reactive(outcome()) , dat_spatial = dat_spatial, dat_temp = dat_temp, dat_sptemp = dat_sptemp, dat_rr = dat_rr, shapefileT = shapefileT, dat_covar = dat_covar, res_model = res_model)
  callModule(outcomeModule, id = "hosp_spatial", tab = reactive(tab()), outcome = reactive(outcome()) , dat_spatial = dat_spatial, dat_temp = dat_temp, dat_sptemp = dat_sptemp, dat_rr = dat_rr, shapefileT = shapefileT, dat_covar = dat_covar, res_model = res_model)
  
  callModule(VacModule, id = "vac", vac = reactive(vac()), ndat_vac = ndat_vac, dat_cat = dat_cat, shapefileT = shapefileT, dat_covar = dat_covar)
  
}


shinyApp(ui, server)

