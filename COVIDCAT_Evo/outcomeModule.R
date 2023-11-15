#Module specifying the ui and server components for each one of the outcomes (cases, hospitalization and vaccination)
#Example of modulized shiny app similar to mine: https://github.com/rstudio/ShinyDeveloperConference/tree/master/Modules/Exercise-2/solution


outcomeModuleUI <- function(id) {
  
  ns <- NS(id)
  
  title_id <- case_match(
    id,
    "cas" ~ "Total RR",
    "hosp" ~ "Total RR ",
    "cas_sptemp" ~ "Spatio-temporal RR",
    "hosp_sptemp" ~ "Spatio-temporal RR ",
    "cas_spatial" ~ "Spatial RR",
    "hosp_spatial" ~ "Spatial RR "
  )
  
  tabPanel(
    title_id,
    useShinyjs(),
    #Define the leaflet with the map
    leafletOutput(ns("map"),width="100%"),
    #Define the left-upper box with the title and the date animated input
    absolutePanel(
      top=60, left = 80, draggable = FALSE, width = "20%", 
      style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 6px 8px;",
      tags$b(tags$span(style="font-size:30px","Covid-19 in Catalonia")),
      fluidRow(
        column(8,
               tags$b(tags$span(style="font-size:18px;", textOutput(ns("title"))))
               ),
        column(4,
               uiOutput(ns("ui_description"))
               )
      ),
      hr(),
      tags$div(
        style = "margin-left:5%",
        sliderInput(
          ns("data"),
          "Evolution",
          min = ymd("2020-03-08"),
          max = ymd("2022-07-24"),
          value = ymd("2020-03-08"),
          step = 7,
          animate = animationOptions(interval = 1500)
        )
      ),
      tags$div(
        style = "margin-left:5%; font-style:italic",
        textOutput(ns("wave"))
      )
    ),
    #Define the left ABS selector
    absolutePanel(
      top=350, left = 80, draggable = FALSE, width = "20%", 
      style = "z-index:1; min-width: 300px;background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 6px 8px;",
      #Define button to close the box:
      actionButton(
        ns('coll_btn'),
        "-",
        "data-toggle" = 'collapse',
        "data-target" = str_glue("#{ns('coll')}"),
        style = "opacity: .80; color: #fff; background-color:  #AF292E; border-color: #a153e5; float:right; font-size:80%;padding-top:2px;padding-bottom:2px;"
      ),
      tags$b(tags$span(style="font-size:15px","ABS selector")),
      hr(),
      tags$div(
        id = ns("coll"), class = "collapse in",
        selectizeInput(ns("region"),"Health region", c("None", "Barcelona", "Camp de Tarragona", "Lleida", "Girona", "Terres de l'Ebre", "Catalunya Central", "Alt Pirineu i Aran")),
        selectizeInput(ns("abs"), label = NULL, multiple = TRUE, choices = NULL, options = list(placeholder = "Select ABS", maxItems = 1)),
        hr(),
        uiOutput(ns("ui_abs"))
      )
    ),
    #Define the box containing a graphic with the evolution of the measured outcome in the total of Catalonia
    uiOutput(ns("ui_plot")),
    #Define the right upper box with the model information
    modModuleUI(ns("mod"))
  )
  
  
}

outcomeModule <- function(input, output, session, tab, outcome, dat_spatial, dat_temp, dat_sptemp, dat_rr, shapefileT, dat_covar, res_model = res_model) {
  
  #Call module
  callModule(modModule, id = "mod", outcome = reactive(outcome()), res_model = res_model)
  
  output$ui_plot <- renderUI({
    if(!tab() %in% c("cas_spatial", "hosp_spatial")) {
      absolutePanel(
        bottom = 20, right = 10, draggable = FALSE, width = "33%", height = "30%",
        style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 6px 8px;",
        plotlyOutput(session$ns("plot"), height = "100%")
      )
    }
  })
  
  #Update start value if hospitalization or cases is selected
  observeEvent(tab(), {
    if(tab() %in% c("cas", "cas_sptemp")) {
      updateSliderInput(session, "data", label = "Evolution", value = ymd("2020-03-08"))
    } else if(tab() %in% c("hosp", "hosp_sptemp")) {
      updateSliderInput(session, "data", label = "Evolution", value = ymd("2020-05-03"))
    } else {
      updateSliderInput(session, "data", label = "All the period", value = ymd("2022-07-24"))
    }
    toggleState("data", condition = !tab() %in% c("cas_spatial", "hosp_spatial"))
  })
  
  #Update the sign of the closable button if it has been clicked
  observeEvent(input$coll_btn,{
    if(input$coll_btn%%2==0){
      updateActionButton(session,
                         "coll_btn",
                         label="-")
    }else{
      updateActionButton(session,
                         "coll_btn",
                         label="+")
    }
  })
  
  #Update selection ABS
  observeEvent(input$region, {
    if(!is.null(input$region)) {
      #If no abs is selected
      if(is.null(input$abs)) {
        if(input$region == "None") {
          updateSelectizeInput(session, "abs", choices = sort(shapefileT$abs))
        } else {
          shapefileT_RS <- shapefileT@data %>% 
            filter(NOMRS == input$region)
          updateSelectizeInput(session, "abs", choices = sort(shapefileT_RS$abs))
        }
      } else {
        #If some abs is selected we will update the abs only if its erroneous or if we select none
        if(input$region == "None") {
          updateSelectizeInput(session, "abs", choices = sort(shapefileT$abs))
        } else {
          if(input$region != shapefileT$NOMRS[shapefileT$abs == input$abs]) {
            shapefileT_RS <- shapefileT@data %>% 
              filter(NOMRS == input$region)
            updateSelectizeInput(session, "abs", choices = sort(shapefileT_RS$abs))
          }
        }
      }
    }
  })
  
  #Update selection Health region
  observeEvent(input$abs, {
      if(!is.null(input$abs)) {
        updateSelectizeInput(session, "region", selected = shapefileT$NOMRS[shapefileT$abs == input$abs])
      }
  })
  
  output$ui_abs <- renderUI({
    if(!is.null(input$abs)) {
      tabsetPanel(
        tabPanel(
          "Socio-economic components",
          plotlyOutput(session$ns("plot_abs"), height = 300),
          "Quintile number of each socio-economic deprivation component"
        ),
        tabPanel(
          "Table characteristics",          
          dataTableOutput(session$ns("table_abs"))
        )
      )
    }
  })
  
  output$plot_abs <- renderPlotly({

    if(!is.null(input$abs)) {

      sdat_covar <- dat_covar %>%
        filter(abs == input$abs) %>%
        #We only plot the included covariates
        dplyr::select(q_poblacio_amb_rendes_inferiors_a_18_000_euros , q_poblacio_exempta_de_copagament_farmaceutic, q_poblacio_amb_nivell_dinstruccio_insuficient , q_taxa_de_mortalitat_prematura , q_hospitalitzacions_evitables)
      # mutate_all(~log(.x))

      plot_ly(
        type = 'scatterpolar',
        r = as.integer(sdat_covar),
        theta = c("Income\n < 18k€", "Pharmaceutical\n co-payment", "Inadequate\n education", "Premature\n mortality", "Avoidable\n hospitalisations"),
        fill = 'toself',
        hoverinfo = "none"
      )  %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              autorange = FALSE,
              range = list(0, 5),
              tickmode = "array",
              tickvals = 1:5,
              ticktext = 1:5
            )
          ),
          showlegend = F,
          margin = list(
            l = 100, r = 100
          )
        ) %>%
        plotly::config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"))

    }

  })
  
  output$table_abs <- renderDataTable({
    
    if(!is.null(input$abs)) {
      
      sdat_covar <- dat_covar %>% 
              filter(abs == input$abs) %>%
              dplyr::select(N, perc_women, perc_70, urban, isc_reescalat) %>% 
              mutate_if(is.numeric, ~round(.x, 2)) %>% 
              mutate_all(as.character) 
      
      names(sdat_covar) <- c("Population", "Women (%)", "> 70 years (%)", "Urban/Rural", "Socio-economic index (%)")
      
      table <- sdat_covar %>% 
        pivot_longer(everything(), names_to = "Characteristics", values_to = input$abs)
      
      Tdat_covar <- dat_covar %>%
        summarise_at(vars(starts_with("N")), sum) %>% 
        mutate(
          perc_women = round(N_women*100/N, 2),
          perc_70 = round(N_70*100/N, 2),
        ) %>% 
        mutate_all(as.character) %>% 
        dplyr::select(N, perc_women, perc_70)
      
      names(Tdat_covar) <- c("Population", "Women (%)", "> 70 years (%)")
      
      tableCAT <- Tdat_covar %>% 
        pivot_longer(everything(), names_to = "Characteristics", values_to = "Catalonia")
      
      table <- table %>% 
        left_join(tableCAT, by = "Characteristics")
      
      datatable(table,
                rownames = NULL,
                options = list(pageLength = 5,
                               dom = "t",
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))
                               ))
        
    }
    
  })
  
  sdat <- reactive({
    if(tab() %in% c("cas", "hosp")) {
      req(input$data)
      dat_rr %>% 
        filter(data == input$data, outcomes == outcome()) 
    } else if(tab() %in% c("cas_sptemp", "hosp_sptemp")) {
      req(input$data)
      dat_sptemp %>% 
        filter(data == input$data, outcomes == outcome())
    } else if(tab() %in% c("cas_spatial", "hosp_spatial")) {
      dat_spatial %>% 
        filter(outcomes == outcome())
    }
    
  })
  
  output$title <- renderText({
    case_match(
      tab(),
      "cas" ~ "COVID-19 cases RR",
      "hosp" ~ "COVID-19 hospitalisations RR",
      "cas_sptemp" ~ "COVID-19 cases spatio-temporal RR",
      "hosp_sptemp" ~ "COVID-19 hospitalisation spatio-temporal RR",
      "cas_spatial" ~ "COVID-19 cases spatial RR",
      "hosp_spatial" ~ "COVID-19 hospitalisations spatial RR" 
    )
  })
  
  output$ui_description <- renderUI({
    desc <- case_match(
      tab(),
      "cas" ~ "Estimated total relative risk (RR) for cases, in each ABS and week. It represents the lack/excess risk of an ABS and week, compared to the general population on average over the entire pandemic period.",
      "hosp" ~ "Estimated total relative risk (RR) for hospitalisations, in each ABS and week. It represents the lack/excess risk of an ABS and week, compared to the average for the general population over the whole period.",
      "cas_sptemp" ~ "Estimated relative risk (RR) for cases given by the spatio-temporal effect, in each ABS and week. It represents the lack/excess risk outbreaks in some ABS at some specific waves.",
      "hosp_sptemp" ~ "Estimated relative risk (RR) for cases given by the spatio-temporal effect, in each ABS and week. It represents the lack/excess risk outbreaks in some ABS at some specific waves.",
      "cas_spatial" ~ "Estimated relative risk (RR) for cases given by the spatial effect, in each ABS for all the period. It represents the lack/excess risk of an ABS compared to the general population on average over the whole period.",
      "hosp_spatial" ~ "Estimated relative risk (RR) for cases given by the spatio-temporal effect, in each ABS for all the period. It represents the lack/excess risk of an ABS compared to the general population on average over the whole period."
    )
    tipify(actionBttn(session$ns("hover_btn"), label = NULL, icon = icon("circle-info"), style = "stretch", color = "royal"), desc)
  })
  
  
  output$wave <- renderText({
    req(input$data)
      
      data_inici <- dmy(c("25/02/2020","01/10/2020","07/12/2020","15/03/2021","13/06/2021","02/11/2021"))
      data_fi <- dmy(c("01/10/2020","07/12/2020","15/03/2021","13/06/2021","02/11/2021","01/08/2022"))
      
      wave <- tibble(data_inici, data_fi) %>% 
        mutate(id = c("1st wave",
                      "2nd wave",
                      "3rd wave",
                      "4th wave",
                      "5th wave",
                      "6th wave"),
               inside = (data_inici <= input$data &
                           input$data < data_fi)
        )
        
      if(any(wave$inside)) {
        wave %>%
          filter(inside) %>%
          pull(id)
      } else {
        NULL
      }
        
  })
  
  #Compile initial spatio-temporal map:
  output$map <- renderLeaflet({
      provider<-"OpenStreetMap.Mapnik"
      if(!tab() %in% c("cas_spatial", "hosp_spatial")) {
        if(tab() %in% c("cas", "hosp")) {
          domain <- range(log(dat_rr$rr[dat_rr$outcomes == outcome()]), na.rm = TRUE)
        } else {
          domain <- range(log(dat_sptemp$rr[dat_sptemp$outcomes == outcome()]), na.rm = TRUE)
        }
        
        rc1 <- colorRampPalette(colors = c("#008080","#fdfbe4"), space = "Lab")(abs(domain)[1])
        rc2 <- colorRampPalette(colors = c("#fdfbe4","#b2182b"), space = "Lab")(domain[2])
        pal <- colorNumeric(c(rc1, rc2), domain = domain)
        
      } else {
        
        domain <- range(log(sdat()$rr), na.rm = TRUE)
        pal <- colorNumeric(c("#008080", "#fdfbe4", "#b2182b"), domain = domain)
        
      }
      
      round_legend <- case_match(
        tab(),
        "cas" ~ 3,
        "hosp" ~ 2,
        .default = 1
      )
      
      leaflet(options = leafletOptions(zoomControl = FALSE))%>%
        addProviderTiles(provider=provider)%>%
        htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topleft' }).addTo(this)
      }") %>%
        addResetMapButton() %>%
        setView(1.5209,41.5912,zoom=8) %>%
        addLegend(layerId="legend", pal = pal, values = domain, title = "RR", position = "topright", labFormat = labelFormat(
          transform = function(x) round(case_when(!x %in% c(-4, -2, -1, 1, 2, 3) ~ exp(x), x == -4 ~ 0.02, x == -2 ~ 0.14, x == -1 ~ 0.4, x == 1 ~ 2.7, x == 2 ~ 7.4, x == 3 ~ 20), round_legend)
        )
        )
  })

  #Compile initial plotly with the temporal effect:
  output$plot <- renderPlotly({
    
    #Start date of each wave
    data_inici <- dmy(c("01/10/2020","07/12/2020","15/03/2021","13/06/2021","02/11/2021"))
    
    sel_data <- case_when(
      outcome() == "cas" ~ ymd("2020-03-08"),
      outcome() == "hosp" ~ ymd("2020-05-03")
    )
    
    sdat_temp <- dat_temp %>% 
      filter(outcomes == outcome()) %>% 
      mutate(size = ifelse(data == sel_data, 10, 0.01),
             color = ifelse(data == sel_data, "black", "#DD8888")
      )
    
    #Function that will add a vertical line in the x specified date
    add_vline = function(x) {
      if(!is.na(x)) {
        list(
          type = "line",
          x0 = x,
          x1 = x,
          y0 = min(sdat_temp$rr),
          y1 = max(sdat_temp$rr),
          line = list(width = 0.5, dash = "dot")
        )
      } else {
        list(
          xref = "paper",
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          y0 = 1, 
          y1 = 1, 
          line = list(width = 0.5)
        )
      }
      
    }
    
    title <- case_when(
      outcome() == "cas" ~ "Temporal RR",
      outcome() == "hosp" ~ "Temporal RR"
    )
    
    x_axis_breaks  <- seq(
      from = ymd("2020-03-01"), 
      to = ymd("2022-08-01"),
      by = "3 months"
    )   
    
    ticktext  <- format(x_axis_breaks, "%y-%m")
    
      plot_ly(sdat_temp, x = ~data, y = ~rr, type = "scatter", mode = "lines", hovertext = ~str_glue("<b>{data}</b><br><i>RR</i> = {round(rr, 2)} [{round(rr_lci, 2)}, {round(rr_uci, 2)}]"), hoverinfo = "text", line = list(color = "#DD8888")) %>% 
      add_trace(x = ~data, y = ~rr, type = "scatter", mode = "markers",  marker = list(color = ~color, size = ~size)) %>% 
      layout(showlegend = FALSE,
             #Vertical lines for each wave
             shapes = map(c(data_inici, NA), add_vline), 
             yaxis = list(title = title, type = "log"),
             xaxis = list(range = ymd(c("2020-02-01", "2022-08-01")),
                          ticktext = ticktext,
                          tickvals = x_axis_breaks,
                          tickmode = "array"
                          )
      ) %>% 
      plotly::config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"))
    
  })
  
  #Update map with the selected date and region/ABS:
  observeEvent(list(tab(), outcome(), sdat(), input$region, input$abs), {
    if(!is.null(sdat()) & nrow(sdat()) > 0){
      #Update map
      
      if(!tab() %in% c("cas_spatial", "hosp_spatial")) {
        
        if(tab() %in% c("cas", "hosp")) {
          domain <- range(log(dat_rr$rr), na.rm = TRUE)
        } else {
          domain <- range(log(dat_sptemp$rr), na.rm = TRUE)
        }
        
        rc1 <- colorRampPalette(colors = c("#008080","#fdfbe4"), space = "Lab")(abs(domain)[1])
        rc2 <- colorRampPalette(colors = c("#fdfbe4","#b2182b"), space = "Lab")(domain[2])
        pal <- colorNumeric(c(rc1, rc2), domain = domain)
        
      } else {
        
        domain <- range(log(sdat()$rr), na.rm = TRUE)
        pal <- colorNumeric(c("#008080", "#fdfbe4", "#b2182b"), domain = domain)
        
      }
      
      map <- shapefileT
      
      map@data <- map@data %>% 
        left_join(sdat(), by = c("OBJECTID" = "idarea")) %>% 
        arrange(OBJECTID)
      
      if(!tab() %in% c("cas_spatial", "hosp_spatial")) {
        labels <- map@data %>% 
          mutate(lab = str_glue("<b>{abs}</b> (<i>{data}</i>)
                              <br>
                              <i>RR</i> = {round(rr, 2)} [{round(rr_lci, 2)}, {round(rr_uci, 2)}]
                              ")) %>% 
          pull(lab) %>%
          lapply(htmltools::HTML)
      } else {
        labels <- map@data %>% 
          mutate(lab = str_glue("<b>{abs}</b> (<i>Overall</i>)
                              <br>
                              <i>RR</i> = {round(rr, 2)} [{round(rr_lci, 2)}, {round(rr_uci, 2)}]
                              ")) %>% 
          pull(lab) %>%
          lapply(htmltools::HTML)
      }
      
      
      #Set lng, lat, zoom of the map when the health region is selected:
      if(input$region == "None" & is.null(input$abs)) {
        
        lng <- 1.5209
        lat <- 41.5912
        zoom <- 8
        
      } else {
        
        if(input$region != "None") {
          rs <- shapefileT@data %>% 
            filter(NOMRS == input$region) %>% 
            distinct(RS_Coord_X, RS_Coord_Y)
          
          lng <- rs$RS_Coord_X
          lat <- rs$RS_Coord_Y
          
          if(input$region == "Barcelona") {
            zoom <- 11  
          } else {
            zoom <- 10
          }
          
        }
        
        #La tria de l'ABS sempre predominarà
        if(!is.null(input$abs)) {
          
          abs <- shapefileT@data %>% 
            filter(abs == input$abs) %>% 
            distinct(NOMRS, Coord_X, Coord_Y)
          
          lng <- abs$Coord_X
          lat <- abs$Coord_Y
          
          if(abs$NOMRS == "Barcelona") {
            zoom <- 13
          } else {
            zoom <- 12
          }
          
        }
        
      }
      
      leafletProxy("map", data = map) %>%
        addPolygons(
          group = "polygons",
          weight = 1,
          color = "grey",
          smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            color = "white",
            weight = 4,
            bringToFront = TRUE
          ),
          label = ~ labels,
          fillColor = ~ pal(log(rr)),
          fillOpacity = 0.8,
          layerId =  ~ abs
        ) %>% 
        setView(lng = lng, lat = lat, zoom = zoom)
    }
    
  })
  
  #Update plot RR temporal with selected date
  observeEvent(sdat(), {
    if(!tab() %in% c("cas_spatial", "hosp_spatial")) {
      req(input$data)
        #Update plotly
        sdat_temp <- dat_temp %>% 
          filter(outcomes == outcome()) %>% 
          mutate(size = ifelse(data == input$data, 10, 0.01),
                 color = ifelse(data == input$data, "black", "#DD8888")
          )
        
        plotlyProxy("plot", session) %>%
          plotlyProxyInvoke("restyle",list(marker=list(size = sdat_temp$size, color = sdat_temp$color)), 1)
      }
  })
  
  
}
