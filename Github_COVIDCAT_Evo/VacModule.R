#Module specifying the ui and server components of vaccination

VacModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    #Define the leaflet with the map
    leafletOutput(ns("map"),width="100%"),
    #Define the left-upper box with the title and the date animated input
    absolutePanel(
      top=60, left = 80, draggable = FALSE, width = "20%", 
      style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 6px 8px;",
      tags$b(tags$span(style="font-size:30px","Covid-19 in Catalonia")),
      tags$b(tags$span(style="font-size:20px;", textOutput(ns("title")))),
      tags$i(tags$div(style="margin-top:10px;margin-bottom:-10px;", textOutput(ns("description")))),
      hr(),
      tags$div(
        style = "margin-left:5%",
        sliderInput(
          ns("data"),
          "Evolution",
          min = ymd("2020-03-08"),
          max = ymd("2022-07-24"),
          value = ymd("2021-01-31"),
          step = 7,
          animate = animationOptions(interval = 1500)
        )
      ),
      tags$div(
        style = "margin-left:5%; font-style:italic",
        textOutput(ns("wave"))
      )
    ),
    #Define the left upper selector (socioeconomic index)
    absolutePanel(
      top=360, left = 80, draggable = FALSE, width = "20%", 
      style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 6px 8px;",
      #Define button to close the box:
      actionButton(
        ns('coll_btn'),
        "-",
        "data-toggle" = 'collapse',
        "data-target" = '#coll_vac',
        style = "opacity: .80; color: #fff; background-color:  #AF292E; border-color: #a153e5; float:right; font-size:80%;padding-top:2px;padding-bottom:2px;"
      ),
      tags$b(tags$span(style="font-size:15px","ABS selector")),
      hr(),
      tags$div(
        id = "coll_vac", class = "collapse in",
      selectizeInput(ns("region"),"Health regions", c("None", "Barcelona", "Camp de Tarragona", "Lleida", "Girona", "Terres de l'Ebre", "Catalunya Central", "Alt Pirineu i Aran")),
      selectizeInput(ns("abs"), label = NULL, multiple = TRUE, choices = NULL, options = list(placeholder = "Select ABS", maxItems = 1)),
      hr(),
      uiOutput(ns("ui_abs"))
      )
    ),
    #Define the box containing a graphic with the evolution of the measured outcome in the total of Catalonia
    absolutePanel(
      bottom = 20, right = 10, draggable = FALSE, width = "33%", height = "30%",
      style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 6px 8px;",
      plotlyOutput(ns("plot"), height = "100%")
    )
  )
  
}

VacModule <- function(input, output, session, vac, ndat_vac, dat_cat, shapefileT, dat_covar) {

  observeEvent(vac(), {
    updateSliderInput(session, "data", value = ymd("2021-01-31"))
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
              range = list(1, 5),
              dtick = 1
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
  
  sdat_vac <- reactive({
    req(input$data)
      ndat_vac %>% 
        filter(data == input$data) %>% 
        rename_all(~ gsub(str_glue("_vac"), "", .x)) %>% 
        mutate(
          color = "grey",
          weight = 1
        ) 
  })
  
  output$title <- renderText({
    "Fully vaccinated percentage"
  })
  
  output$description <- renderText({
      "Cumulative percentage of\n individuals fully vaccinated"
  })
  
  output$wave <- renderText({
    
    req(input$data)
      
      data_inici <- dmy(c("25/02/2020","01/10/2020","07/12/2020","15/03/2021","13/06/2021","02/11/2021"))
      data_fi <- dmy(c("04/07/2020","06/12/2020","14/03/2021","12/06/2021","01/11/2021","24/07/2022"))
        
      wave <- tibble(data_inici, data_fi) %>% 
        mutate(id = c("1st wave",
                      "2nd wave",
                      "3rd wave",
                      "4th wave",
                      "5th wave",
                      "6th wave"),
               inside = (data_inici <= input$data &
                           input$data <= data_fi)
        )
        
      if(any(wave$inside)) {
        wave %>%
          filter(inside) %>%
          pull(id)
      } else {
        NULL
      }
  })
  
  #Compile initial map:
  output$map <- renderLeaflet({
    provider<-"OpenStreetMap.Mapnik"
    pal <- colorNumeric(c("white", carto_pal(7, "Sunset")), domain = c(0, 100))
    
    leaflet(options = leafletOptions(zoomControl = FALSE))%>%
      addProviderTiles(provider=provider)%>%
      addResetMapButton() %>%
      setView(1.5209,41.5912,zoom=8) %>%
      addLegend(layerId="legend", pal = pal, values = c(0, 100), opacity = 0.5, title = "%", position = "topright") 
      
  })
  
  #Compile initial plotly with the total of Catalonia:
  output$plot <- renderPlotly({
    
    #Start date of each wave
    data_inici <- dmy(c("25/02/2020","01/10/2020","07/12/2020","15/03/2021","13/06/2021","02/11/2021"))
    sdat_cat <- dat_cat %>% 
      rename_all(~ gsub(str_glue("_{vac()}"), "", .x)) %>% 
      mutate(size = ifelse(data == ymd("2021-01-31"), 10, 0.01),
             color = ifelse(data == ymd("2021-01-31"), "black", "#DD8888")
      )
    
    #Function that will add a vertical line in the x specified date
    add_vline = function(x) {
      list(
        type = "line",
        x0 = x,
        x1 = x,
        y0 = 0,
        y1 = 105,
        line = list(width = 1, dash = "dot")
      )
    }
    
    plot_ly(sdat_cat, x = ~data, y = ~Trate, type = "bar", hovertext = ~str_glue("<b>{data}:</b> {round(Trate, 2)}"), hoverinfo = "text", marker = list(color = "#DD8888")) %>% 
      add_trace(x = ~data, y = ~Trate, type = "scatter", mode = "markers",  marker = list(color = ~color, size = ~size)) %>% 
      layout(yaxis = list(title = "Weekly fully vac. (%)", range = list(0, 105)),
             showlegend = FALSE,
             #Vertical lines for each wave
             shapes = map(data_inici, add_vline)
      ) %>% 
      plotly::config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"))
    
  })
  
  #Update map with the selected date:
  observeEvent(list(vac(), sdat_vac(), input$region), {
      #Update map
      pal <- colorNumeric(c("white", carto_pal(7, "Sunset")), domain = c(0, 100))
      
      map <- shapefileT
      
      map@data <- map@data %>% 
        left_join(sdat_vac(), by = "abs") %>% 
        arrange(OBJECTID)
      
      title <- "Fully vaccination coverage (%)"
      
      labels <- map@data %>% 
        mutate(lab = str_glue("<b>{abs}</b> (<i>{data}</i>)
                              <br>
                              <i>Observed:</i> {n}<br>
                              <i>{title}:</i> {round(rate, 2)}
                              ")) %>% 
        pull(lab) %>%
        lapply(htmltools::HTML)
      
      #Set lng, lat, zoom of the map when the health region is selected:
      if(input$region == "None") {
        lng <- 1.5209
        lat <- 41.5912
        zoom <- 8
      } else {
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
      
      leafletProxy("map", data = map) %>%
        addPolygons(
          group = "polygons",
          weight = ~weight,
          color = ~color,
          smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            color = "white",
            weight = 4,
            bringToFront = TRUE
          ),
          label = ~ labels,
          fillColor = ~ pal(rate),
          fillOpacity = 0.8,
          layerId =  ~ abs
        ) %>% 
        setView(lng = lng, lat = lat, zoom = zoom)
      
  })
  
  observeEvent(list(vac(), sdat_vac()), {
    req(input$data)
      #Update plotly
      sdat_cat <- dat_cat %>% 
        rename_all(~ gsub("_vac", "", .x)) %>% 
        mutate(size = ifelse(data == input$data, 10, 0.01),
               color = ifelse(data == input$data, "black", "#DD8888")
        )
      
      plotlyProxy("plot", session) %>%
        plotlyProxyInvoke("restyle",list(marker=list(size = sdat_cat$size, color = sdat_cat$color)), 1)
  })
  
  
}
