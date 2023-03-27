#Module specifying the ui and server components for each one of the outcomes (cases, hospitalization and vaccination)
#Example of modulized shiny app similar to mine: https://github.com/rstudio/ShinyDeveloperConference/tree/master/Modules/Exercise-2/solution


outcomeModuleUI <- function(id) {
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
          value = ymd("2020-03-08"),
          step = 7,
          animate = animationOptions(interval = 2000)
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
      tags$b(tags$span(style="font-size:15px","ABS selectors")),
      hr(),
      selectizeInput(ns("region"),"Health regions", c("Not selected", "Barcelona", "Camp de Tarragona", "Lleida", "Girona", "Terres de l'Ebre", "Catalunya Central", "Alt Pirineu i Aran")),
      selectizeInput(ns("se_index"),"Socioeconomic status", c("Not selected","Very low", "Low", "Middle low", "Middle high", "High", "Very high"))
    ),
    #Define the box containing a graphic with the evolution of the measured outcome in the total of Catalonia
    absolutePanel(
      bottom = 20, right = 10, draggable = FALSE, width = "33%", height = "30%",
      style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 6px 8px;",
      plotlyOutput(ns("plot"), height = "100%")
    ),
    #Define the box containing the information about the restrictions in that time:
    absolutePanel(
      top = "30%", right = 30, width = "10%", draggable = FALSE, 
      style = "z-index:500; background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 10px",
      htmlOutput(ns("restrictions"))
    )
  )
  
}

outcomeModule <- function(input, output, session, outcome, dat_sae, dat_cat, shapefileT, dat_rest, dat_se) {
  
  #Update start value if hospitalization or vaccination is selected
  observeEvent(outcome(), {
    if(outcome() == "hosp"){
      updateSliderInput(session, "data", value = ymd("2020-05-03"))
    }else if(outcome() == "vac"){
      updateSliderInput(session, "data", value = ymd("2020-12-27"))
    }else{
      updateSliderInput(session, "data", value = ymd("2020-03-08"))
    }
    
  })
  
  sdat_sae <- reactive({
    if(!is.null(input$data) & !is.null(input$se_index)){
      df <- dat_sae %>% 
        filter(data == input$data) %>% 
        rename_all(~ gsub(str_glue("_{outcome()}"), "", .x)) %>% 
        mutate(
          color = "grey",
          weight = 1
        )
      
      if(input$se_index != "Not selected") {
        
        df <- df %>% 
          left_join(dat_se, by = "codi_abs") %>% 
          mutate(
            color = case_when(
              index_socioeconomic_cat == input$se_index ~ "purple",
              TRUE ~ color
            ),
            weight = case_when(
              index_socioeconomic_cat == input$se_index ~ 3,
              TRUE ~ weight
            )
          )
        
      }
      
      df
      
    }
  })
  
  output$title <- renderText({
    case_when(
      outcome() == "cas" ~ "Case incidence",
      outcome() == "hosp" ~ "Hospitalization rate",
      outcome() == "vac" ~ "Fully vaccinated percentage"
    )
  })
  
  output$description <- renderText({
    case_when(
      outcome() == "cas" ~ "Smooth new confirmed cases\n per 100k pop per week",
      outcome() == "hosp" ~ "Smooth new COVID- 19 hospitalizations\n per 100k pop per week",
      outcome() == "vac" ~ "Smooth cumulative percentage of\n individuals fully vaccinated"
    )
  })
  
  output$wave <- renderText({
    
    if(!is.null(input$data)) {
      
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
        
      }
  })
  
  output$restrictions <- renderText({
    if(!is.null(input$data)) {
      sdat_rest <- dat_rest %>% 
        filter(input$data >= start, input$data <= end)
      
      if(nrow(sdat_rest) > 0) {
        text <- sdat_rest %>% 
          rowwise() %>% 
          mutate(
            text = str_glue("<div style = 'margin-top:5%'>{icon} <span style = 'position:absolute; left:20%;'>{Description}</span></div>")
          ) %>% 
          pull(text)
        
        paste("<div style = 'margin-top:5%;'>", paste(c("<b>Restrictions in place:</b><br>", text), collapse = "<br>"), "<br></div>")
      }else {
        'There are no restrictions in place'
      }
      #To make them work:
      # https://appsilon.com/r-shiny-fontawesome-icons/
      
    }
  })
  
  #Compile initial map:
  output$map <- renderLeaflet({
    provider<-"OpenStreetMap.Mapnik"
    pal <- colorFactor(palette="YlOrRd", levels = levels(dat_sae %>% pull(str_glue("srate_{outcome()}_cat"))))
    values <- factor(1:length(levels(dat_sae %>% pull(str_glue("srate_{outcome()}_cat")))), labels = levels(dat_sae %>% pull(str_glue("srate_{outcome()}_cat"))))
    title <- case_when(
      outcome() == "cas" ~ "Case inc.",
      outcome() == "hosp" ~ "Hosp. rate",
      outcome() == "vac" ~ "Fully vac. %"
    )
    
    leaflet(options = leafletOptions(zoomControl = FALSE))%>%
      addProviderTiles(provider=provider)%>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topleft' }).addTo(this)
    }") %>%
      addResetMapButton() %>%
      setView(1.5209,41.5912,zoom=8) %>%
      addLegend(layerId="legend", pal = pal, values = values, opacity = 0.5, title = title, position = "topright")
  })
  
  #Compile initial plotly with the total of Catalonia:
  output$plot <- renderPlotly({
    
    #Start date of each wave
    data_inici <- dmy(c("25/02/2020","01/10/2020","07/12/2020","15/03/2021","13/06/2021","02/11/2021"))
    
    sdat_cat <- dat_cat %>% 
      rename_all(~ gsub(str_glue("_{outcome()}"), "", .x)) %>% 
      mutate(size = ifelse(data == ymd("2020-03-08"), 10, 0.01),
             color = ifelse(data == ymd("2020-03-08"), "black", "#DD8888")
      )
    
    #Function that will add a vertical line in the x specified date
    add_vline = function(x) {
      list(
        type = "line",
        x0 = x,
        x1 = x,
        y0 = 0,
        y1 = max(sdat_cat$rate),
        line = list(width = 1, dash = "dot")
      )
    }
    
    plot_ly(sdat_cat, x = ~data, y = ~rate, type = "bar", hovertext = ~str_glue("<b>{data}:</b> {round(rate, 2)}"), hoverinfo = "text", marker = list(color = "#DD8888")) %>% 
      add_trace(x = ~data, y = ~rate, type = "scatter", mode = "markers",  marker = list(color = ~color, size = ~size)) %>% 
      layout(showlegend = FALSE,
             #Vertical lines for each wave
             shapes = map(data_inici, add_vline)
      ) %>% 
      plotly::config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"))
    
  })
  
  #Update map with the selected date:
  observeEvent(list(sdat_sae(), input$region), {
    
    if(!is.null(sdat_sae())){
      #Update map
      pal <- colorFactor(palette="YlOrRd", levels = levels(sdat_sae()$srate_cat))
      
      map <- shapefileT
      
      map@data <- map@data %>% 
        left_join(sdat_sae(), by = "codi_abs") %>% 
        arrange(OBJECTID)
      
      title <- case_when(
        outcome() == "cas" ~ "Smooth incidence (x100k)",
        outcome() == "hosp" ~ "Smooth rate (x100k)",
        outcome() == "vac" ~ "Smooth %"
      )
      labels <- map@data %>% 
        mutate(lab = str_glue("<b>{abs}</b> (<i>{data}</i>)
                              <br>
                              <i>Observed:</i> {n}<br>
                              <i>Expected:</i> {round(exp, 2)}</br>
                              <i>{title}:</i> {round(srate, 2)} ({round(srate_lci, 2)}, {round(srate_uci, 2)})<br>
                              <i>Smooth SIR:</i> {round(rr, 2)} ({round(rr_lci, 2)}, {round(rr_uci, 2)})
                              ")) %>% 
        pull(lab) %>%
        lapply(htmltools::HTML)
      
      #Set lng, lat, zoom of the map when the health region is selected:
      if(input$region == "Not selected") {
        lng <- 1.5209
        lat <- 41.5912
        zoom <- 8
      } else {
        rs <- shapefileT@data %>% 
          filter(NOMRS == input$region) %>% 
          distinct(RS_Coord_X, RS_Coord_Y)
        
        lng <- rs$RS_Coord_X
        lat <- rs$RS_Coord_Y
        zoom <- 10
      }
      
      print(c(lng, lat, zoom))
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
          fillColor = ~ pal(srate_cat),
          fillOpacity = 0.8,
          layerId =  ~ abs
        ) %>% 
        setView(lng = lng, lat = lat, zoom = zoom)
        
      
    }
    
  })
  
  observeEvent(sdat_sae(), {
    if(!is.null(input$data)){
      #Update plotly
      sdat_cat <- dat_cat %>% 
        rename_all(~ gsub(str_glue("_{outcome()}"), "", .x)) %>% 
        mutate(size = ifelse(data == input$data, 10, 0.01),
               color = ifelse(data == input$data, "black", "#DD8888")
        )
      
      plotlyProxy("plot", session) %>%
        plotlyProxyInvoke("restyle",list(marker=list(size = sdat_cat$size, color = sdat_cat$color)), 1)
    }
  })
  
  
}
