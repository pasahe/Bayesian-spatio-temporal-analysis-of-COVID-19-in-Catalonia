#Module specifying the ui and server components of the model information

modModuleUI <- function(id) {
  ns <- NS(id)
  
  absolutePanel(
    top=60, right=100, draggable = FALSE, width = "33%", 
    style = "z-index:500; min-width: 300px;background-color:rgba(255,255,255,0.8);box-shadow: 0 0 15px rgba(0,0,0,0.2);border-radius: 5px;padding: 6px 8px;",
    #Define button to close the box:
    actionButton(
      ns('coll_btn2'),
      "+",
      "data-toggle" = 'collapse',
      "data-target" = str_glue("#{ns('coll2')}"),
      style = "opacity: .80; color: #fff; background-color:  #AF292E; border-color: #a153e5; float:right; font-size:80%;padding-top:2px;padding-bottom:2px;"
    ),
    tags$b(tags$span(style="font-size:15px","Model information")),
    hr(),
    tags$div(
      id = ns("coll2"), class = "collapse",
      tabsetPanel(
        tabPanel(
          "Information",
          uiOutput(ns("ui_info"))
        ),
        tabPanel(
          "Summary (fixed effects)",
          dataTableOutput(ns("summary_fixed"))
        ),
        tabPanel(
          "Summary (random effects)",
          dataTableOutput(ns("summary_random"))
        )
      )
    )
  )
  
}

modModule <- function(input, output, session, outcome, res_model) {
  
  #Compile initially the matchjax for every component (if it isn't compiled initially it doesn't read the math characters)
  info <- tibble(outcome = c("cas", "hosp"),
                 labels = c("cases", "hospitalisations")) %>% 
    mutate(
      text = map(labels,
                 ~ withMathJax(HTML(
                   paste(
                     paste0(
                       r"(\begin{equation}
                       \begin{split}
                       Y_{it} \mid \theta_{it} & \sim Poisson(E_i \theta_{it}) \\
                       \log{\theta_{it}} &= \alpha + \sum\limits_{k=1}^{K}\beta_k x_{ki} + b_i + \gamma_t + \delta_{it}
                       \end{split}
                       \end{equation}
                      )",
                      "<table style = 'width: 100%'>
                      <tr>
                      <td>\\(Y_{it}\\): ",
                      str_glue("Weekly COVID-19 reported {.x} in the ABS"),
                      "<br>",
                      "\\(E_{it}\\): ",
                      str_glue("Weekly COVID-19 expected {.x} in the ABS"),
                      "</td>",
                      "<td style = 'padding-left: 5%;'>
                      \\(x_{ki}\\): ABS socio-demographic characteristics (urban/rural and socio-economic components)
                      </td>
                      </tr>
                      </table>
                      "
                                       ),
                      '<center><table style = "width: 70%;">
                        <thead>
                        <tr>
                        <th style="text-align:left;"> Symbol </th>
                        <th style="text-align:left; padding-left: 5%;"> Effect </th>
                        <th style="text-align:left;"> Model </th>
                        </tr>
                        </thead>
                        <tbody>
                        <tr>
                        <td style="text-align:left;"> \\(\\theta_{it}\\) </td>
                        <td style="text-align:left; padding-left: 5%;"> Relative Risk of the ABS in the week </td>
                        <td style="text-align:left;">  </td>
                        </tr>
                        <tr>
                        <td style="text-align:left;"> \\(\\alpha\\) </td>
                        <td style="text-align:left; padding-left: 5%;"> Model Intercept </td>
                        <td style="text-align:left;">  </td>
                        </tr>
                        <tr>
                        <td style="text-align:left;"> \\(b_i\\) </td>
                        <td style="text-align:left; padding-left: 5%;"> Random spatial effect </td>
                        <td style="text-align:left;"> BYM2 </td>
                        </tr>
                        <tr>
                        <td style="text-align:left;"> \\(\\gamma_t\\) </td>
                        <td style="text-align:left; padding-left: 5%;"> Random temporal structured effect </td>
                        <td style="text-align:left;"> RW1 </td>
                        </tr>
                        <tr>
                        <td style="text-align:left;"> \\(\\delta_{it}\\) </td>
                        <td style="text-align:left; padding-left: 5%;"> Random spatio-temporal effect </td>
                        <td style="text-align:left;"> Knorr-Held type II </td>
                        </tr>
                        </tbody>
                        </table></center>',
                      sep = "<br>"
                     )
                    ))
                 )
    )
  
  output$ui_info <- renderUI({
    info$text[info$outcome == outcome()][[1]]
  })
  
  #Update the sign of the closable button if it has been clicked
  observeEvent(input$coll_btn2,{
    if(input$coll_btn2%%2==0){
      updateActionButton(session,
                         "coll_btn2",
                         label="+")
    }else{
      updateActionButton(session,
                         "coll_btn2",
                         label="-")
    }
  })
  
  #Define formula in function of the selected outcome
  output$info <- renderText({
    
    Ylab <- case_when(
      outcome() == "cas" ~ "Weekly COVID-19 reported cases in the ABS",
      outcome() == "hosp" ~ "Weekly COVID-19 reported hospitalisations in the ABS"
    )
    
    Elab <- case_when(
      outcome() == "cas" ~ "Weekly COVID-19 expected cases in the ABS",
      outcome() == "hosp" ~ "Weekly COVID-19 expected hospitalisations in the ABS"
    )
    
    paste(
      paste0(
     r"(\begin{equation}
     \begin{split}
     Y_{it} \mid \theta_{it} & \sim Poisson(E_i \theta_{it}) \\
     \log{\theta_{it}} &= \alpha + \sum\limits_{k=1}^{K}\beta_k x_{ki} + b_i + \gamma_t + \delta_{it}
     \end{split}
     \end{equation}
    )",
    "<table style = 'width: 100%'>
    <tr>
    <td>\\(Y_{it}\\): ", str_glue("{Ylab}"), "<br>",
    "\\(E_{it}\\): ", str_glue("{Elab}"), "</td>",
    "<td style = 'padding-left: 5%;'>
    \\(x_{ki}\\): ABS socio-demographic characteristics (urban/rural and socio-economic components)
    </td>
    </tr>
    </table>
    "
    ),
    '<center><table style = "width: 70%;">
      <thead>
      <tr>
      <th style="text-align:left;"> Symbol </th>
      <th style="text-align:left; padding-left: 5%;"> Effect </th>
      <th style="text-align:left;"> Model </th>
      </tr>
      </thead>
      <tbody>
      <tr>
      <td style="text-align:left;"> \\(\\theta_{it}\\) </td>
      <td style="text-align:left; padding-left: 5%;"> Relative Risk of the ABS in the week </td>
      <td style="text-align:left;">  </td>
      </tr>
      <tr>
      <td style="text-align:left;"> \\(\\alpha\\) </td>
      <td style="text-align:left; padding-left: 5%;"> Model Intercept </td>
      <td style="text-align:left;">  </td>
      </tr>
      <tr>
      <td style="text-align:left;"> \\(b_i\\) </td>
      <td style="text-align:left; padding-left: 5%;"> Random spatial effect </td>
      <td style="text-align:left;"> BYM2 </td>
      </tr>
      <tr>
      <td style="text-align:left;"> \\(\\gamma_t\\) </td>
      <td style="text-align:left; padding-left: 5%;"> Random temporal structured effect </td>
      <td style="text-align:left;"> RW1 </td>
      </tr>
      <tr>
      <td style="text-align:left;"> \\(\\delta_{it}\\) </td>
      <td style="text-align:left; padding-left: 5%;"> Random spatio-temporal effect </td>
      <td style="text-align:left;"> Knorr-Held type II </td>
      </tr>
      </tbody>
      </table></center>',
    sep = "<br>")
    
    
    #kable to generate the table in latex
    # tibble(
    #   symbol = c("\\(\\theta_{it}\\)","\\(\\alpha\\)", "\\(b_i\\)", "\\(\\gamma_t\\)", "\\(\\delta_{it}\\)"),
    #   label = c("Relative Risk of the ABS in the week", "Model Intercept", "Random spatial effect", "Random temporal structured effect", "Random spatio-temporal effect"),
    #   model = c("", "", "BYM2", "RW1", "Knorr-Held type II")
    # ) %>% 
    #   kable()
    
    
  })
  
  #Model summary table fixed effects
  output$summary_fixed <- renderDataTable({
    
    table <- res_model %>% 
      filter(outcomes == outcome()) %>% 
      dplyr::select(hp) %>% 
      unnest(hp) %>% 
      filter(
        var %in%
        c(
          "(Intercept)",
          "urbanUrban",
          "poblacio_exempta_de_copagament_farmaceutic",
          "poblacio_amb_rendes_inferiors_a_18_000_euros",
          "poblacio_amb_nivell_dinstruccio_insuficient",
          "taxa_de_mortalitat_prematura",
          "hospitalitzacions_evitables"
        )
      ) %>% 
      mutate(
        var = factor(var, levels = c(
          "(Intercept)",
          "urbanUrban",
          "poblacio_exempta_de_copagament_farmaceutic",
          "poblacio_amb_rendes_inferiors_a_18_000_euros",
          "poblacio_amb_nivell_dinstruccio_insuficient",
          "taxa_de_mortalitat_prematura",
          "hospitalitzacions_evitables"
        ), labels = c(
          "(Intercept)",
          "Urban vs Rural",
          "Population exempted from pharmaceutical co-payment",
          "Population income < 18k€",
          "Population with inadequate education",
          "Premature mortality rate",
          "Avoidable hospitalisations"
        ))
      )
    
    
    datatable(
      table,
      rownames = NULL,
      colnames = c("Effect", "RR [CI95%]"),
      options = list(pageLength = 7,
                     dom = "t",
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
    
  })
  
  #Model summary table random effects
  output$summary_random <- renderDataTable({
    
    table <- res_model %>% 
      filter(outcomes == outcome()) %>% 
      dplyr::select(hp) %>% 
      unnest(hp) %>% 
      filter(
        var %in%
          c(
            "Precision for idarea",
            "Phi for idarea",
            "Precision for idtime",
            "Precision for idareatime"
          )
      ) %>% 
      mutate(
        var = factor(var, levels = c(
          "Precision for idarea",
          "Phi for idarea",
          "Precision for idtime",
          "Precision for idareatime"
        ), labels = c(
          "Standard Deviation (spatial effect)",
          "Phi (spatial effect)",
          "Standard Deviation (temporal effect)",
          "Standard Deviation (spatio-temporal effect)"
        ))
      )
    
    
    datatable(
      table,
      rownames = NULL,
      colnames = c("Hyperparameter", "RR [CI95%]"),
      options = list(pageLength = 4,
                     dom = "t",
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
    
  })
  
}
