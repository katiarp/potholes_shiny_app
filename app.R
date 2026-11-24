library(shiny)
library(tidyverse)
library(fpp3)
library(gt)

# loads once outside server
potholes <- readRDS('data/potholes_monthly_district.rds')

first_month <- min(potholes$month)
last_month <- max(potholes$month)

### UI object
ui <- fluidPage(
  titlePanel('San Diego Pothole Requests: Forecasting by Council District'),
  sidebarLayout(
    sidebarPanel(
      selectInput('districts', 'Choose Council District(s)',
        choices = levels(potholes$council_district),
        selected = c('4', '6'),
        multiple = TRUE),
        actionButton("clear_btn", "Clear Selection", class = 'btn-primary'),
      sliderInput('date_range', 'Date range',
        min = as.Date(first_month), max = as.Date(last_month),
        value = c(as.Date(first_month), as.Date(last_month)),
        timeFormat = '%b %Y'),
        # helpText('Changes view only. Models always use all data up to the split.'),
      sliderInput('split', 'Last training month',
        # three years minimum for training
        min = as.Date(first_month + 36),
        # 1 month for validation minimum
        max = as.Date(last_month - 12),
        value = as.Date(yearmonth('2025 Oct')),
        timeFormat = '%b %Y'),
      helpText('Models train on data through this month. The 12+ months after it are used for validation.'),

      sliderInput('h', 'Forecast horizon (months)',
        min = 1, max = 24, value = 12, step =1),
      checkboxGroupInput('models', 'Models to show',
        choices = c('TSLM', 'ETS', 'ARIMA', 'ENSEMBLE'),
        selected = c('TSLM', 'ETS')),
      helpText('ENSEMBLE is the average of the ETS and ARIMA forecasts.'),
      actionButton('fit_btn', 'Fit model', class = 'btn-primary'),
      helpText('Click after changing districts or the split'),
      
    
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Overview', plotOutput('overview_plot')),
        tabPanel('Model Specs & Accuracy', verbatimTextOutput('fit_check'), br(), gt_output('spec_tble'), gt_output('acc_tbl')),

        tabPanel('Forecasts', plotOutput('fc_plot')),
        tabPanel('Share of Citywide')
      )
    )
  )
)


### Server object
server <- function(input, output, session){
  # two horizons: full validation window for scoring, user's h for the plot
  # split_fit only updates on Fit click, so it always matches the fitted models
  split_fit <- reactive(yearmonth(input$split)) |>
    bindEvent(input$fit_btn, ignoreNULL = FALSE)
  h_valid  <- reactive(length(seq(split_fit() + 1, last_month, by = 1)))
  fc_valid <- reactive(fit() |> forecast(h = h_valid()))
  fc_plot  <- reactive(fit() |> forecast(h = input$h))


  filtered <- reactive({
    req(input$districts)
    potholes |> 
      filter(council_district %in% input$districts, 
            month >= yearmonth(input$date_range[1]),
            month <= yearmonth(input$date_range[2]))
  })

  # requests by council district
  observeEvent(input$clear_btn, {
    updateSelectInput(session, 'districts', selected = character(0))
  })

  output$overview_plot <- renderPlot({
    filtered() |> 
      autoplot(requests_unique) + 
      geom_vline(xintercept = as.Date(yearmonth(input$split) + 1),
                 linetype = 'dashed', colour = 'orange') +
      facet_wrap(~ council_district, ncol = 1, scales = 'free_y') +
      labs(title = 'Unique pothole requests by council district',
           subtitle = 'Dashed line = start of validation period',
           x = 'Month', y = 'Unique requests') +
      theme(legend.position = 'none') 
  }, height = function() 200 * length(input$districts))

  # training data: selected districts all history up to the split
  train <- reactive({
    req(input$districts)
    potholes |> 
      filter(council_district %in% input$districts,
        month <= split_fit())
  })
  # fits all four models; check boxes filter display
  fit <- reactive({
    train() |> 
      model(
        TSLM  = TSLM(requests_unique ~ trend() + season()),
        ETS   = ETS(requests_unique),
        ARIMA = ARIMA(requests_unique)
      ) |>
      mutate(ENSEMBLE = (ETS + ARIMA) / 2)
  }) |> 
    bindEvent(input$fit_btn, ignoreNULL = FALSE)
  
  # output$fit_check <- renderPrint(fit())
  # model specifications
  output$spec_tble <- render_gt({
    fit() |> 
      as_tibble() |> 
      transmute(`Council District` = council_district,
                ETS = map_chr(ETS, model_sum),
                ARIMA = map_chr(ARIMA, model_sum)) |> 
      gt() |> 
      tab_header(title = 'Auto-selected model specifications')
  })

  # accuracy table valid note
  output$acc_tbl <- render_gt({
    req(input$models)
    bind_rows(
      accuracy(fit()) |> mutate(Window = 'Training'),
      accuracy(fc_valid(), potholes) |> mutate(Window = 'Validation')
    ) |> 
      filter(.model %in% input$models) |> 
      select(Window, `Council District` = council_district,
        Model = .model, RMSE, MAE, MAPE) |> 
      arrange(Window, `Council District`, RMSE) |> 
      gt(groupname_col = 'Window') |> 
      fmt_number(c(RMSE, MAE, MAPE), decimals = 1) |> 
      tab_source_note(paste0(
        'Validation scores use all ',  h_valid(), ' months after the split (',
        format(split_fit() + 1), ' - ', format(last_month), 
        '), regardless of forecast horizon'))
  })

  # forecasts vs actuals, same layout as the analysis.qmd figure.
  # history starts 2025 Jan, or 9 months before the split if the split is earlier
  output$fc_plot <- renderPlot({
    req(input$models)
    hist_start <- min(yearmonth('2025 Jan'), split_fit() - 9)
    actuals <- potholes |>
      filter(council_district %in% unique(fit()$council_district)) |>
      filter_index(format(hist_start) ~ .)

    fc_plot() |>
      filter(.model %in% input$models) |>
      autoplot(actuals) +
      facet_grid(council_district ~ .model, scales = 'free_y') +
      labs(
        title = 'Unique Pothole Request Forecasts by Council District',
        y = 'Number of Unique Requests',
        x = 'Month'
      )
  }, height = function() 250 * max(1, length(unique(fit()$council_district))))
  

}




shinyApp(ui, server)