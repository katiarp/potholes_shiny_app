library(shiny)
library(tidyverse)
library(fpp3)
library(urca)

# loads once outside server
potholes <- readRDS('data/potholes_monthly_district.rds')

first_month <- min(potholes$month)
last_month <- max(potholes$month)

citywide <- potholes |>
  as_tibble() |>
  group_by(month) |>
  summarise(
    requests_unique = sum(requests_unique),
    total_reports   = sum(total_reports)
  ) |>
  mutate(council_district = 'Citywide')

potholes <- potholes |>
  as_tibble() |>
  mutate(council_district = as.character(council_district)) |>
  bind_rows(citywide) |>
  mutate(council_district = factor(
    council_district,
    levels = c('Citywide', as.character(1:9))
  )) |> 
  as_tsibble(index = month, key = council_district)

default_districts <- c('6', '8')
default_split     <- as.Date(yearmonth('2025 Oct'))
default_models    <- c('TSLM', 'ETS')

### UI object
ui <- fluidPage(
  titlePanel('San Diego Pothole Requests: Forecasting by Council District'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        'districts', 'Select Council District(s)',
        choices = levels(potholes$council_district),
        selected = default_districts,
        multiple = TRUE
      ),
      actionButton('reset', 'Reset to defaults', class = 'btn-primary'),
      sliderInput(
        'date_range', 'Date range',
        min = as.Date(first_month), max = as.Date(last_month),
        value = c(as.Date(first_month), as.Date(last_month)),
        timeFormat = '%b %Y'
      ),
      helpText('Changes view only. Models always use all data up to the split.'),
      sliderInput(
        'split', 'Last training month',
        # three years minimum for training
        min = as.Date(first_month + 36),
        # 1 month for validation minimum
        max = as.Date(last_month - 12),
        value = default_split,
        timeFormat = '%b %Y'
      ),
      helpText('Models train on data through this month. The 12+ months after it are used for validation.'),
      sliderInput(
        'h', 'Forecast horizon (months)',
        min = 1, max = 24, value = 12, step = 1
      ),
      checkboxGroupInput(
        'models', 'Models to show',
        choices = c('TSLM', 'ETS', 'ARIMA', 'ENSEMBLE'),
        selected = default_models
      ),
      helpText('ENSEMBLE is the average of the ETS and ARIMA forecasts.'),
      actionButton('fit_btn', 'Fit model', class = 'btn-primary'),
      helpText('Click after changing districts or the split')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          'Overview',
          plotOutput('overview_plot', height = 'auto'),
          checkboxInput('free_y_overview', 'Separate y-axis per district', value = FALSE)
        ),
        tabPanel(
          'Model Specs & Accuracy',
          h4('Auto-selected Model Specifications'),
          tableOutput('spec_tble'),
          h4('Training vs. Validation Accuracy'),
          tableOutput('acc_tbl'),
          helpText(textOutput('acc_note'))
        ),
        tabPanel(
          'Best Model',
          h4('Best model by district (validation window)'),
          radioButtons(
            'metric', 'Rank models by',
            choices = c('RMSE', 'MAE', 'MAPE'),
            selected = 'MAPE', inline = TRUE
          ),
          tableOutput('best_tbl'),
          helpText(textOutput('best_note'))
        ),
        tabPanel(
          'Forecasts',
          plotOutput('forecast_plot', height = 'auto'),
          helpText(textOutput('fc_note')),
          checkboxInput('free_y_forecast', 'Separate y-axis per district', value = FALSE)
        )
      )
    )
  )
)


### Server object
server <- function(input, output, session) {

  split_used <- reactive(yearmonth(input$split)) |>
    bindEvent(input$fit_btn, ignoreNULL = FALSE)

  # two horizons: full validation window for scoring, user's h for the plot
  # split_used only updates on Fit click, so it always matches the fitted models
  h_valid  <- reactive(length(seq(split_used() + 1, last_month, by = 1)))
  fc_valid <- reactive(fit() |> forecast(h = h_valid()))
  fc_plot  <- reactive(fit() |> forecast(h = input$h))

  filtered <- reactive({
    req(input$districts)
    potholes |>
      filter(
        council_district %in% input$districts,
        month >= yearmonth(input$date_range[1]),
        month <= yearmonth(input$date_range[2])
      )
  })

  # requests by council district and default reset
  observeEvent(input$reset, {
    updateSelectInput(session, 'districts', selected = default_districts)
    updateSliderInput(
      session, 'date_range',
      value = c(as.Date(first_month), as.Date(last_month)),
      timeFormat = '%b %Y'
    )
    updateSliderInput(session, 'split', value = default_split, timeFormat = '%b %Y')
    updateSliderInput(session, 'h', value = 12)
    updateCheckboxGroupInput(session, 'models', selected = default_models)
    updateCheckboxInput(session, 'free_y_overview', value = FALSE)
    updateCheckboxInput(session, 'free_y_forecast', value = FALSE)
  })

  # Overview Plot
  output$overview_plot <- renderPlot({
    validate(need(input$districts, 'Select at least one district to see the plot.'))
    filtered() |>
      autoplot(requests_unique) +
      geom_vline(
        xintercept = as.Date(yearmonth(input$split) + 1),
        linetype = 'dashed', colour = 'orange'
      ) +
      facet_wrap(
        ~ council_district,
        ncol = 1,
        labeller = labeller(council_district = \(x) ifelse(x == 'Citywide', x, paste('District', x))),
        scales = if (input$free_y_overview) 'free_y' else 'fixed'
        # scales = 'free_y'
      ) + coord_cartesian(xlim = as.Date(input$date_range)) +
      labs(
        title = 'Unique pothole requests by council district',
        subtitle = 'Dashed line = start of validation period',
        x = 'Month', y = 'Unique requests'
      ) +
      theme_gray(base_size = 17) +
      theme(legend.position = 'none')
  }, height = function() 200 * max(1, length(input$districts)))

  # training data: selected districts all history up to the split
  train <- reactive({
    req(input$districts)
    potholes |>
      filter(
        council_district %in% input$districts,
        month <= split_used()
      )
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

  # model specifications
  output$spec_tble <- renderTable({
    fit() |>
      as_tibble() |>
      transmute(
        `Council District` = council_district,
        ETS = map_chr(ETS, model_sum),
        ARIMA = map_chr(ARIMA, model_sum)
      )
  }, striped = TRUE, bordered = TRUE)

  # accuracy table and validation note
  output$acc_tbl <- renderTable({
    req(input$models)
    bind_rows(
      accuracy(fit()) |> mutate(Window = 'Training'),
      accuracy(fc_valid(), potholes) |> mutate(Window = 'Validation')
    ) |>
      filter(.model %in% input$models) |>
      select(
        Window, `Council District` = council_district,
        Model = .model, RMSE, MAE, MAPE
      ) |>
      arrange(Window, `Council District`, RMSE)
  }, striped = TRUE, bordered = TRUE, digits = 1)

  output$acc_note <- renderText(paste0(
    'Validation scores use all ', h_valid(), ' months after the split (',
    format(split_used() + 1), ' - ', format(last_month),
    '), regardless of forecast horizon'
  ))

  # Best Model
  acc_valid <- reactive({
    req(input$models)
    accuracy(fc_valid(), potholes) |>
      filter(.model %in% input$models)
  })

  output$best_tbl <- renderTable({
    best <- acc_valid() |>
      group_by(council_district) |>
      slice_min(.data[[input$metric]], n = 1, with_ties = FALSE) |>
      select(council_district, `Best Model` = .model)

    acc_valid() |>
      select(council_district, .model, all_of(input$metric)) |>
      pivot_wider(names_from = .model, values_from = all_of(input$metric)) |>
      left_join(best, by = 'council_district') |>
      rename(`Council District` = council_district) |>
      select(`Council District`, any_of(c('TSLM', 'ETS', 'ARIMA', 'ENSEMBLE')), `Best Model`)
  }, striped = TRUE, bordered = TRUE, digits = 1)

  output$best_note <- renderText(paste0(
    'Values show validation ', input$metric, ' for ',
    format(split_used() + 1), ' - ', format(last_month),
    '. Only checked models are compared.'
  ))

  # forecasts vs actuals: history starts 24 months before the split
  # (split_used() moves only on Fit click, so plot and models always match)
  output$forecast_plot <- renderPlot({
    validate(
      need(input$districts, 'Select at least one district to see the plot.'),
      need(input$models, 'Select at least one model to see the plot.')
    )

    history <- potholes |>
      filter(
        council_district %in% unique(fit()$council_district),
        month >= split_used() - 24,
        month <= split_used() + input$h
      )

    model_cols <- c(TSLM = '#7B61FF', ETS = '#00A6A6', ARIMA = '#E4572E', ENSEMBLE = '#76B041')
    fc_plot() |>
      filter(.model %in% input$models) |>
      autoplot(history) +
      facet_grid(
        council_district ~ .model,
        labeller = labeller(council_district = \(x) ifelse(x == 'Citywide', x, paste('District', x))),
        scales = if (input$free_y_forecast) 'free_y' else 'fixed'
      ) +
      scale_colour_manual(values = model_cols) +
      scale_fill_manual(values = model_cols) +
      labs(
        title = 'Unique Pothole Request Forecasts by Council District',
        y = 'Number of Unique Requests',
        x = 'Month'
      ) +
      theme_gray(base_size = 17) +
      theme(strip.text.y = element_text(angle = 0))
  }, height = function() 250 * max(1, length(unique(fit()$council_district))))

  # note under the forecast plot
  output$fc_note <- renderText(paste0(
    'Shaded bands: 80% and 95% prediction intervals. Horizon: ', input$h,
    ' months (set by slider). Accuracy uses the full validation window (',
    format(split_used() + 1), ' - ', format(last_month), ').'
  ))
}

shinyApp(ui, server)
