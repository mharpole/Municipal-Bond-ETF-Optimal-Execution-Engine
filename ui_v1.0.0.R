# ============================================================================
# ui.R
# Municipal Bond ETF — Optimal Execution Engine (dashboard UI)
# Version: 1.0.0
# ============================================================================

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(DT)

source("global_helpers.R")

ticker_choices <- setNames(TICKER_INFO$ticker, paste0(TICKER_INFO$ticker, " \u2014 ", TICKER_INFO$name))
seasonality_choices <- names(SEASONALITY_MAP)
market_impact_choices <- names(MARKET_IMPACT_PRESETS)

ui <- page_sidebar(

  title = "Municipal Bond ETF \u2014 Optimal Execution Engine",

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1f4e79",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),

  sidebar = sidebar(
    width = 360,
    title = "Execution Configuration",

    card(
      class = "mb-3",
      card_header("Saved Configurations"),
      selectInput("saved_order_select", NULL, choices = c("\u2014 Select \u2014" = "")),
      layout_columns(
        col_widths = c(6, 6),
        actionButton("load_order_btn", "Load", icon = icon("folder-open"),
                     class = "btn-outline-primary btn-sm w-100"),
        actionButton("delete_order_btn", "Delete", icon = icon("trash"),
                     class = "btn-outline-danger btn-sm w-100")
      ),
      tags$hr(class = "my-2"),
      textInput("order_name", NULL, placeholder = "Name this configuration\u2026"),
      actionButton("save_order_btn", "Save Current Configuration", icon = icon("save"),
                   class = "btn-outline-secondary btn-sm w-100")
    ),

    pickerInput(
      "ticker", "Asset Picker",
      choices = ticker_choices, selected = "VTEB",
      options = list(`live-search` = TRUE)
    ),
    uiOutput("market_data_status"),
    tags$div(
      class = "text-muted small mb-3 mt-1",
      "Rate level/volatility (Treasury yields) and credit stress (HY OAS) are ",
      "pulled automatically \u2014 see README for sources. No manual proxy inputs needed."
    ),

    sliderInput("horizon_days", "Execution Horizon (Trading Days)",
                min = 5, max = 60, value = 30, step = 1),

    selectInput("seasonality", "Seasonality / Flow Regime",
                choices = seasonality_choices, selected = "Neutral"),

    sliderInput("lambda", "Risk Aversion (\u03bb)",
                min = 0, max = 1000000, value = 200000, step = 10000),
    tags$div(
      class = "text-muted small mb-3",
      "Low \u03bb: patient, cost-minimizing schedule. High \u03bb: front-loaded schedule that ",
      "prioritizes certainty of execution over price improvement."
    ),

    tags$hr(),
    tags$div(class = "text-muted small mb-2", "ORDER DETAILS"),

    layout_columns(
      col_widths = c(6, 6),
      numericInput("price0", "ETF Price ($, auto)", value = 51.20, min = 1, step = 0.01),
      numericInput("prem0", "Premium/Discount (%)", value = -0.10, step = 0.01)
    ),
    layout_columns(
      col_widths = c(6, 6),
      numericInput("duration", "Duration (yrs, auto)", value = 6.1, min = 0.1, step = 0.1),
      numericInput("order_notional", "Order Notional ($)", value = 250000, min = 1000, step = 1000)
    ),
    radioButtons("tranche_freq", "Schedule Granularity",
                 choices = c("Daily", "Weekly"), selected = "Daily", inline = TRUE),
    selectInput("market_impact", "Market Impact / Liquidity",
                choices = market_impact_choices, selected = market_impact_choices[2]),

    actionButton("run_btn", "Run Simulation", icon = icon("play"),
                 class = "btn-primary w-100 mt-3"),

    tags$div(class = "text-muted small mt-3 text-center",
             paste0("v", APP_VERSION))
  ),

  navset_card_tab(

    nav_panel(
      "Threshold Schedule",
      p(class = "text-muted mt-2",
        "Optimal buy-limit thresholds derived from the Almgren-Chriss execution ",
        "trajectory, calibrated against the Monte Carlo simulated price distribution ",
        "for each tranche."),
      shinycssloaders::withSpinner(DTOutput("schedule_table"), type = 6, color = "#1f4e79")
    ),

    nav_panel(
      "Simulation Paths",
      p(class = "text-muted mt-2",
        "Percentile bands are computed from the full Monte Carlo simulation; the ",
        "shaded spaghetti lines show a representative random sample of individual ",
        "paths for visual clarity. The dotted red line is the derived buy-limit schedule."),
      shinycssloaders::withSpinner(plotlyOutput("sim_plot", height = "460px"), type = 6, color = "#1f4e79"),
      shinycssloaders::withSpinner(plotlyOutput("premium_plot", height = "300px"), type = 6, color = "#1f4e79")
    ),

    nav_panel(
      "Trade Metrics",
      shinycssloaders::withSpinner(uiOutput("metrics_boxes"), type = 6, color = "#1f4e79"),
      br(),
      h5("Model Diagnostics"),
      tableOutput("model_diag")
    )
  )
)

ui
