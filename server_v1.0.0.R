# ============================================================================
# server.R
# Municipal Bond ETF — Optimal Execution Engine (server logic)
# Version: 1.0.0
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(plotly)
library(DT)

source("global_helpers.R")

server <- function(input, output, session) {

  # ---- Market data: price (per ticker) + rate/credit environment (market-
  # wide, fetched once per session — cached_fred_series() still avoids a
  # network hit if another session already refreshed it within 24h) --------

  live_calibration <- reactiveVal(NULL)
  suppress_ticker_autofetch <- reactiveVal(FALSE)

  # Rate & credit environment don't depend on the ticker, so they're fetched
  # once at session start rather than on every ticker change.
  market_env <- list(rate = fetch_rate_environment(), credit = fetch_credit_stress())

  observeEvent(input$ticker, {
    if (isTRUE(suppress_ticker_autofetch())) {
      suppress_ticker_autofetch(FALSE)
      return()
    }
    cal <- fetch_live_calibration(input$ticker)
    updateNumericInput(session, "price0", value = round(cal$price0, 2))

    info <- TICKER_INFO[TICKER_INFO$ticker == input$ticker, ]
    if (nrow(info) == 1) {
      updateNumericInput(session, "duration", value = info$default_duration)
    }
    live_calibration(cal)
  }, ignoreInit = FALSE)

  output$market_data_status <- renderUI({
    cal <- live_calibration()
    req(cal)

    rate_env <- market_env$rate
    credit_env <- market_env$credit

    line <- function(problem, text) {
      tags$div(
        class = paste(if (problem) "text-warning" else "text-success", "small"),
        if (problem) icon("triangle-exclamation") else icon("circle-check"), " ", text
      )
    }

    price_line <- if (!is.na(cal$last_price_date)) {
      line(isTRUE(cal$stale), sprintf("Price: %s (%s)", cal$source, format(cal$last_price_date, "%b %d")))
    } else {
      line(TRUE, cal$source)
    }

    rate_line <- if (!is.na(rate_env$as_of)) {
      line(grepl("stale|fallback", rate_env$source, ignore.case = TRUE),
           sprintf("Rates: %s (%s) \u2014 r\u2080 %.2f%%, r\u0304 %.2f%%",
                   rate_env$source, format(rate_env$as_of, "%b %d"), rate_env$r0 * 100, rate_env$r_bar * 100))
    } else {
      line(TRUE, rate_env$source)
    }

    credit_line <- if (!is.na(credit_env$as_of)) {
      line(grepl("stale|fallback", credit_env$source, ignore.case = TRUE),
           sprintf("Credit: %s (%s) \u2014 HY OAS %.2f%%",
                   credit_env$source, format(credit_env$as_of, "%b %d"), credit_env$spread_pct))
    } else {
      line(TRUE, credit_env$source)
    }

    tagList(price_line, rate_line, credit_line)
  })

  # ---- Saved order configurations ------------------------------------------

  saved_orders <- reactiveVal(load_saved_orders())

  observe({
    nms <- names(saved_orders())
    updateSelectInput(session, "saved_order_select",
                       choices = c("\u2014 Select \u2014" = "", nms),
                       selected = "")
  })

  apply_saved_order <- function(cfg) {
    for (id in names(cfg)) {
      val <- cfg[[id]]
      if (is.null(val)) next
      type <- ORDER_INPUT_TYPES[[id]]
      switch(type,
        numeric = updateNumericInput(session, id, value = val),
        slider  = updateSliderInput(session, id, value = val),
        select  = updateSelectInput(session, id, selected = val),
        radio   = updateRadioButtons(session, id, selected = val),
        picker  = shinyWidgets::updatePickerInput(session, id, selected = val)
      )
    }
  }

  observeEvent(input$save_order_btn, {
    nm <- trimws(input$order_name)
    req(nchar(nm) > 0)

    snapshot <- setNames(
      lapply(ALL_ORDER_INPUT_IDS, function(id) input[[id]]),
      ALL_ORDER_INPUT_IDS
    )
    orders <- saved_orders()
    orders[[nm]] <- snapshot
    saved_orders(orders)
    save_saved_orders(orders)

    updateTextInput(session, "order_name", value = "")
    updateSelectInput(session, "saved_order_select", selected = nm)
    showNotification(paste0("Saved configuration \u201c", nm, "\u201d"), type = "message", duration = 3)
  })

  observeEvent(input$load_order_btn, {
    nm <- input$saved_order_select
    req(nchar(nm) > 0)
    cfg <- saved_orders()[[nm]]
    req(!is.null(cfg))

    suppress_ticker_autofetch(TRUE)
    apply_saved_order(cfg)
    showNotification(paste0("Loaded configuration \u201c", nm, "\u201d"), type = "message", duration = 3)
  })

  observeEvent(input$delete_order_btn, {
    nm <- input$saved_order_select
    req(nchar(nm) > 0)
    orders <- saved_orders()
    orders[[nm]] <- NULL
    saved_orders(orders)
    save_saved_orders(orders)
    updateSelectInput(session, "saved_order_select", selected = "")
    showNotification(paste0("Deleted configuration \u201c", nm, "\u201d"), type = "warning", duration = 3)
  })

  # ---- Core compute: runs once on load, then on every "Run Simulation" click

  engine_result <- eventReactive(input$run_btn, {

    validate(
      need(input$horizon_days >= 5, "Execution horizon must be at least 5 trading days."),
      need(input$price0 > 0, "Current ETF price must be positive."),
      need(input$order_notional > 0, "Order notional must be positive.")
    )

    inputs <- list(
      ticker         = input$ticker,
      horizon_days   = as.integer(input$horizon_days),
      start_date     = Sys.Date(),
      seasonality    = input$seasonality,
      lambda         = input$lambda,
      price0         = input$price0,
      prem0          = input$prem0 / 100,
      duration       = input$duration,
      order_notional = input$order_notional,
      market_impact  = input$market_impact
    )

    withProgress(message = "Running Monte Carlo execution engine\u2026", value = 0.1, {
      incProgress(0.2, detail = "Pulling rate & credit environment\u2026")
      res <- run_engine(inputs)
      incProgress(0.7, detail = "Deriving optimal thresholds\u2026")
      res
    })
  })

  # ---- Tab 1: Threshold Schedule ------------------------------------------

  output$schedule_table <- DT::renderDataTable({
    res <- engine_result()
    req(res)
    sched <- res$schedule

    if (input$tranche_freq == "Weekly") {
      display <- sched %>%
        mutate(week = ceiling(day / 5)) %>%
        group_by(week) %>%
        summarise(
          Period = paste(format(min(date), "%b %d"), "-", format(max(date), "%b %d")),
          `Shares to Buy` = round(sum(ac_shares)),
          `Cumulative %` = max(ac_cum_pct),
          `Target Fill Prob` = max(target_fill_prob),
          `Buy Limit ($)` = max(threshold_price),
          `Median Simulated Price ($)` = dplyr::last(median_sim_price),
          .groups = "drop"
        ) %>%
        select(-week)
    } else {
      display <- sched %>%
        mutate(Period = format(date, "%a, %b %d")) %>%
        select(Period,
               `Shares to Buy` = ac_shares,
               `Cumulative %` = ac_cum_pct,
               `Target Fill Prob` = target_fill_prob,
               `Buy Limit ($)` = threshold_price,
               `Median Simulated Price ($)` = median_sim_price)
    }

    DT::datatable(
      display, rownames = FALSE,
      options = list(pageLength = 15, dom = "tip", scrollX = TRUE)
    ) %>%
      DT::formatCurrency(c("Buy Limit ($)", "Median Simulated Price ($)"), currency = "$") %>%
      DT::formatPercentage(c("Cumulative %", "Target Fill Prob"), digits = 1) %>%
      DT::formatRound("Shares to Buy", digits = 0)
  })

  # ---- Tab 2: Simulation Paths ---------------------------------------------

  output$sim_plot <- plotly::renderPlotly({
    res <- engine_result()
    req(res)
    sim <- res$sim
    n_paths <- nrow(sim$etf_price)
    n_steps <- ncol(sim$etf_price) - 1
    days <- 0:n_steps

    sample_n <- min(200, n_paths)
    sample_idx <- sample.int(n_paths, sample_n)
    x_spag <- rep(c(days, NA), times = sample_n)
    y_mat <- cbind(sim$etf_price[sample_idx, , drop = FALSE], NA)
    y_spag <- as.vector(t(y_mat))

    probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    qmat <- apply(sim$etf_price, 2, quantile, probs = probs)

    threshold_y <- c(NA, res$thresholds$thresholds)

    p <- plot_ly()
    p <- add_lines(p, x = x_spag, y = y_spag,
                    line = list(color = "rgba(70,130,180,0.10)", width = 1),
                    hoverinfo = "skip", showlegend = FALSE, name = "Sample Paths")
    p <- add_ribbons(p, x = days, ymin = qmat["5%", ], ymax = qmat["95%", ],
                      fillcolor = "rgba(70,130,180,0.15)", line = list(width = 0),
                      name = "5th\u201395th Pctile (full sim)")
    p <- add_ribbons(p, x = days, ymin = qmat["25%", ], ymax = qmat["75%", ],
                      fillcolor = "rgba(70,130,180,0.32)", line = list(width = 0),
                      name = "25th\u201375th Pctile")
    p <- add_lines(p, x = days, y = qmat["50%", ],
                    line = list(color = "#1f4e79", width = 2.5), name = "Median Path")
    p <- add_lines(p, x = days, y = threshold_y,
                    line = list(color = "#c0392b", width = 2, dash = "dot"),
                    name = "Optimal Buy Limit")

    p %>% layout(
      title = list(
        text = paste0(format(n_paths, big.mark = ","), " Simulated Paths \u2014 ", res$ticker,
                       " (", sample_n, " sampled for display)"),
        font = list(size = 15)
      ),
      xaxis = list(title = "Trading Day"),
      yaxis = list(title = "ETF Price ($)", tickprefix = "$"),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2)
    )
  })

  output$premium_plot <- plotly::renderPlotly({
    res <- engine_result()
    req(res)
    final_prem <- res$sim$premium[, ncol(res$sim$premium)] * 100

    plot_ly(x = final_prem, type = "histogram", nbinsx = 60,
            marker = list(color = "rgba(31,78,121,0.75)",
                          line = list(color = "white", width = 0.3))) %>%
      layout(
        title = list(text = "Simulated Premium / Discount to NAV \u2014 End of Horizon",
                     font = list(size = 14)),
        xaxis = list(title = "Premium (+) / Discount (\u2013) vs. NAV (%)", ticksuffix = "%"),
        yaxis = list(title = "Simulated Paths")
      )
  })

  # ---- Tab 3: Trade Metrics ------------------------------------------------

  output$metrics_boxes <- renderUI({
    res <- engine_result()
    req(res)
    m <- res$metrics

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Empirical Fill Probability",
        value = sprintf("%.1f%%", m$fill_probability * 100),
        showcase = icon("check-circle"), theme = "success"
      ),
      value_box(
        title = "Expected Execution Cost",
        value = sprintf("%.1f bps", m$expected_cost_bps),
        showcase = icon("chart-line"), theme = "primary"
      ),
      value_box(
        title = "Expected Shortfall (CVaR 95%)",
        value = sprintf("%.1f bps", m$expected_shortfall_bps),
        showcase = icon("triangle-exclamation"), theme = "warning"
      ),
      value_box(
        title = "Structural Safety Margin",
        value = sprintf("%.1f bps", m$safety_margin_bps),
        showcase = icon("shield-halved"), theme = "info"
      )
    )
  })

  output$model_diag <- renderTable({
    res <- engine_result()
    req(res)
    tibble(
      Metric = c("Total Order (Shares)", "Total Order (Notional $)",
                 "Simulated Daily Volatility (\u03c3)", "AC Urgency Parameter (\u03ba\u00b7T)",
                 "Market Impact Assumption", "Macro Event Days in Horizon",
                 "Simulation Paths"),
      Value = c(
        sprintf("%.0f", res$X_shares),
        sprintf("$%s", format(round(res$X_shares * res$sim$etf_price[1, 1]), big.mark = ",")),
        sprintf("%.3f%%", res$sigma_daily * 100),
        sprintf("%.2f", res$ac_kappa_T),
        res$market_impact,
        if (length(res$jump_dates) > 0) paste(format(res$jump_dates, "%b %d"), collapse = ", ") else "None in horizon",
        format(nrow(res$sim$etf_price), big.mark = ",")
      )
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")
}

server
