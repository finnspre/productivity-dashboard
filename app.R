library(shiny)
library(dplyr)
library(plotly)
library(DT)

LP_DATA_FILE <- "lp_data.RData"

# Categorical palette, fixed order -- never cycled, never reassigned by
# filter state. Each selectable industry gets its own fixed slot so a chart
# with several industries selected reads at a glance.
CATEGORICAL_PALETTE <- c(
  "#2a78d6", # blue
  "#008300", # green
  "#e87ba4", # magenta
  "#eda100", # yellow
  "#1baf7a", # aqua
  "#eb6834", # orange
  "#4a3aa7", # violet
  "#e34948"  # red
)
INK_PRIMARY <- "#0b0b0b"
INK_MUTED <- "#898781"
GRIDLINE <- "#e1e0d9"
CHART_SURFACE <- "#fcfcfb"
FONT_FAMILY <- "Roboto, sans-serif"

# lp_data's rows are already grouped sensibly by first appearance (Aggregate
# Business Sector first, then goods-producing industries together, then
# services-producing industries together), so the only preferred-order
# override needed is pinning the aggregate to the front.
DEFAULT_INDUSTRY <- "Aggregate Business Sector"

# data_pipeline.R pulls labour productivity directly from Statistics Canada
# (cansim tables 36-10-0206-01 and 36-10-0207-01), already filtered to the
# labour productivity measure only, and saves the result as `lp_data` here.
load_lp_data <- function(path = LP_DATA_FILE) {
  if (!file.exists(path)) {
    stop(
      "lp_data.RData not found. Run data_pipeline.R from this project folder ",
      "first to pull labour productivity data from Statistics Canada."
    )
  }
  e <- new.env()
  load(path, envir = e)
  df <- e$lp_data
  df <- rename(df, Industry = `North American Industry Classification System (NAICS)`)
  df$Year <- as.integer(df$Year)
  df$Value <- as.numeric(df$Value)
  df[, c("Year", "Industry", "Value")]
}

ordered_unique <- function(values, preferred_order) {
  known <- preferred_order[preferred_order %in% values]
  extra <- unique(values)[!unique(values) %in% known]
  c(known, extra)
}

# Every industry present in the data, in a stable display order (default
# industry first, the rest following the data's own grouping).
industry_choices <- function(df) {
  ordered_unique(df$Industry, DEFAULT_INDUSTRY)
}

# Fixed hue per industry so a color never gets reassigned to a different
# industry as the selection changes.
industry_color_map <- function(df) {
  industries <- industry_choices(df)
  n <- length(industries)
  colors <- if (n <= length(CATEGORICAL_PALETTE)) {
    CATEGORICAL_PALETTE[seq_len(n)]
  } else {
    c(CATEGORICAL_PALETTE, rep(INK_MUTED, n - length(CATEGORICAL_PALETTE)))
  }
  setNames(colors, industries)
}

# Axis/title wording for the currently active view -- growth mode ignores
# rebasing entirely (rebasing a series by a constant doesn't change its
# period-over-period % change, so there's nothing for it to reflect).
display_axis_title <- function(view_mode, rebase_toggle, base_year) {
  if (identical(view_mode, "growth")) {
    "Year-over-year growth (%)"
  } else if (isTRUE(rebase_toggle)) {
    paste0("Labour productivity index (", base_year, "=100)")
  } else {
    "Labour productivity index (2017=100)"
  }
}

# Short chart-title prefix -- display_axis_title() is verbose (includes the
# base year) which reads fine on an axis but crowds a "<title>, <year>" line.
display_short_label <- function(view_mode, rebase_toggle) {
  if (identical(view_mode, "growth")) "YoY growth" else if (isTRUE(rebase_toggle)) "Rebased index" else "Index"
}

# Shared row/column shaping for both the Data Table and the CSV export, so
# the download is always a WYSIWYG match of what's on screen.
build_export_df <- function(df, rebase_toggle, base_year) {
  df <- df %>% arrange(Industry, Year)
  cols <- c("Year", "Industry", "Value", "GrowthPct")
  if (isTRUE(rebase_toggle)) cols <- c(cols, "RebasedValue")
  df[, cols]
}

export_column_labels <- function(cols, base_year) {
  labels <- c(
    Year = "Year", Industry = "Industry",
    Value = "Labour productivity index (2017=100)",
    GrowthPct = "YoY growth (%)"
  )
  if ("RebasedValue" %in% cols) {
    labels["RebasedValue"] <- paste0("Rebased index (", base_year, "=100)")
  }
  labels[cols]
}

ui <- function(request) {
  # Computed directly from the data at page-build time (not via a
  # server-side update*Input call) so the default selection is already
  # present in the first HTML the browser receives.
  init_df <- load_lp_data()

  fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"),
    tags$style("body, .selectize-input, .selectize-dropdown { font-family: 'Roboto', sans-serif; }")
  ),
  titlePanel("Canadian Productivity Dashboard"),
  h5("Labour productivity by industry", style = "color:#52514e; margin-top:-10px;"),
  sidebarLayout(
    sidebarPanel(
      tags$strong("Compare"),
      selectizeInput(
        "industries", "Industry",
        choices = industry_choices(init_df), selected = DEFAULT_INDUSTRY, multiple = TRUE,
        options = list(placeholder = "Add an industry...")
      ),
      tags$p(
        style = "margin-top: -8px; color: #898781; font-size: 0.8em;",
        "Charts stay readable up to about 6 industries at once."
      ),
      tags$strong("Display"),
      radioButtons(
        "view_mode", "Show",
        choices = c("Index level" = "level", "YoY growth %" = "growth"),
        selected = "level", inline = TRUE
      ),
      conditionalPanel(
        "input.view_mode == 'level'",
        checkboxInput("rebase_toggle", "Rebase to a base year", value = FALSE),
        conditionalPanel(
          "input.view_mode == 'level' && input.rebase_toggle == true",
          sliderInput(
            "base_year", "Base year",
            min = min(init_df$Year), max = max(init_df$Year),
            value = min(init_df$Year), step = 1, sep = ""
          )
        )
      ),
      sliderInput(
        "year_range", "Time frame",
        min = min(init_df$Year), max = max(init_df$Year),
        value = c(min(init_df$Year), max(init_df$Year)),
        step = 1, sep = ""
      ),
      actionButton("refresh", "Refresh Data", icon = icon("rotate-right")),
      downloadButton("download_csv", "Download CSV", icon = icon("download")),
      tags$p(
        style = "margin-top: 16px; color: #52514e; font-size: 0.85em;",
        "Data source: Statistics Canada labour productivity data, pulled via ",
        tags$b("data_pipeline.R"), " and cached in lp_data.RData. Re-run that ",
        "script to pull fresh data, then click ", tags$b("Refresh Data"),
        " to update the charts without restarting the app."
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trend", plotlyOutput("trend_chart", height = "480px")),
        tabPanel("Latest Year", plotlyOutput("latest_chart", height = "480px")),
        tabPanel("Growth Ranking", plotlyOutput("ranking_chart", height = "480px")),
        tabPanel("Data Table", DTOutput("data_table"))
      )
    )
  )
  )
}

server <- function(input, output, session) {
  raw_data <- reactiveVal(load_lp_data())

  # Keeps the industry selector, time frame slider, and base year slider in
  # sync with what's available in the data (added/removed on refresh),
  # preserving the user's current picks -- clamped to the new bounds --
  # where possible.
  observe({
    df <- raw_data()
    choices <- industry_choices(df)
    current <- input$industries
    selected <- if (is.null(current)) DEFAULT_INDUSTRY else intersect(current, choices)
    updateSelectizeInput(session, "industries", choices = choices, selected = selected, server = FALSE)

    year_min <- min(df$Year)
    year_max <- max(df$Year)
    current_range <- input$year_range
    range_value <- if (is.null(current_range)) {
      c(year_min, year_max)
    } else {
      c(max(current_range[1], year_min), min(current_range[2], year_max))
    }
    updateSliderInput(session, "year_range", min = year_min, max = year_max, value = range_value)

    year_choices <- sort(unique(df$Year))
    current_base <- input$base_year
    new_base <- if (is.null(current_base) || !(current_base %in% year_choices)) {
      min(year_choices)
    } else {
      current_base
    }
    updateSliderInput(session, "base_year", min = year_min, max = year_max, value = new_base)
  }) |> bindEvent(raw_data(), once = FALSE)

  observeEvent(input$refresh, {
    tryCatch(
      raw_data(load_lp_data()),
      error = function(e) {
        showNotification(paste("Could not reload data:", conditionMessage(e)), type = "error")
      }
    )
  })

  # Full history (not yet limited to the time-frame slider) for the selected
  # industries, with year-over-year growth computed against each industry's
  # actual prior year -- including years outside the currently zoomed window.
  history_with_growth <- reactive({
    req(length(input$industries) > 0)
    raw_data() %>%
      filter(Industry %in% input$industries) %>%
      arrange(Industry, Year) %>%
      group_by(Industry) %>%
      mutate(GrowthPct = (Value / lag(Value) - 1) * 100) %>%
      ungroup()
  })

  # Adds RebasedValue (per industry, relative to input$base_year, looked up
  # against the full history so the base year can sit outside the zoomed
  # time frame) and DisplayValue (whichever column the current view mode
  # calls for). Returns both the data and the list of industries excluded
  # from rebasing for lacking the chosen base year.
  transform_result <- reactive({
    df <- history_with_growth()

    if (isTRUE(input$rebase_toggle) && !is.null(input$base_year)) {
      base_values <- df %>%
        filter(Year == input$base_year) %>%
        select(Industry, BaseValue = Value)
      df <- df %>% left_join(base_values, by = "Industry")
      missing <- setdiff(unique(df$Industry), base_values$Industry)
      df$RebasedValue <- df$Value / df$BaseValue * 100
      df <- df %>% select(-BaseValue)
    } else {
      df$RebasedValue <- NA_real_
      missing <- character(0)
    }

    df$DisplayValue <- if (identical(input$view_mode, "growth")) {
      df$GrowthPct
    } else if (isTRUE(input$rebase_toggle)) {
      df$RebasedValue
    } else {
      df$Value
    }

    list(df = df, missing = missing)
  })

  display_data <- reactive(transform_result()$df)

  rebase_missing_industries <- reactive({
    if (identical(input$view_mode, "growth") || !isTRUE(input$rebase_toggle)) {
      character(0)
    } else {
      transform_result()$missing
    }
  })

  observeEvent(rebase_missing_industries(), {
    missing <- rebase_missing_industries()
    if (length(missing) > 0) {
      showNotification(
        paste0(
          "No ", input$base_year, " data for: ", paste(missing, collapse = ", "),
          " -- excluded from the rebased view."
        ),
        type = "warning"
      )
    }
  })

  filtered_data <- reactive({
    req(input$year_range)
    display_data() %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })

  colors <- reactive(industry_color_map(raw_data()))

  output$trend_chart <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(DisplayValue))
    req(nrow(df) > 0)
    pal <- colors()
    industries <- industry_choices(df)
    axis_title <- display_axis_title(input$view_mode, input$rebase_toggle, input$base_year)
    hover_suffix <- if (identical(input$view_mode, "growth")) "%" else ""

    p <- plot_ly()
    for (ind in industries) {
      sd <- df %>% filter(Industry == ind) %>% arrange(Year)
      if (nrow(sd) == 0) next
      p <- p %>% add_trace(
        data = sd, x = ~Year, y = ~DisplayValue,
        type = "scatter", mode = "lines+markers",
        name = ind, legendgroup = ind,
        line = list(color = pal[[ind]], width = 2),
        marker = list(color = pal[[ind]], size = 7),
        hovertemplate = paste0("%{x}<br>", ind, ": %{y:.1f}", hover_suffix, "<extra></extra>")
      )
    }

    label_points <- df %>%
      group_by(Industry) %>%
      filter(Year == max(Year)) %>%
      ungroup()

    p <- p %>% add_annotations(
      data = label_points,
      x = ~Year, y = ~DisplayValue, text = ~Industry,
      xanchor = "left", yanchor = "middle", xshift = 8,
      showarrow = FALSE, font = list(color = INK_PRIMARY, size = 11, family = FONT_FAMILY)
    )

    p %>% layout(
      xaxis = list(title = "Year", dtick = 1, gridcolor = GRIDLINE, color = INK_MUTED,
                   range = c(min(df$Year), max(df$Year) + 2.2)),
      yaxis = list(title = axis_title, gridcolor = GRIDLINE, color = INK_MUTED),
      paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
      font = list(color = INK_PRIMARY, family = FONT_FAMILY),
      legend = list(orientation = "h", y = -0.2)
    )
  })

  output$latest_chart <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(DisplayValue))
    req(nrow(df) > 0)
    pal <- colors()
    axis_title <- display_axis_title(input$view_mode, input$rebase_toggle, input$base_year)
    hover_suffix <- if (identical(input$view_mode, "growth")) "%" else ""

    latest_year <- max(df$Year)
    snapshot <- df %>%
      filter(Year == latest_year) %>%
      arrange(DisplayValue)
    req(nrow(snapshot) > 0)
    snapshot$Industry <- factor(snapshot$Industry, levels = snapshot$Industry)

    plot_ly(
      data = snapshot, x = ~DisplayValue, y = ~Industry,
      type = "bar", orientation = "h",
      marker = list(color = unname(pal[as.character(snapshot$Industry)])),
      hovertemplate = paste0("%{y}: %{x:.1f}", hover_suffix, "<extra></extra>")
    ) %>%
      layout(
        title = paste0(display_short_label(input$view_mode, input$rebase_toggle), ", ", latest_year),
        xaxis = list(title = axis_title, gridcolor = GRIDLINE, color = INK_MUTED),
        yaxis = list(title = "", gridcolor = GRIDLINE, color = INK_MUTED),
        paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
        font = list(color = INK_PRIMARY, family = FONT_FAMILY)
      )
  })

  # Compound annual growth rate over the selected time frame, always from
  # the raw index -- rebasing a series by a constant cancels out in a
  # ratio, and growth mode isn't a level series with a coherent multi-year
  # ratio at all, so neither toggle has anything to contribute here.
  ranking_data <- reactive({
    req(length(input$industries) > 0)
    rng <- req(input$year_range)
    start_year <- rng[1]
    end_year <- rng[2]
    validate(need(end_year > start_year, "Select a time frame spanning at least two years to compute CAGR."))

    df <- raw_data()
    start_df <- df %>% filter(Industry %in% input$industries, Year == start_year) %>%
      select(Industry, StartValue = Value)
    end_df <- df %>% filter(Industry %in% input$industries, Year == end_year) %>%
      select(Industry, EndValue = Value)
    joined <- inner_join(start_df, end_df, by = "Industry")
    joined$CAGR <- (joined$EndValue / joined$StartValue)^(1 / (end_year - start_year)) - 1
    joined
  })

  ranking_dropped_industries <- reactive({
    rd <- tryCatch(ranking_data(), error = function(e) NULL)
    if (is.null(rd)) character(0) else setdiff(input$industries, rd$Industry)
  })

  observeEvent(ranking_dropped_industries(), {
    dropped <- ranking_dropped_industries()
    if (length(dropped) > 0) {
      showNotification(
        paste0(
          "Missing start/end-year data for: ", paste(dropped, collapse = ", "),
          " -- excluded from the growth ranking."
        ),
        type = "warning"
      )
    }
  })

  output$ranking_chart <- renderPlotly({
    rd <- ranking_data() %>% arrange(CAGR)
    req(nrow(rd) > 0)
    pal <- colors()
    rd$Industry <- factor(rd$Industry, levels = rd$Industry)

    plot_ly(
      data = rd, x = ~CAGR * 100, y = ~Industry,
      type = "bar", orientation = "h",
      marker = list(color = unname(pal[as.character(rd$Industry)])),
      hovertemplate = "%{y}: %{x:.2f}%<extra></extra>"
    ) %>%
      layout(
        title = paste0("CAGR, ", input$year_range[1], "-", input$year_range[2]),
        xaxis = list(title = "Compound annual growth rate (%)", gridcolor = GRIDLINE, color = INK_MUTED),
        yaxis = list(title = "", gridcolor = GRIDLINE, color = INK_MUTED),
        paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
        font = list(color = INK_PRIMARY, family = FONT_FAMILY)
      )
  })

  output$data_table <- renderDT({
    df <- build_export_df(filtered_data(), input$rebase_toggle, input$base_year)
    datatable(
      df, rownames = FALSE, options = list(pageLength = 15),
      colnames = unname(export_column_labels(names(df), input$base_year))
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      mode_part <- if (identical(input$view_mode, "growth")) {
        "yoy-growth"
      } else if (isTRUE(input$rebase_toggle)) {
        paste0("rebased-", input$base_year)
      } else {
        "index"
      }
      sprintf(
        "productivity_%s_%s-%s_%s.csv",
        mode_part, input$year_range[1], input$year_range[2], format(Sys.Date(), "%Y%m%d")
      )
    },
    content = function(file) {
      df <- build_export_df(filtered_data(), input$rebase_toggle, input$base_year)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
