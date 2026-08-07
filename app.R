library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(htmltools)

LP_DATA_FILE <- Sys.getenv("LP_DATA_FILE", "lp_data.RData")

# Categorical palette
CATEGORICAL_PALETTE <- c(
  "#012F72", # Maple Leaf Blue
  "#CE2E2E", # Canadiens Red
  "#3C8745", # Canucks Green
  "#FFC550", # Flames Yellow
  "#0598D8", # Jets Blue
  "#F74C16", # Oilers Orange
  "#7E1F86", # King Purple
  "#EF84EF"  # Panther Pink
)
INK_PRIMARY <- "#0C0C0C"   # Senators Black
INK_MUTED <- "#6B6B6B"     # Dark Grey
GRIDLINE <- "#D9D9D9"      # Light Grey
CHART_SURFACE <- "#FFFFFF" # Snow White
FONT_FAMILY <- "Roboto, sans-serif"

DEFAULT_VARIABLE <- "Labour productivity"
DEFAULT_GEOGRAPHY <- "Canada"
DEFAULT_INDUSTRY <- "All industries"

GEOGRAPHY_ORDER <- c(
  "Canada", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia",
  "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta",
  "British Columbia", "Yukon", "Northwest Territories", "Nunavut"
)
VARIABLE_ORDER <- c(
  "Labour productivity", "Real value added", "Nominal value added",
  "Total number of jobs", "Hours worked for all jobs",
  "Annual average number of hours worked for all jobs",
  "Total compensation for all jobs", "Total compensation per hour worked",
  "Unit labour cost", "Unit labour cost in US dollars", "Labour share"
)

# The Industry detail toggle on the Rankings tab -- selects a *maximum*
# level of detail, not an exact one, so "3-digit" still includes the
# Aggregate and 2-digit rows too (see industry_levels_upto() below).
DEFAULT_INDUSTRY_LEVEL <- "Aggregate"
INDUSTRY_LEVEL_ORDER <- c("Aggregate", "2-digit", "3-digit")

# Plain-language explanation shown below the main panel for the currently
# selected variable. "Total number of jobs" maps to "" since it needs no
# explanation -- renderUI treats an empty/missing entry as "show nothing".
VARIABLE_DEFINITIONS <- c(
  "Total number of jobs" = "",
  "Hours worked for all jobs" = "The total number of hours that a person devotes to work, whether paid or unpaid.",
  "Annual average number of hours worked for all jobs" = "Hours worked for all jobs divided by total number of jobs.",
  "Total compensation for all jobs" = "All payments in cash or in-kind made by domestic producers to workers for services rendered.",
  "Nominal value added" = "The dollar value of what an industry produces, minus the cost of the inputs (materials, energy, etc.) it used up to produce it. Measured in today's dollars, so it is affected by inflation.",
  "Real value added" = "The total dollar value of an industry's output minus the cost of the inputs (materials, energy, etc.) used to produce it. Adjusted by Statistics Canada to 2017 dollars by default, removing the effects of inflation.",
  "Labour productivity" = "A measure of how efficiently goods and services are produced by workers. Calculated by Statistics Canada as real value added divided by total hours worked.",
  "Total compensation per hour worked" = "Total compensation for all jobs divided by number of hours worked.",
  "Unit labour cost" = "Measures the average cost of labour required to produce one unit of output. Calculated by Statistics Canada as the current dollar labour compensation divided by real value added (or equivalently labour compensation per hour worked to labour productivity). It rises when hourly compensation grows faster than labour productivity, and is widely used to measure inflation pressure from wage growth.",
  "Unit labour cost in US dollars" = "Equivalent to the ratio of the Canadian unit labour cost to the exchange rate (the U.S. dollar value expressed in Canadian dollars, based on the monthly average).",
  "Labour share" = "The ratio of total compensation as a percentage of the nominal value added."
)

# Levels up to and including `level` -- e.g. "2-digit" resolves to
# c("Aggregate", "2-digit"), so picking a detail level always keeps the
# coarser rows too rather than switching to only that one level.
industry_levels_upto <- function(level) {
  idx <- match(level, INDUSTRY_LEVEL_ORDER)
  if (length(idx) == 0 || is.na(idx)) return(character(0))
  INDUSTRY_LEVEL_ORDER[seq_len(idx)]
}

# Immediate parent of each 2-digit and 3-digit industry, derived once from
# the "Hierarchy for Industry" dot-path in Stats Canada table 36-10-0480-01
# (that column isn't kept in lp_data.RData -- only the Industry name and
# IndustryLevel survive the pipeline). 2-digit sectors nest under Business/
# Non-business sector industries; 3-digit subsectors nest under their
# 2-digit sector. Used only to order/indent the Industry picker.
INDUSTRY_PARENT <- c(
  "Accommodation and food services" = "Business sector industries",
  "Administrative and support, waste management and remediation services" = "Business sector industries",
  "Agriculture, forestry, fishing and hunting" = "Business sector industries",
  "Arts, entertainment and recreation" = "Business sector industries",
  "Construction" = "Business sector industries",
  "Educational services" = "Business sector industries",
  "Federal government services" = "Non-business sector industries",
  "Finance and insurance" = "Business sector industries",
  "Government educational services" = "Non-business sector industries",
  "Government health services" = "Non-business sector industries",
  "Health care and social assistance" = "Business sector industries",
  "Holding companies" = "Business sector industries",
  "Information and cultural industries" = "Business sector industries",
  "Local, municipal and Indigenous government services" = "Non-business sector industries",
  "Manufacturing" = "Business sector industries",
  "Mining and oil and gas extraction" = "Business sector industries",
  "Non-profit institutions" = "Non-business sector industries",
  "Other private services" = "Business sector industries",
  "Professional, scientific and technical services" = "Business sector industries",
  "Provincial and territorial government services" = "Non-business sector industries",
  "Real estate, rental and leasing" = "Business sector industries",
  "Retail trade" = "Business sector industries",
  "Transportation and warehousing" = "Business sector industries",
  "Utilities" = "Business sector industries",
  "Wholesale trade" = "Business sector industries",
  "Accommodation services" = "Accommodation and food services",
  "Activities related to credit intermediation" = "Finance and insurance",
  "Administrative and support services" = "Administrative and support, waste management and remediation services",
  "Advertising, public relations, and related services" = "Professional, scientific and technical services",
  "Air transportation" = "Transportation and warehousing",
  "Ambulatory health care services" = "Non-profit institutions",
  "Amusement, gambling and recreation industries" = "Arts, entertainment and recreation",
  "Architectural, engineering and related services" = "Professional, scientific and technical services",
  "Beverage and tobacco product manufacturing" = "Manufacturing",
  "Broadcasting (except internet)" = "Information and cultural industries",
  "Building material and garden equipment and supplies dealers" = "Retail trade",
  "Building material and supplies wholesaler-distributors" = "Wholesale trade",
  "Chemical manufacturing" = "Manufacturing",
  "Clothing and clothing accessories stores" = "Retail trade",
  "Clothing and leather and allied product manufacturing" = "Manufacturing",
  "Community colleges and C.E.G.E.P.s" = "Government educational services",
  "Computer and electronic product manufacturing" = "Manufacturing",
  "Computer systems design and related services" = "Professional, scientific and technical services",
  "Crop and animal production" = "Agriculture, forestry, fishing and hunting",
  "Data processing, hosting, and related services" = "Information and cultural industries",
  "Defence services" = "Federal government services",
  "Depository credit intermediation and monetary authorities" = "Finance and insurance",
  "Electric power generation, transmission and distribution" = "Utilities",
  "Electrical equipment, appliance and component manufacturing" = "Manufacturing",
  "Electronics and appliance stores" = "Retail trade",
  "Elementary and secondary schools" = "Government educational services",
  "Engineering construction" = "Construction",
  "Fabricated metal product manufacturing" = "Manufacturing",
  "Farm product wholesaler-distributors" = "Wholesale trade",
  "Federal government services (excluding defence)" = "Federal government services",
  "Financial investment services, funds and other financial vehicles" = "Finance and insurance",
  "Fishing, hunting and trapping" = "Agriculture, forestry, fishing and hunting",
  "Food and beverage stores" = "Retail trade",
  "Food manufacturing" = "Manufacturing",
  "Food services and drinking places" = "Accommodation and food services",
  "Food, beverage and tobacco wholesaler-distributors" = "Wholesale trade",
  "Forestry and logging" = "Agriculture, forestry, fishing and hunting",
  "Furniture and home furnishings stores" = "Retail trade",
  "Furniture and related product manufacturing" = "Manufacturing",
  "Gasoline stations" = "Retail trade",
  "General merchandise stores" = "Retail trade",
  "Grant-making, civic, and professional and similar organizations" = "Non-profit institutions",
  "Health and personal care stores" = "Retail trade",
  "Health care" = "Health care and social assistance",
  "Hospitals" = "Government health services",
  "Indigenous government services" = "Local, municipal and Indigenous government services",
  "Insurance carriers and related activities" = "Finance and insurance",
  "Legal, accounting and related services" = "Professional, scientific and technical services",
  "Lessors of non-financial intangible assets (except copyrighted works)" = "Real estate, rental and leasing",
  "Machinery manufacturing" = "Manufacturing",
  "Machinery, equipment and supplies wholesaler-distributors" = "Wholesale trade",
  "Mining and quarrying (except oil and gas)" = "Mining and oil and gas extraction",
  "Miscellaneous manufacturing" = "Manufacturing",
  "Miscellaneous store retailers" = "Retail trade",
  "Miscellaneous wholesaler-distributors" = "Wholesale trade",
  "Motion picture and sound recording industries" = "Information and cultural industries",
  "Motor vehicle and parts dealers" = "Retail trade",
  "Motor vehicle and parts wholesaler-distributors" = "Wholesale trade",
  "Municipal government services" = "Local, municipal and Indigenous government services",
  "Natural gas distribution, water, sewage and other systems" = "Utilities",
  "Non-depository credit intermediation" = "Finance and insurance",
  "Non-metallic mineral product manufacturing" = "Manufacturing",
  "Non-profit arts, entertainment and recreation" = "Non-profit institutions",
  "Non-profit education institutions" = "Non-profit institutions",
  "Non-profit welfare organizations" = "Non-profit institutions",
  "Non-residential building construction" = "Construction",
  "Non-store retailers" = "Retail trade",
  "Nursing and residential care facilities" = "Government health services",
  "Oil and gas extraction" = "Mining and oil and gas extraction",
  "Other activities of the construction industry" = "Construction",
  "Other educational services" = "Government educational services",
  "Other information services" = "Information and cultural industries",
  "Other non-profit institutions serving households" = "Non-profit institutions",
  "Other professional, scientific and technical services including scientific research and development" = "Professional, scientific and technical services",
  "Paper manufacturing" = "Manufacturing",
  "Performing arts, spectator sports and related industries, and heritage institutions" = "Arts, entertainment and recreation",
  "Personal and household goods wholesaler-distributors" = "Wholesale trade",
  "Personal services and private households" = "Other private services",
  "Petroleum and coal product manufacturing" = "Manufacturing",
  "Petroleum product wholesaler-distributors" = "Wholesale trade",
  "Pipeline transportation" = "Transportation and warehousing",
  "Plastics and rubber products manufacturing" = "Manufacturing",
  "Postal service and couriers and messengers" = "Transportation and warehousing",
  "Primary metal manufacturing" = "Manufacturing",
  "Printing and related support activities" = "Manufacturing",
  "Private educational services" = "Educational services",
  "Professional and similar organizations" = "Other private services",
  "Publishing industries (except internet)" = "Information and cultural industries",
  "Rail transportation" = "Transportation and warehousing",
  "Real estate" = "Real estate, rental and leasing",
  "Religious organizations" = "Non-profit institutions",
  "Rental and leasing services" = "Real estate, rental and leasing",
  "Repair and maintenance" = "Other private services",
  "Repair construction" = "Construction",
  "Residential building construction" = "Construction",
  "Social assistance" = "Health care and social assistance",
  "Sporting goods, hobby, book and music stores" = "Retail trade",
  "Support activities for agriculture and forestry" = "Agriculture, forestry, fishing and hunting",
  "Support activities for mining and oil and gas extraction" = "Mining and oil and gas extraction",
  "Support activities for transportation" = "Transportation and warehousing",
  "Telecommunications" = "Information and cultural industries",
  "Textile and textile product mills" = "Manufacturing",
  "Transit, ground passenger and scenic and sightseeing transportation" = "Transportation and warehousing",
  "Transportation equipment manufacturing" = "Manufacturing",
  "Truck transportation" = "Transportation and warehousing",
  "Universities" = "Government educational services",
  "Warehousing and storage" = "Transportation and warehousing",
  "Waste management and remediation services" = "Administrative and support, waste management and remediation services",
  "Water transportation" = "Transportation and warehousing",
  "Wholesale electronic markets, and agents and brokers" = "Wholesale trade",
  "Wood product manufacturing" = "Manufacturing"
)

# Industry choices for the compare-picker, ordered so each industry
# immediately follows its parent (root aggregates, then their 2-digit
# children, then each 2-digit's 3-digit children) and indented with
# non-breaking spaces per level -- so it's visually clear which sector a
# subsector belongs to instead of one flat alphabetical list. Values stay
# the plain Industry name; only the displayed label is indented.
industry_choices_tree <- function(df) {
  available <- unique(df$Industry)
  order <- character(0)
  labels <- character(0)

  add_children <- function(parent, depth) {
    children <- sort(available[match(available, names(INDUSTRY_PARENT), 0L) > 0 &
                                  INDUSTRY_PARENT[available] == parent])
    for (child in children) {
      order <<- c(order, child)
      labels <<- c(labels, paste0(strrep("    ", depth), child))
      add_children(child, depth + 1)
    }
  }

  roots <- intersect(c("All industries", "Business sector industries", "Non-business sector industries"), available)
  for (r in roots) {
    order <- c(order, r)
    labels <- c(labels, r)
    add_children(r, 1)
  }

  # Anything present but not reachable from the roots above (e.g. a future
  # industry not yet mapped in INDUSTRY_PARENT) still shows up, unindented,
  # rather than silently disappearing from the picker.
  leftover <- sort(setdiff(available, order))
  order <- c(order, leftover)
  labels <- c(labels, leftover)

  setNames(order, labels)
}

# data_pipeline.R pulls Stats Canada table 36-10-0480-01 then shapes/
# renames it into Year/Geography/Variable/Industry/
# IndustryLevel/Value/UOM columns.
load_lp_data <- function(path = LP_DATA_FILE) {
  if (!file.exists(path)) {
    stop(
      "lp_data.RData not found. Run data_pipeline.R from this project folder ",
      "first to pull productivity data from Statistics Canada."
    )
  }
  e <- new.env()
  load(path, envir = e)
  df <- e$lp_data
  # A handful of 3-digit industries come back from Statistics Canada named
  # "X ==> Y" (e.g. "Ambulatory health care services ==> Non-profit
  # institutions") -- Y there is just the true parent under this table's
  # non-commercial-activity reclassification, which INDUSTRY_PARENT already
  # encodes. Strip the "==> ..." suffix so the picker shows the plain
  # industry name and INDUSTRY_PARENT nests it under its real parent instead
  # of it dangling, unindented, as its own oddly-named entry.
  df$Industry <- sub("\\s*==>.*$", "", df$Industry)
  df
}
# Check lp_data.RData for modifications every 6000 seconds (10 minutes) by default
# but allow tests to override this to avoid waiting 10 real minutes for a test to exercise the reactiveFileReader.
# Session = NULL means this reader isn't tied to (or
# torn down with) any one visitor's session. 
RAW_DATA_POLL_MS <- as.numeric(Sys.getenv("RAW_DATA_POLL_MS", 10 * 60 * 1000))
RAW_DATA_READER <- reactiveFileReader(RAW_DATA_POLL_MS, session = NULL, LP_DATA_FILE, load_lp_data)

ordered_unique <- function(values, preferred_order) {
  known <- preferred_order[preferred_order %in% values]
  extra <- unique(values)[!unique(values) %in% known]
  c(known, extra)
}

# Every distinct value of a series dimension (Industry or Geography)
# present in the data, in a stable display order (preferred entries first,
# the rest following the data's own grouping).
series_choices <- function(df, dim_col, preferred_order = character(0)) {
  ordered_unique(df[[dim_col]], preferred_order)
}

# Assign palette colours to series in the order given.
# Overflow series share muted grey.
series_color_map <- function(series) {
  n <- length(series)
  colors <- if (n <= length(CATEGORICAL_PALETTE)) {
    CATEGORICAL_PALETTE[seq_len(n)]
  } else {
    c(CATEGORICAL_PALETTE, rep(INK_MUTED, n - length(CATEGORICAL_PALETTE)))
  }
  setNames(colors, series)
}

# Comparison series is a specific (Industry, Geography) pair. 
# Pair_label is the human-readable chart legend/checklist text; pair_key is a stable
# machine identifier.
# Kept separate so renaming the display format never
# breaks identity comparisons.
pair_label <- function(industry, geography) paste(industry, geography, sep = " — ")
pair_key <- function(industry, geography) paste(industry, geography, sep = "|||")

# The single active pair a fresh session starts with. 
# Used identically by ui() and server() so the two can never drift apart.
default_pair_row <- function() {
  data.frame(
    Industry = DEFAULT_INDUSTRY, Geography = DEFAULT_GEOGRAPHY,
    SeriesLabel = pair_label(DEFAULT_INDUSTRY, DEFAULT_GEOGRAPHY),
    PairKey = pair_key(DEFAULT_INDUSTRY, DEFAULT_GEOGRAPHY),
    stringsAsFactors = FALSE
  )
}

# Axis wording for the currently active view -- growth mode ignores
# rebasing entirely (rebasing a series by a constant doesn't change its
# period-over-period % change, so there's nothing for it to reflect).
# Level mode uses the selected variable's own name/unit rather than a
# hardcoded label, since different variables carry different units.
display_axis_title <- function(view_mode, rebase_toggle, base_year, variable, uom) {
  if (identical(view_mode, "growth")) {
    "Annual growth (%)"
  } else if (isTRUE(rebase_toggle)) {
    paste0("Rebased index (", base_year, "=100)")
  } else {
    paste0(variable, " (", uom, ")")
  }
}

# Chart title for the Trend and Bar Chart tabs -- just the variable and the
# selected time frame, so it updates automatically as either changes rather
# than staying a static/generic title.
display_chart_title <- function(variable, year_range) {
  paste0(variable, " (", year_range[1], "-", year_range[2], ")")
}

# Shared row/column shaping for both the Data Table and the CSV export, so
# the download is always a WYSIWYG match of what's on screen. Exports the
# underlying dimensions (Geography, Industry, Variable, UOM) rather than
# the internal SeriesLabel convenience column.
build_export_df <- function(df, rebase_toggle, base_year) {
  df <- df %>% arrange(SeriesLabel, Year)
  cols <- c("Year", "Geography", "Industry", "Variable", "Value", "UOM", "GrowthPct")
  if (isTRUE(rebase_toggle)) cols <- c(cols, "RebasedValue")
  df[, cols]
}

export_column_labels <- function(cols, base_year, variable, uom) {
  labels <- c(
    Year = "Year", Geography = "Geography", Industry = "Industry", Variable = "Variable",
    Value = paste0(variable, " (", uom, ")"),
    UOM = "Unit", GrowthPct = "Annual growth (%)"
  )
  if ("RebasedValue" %in% cols) {
    labels["RebasedValue"] <- paste0("Rebased index (", base_year, "=100)")
  }
  labels[cols]
}

# The Trends tab: a focused single-series view (one Variable + one
# Geography + one Industry -> one line), unlike the other 3 tabs which
# still compare multiple (Industry, Geography) pairs at once. Kept as its
# own dedicated module rather than another "kind" of tab_module_ui/
# tab_module_server below, since its sidebar shape and reactive pipeline
# are genuinely different (no active_pairs/Add-series/multi-color
# machinery), not just a different final render step.
trend_tab_ui <- function(id, init_df) {
  ns <- NS(id)

  card(
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        selectInput(
          ns("variable"), "Variable",
          choices = series_choices(init_df, "Variable", VARIABLE_ORDER), selected = DEFAULT_VARIABLE
        ),
        uiOutput(ns("variable_definition")),
        selectInput(
          ns("geography"), "Geography",
          choices = series_choices(init_df, "Geography", GEOGRAPHY_ORDER), selected = DEFAULT_GEOGRAPHY,
          selectize = FALSE
        ),
        # Always the full Aggregate+2-digit+3-digit tree -- no separate
        # industry_level toggle (none of the tabs have one).
        selectizeInput(
          ns("industry"), "Industry",
          choices = industry_choices_tree(init_df), selected = DEFAULT_INDUSTRY,
          options = list(
            placeholder = "Search industries...",
            # Same indentation-that-survives-wrapped-lines trick as the
            # other tabs' Industry picker -- see their comment for why.
            render = I("{
              option: function(item, escape) {
                var text = item.label, depth = 0;
                while (text.charCodeAt(depth) === 160) depth++;
                return '<div style=\"padding-left:' + (depth * 4) + 'px\">' +
                  escape(text.slice(depth)) + '</div>';
              },
              item: function(item, escape) {
                var text = item.label, depth = 0;
                while (text.charCodeAt(depth) === 160) depth++;
                return '<div>' + escape(text.slice(depth)) + '</div>';
              }
            }")
          )
        ),
        # A native <details> disclosure -- zero extra JS dependencies,
        # keyboard-accessible by default. Verified (headless-browser +
        # shinytest2) that a sliderInput initialized while collapsed inside
        # this renders and syncs identically to one that was never hidden,
        # so no "re-init on open" workaround is needed.
        tags$details(
          id = ns("more_options"), class = "trend-more-options",
          tags$summary("More options"),
          sliderInput(
            ns("year_range"), "Time frame",
            min = min(init_df$Year), max = max(init_df$Year),
            value = c(min(init_df$Year), max(init_df$Year)),
            step = 1, sep = ""
          ),
          radioButtons(
            ns("view_mode"), "Measure",
            choices = c("Index level" = "level", "Annual growth %" = "growth"),
            selected = "level", inline = TRUE
          ),
          conditionalPanel(
            "input.view_mode == 'level'", ns = ns,
            checkboxInput(ns("rebase_toggle"), "Rebase to a base year", value = FALSE),
            conditionalPanel(
              "input.view_mode == 'level' && input.rebase_toggle == true", ns = ns,
              selectInput(
                ns("base_year"), "Base year",
                choices = sort(unique(init_df$Year)), selected = min(init_df$Year),
                selectize = FALSE
              )
            )
          ),
          downloadButton(ns("download_csv"), "Download CSV", icon = icon("download"))
        )
      ),
      plotlyOutput(ns("chart"), height = "100%"),
      p(class = "text-muted small", "Source: Statistics Canada Table 36-10-0480-01")
    )
  )
}

trend_tab_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {

    # Keeps Variable/Geography/Industry/time-frame/base-year in sync with
    # what's in the data, preserving the user's current picks where still
    # valid. Unlike the multi-pair tabs, an invalid pick falls back to the
    # DEFAULT_* constants rather than going blank -- Geography/Industry are
    # always-active single selections here, not an "add to compare" picker
    # that's allowed to sit empty.
    observe({
      df <- raw_data()

      variable_choices <- series_choices(df, "Variable", VARIABLE_ORDER)
      new_variable <- if (is.null(input$variable) || !(input$variable %in% variable_choices)) {
        DEFAULT_VARIABLE
      } else {
        input$variable
      }
      updateSelectInput(session, "variable", choices = variable_choices, selected = new_variable)

      geo_choices <- series_choices(df, "Geography", GEOGRAPHY_ORDER)
      new_geo <- if (is.null(input$geography) || !(input$geography %in% geo_choices)) {
        DEFAULT_GEOGRAPHY
      } else {
        input$geography
      }
      updateSelectInput(session, "geography", choices = geo_choices, selected = new_geo)

      industry_choices <- industry_choices_tree(df)
      new_industry <- if (is.null(input$industry) || !(input$industry %in% industry_choices)) {
        DEFAULT_INDUSTRY
      } else {
        input$industry
      }
      updateSelectizeInput(session, "industry", choices = industry_choices, selected = new_industry)

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
      current_base <- suppressWarnings(as.numeric(input$base_year))
      new_base <- if (is.null(input$base_year) || is.na(current_base) || !(current_base %in% year_choices)) {
        min(year_choices)
      } else {
        current_base
      }
      updateSelectInput(session, "base_year", choices = year_choices, selected = new_base)
    }) |> bindEvent(raw_data(), once = FALSE)

    # Single (Variable, Industry, Geography) match -- no SeriesLabel
    # filtering/looping needed, but SeriesLabel is still added so
    # build_export_df()/export_column_labels() work completely unchanged.
    scoped_raw <- reactive({
      req(input$variable, input$industry, input$geography)
      raw_data() %>%
        filter(Variable == input$variable, Industry == input$industry, Geography == input$geography) %>%
        mutate(SeriesLabel = pair_label(input$industry, input$geography))
    })

    variable_uom <- reactive({
      vals <- raw_data() %>% filter(Variable == input$variable) %>% pull(UOM)
      vals[1]
    })

    history_with_growth <- reactive({
      validate(need(nrow(scoped_raw()) > 0, "No data for this variable/industry/geography combination."))
      scoped_raw() %>%
        arrange(Year) %>%
        mutate(GrowthPct = (Value / lag(Value) - 1) * 100)
    })

    transform_result <- reactive({
      df <- history_with_growth()
      base_year_num <- suppressWarnings(as.numeric(input$base_year))

      if (isTRUE(input$rebase_toggle) && !is.na(base_year_num)) {
        base_row <- df %>% filter(Year == base_year_num)
        has_base <- nrow(base_row) > 0
        df$RebasedValue <- if (has_base) df$Value / base_row$Value[1] * 100 else NA_real_
      } else {
        df$RebasedValue <- NA_real_
        has_base <- TRUE
      }

      df$DisplayValue <- if (identical(input$view_mode, "growth")) {
        df$GrowthPct
      } else if (isTRUE(input$rebase_toggle)) {
        df$RebasedValue
      } else {
        df$Value
      }

      list(df = df, missing_base = isTRUE(input$rebase_toggle) && !has_base)
    })

    display_data <- reactive(transform_result()$df)

    rebase_missing <- reactive({
      if (identical(input$view_mode, "growth")) FALSE else isTRUE(transform_result()$missing_base)
    })

    observeEvent(rebase_missing(), {
      if (isTRUE(rebase_missing())) {
        showNotification(
          paste0(
            "No ", input$base_year, " data for this series -- excluded from the rebased view."
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

    output$chart <- renderPlotly({
      df <- filtered_data() %>% filter(!is.na(DisplayValue)) %>% arrange(Year)
      req(nrow(df) > 0)
      axis_title <- display_axis_title(input$view_mode, input$rebase_toggle, input$base_year, input$variable, variable_uom())
      hover_suffix <- if (identical(input$view_mode, "growth")) "%" else ""
      line_color <- CATEGORICAL_PALETTE[1]
      # Chart title only states the variable/timeframe -- with a single
      # line there's no legend to identify which industry/geography it is,
      # so name the pair as a subtitle and in the hover text instead.
      subtitle <- pair_label(input$industry, input$geography)

      plot_ly(
        data = df, x = ~Year, y = ~DisplayValue,
        type = "scatter", mode = "lines+markers",
        line = list(color = line_color, width = 2),
        marker = list(color = line_color, size = 7),
        hovertemplate = paste0(subtitle, "<br>%{x}: %{y:.1f}", hover_suffix, "<extra></extra>")
      ) %>%
        layout(
          title = list(text = paste0(
            display_chart_title(input$variable, input$year_range),
            "<br><sup style='color:", INK_MUTED, "'>", subtitle, "</sup>"
          )),
          xaxis = list(title = "Year", dtick = 1, gridcolor = GRIDLINE, color = INK_MUTED),
          yaxis = list(title = axis_title, gridcolor = GRIDLINE, color = INK_MUTED),
          paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
          font = list(color = INK_PRIMARY, family = FONT_FAMILY),
          showlegend = FALSE
        )
    })

    output$variable_definition <- renderUI({
      def <- VARIABLE_DEFINITIONS[[input$variable]]
      if (is.null(def) || !nzchar(def)) return(NULL)
      p(class = "text-muted small", strong(paste0(input$variable, ": ")), def)
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        mode_part <- if (identical(input$view_mode, "growth")) {
          "annual-growth"
        } else if (isTRUE(input$rebase_toggle)) {
          paste0("rebased-", input$base_year)
        } else {
          "index"
        }
        sprintf(
          "productivity_%s_%s_%s_%s_%s-%s_%s.csv",
          gsub("[^A-Za-z0-9]+", "-", input$variable),
          gsub("[^A-Za-z0-9]+", "-", input$industry),
          gsub("[^A-Za-z0-9]+", "-", input$geography),
          mode_part, input$year_range[1], input$year_range[2], format(Sys.Date(), "%Y%m%d")
        )
      },
      content = function(file) {
        df <- build_export_df(filtered_data(), input$rebase_toggle, input$base_year)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}

# The Rankings tab: pick one Variable + one Geography (+ a maximum Industry
# detail level), see every matching industry's compound annual growth rate
# (CAGR) over the full time span of the data, as a scatter plot. Unlike
# Trends/Compare there's no picker for a *specific* industry -- all
# industries up to the chosen detail level are shown at once -- so this gets
# its own dedicated module rather than another tab_module_ui/
# tab_module_server "kind", same reasoning as the Trends tab.
ranking_tab_ui <- function(id, init_df) {
  ns <- NS(id)

  card(
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        selectInput(
          ns("variable"), "Variable",
          choices = series_choices(init_df, "Variable", VARIABLE_ORDER), selected = DEFAULT_VARIABLE
        ),
        uiOutput(ns("variable_definition")),
        selectInput(
          ns("geography"), "Geography",
          choices = series_choices(init_df, "Geography", GEOGRAPHY_ORDER), selected = DEFAULT_GEOGRAPHY,
          selectize = FALSE
        ),
        sliderInput(
          ns("year_range"), "Time frame",
          min = min(init_df$Year), max = max(init_df$Year),
          value = c(min(init_df$Year), max(init_df$Year)),
          step = 1, sep = ""
        ),
        radioButtons(
          ns("industry_level"), "Industry detail",
          choices = c("Aggregate" = "Aggregate", "2-digit sector" = "2-digit", "3-digit subsector" = "3-digit"),
          selected = DEFAULT_INDUSTRY_LEVEL, inline = TRUE
        ),
        # Reuses the Trends tab's .trend-more-options styling (chevron
        # summary, no default browser triangle) -- the class name is
        # generic despite where it was first introduced.
        tags$details(
          id = ns("more_options"), class = "trend-more-options",
          tags$summary("More options"),
          radioButtons(
            ns("chart_type"), "Chart type",
            choices = c("Scatter" = "scatter", "Bar" = "bar"), selected = "bar", inline = TRUE
          ),
          downloadButton(ns("download_csv"), "Download CSV", icon = icon("download"))
        )
      ),
      plotlyOutput(ns("chart"), height = "100%"),
      p(class = "text-muted small", "Source: Statistics Canada Table 36-10-0480-01")
    )
  )
}

ranking_tab_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {

    # Same DEFAULT_*-fallback sync pattern as the Trends tab -- Variable and
    # Geography are always-active single selections here too, never blank.
    observe({
      df <- raw_data()

      variable_choices <- series_choices(df, "Variable", VARIABLE_ORDER)
      new_variable <- if (is.null(input$variable) || !(input$variable %in% variable_choices)) {
        DEFAULT_VARIABLE
      } else {
        input$variable
      }
      updateSelectInput(session, "variable", choices = variable_choices, selected = new_variable)

      geo_choices <- series_choices(df, "Geography", GEOGRAPHY_ORDER)
      new_geo <- if (is.null(input$geography) || !(input$geography %in% geo_choices)) {
        DEFAULT_GEOGRAPHY
      } else {
        input$geography
      }
      updateSelectInput(session, "geography", choices = geo_choices, selected = new_geo)

      year_min <- min(df$Year)
      year_max <- max(df$Year)
      current_range <- input$year_range
      range_value <- if (is.null(current_range)) {
        c(year_min, year_max)
      } else {
        c(max(current_range[1], year_min), min(current_range[2], year_max))
      }
      updateSliderInput(session, "year_range", min = year_min, max = year_max, value = range_value)
    }) |> bindEvent(raw_data(), once = FALSE)

    scoped_raw <- reactive({
      req(input$variable, input$geography, input$industry_level)
      raw_data() %>%
        filter(
          Variable == input$variable, Geography == input$geography,
          IndustryLevel %in% industry_levels_upto(input$industry_level)
        )
    })

    variable_uom <- reactive({
      vals <- raw_data() %>% filter(Variable == input$variable) %>% pull(UOM)
      vals[1]
    })

    # CAGR from the start to the end of the selected time frame.
    ranking_data <- reactive({
      df <- scoped_raw()
      validate(need(nrow(df) > 0, "No data for this variable/geography combination."))
      rng <- req(input$year_range)
      start_year <- rng[1]
      end_year <- rng[2]
      validate(need(end_year > start_year, "Select a time frame spanning at least two years to compute CAGR."))

      start_df <- df %>% filter(Year == start_year) %>% select(Industry, StartValue = Value)
      end_df <- df %>% filter(Year == end_year) %>% select(Industry, EndValue = Value)
      joined <- inner_join(start_df, end_df, by = "Industry")
      joined$CAGR <- (joined$EndValue / joined$StartValue)^(1 / (end_year - start_year)) - 1
      joined
    })

    # A custom time frame can land on years some industries lack data for
    # (added/discontinued series) -- inner_join() above silently drops them,
    # so surface which ones instead of just shrinking the chart unexplained.
    ranking_dropped_industries <- reactive({
      rd <- tryCatch(ranking_data(), error = function(e) NULL)
      if (is.null(rd)) character(0) else setdiff(unique(scoped_raw()$Industry), rd$Industry)
    })

    observeEvent(ranking_dropped_industries(), {
      dropped <- ranking_dropped_industries()
      if (length(dropped) > 0) {
        showNotification(
          paste0(
            "Missing start/end-year data for: ", paste(dropped, collapse = ", "),
            " -- excluded from the ranking."
          ),
          type = "warning"
        )
      }
    })

    output$chart <- renderPlotly({
      rd <- ranking_data() %>% arrange(CAGR)
      req(nrow(rd) > 0)
      rd$Industry <- factor(rd$Industry, levels = rd$Industry)

      # Same ranked CAGR data either way -- scatter (one point per industry,
      # no bar length) reads better once the 3-digit level pulls in 100+
      # industries and overlapping bars get visually noisy; bar is the more
      # familiar shape for a shorter (e.g. Aggregate-level) list.
      p <- if (identical(input$chart_type, "bar")) {
        plot_ly(
          data = rd, x = ~CAGR * 100, y = ~Industry,
          type = "bar", orientation = "h",
          marker = list(color = CATEGORICAL_PALETTE[1]),
          hovertemplate = "%{y}: %{x:.2f}%<extra></extra>"
        )
      } else {
        plot_ly(
          data = rd, x = ~CAGR * 100, y = ~Industry,
          type = "scatter", mode = "markers",
          marker = list(color = CATEGORICAL_PALETTE[1], size = 9),
          hovertemplate = "%{y}: %{x:.2f}%<extra></extra>"
        )
      }

      p %>% layout(
        title = paste0(
          input$variable, " by industry — ", input$geography,
          " (", input$year_range[1], "-", input$year_range[2], ")"
        ),
        xaxis = list(title = "Compound annual growth rate (%)", gridcolor = GRIDLINE, color = INK_MUTED),
        yaxis = list(title = "", gridcolor = GRIDLINE, color = INK_MUTED),
        paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
        font = list(color = INK_PRIMARY, family = FONT_FAMILY)
      )
    })

    output$variable_definition <- renderUI({
      def <- VARIABLE_DEFINITIONS[[input$variable]]
      if (is.null(def) || !nzchar(def)) return(NULL)
      p(class = "text-muted small", strong(paste0(input$variable, ": ")), def)
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        sprintf(
          "productivity_ranking_%s_%s_%s_%s-%s_%s.csv",
          gsub("[^A-Za-z0-9]+", "-", input$variable),
          gsub("[^A-Za-z0-9]+", "-", input$geography),
          input$industry_level,
          input$year_range[1], input$year_range[2], format(Sys.Date(), "%Y%m%d")
        )
      },
      content = function(file) {
        out <- ranking_data() %>%
          arrange(desc(CAGR)) %>%
          transmute(
            Industry, Geography = input$geography, Variable = input$variable,
            StartYear = input$year_range[1], StartValue, EndYear = input$year_range[2], EndValue,
            `CAGR (%)` = CAGR * 100
          )
        write.csv(out, file, row.names = FALSE)
      }
    )
  })
}

# One tab's worth of sidebar + main content, namespaced under `id` so each
# of the 2 remaining tabs (kind = "bar" | "table" -- Trends and Rankings now
# have their own dedicated modules above) gets its own fully independent
# Variable/Compare/Display state instead of sharing one page-level sidebar.
# `ns = ns` on conditionalPanel is what makes the condition strings below
# resolve against THIS tab's namespaced widgets client-side, without
# hand-building "input['id-view_mode']" strings.
tab_module_ui <- function(id, init_df, kind) {
  ns <- NS(id)

  main_panel <- switch(
    kind,
    table = DTOutput(ns("chart")),
    plotlyOutput(ns("chart"), height = "100%")
  )

  card(
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        selectInput(
          ns("variable"), "Variable",
          choices = series_choices(init_df, "Variable", VARIABLE_ORDER), selected = DEFAULT_VARIABLE
        ),
        uiOutput(ns("variable_definition")),
        tags$strong("Compare"),
        # A leading "" choice is what makes a genuinely empty starting
        # selection possible -- with no option marked selected, the browser's
        # native <select> defaults to whichever option comes first, so an
        # empty string has to *be* that first option for the box to open
        # blank (showing the placeholder) instead of silently defaulting to
        # the first real choice. The default series is still there, just
        # already-applied under "Series to compare" via default_pair_row().
        # Always the full Aggregate+2-digit+3-digit tree -- no industry_level
        # toggle, matching the Trends tab.
        selectizeInput(
          ns("pair_industry"), "Industry",
          choices = c("", industry_choices_tree(init_df)),
          selected = character(0),
          options = list(
            placeholder = "Search industries...",
            # Leading non-breaking spaces on the label only indent the first
            # line of a wrapped option -- render as padding-left on the whole
            # option div instead, so long industry names wrap without their
            # second line snapping back to the left edge.
            render = I("{
              option: function(item, escape) {
                var text = item.label, depth = 0;
                while (text.charCodeAt(depth) === 160) depth++;
                return '<div style=\"padding-left:' + (depth * 4) + 'px\">' +
                  escape(text.slice(depth)) + '</div>';
              },
              item: function(item, escape) {
                var text = item.label, depth = 0;
                while (text.charCodeAt(depth) === 160) depth++;
                return '<div>' + escape(text.slice(depth)) + '</div>';
              }
            }")
          )
        ),
        selectizeInput(
          ns("pair_geography"), "Geography",
          choices = c("", GEOGRAPHY_ORDER), selected = character(0),
          options = list(placeholder = "Search geographies...")
        ),
        actionButton(ns("add_pair"), "Add series", icon = icon("plus")),
        checkboxGroupInput(
          ns("active_pairs_keep"), "Series to compare",
          choices = setNames(default_pair_row()$PairKey, default_pair_row()$SeriesLabel),
          selected = default_pair_row()$PairKey
        ),
        p(
          class = "text-muted small",
          "Uncheck a series to remove it. Charts stay readable up to about 6 series at once."
        ),
        tags$strong("Display"),
        radioButtons(
          ns("view_mode"), "Measure",
          choices = c("Index level" = "level", "Annual growth %" = "growth"),
          selected = "level", inline = TRUE
        ),
        sliderInput(
          ns("year_range"), "Time frame",
          min = min(init_df$Year), max = max(init_df$Year),
          value = c(min(init_df$Year), max(init_df$Year)),
          step = 1, sep = ""
        ),
        conditionalPanel(
          "input.view_mode == 'level'", ns = ns,
          checkboxInput(ns("rebase_toggle"), "Rebase to a base year", value = FALSE),
          conditionalPanel(
            "input.view_mode == 'level' && input.rebase_toggle == true", ns = ns,
            sliderInput(
              ns("base_year"), "Base year",
              min = min(init_df$Year), max = max(init_df$Year),
              value = min(init_df$Year), step = 1, sep = ""
            )
          )
        ),
        downloadButton(ns("download_csv"), "Download CSV", icon = icon("download"))
      ),
      main_panel,
      p(class = "text-muted small", "Source: Statistics Canada Table 36-10-0480-01")
    )
  )
}

# Reactive pipeline shared by the 2 remaining tabs (kind = "bar" | "table"),
# only diverging at the final render step (kind-specific chart/table).
# Instantiated once per tab id so each tab's
# Variable/Compare/Display selections are fully independent -- module
# namespacing (see tab_module_ui) is what makes multiple simultaneous
# copies of the same widget ids possible in one page.
tab_module_server <- function(id, raw_data, kind) {
  moduleServer(id, function(input, output, session) {

    # Keeps the variable, geography, time frame, and base year selectors in
    # sync with what's available in the data (added/removed whenever
    # RAW_DATA_READER's shared file-watcher picks up a new pipeline run),
    # preserving the user's current picks -- clamped to the new bounds --
    # where possible. This only affects what's *offered* when adding a new
    # pair -- pairs already in active_pairs() are unaffected, since they're
    # already resolved to a concrete Industry/Geography.
    observe({
      df <- raw_data()

      variable_choices <- series_choices(df, "Variable", VARIABLE_ORDER)
      current_variable <- input$variable
      new_variable <- if (is.null(current_variable) || !(current_variable %in% variable_choices)) {
        DEFAULT_VARIABLE
      } else {
        current_variable
      }
      updateSelectInput(session, "variable", choices = variable_choices, selected = new_variable)

      geo_choices <- series_choices(df, "Geography", GEOGRAPHY_ORDER)
      current_geo <- input$pair_geography
      new_geo <- if (is.null(current_geo) || !(current_geo %in% geo_choices)) {
        character(0)
      } else {
        current_geo
      }
      updateSelectizeInput(session, "pair_geography", choices = c("", geo_choices), selected = new_geo)

      industry_choices <- industry_choices_tree(df)
      current_industry <- input$pair_industry
      new_industry <- if (is.null(current_industry) || !(current_industry %in% industry_choices)) {
        character(0)
      } else {
        current_industry
      }
      updateSelectizeInput(session, "pair_industry", choices = c("", industry_choices), selected = new_industry)

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

    # The set of (Industry, Geography) pairs currently being compared --
    # replaces the old compare_mode-driven industries_multi/geos_multi/
    # geo_single/industry_single inputs with one free-form list that can mix
    # any industry with any geography.
    active_pairs <- reactiveVal(default_pair_row())

    sync_active_pairs_widget <- function(pairs) {
      updateCheckboxGroupInput(
        session, "active_pairs_keep",
        choices = setNames(pairs$PairKey, pairs$SeriesLabel), selected = pairs$PairKey
      )
    }

    observeEvent(input$add_pair, {
      req(input$pair_industry, input$pair_geography)
      key <- pair_key(input$pair_industry, input$pair_geography)
      current <- active_pairs()
      if (!(key %in% current$PairKey)) {
        new_row <- data.frame(
          Industry = input$pair_industry, Geography = input$pair_geography,
          SeriesLabel = pair_label(input$pair_industry, input$pair_geography),
          PairKey = key, stringsAsFactors = FALSE
        )
        current <- rbind(current, new_row)
        active_pairs(current)
      }
      sync_active_pairs_widget(active_pairs())

      # Clear the pickers back to their empty/placeholder state after every
      # add -- mirrors the blank starting state (see the "" choice comment
      # above) so adding a series always ends with a clean search box ready
      # for the next pick, instead of leaving the just-added pick sitting
      # there looking like unconsumed input.
      updateSelectizeInput(session, "pair_geography", selected = character(0))
      updateSelectizeInput(session, "pair_industry", selected = character(0))
    })

    # Unchecking a box in the checklist is how a pair gets removed. The
    # widget is re-synced (not just active_pairs()) so the unchecked box
    # actually disappears -- otherwise it would linger as checkable but
    # inert, since its underlying row is gone from active_pairs().
    observeEvent(input$active_pairs_keep, {
      current <- active_pairs()
      kept <- current[current$PairKey %in% input$active_pairs_keep, ]
      active_pairs(kept)
      sync_active_pairs_widget(kept)
    }, ignoreNULL = FALSE)

    # Adds the SeriesLabel every downstream reactive/chart keys off of, so
    # the rest of the reactive chain doesn't need to know pairs exist --
    # it's already generic over "some series dimension".
    scoped_raw <- reactive({
      raw_data() %>%
        filter(Variable == input$variable) %>%
        mutate(SeriesLabel = pair_label(Industry, Geography))
    })

    selected_series <- reactive(active_pairs()$SeriesLabel)

    variable_uom <- reactive({
      vals <- raw_data() %>% filter(Variable == input$variable) %>% pull(UOM)
      vals[1]
    })

    # Full history (not yet limited to the time-frame slider) for the
    # selected series, with year-over-year growth computed against each
    # series' actual prior year -- including years outside the currently
    # zoomed window.
    history_with_growth <- reactive({
      validate(need(nrow(scoped_raw()) > 0, "No data for this variable/geography/industry-detail combination."))
      req(length(selected_series()) > 0)
      scoped_raw() %>%
        filter(SeriesLabel %in% selected_series()) %>%
        arrange(SeriesLabel, Year) %>%
        group_by(SeriesLabel) %>%
        mutate(GrowthPct = (Value / lag(Value) - 1) * 100) %>%
        ungroup()
    })

    # Adds RebasedValue (per series, relative to input$base_year, looked up
    # against the full history so the base year can sit outside the zoomed
    # time frame) and DisplayValue (whichever column the current view mode
    # calls for). Returns both the data and the list of series excluded from
    # rebasing for lacking the chosen base year.
    transform_result <- reactive({
      df <- history_with_growth()

      if (isTRUE(input$rebase_toggle) && !is.null(input$base_year)) {
        base_values <- df %>%
          filter(Year == input$base_year) %>%
          select(SeriesLabel, BaseValue = Value)
        df <- df %>% left_join(base_values, by = "SeriesLabel")
        missing <- setdiff(unique(df$SeriesLabel), base_values$SeriesLabel)
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

    rebase_missing_series <- reactive({
      if (identical(input$view_mode, "growth") || !isTRUE(input$rebase_toggle)) {
        character(0)
      } else {
        transform_result()$missing
      }
    })

    observeEvent(rebase_missing_series(), {
      missing <- rebase_missing_series()
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

    # Colors are assigned in the order pairs were added to active_pairs(),
    # not from a fixed universe -- with ~1,900 possible (Industry,
    # Geography) combinations, a fixed domain would mean almost every series
    # falls through to the muted overflow color. This does mean a series'
    # color can shift by one slot if an earlier pair is removed.
    colors <- reactive(series_color_map(active_pairs()$SeriesLabel))

    if (kind == "table") {
      output$chart <- renderDT({
        df <- build_export_df(filtered_data(), input$rebase_toggle, input$base_year)
        datatable(
          df, rownames = FALSE, options = list(pageLength = 15),
          colnames = unname(export_column_labels(names(df), input$base_year, input$variable, variable_uom()))
        )
      })
    } else {
      # kind == "bar" (the only other kind this shared module still serves)
      output$chart <- renderPlotly({
        df <- filtered_data() %>% filter(!is.na(DisplayValue))
        req(nrow(df) > 0)
        pal <- colors()
        series <- series_choices(df, "SeriesLabel")
        axis_title <- display_axis_title(input$view_mode, input$rebase_toggle, input$base_year, input$variable, variable_uom())
        hover_suffix <- if (identical(input$view_mode, "growth")) "%" else ""

        p <- plot_ly()
        for (s in series) {
          sd <- df %>% filter(SeriesLabel == s) %>% arrange(Year)
          if (nrow(sd) == 0) next
          p <- p %>% add_trace(
            data = sd, x = ~Year, y = ~DisplayValue,
            type = "bar", name = s, legendgroup = s,
            marker = list(color = pal[[s]]),
            hovertemplate = paste0("%{x}<br>", s, ": %{y:.1f}", hover_suffix, "<extra></extra>")
          )
        }

        p %>% layout(
          title = display_chart_title(input$variable, input$year_range),
          barmode = "group",
          xaxis = list(title = "Year", dtick = 1, gridcolor = GRIDLINE, color = INK_MUTED),
          yaxis = list(title = axis_title, gridcolor = GRIDLINE, color = INK_MUTED),
          paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
          font = list(color = INK_PRIMARY, family = FONT_FAMILY),
          legend = list(orientation = "h", y = -0.2)
        )
      })
    }

    output$variable_definition <- renderUI({
      def <- VARIABLE_DEFINITIONS[[input$variable]]
      if (is.null(def) || !nzchar(def)) return(NULL)
      p(class = "text-muted small", strong(paste0(input$variable, ": ")), def)
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        mode_part <- if (identical(input$view_mode, "growth")) {
          "annual-growth"
        } else if (isTRUE(input$rebase_toggle)) {
          paste0("rebased-", input$base_year)
        } else {
          "index"
        }
        sprintf(
          "productivity_%s_%s_%s-%s_%s.csv",
          gsub("[^A-Za-z0-9]+", "-", input$variable), mode_part,
          input$year_range[1], input$year_range[2], format(Sys.Date(), "%Y%m%d")
        )
      },
      content = function(file) {
        df <- build_export_df(filtered_data(), input$rebase_toggle, input$base_year)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}

ui <- function(request) {
  # ui() runs per HTTP
  # request, before any session/reactive context exists, so it can't read
  # a reactive value. Computed directly from the data at page-build time
  # (not via a server-side update*Input call) so the default selection is
  # already present in the first HTML the browser receives.
  init_df <- load_lp_data()

  page_fillable(
    title = "Canadian Productivity Dashboard", # browser tab title only -- no on-page heading
    # page() renders straight into <body> with no margin/padding of its
    # own, so the intro text would otherwise butt right up against the
    # browser edge -- give the whole page some breathing room on top/sides.
    #
    # nav-pills only puts a visible background on the *active* pill by
    # default -- give every pill a border + margin so all 4 read as
    # distinct, fully-rounded (pill/capsule-shaped, not just rounded-corner)
    # buttons with a gap between them, not one continuous flush bar with
    # just the selected segment highlighted. margin-bottom on the row
    # itself puts a little breathing room between the buttons and the card
    # below them.
    #
    # Colors/font reuse the same named constants the charts use (see top of
    # file) so the page chrome and the plots stay a single source of truth
    # instead of a second set of hardcoded hex values drifting out of sync.
    #
    # color-scheme: light tells the browser this page is deliberately
    # light-themed -- without it, some browsers' "auto dark mode for
    # websites" feature (on for some users at the OS/browser level) inverts
    # our light grey/white palette into a dark one on its own.
    tags$style(HTML(sprintf(
      "html { color-scheme: light; }
       body { padding: 2rem 2.5rem; background-color: %s; font-family: %s; color: %s; }
       .card { background-color: %s; border-radius: 1.25rem; }
       .nav-pills { margin-bottom: 1rem; }
       .nav-pills .nav-link {
         border: 1px solid var(--bs-border-color, #dee2e6); border-radius: 50rem; margin: 0 0.5rem;
         color: %s; background-color: %s;
       }
       .nav-pills .nav-link.active { background-color: %s; color: %s; }
       .trend-more-options > summary { cursor: pointer; list-style: none; display: flex; align-items: center;
         gap: 0.35rem; color: var(--bs-link-color, #0d6efd); font-size: 0.9rem; margin-bottom: 0.5rem; }
       .trend-more-options > summary::-webkit-details-marker { display: none; }
       .trend-more-options > summary::before { content: '\\25B8'; transition: transform 0.15s ease; }
       .trend-more-options[open] > summary::before { transform: rotate(90deg); }
       /* page_fillable() makes <body> a fill-height flex column, but
          navset_pill()'s .tabbable/.tab-content/.tab-pane wrapper markup
          is all plain block, none of it a flex container passing that
          fill height further down -- without these three, the card
          (which bslib DOES mark fill-aware) never receives a stretched
          height to fill. */
       .tabbable { flex: 1 1 auto; display: flex; flex-direction: column; min-height: 0; }
       .tab-content { flex: 1 1 auto; display: flex; flex-direction: column; min-height: 0; }
       .tab-content > .tab-pane.active { flex: 1 1 auto; display: flex; flex-direction: column; min-height: 0; }
       /* bslib's own .card.html-fill-item CSS defaults to flex: 0 1 auto
          (content-sized, not growing) -- override just the one directly
          inside our now-fillable tab pane so it actually claims the space
          the layers above are now correctly offering it. */
       .tab-pane.active > .card { flex: 1 1 auto; min-height: 0; }",
      CHART_SURFACE, FONT_FAMILY, INK_PRIMARY,
      CHART_SURFACE,
      INK_PRIMARY, CHART_SURFACE,
      CATEGORICAL_PALETTE[1], CHART_SURFACE
    ))),
    p(
      class = "text-muted",
      style = "margin-bottom: 0.75rem;",
      "Use this data dashboard to explore labour productivity trends across Canadian industries ",
      "using Statistics Canada data. Compare industries over time, examine recent growth and ",
      "customized subperiods, rank long-run performance, and download underlying data."
    ),
    # navset_pill (button-styled tabs) + nav-justified (stretched to fill
    # the page width in equal-width segments) instead of the default
    # left-aligned, content-width nav-tabs underline style.
    #
    # Depends on bslib's internal navset_pill() markup exposing a
    # "ul.nav" element to target -- if a future bslib version restructures
    # that, find() just matches nothing and addClass() is then a no-op
    # (verified: this degrades to un-justified tabs, it does not error or
    # break tab switching), so a bslib upgrade is a "check this still looks
    # right" item, not a "the app is broken" risk.
    tagQuery(
      navset_pill(
        nav_panel("Trends", trend_tab_ui("trend", init_df)),
        nav_panel("Rankings", ranking_tab_ui("ranking", init_df)),
        nav_panel("Compare", tab_module_ui("bar", init_df, "bar")),
        nav_panel("Data", tab_module_ui("table", init_df, "table"))
      )
    )$find("ul.nav")$addClass("nav-justified")$allTags()
  )
}

server <- function(input, output, session) {
  raw_data <- RAW_DATA_READER # single shared reactiveFileReader, not duplicated per tab

  trend_tab_server("trend", raw_data)
  ranking_tab_server("ranking", raw_data)
  tab_module_server("bar", raw_data, "bar")
  tab_module_server("table", raw_data, "table")
}

shinyApp(ui, server)
