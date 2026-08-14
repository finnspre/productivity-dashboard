library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(htmltools)

LP_DATA_FILE <- Sys.getenv("LP_DATA_FILE", "lp_data.RData")

# Appends a ?v=<mtime> cache-buster to a www/ asset path, so editing e.g.
# industry_tree.js takes effect on a plain reload instead of silently
# serving a stale cached copy from before the edit (the static file's
# *name* doesn't change, and Shiny doesn't send cache-control headers that
# would force revalidation on every load, so without this a browser that
# already cached the old file can hang onto it across an ordinary reload).
versioned_asset <- function(path) {
  mtime <- file.mtime(file.path("www", path))
  paste0(path, "?v=", if (is.na(mtime)) "0" else as.integer(mtime))
}

# Categorical palette -- these are the real CSLS brand colours (the NHL-team
# nicknames are just internal mnemonics), so the *values* are fixed. The
# order below isn't the order they happen to have been typed in, though: it's
# the ordering (of the same 8 hex values, slot 1 held fixed since it also
# drives page chrome -- see below) that maximizes worst-case adjacent-pair
# separation under simulated colour-blindness. Computed by porting the
# dataviz skill's own OKLab + Machado-Oliveira-Fernandes (2009) CVD-simulation
# math to R and brute-forcing all 5,040 orderings that keep slot 1 fixed:
# the *previous* order had Canadiens Red directly beside Canucks Green (slots
# 2-3), which is the canonical red-green colour-blindness confusion pair --
# CVD deltaE 4.2 there, below the 6.0 hard floor, invisible to normal vision
# (deltaE 27.9) which is exactly why this needs computing, not eyeballing.
# This order clears worst-case adjacent CVD deltaE 24.6 (target >=8) and
# worst-case adjacent normal-vision deltaE 28.1 (floor >=15) -- re-run the
# same check before ever reordering this again.
CATEGORICAL_PALETTE <- c(
  "#012F72", # Maple Leaf Blue -- slot 1, held fixed: also the site-wide
             # primary accent (see BRAND_MAPLE_BLUE below -- default-state
             # buttons/links/tab text, and the single-series colour on
             # Trends/Rankings, all key off this slot)
  "#3C8745", # Canucks Green
  "#FFC550", # Flames Yellow
  "#CE2E2E", # Canadiens Red
  "#EF84EF", # Panther Pink
  "#7E1F86", # King Purple
  "#F74C16", # Oilers Orange
  "#0598D8"  # Jets Blue -- slot 8 (see BRAND_JETS_BLUE below -- csls.ca's
             # universal hover/focus/active-state colour: active tab fill,
             # link hover, industry-tree hover/selected highlight)
)
# Secondary channel (colour-blind/print/grayscale accessibility) for series
# identity, cycled alongside CATEGORICAL_PALETTE by index -- see
# series_color_map() and the Compare tab's per-series trace loop. 6 values
# each (not 8) is deliberate: it keeps the dash/shape cycle out of phase with
# the 8-colour cycle, so a repeat of "solid + slot 1's colour" doesn't land on
# the same series index every time round.
LINE_DASH_STYLES <- c("solid", "dash", "dot", "dashdot", "longdash", "longdashdot")
MARKER_SYMBOLS <- c("circle", "diamond", "square", "triangle-up", "cross", "x")

# Page-chrome aliases for the two brand blues csls.ca actually names in its
# own :root (--color-maple-blue / --color-jets-blue -- see
# CSLS-Shiny-Style-Spec.md section 2). Reused by name from CATEGORICAL_PALETTE
# rather than re-typed as separate hex literals, so page chrome and the chart
# palette stay pinned to one source of truth. Chrome-only: the chart code
# below keeps referencing CATEGORICAL_PALETTE[1] directly (unchanged) for the
# Trends/Rankings single-series colour -- these two aliases are for the CSS
# in ui() (nav-pills, links, the industry-tree dropdown, chip list) only.
BRAND_MAPLE_BLUE <- CATEGORICAL_PALETTE[1] # default state: buttons, links, tab/nav text
BRAND_JETS_BLUE <- CATEGORICAL_PALETTE[8]  # hover/focus/active state: tabs, links, highlights

INK_PRIMARY <- "#000000"   # csls.ca body-text black (was #0C0C0C "Senators Black" -- now the exact
                            # site value; only this UI-chrome/axis-text neutral changed, the chart
                            # series palette above is untouched -- see CSLS-Shiny-Style-Spec.md section 2)
INK_MUTED <- "#6B6B6B"     # Dark Grey -- already the exact csls.ca value; 5.33:1 vs CHART_SURFACE, clears WCAG AA text contrast
GRIDLINE <- "#D9D9D9"      # Light Grey -- already the exact csls.ca value -- deliberately low-contrast/recessive, not a bug (see display_axis_title() usage below)
CHART_SURFACE <- "#FFFFFF" # Snow White -- already the exact csls.ca value
FONT_FAMILY <- "Roboto, Arial, sans-serif" # matches csls.ca's --font-base fallback stack exactly

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

# Nested tree for the custom industryTreeInput widget (see
# www/industry_tree.js for the paired Shiny.InputBinding): each node is
# list(value=, label=, children=list(...)), built by walking INDUSTRY_PARENT
# from the 3 root aggregates. Unlike the old flat indented-label vector this
# replaces, the JS side gets real parent/child nesting, which is what lets
# it render a collapsible tree and auto-expand ancestors of a search match.
industry_tree_nodes <- function(df) {
  available <- unique(df$Industry)

  children_of <- function(parent) {
    sort(available[match(available, names(INDUSTRY_PARENT), 0L) > 0 &
                      INDUSTRY_PARENT[available] == parent])
  }
  build_node <- function(name) {
    list(value = name, label = name, children = lapply(children_of(name), build_node))
  }

  roots <- intersect(c("All industries", "Business sector industries", "Non-business sector industries"), available)
  nodes <- lapply(roots, build_node)

  # Anything present but not reachable from the roots above (e.g. a future
  # industry not yet mapped in INDUSTRY_PARENT) still shows up, as an
  # unindented top-level leaf, rather than silently disappearing from the
  # picker -- same "soft fail" behaviour the old flat-list version had.
  reachable <- unlist(lapply(nodes, flatten_tree_values), use.names = FALSE)
  leftover <- sort(setdiff(available, reachable))
  c(nodes, lapply(leftover, function(name) list(value = name, label = name, children = list())))
}

# Recursively collects every `value` in an industry_tree_nodes() node
# (including itself) -- used above to find industries not reachable from
# the 3 roots.
flatten_tree_values <- function(node) {
  c(node$value, unlist(lapply(node$children, flatten_tree_values), use.names = FALSE))
}

# UI generator for the collapsible tree-dropdown Industry picker. Mirrors
# selectizeInput's single-value contract: the value Shiny sees is always a
# plain character(1) Industry name, or "" for "nothing selected" -- never
# NULL/character(0) -- so req()/isTruthy() gating elsewhere didn't need to
# change just because the widget underneath did. All the interactive markup
# (toggle button, search box, expandable rows) is built client-side by
# www/industry_tree.js from the embedded JSON below; this only emits the
# skeleton the binding hydrates.
industryTreeInput <- function(inputId, label = NULL, tree_data, selected = NULL,
                               placeholder = "Search industries...", width = NULL) {
  selected <- if (is.null(selected) || identical(selected, character(0))) "" else selected
  div(
    class = "form-group shiny-input-container industry-tree-input",
    style = css(width = validateCssUnit(width)),
    if (!is.null(label)) tags$label(class = "control-label", `for` = inputId, label),
    tags$div(
      id = inputId, class = "industry-tree",
      `data-selected` = selected, `data-placeholder` = placeholder,
      tags$script(
        type = "application/json", class = "industry-tree-data",
        HTML(jsonlite::toJSON(tree_data, auto_unbox = TRUE))
      )
    )
  )
}

# Server-side counterpart to updateSelectizeInput() etc. for the widget
# above -- uses the same session$sendInputMessage() plumbing every built-in
# update*Input() uses, which Shiny dispatches generically to the bound
# element's receiveMessage(el, data) JS method, so no bespoke
# session$sendCustomMessage()/session$onMessage() wiring is needed on
# either side.
updateIndustryTreeInput <- function(session, inputId, tree_data = NULL, selected = NULL) {
  message <- dropNulls(list(tree_data = tree_data, selected = selected))
  session$sendInputMessage(inputId, message)
}

# Small local stand-in for shiny:::dropNulls (used internally by every
# built-in update*Input()) -- avoids reaching into shiny's internals with
# ::: for the sake of one two-line helper.
dropNulls <- function(x) x[!vapply(x, is.null, logical(1))]

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
  #
  # Regex over the handful of *distinct* Industry values, then remap by
  # match() -- not sub() over all ~588k rows directly. Same result (verified
  # byte-identical against the real data), ~32x faster (0.79s -> 0.02s on
  # this dataset): only ~139 distinct Industry strings exist no matter how
  # many rows there are, so the row-wise regex was redoing the same handful
  # of substitutions hundreds of thousands of times over.
  industry_uniq <- unique(df$Industry)
  industry_uniq_clean <- sub("\\s*==>.*$", "", industry_uniq)
  df$Industry <- industry_uniq_clean[match(df$Industry, industry_uniq)]

  # INDUSTRY_PARENT is a hand-maintained lookup, not derived from this data
  # pull -- industry_choices_tree() already fails *soft* for anything
  # missing from it (shows up unindented at the end of the picker instead
  # of disappearing), but that's easy to miss visually. Log it loudly too,
  # so a future StatCan rename/addition doesn't drift silently forever.
  unmapped <- setdiff(
    unique(df$Industry),
    c("All industries", "Business sector industries", "Non-business sector industries", names(INDUSTRY_PARENT))
  )
  if (length(unmapped) > 0) {
    warning(
      "load_lp_data(): ", length(unmapped), " industry name(s) not in INDUSTRY_PARENT -- ",
      "they'll still appear in pickers (unindented, at the end) instead of nested under their ",
      "true parent until INDUSTRY_PARENT is updated: ", paste(unmapped, collapse = ", "),
      call. = FALSE
    )
  }

  df
}
# Shared cache for load_lp_data(), keyed on the file's mtime -- same
# invalidation check reactiveFileReader (below) already does internally, but
# usable from ui(), which runs per HTTP request *outside* any reactive
# context (reactiveFileReader requires one). Without this, ui() and
# RAW_DATA_READER each independently reloaded the ~1.6s-to-parse RData file,
# so a repeat page view paid that cost again even though the server already
# had an identical copy sitting in RAW_DATA_READER's own reactive cache.
# session = NULL-style sharing: one mutable env for the whole R process, not
# per-session -- consistent with RAW_DATA_READER's own sharing model below.
LP_DATA_CACHE <- new.env(parent = emptyenv())
cached_load_lp_data <- function(path = LP_DATA_FILE) {
  mtime <- file.mtime(path)
  if (is.null(LP_DATA_CACHE$df) || !identical(LP_DATA_CACHE$mtime, mtime)) {
    LP_DATA_CACHE$df <- load_lp_data(path)
    LP_DATA_CACHE$mtime <- mtime
  }
  LP_DATA_CACHE$df
}

# Check lp_data.RData for modifications every 6000 seconds (10 minutes) by default
# but allow tests to override this to avoid waiting 10 real minutes for a test to exercise the reactiveFileReader.
# Session = NULL means this reader isn't tied to (or
# torn down with) any one visitor's session.
RAW_DATA_POLL_MS <- as.numeric(Sys.getenv("RAW_DATA_POLL_MS", 10 * 60 * 1000))
# cached_load_lp_data (not load_lp_data directly) -- see LP_DATA_CACHE above,
# this is what lets ui()'s init_df and this reactive share one in-memory load.
RAW_DATA_READER <- reactiveFileReader(RAW_DATA_POLL_MS, session = NULL, LP_DATA_FILE, cached_load_lp_data)

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
# Overflow series share muted grey -- see series_style_map() below for how
# those overflow series still stay distinguishable from *each other*.
series_color_map <- function(series) {
  n <- length(series)
  colors <- if (n <= length(CATEGORICAL_PALETTE)) {
    CATEGORICAL_PALETTE[seq_len(n)]
  } else {
    c(CATEGORICAL_PALETTE, rep(INK_MUTED, n - length(CATEGORICAL_PALETTE)))
  }
  setNames(colors, series)
}

# Secondary channel (line dash / marker shape) for series identity, indexed
# 1:1 with series_color_map()'s colour assignment -- a colour-blind-friendly
# cue that doesn't depend on hue at all, and the mechanism that keeps
# overflow series (beyond the fixed 8 colours, which never cycle) distinct
# from *each other* despite sharing the same muted grey. LINE_DASH_STYLES/
# MARKER_SYMBOLS are 6-long (not 8), so the dash/shape cycle drifts out of
# phase with the 8-colour cycle instead of always pairing "solid" with slot 1.
series_style_map <- function(series) {
  n <- length(series)
  idx <- seq_len(n)
  list(
    dash = setNames(LINE_DASH_STYLES[((idx - 1) %% length(LINE_DASH_STYLES)) + 1], series),
    symbol = setNames(MARKER_SYMBOLS[((idx - 1) %% length(MARKER_SYMBOLS)) + 1], series)
  )
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

# Metric-specific number formatting for a chart's y-axis ticks and hover
# text -- both driven off this one helper so they never drift apart. Returns
# d3-format strings (Plotly's tick/hover formatting language): `tickformat`
# for the numeric part, `prefix`/`suffix` for a currency symbol or a percent
# sign. growth mode and the rebased-to-100 view override the variable's own
# UOM entirely (a growth rate or an index is never denominated in the
# underlying variable's dollars/jobs/hours), so those are checked first.
# Classification below is keyword-matched against the actual UOM strings
# data_pipeline.R's StatCan pull produces (Percent / Jobs / Hours / dollar
# amounts, some already expressed "in thousands", some as *rates* -- "per
# hour", "per unit of real GDP"). An unrecognized future UOM falls through to
# a plain thousands-separated number rather than erroring.
metric_format_spec <- function(uom, view_mode, rebase_toggle) {
  if (identical(view_mode, "growth")) {
    return(list(tickformat = ".1f", prefix = "", suffix = "%"))
  }
  if (isTRUE(rebase_toggle)) {
    return(list(tickformat = ".1f", prefix = "", suffix = ""))
  }
  uom <- if (is.null(uom)) "" else uom
  if (grepl("percent", uom, ignore.case = TRUE)) {
    list(tickformat = ".1f", prefix = "", suffix = "%")
  } else if (grepl("dollar", uom, ignore.case = TRUE) && grepl("per", uom, ignore.case = TRUE)) {
    # Rates -- productivity, compensation/hour, unit labour cost -- stay in
    # small, meaningful figures, so cents-level precision rather than an
    # abbreviated/rounded-off value.
    list(tickformat = ",.2f", prefix = "$", suffix = "")
  } else if (grepl("dollar", uom, ignore.case = TRUE)) {
    # Levels -- value added, total compensation -- already expressed "in
    # thousands of dollars" by the data itself, so whole dollars, comma-grouped.
    list(tickformat = ",.0f", prefix = "$", suffix = "")
  } else if (grepl("jobs|hours", uom, ignore.case = TRUE)) {
    list(tickformat = ",.0f", prefix = "", suffix = "")
  } else {
    list(tickformat = ",.1f", prefix = "", suffix = "")
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

# Rebases `value` to a percentage of `base_value` (100 = base year level).
# Returns NA -- not Inf/NaN -- whenever that's undefined: a missing base
# value, or a base value of exactly 0. A raw StatCan value of 0 genuinely
# occurs (e.g. a granular industry with no measured activity in a small
# province/year), and naive division would silently produce +/-Inf that
# slips straight past every `is.na()` guard in this file (is.na(Inf) is
# FALSE in R) and can blow out a chart's whole axis with one bad point.
safe_index_to_100 <- function(value, base_value) {
  ifelse(is.na(value) | is.na(base_value) | base_value == 0, NA_real_, value / base_value * 100)
}

# Compound annual growth rate from `start_value` to `end_value` over
# `n_years`. NA (not Inf/NaN) whenever it's undefined: a missing/non-
# positive start value (division by zero or a meaningless negative base),
# a missing end value, or a non-positive year span. A start value of 0 is
# the same "real StatCan zero" case safe_index_to_100() guards against --
# an end value of 0 is left alone (a genuine, meaningful -100% CAGR).
compute_cagr <- function(start_value, end_value, n_years) {
  bad_start <- is.na(start_value) | is.na(n_years) | start_value <= 0 | n_years <= 0
  ratio <- end_value / start_value
  ifelse(bad_start | is.na(ratio), NA_real_, ratio^(1 / n_years) - 1)
}

# True if `ancestor` is `industry` itself or one of its ancestors in the
# Industry hierarchy (see INDUSTRY_PARENT) -- used to flag when two active
# series in the *same* geography would double-count each other for
# additive measures (jobs, hours, compensation): "All industries" and
# "Manufacturing" aren't independent series, one contains the other.
industry_is_ancestor <- function(ancestor, industry) {
  seen <- character(0)
  current <- industry
  repeat {
    if (identical(current, ancestor)) return(TRUE)
    if (current %in% seen) return(FALSE) # cycle guard; shouldn't happen
    seen <- c(seen, current)
    # INDUSTRY_PARENT is a plain named character vector, not a list -- `[[`
    # errors ("subscript out of bounds") on a name it doesn't contain,
    # instead of returning NULL. `[` returns NA instead, which every root
    # category (e.g. "Business sector industries") hits once walked up to,
    # since roots have no parent entry of their own.
    parent <- unname(INDUSTRY_PARENT[current])
    if (is.na(parent)) {
      # INDUSTRY_PARENT only encodes the picker's *indentation* hierarchy
      # (2-digit -> its sector, 3-digit -> its 2-digit) -- it doesn't
      # separately encode that "Business sector industries" and "Non-
      # business sector industries" are themselves subtotals of "All
      # industries" (the whole-economy total), since industry_choices_tree()
      # treats all three as parallel roots, not literally nested. Bridge
      # that one missing edge here so ancestry checks still catch "All
      # industries" vs. any of its components.
      if (current %in% c("Business sector industries", "Non-business sector industries")) {
        parent <- "All industries"
      } else {
        return(FALSE)
      }
    }
    current <- parent
  }
}

# Provenance for whoever's looking at the numbers -- without this, two users
# (or the same user before/after a scheduled data refresh) could see
# different figures with no indication why. Rendered per-tab (via
# uiOutput(ns("data_asof")) + this in each tab's own renderUI) so it sits
# directly under that tab's own "Source: ..." line rather than as a single
# element trailing the whole tab region below the card.
data_asof_ui <- function() {
  p(
    class = "text-muted small",
    "Data last refreshed: ", format(file.mtime(LP_DATA_FILE), "%Y-%m-%d")
  )
}

# Shared "Download" dropdown for the Trends, Rankings, Compare, and Data
# tabs -- replaces each tab's old single-purpose "Download CSV" button with
# a menu tailored to what that tab can actually offer:
#   - Trends/Rankings: chart-as-PNG + displayed data.
#   - Compare: chart-as-PNG + displayed data (kind == "bar" -- it has a
#     chart, but no standing notion of "the full dataset" independent of
#     the current Variable/pairs selection).
#   - Data: displayed data + full dataset (kind == "table" -- it renders a
#     table, not a chart, so no PNG option; and it's the one tab already
#     built around showing/exporting a whole scoped table, so it's the
#     natural home for a full-dataset export too).
#
# The PNG option needs no server-side downloadHandler: it's wired to
# downloadChartPng() in www/ui_helpers.js, which reaches into the already-
# rendered Plotly graph div client-side (via Plotly's own PNG exporter) --
# no round-trip to the server, and no server-side image rendering to keep in
# sync with what's on screen. The CSV option(s) are real downloadHandlers,
# same mechanism the old single button used, just re-styled as dropdown-item
# links (downloadLink(), not downloadButton(), so they don't carry their own
# conflicting btn styling inside the menu).
download_menu_ui <- function(ns, chart_id = ns("chart"), include_png = TRUE, full_dataset = FALSE) {
  tags$div(
    class = "dropdown download-dropdown",
    tags$button(
      class = "btn btn-default dropdown-toggle", type = "button",
      id = ns("download_menu_toggle"),
      `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      icon("download"), " Download"
    ),
    tags$ul(
      class = "dropdown-menu", `aria-labelledby` = ns("download_menu_toggle"),
      if (isTRUE(include_png)) {
        tags$li(tags$button(
          class = "dropdown-item", type = "button",
          onclick = sprintf("downloadChartPng('%s')", chart_id),
          "Download chart as PNG"
        ))
      },
      tags$li(downloadLink(ns("download_csv"), "Download displayed data (.csv)", class = "dropdown-item")),
      if (isTRUE(full_dataset)) {
        tags$li(downloadLink(ns("download_full"), "Download full dataset (.csv)", class = "dropdown-item"))
      }
    )
  )
}

# The Trends tab: a focused single-series view (one Variable + one
# Geography + one Industry -> one line), unlike the other 3 tabs which
# still compare multiple (Industry, Geography) pairs at once. Kept as its
# own dedicated module rather than another "kind" of tab_module_ui/
# tab_module_server below, since its sidebar shape and reactive pipeline
# are genuinely different (no active_pairs/Add-series/multi-color
# machinery), not just a different final render step.
trend_tab_ui <- function(id, init_df, variable_choices, geography_choices, industry_tree) {
  ns <- NS(id)

  card(
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        selectInput(
          ns("variable"), "Variable",
          choices = variable_choices, selected = DEFAULT_VARIABLE
        ),
        uiOutput(ns("variable_definition")),
        # selectize (the default) makes this a type-to-filter searchable
        # dropdown rather than a native <select> -- worth it even with only
        # 14 provinces/territories, for consistency with every other picker
        # in the app.
        selectInput(
          ns("geography"), "Geography",
          choices = geography_choices, selected = DEFAULT_GEOGRAPHY,
          selectize = TRUE
        ),
        # Always the full Aggregate+2-digit+3-digit tree -- no separate
        # industry_level toggle (none of the tabs have one). Collapsible
        # tree dropdown (see www/industry_tree.js) -- closed to just the 3
        # root aggregates by default, arrow to expand a branch, click a
        # label to pick it.
        industryTreeInput(
          ns("industry"), "Industry",
          tree_data = industry_tree, selected = DEFAULT_INDUSTRY
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
            ns("year_range"), "Date range",
            min = min(init_df$Year), max = max(init_df$Year),
            value = c(min(init_df$Year), max(init_df$Year)),
            step = 1, sep = ""
          ),
          radioButtons(
            ns("view_mode"), "View values as",
            choices = c("Level" = "level", "Annual percentage change" = "growth"),
            # Stacked rather than inline -- "Annual percentage change" is
            # too long to sit next to "Level" on one line in the sidebar.
            selected = "level", inline = FALSE
          ),
          conditionalPanel(
            "input.view_mode == 'level'", ns = ns,
            checkboxInput(ns("rebase_toggle"), "Set each series to 100 in a selected year", value = FALSE),
            conditionalPanel(
              "input.view_mode == 'level' && input.rebase_toggle == true", ns = ns,
              selectInput(
                ns("base_year"), "Base year",
                choices = sort(unique(init_df$Year)), selected = min(init_df$Year),
                selectize = FALSE
              )
            )
          ),
          download_menu_ui(ns)
        )
      ),
      plotlyOutput(ns("chart"), height = "100%"),
      p(class = "text-muted small", "Source: Statistics Canada Table 36-10-0480-01"),
      uiOutput(ns("data_asof"))
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
    #
    # ignoreInit = TRUE: this only needs to fire when the underlying file
    # actually changes -- ui() just built this session's initial widgets
    # from the same raw_data() a moment ago, so re-running it again on
    # session connect (bindEvent()'s default) redid that work for nothing
    # and re-pushed an identical Industry tree over the websocket.
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

      new_industry <- if (is.null(input$industry) || !(input$industry %in% unique(df$Industry))) {
        DEFAULT_INDUSTRY
      } else {
        input$industry
      }
      updateIndustryTreeInput(session, "industry", tree_data = industry_tree_nodes(df), selected = new_industry)

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
    }) |> bindEvent(raw_data(), once = FALSE, ignoreInit = TRUE)

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
        base_value <- if (nrow(base_row) > 0) base_row$Value[1] else NA_real_
        df$RebasedValue <- safe_index_to_100(df$Value, base_value)
        # "Missing" covers both no row at the base year at all AND a base
        # value of exactly 0 (rebasing to a 0 is undefined, not just rare --
        # see safe_index_to_100()) -- either way there's nothing to show.
        missing_base <- is.na(base_value) || isTRUE(base_value == 0)
      } else {
        df$RebasedValue <- NA_real_
        missing_base <- FALSE
      }

      df$DisplayValue <- if (identical(input$view_mode, "growth")) {
        df$GrowthPct
      } else if (isTRUE(input$rebase_toggle)) {
        df$RebasedValue
      } else {
        df$Value
      }

      list(df = df, missing_base = missing_base)
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
      fmt <- metric_format_spec(variable_uom(), input$view_mode, input$rebase_toggle)
      line_color <- CATEGORICAL_PALETTE[1]
      # Chart title only states the variable/timeframe -- with a single
      # line there's no legend to identify which industry/geography it is,
      # so name the pair as a subtitle. Value leads (bold) with the series
      # name following on its own line in the hover, same ordering as
      # Compare's multi-series tooltip -- see the comment there for why the
      # name still needs to be in the template even under "x unified"
      # hovermode (its per-row colour swatch isn't a substitute for text).
      subtitle <- pair_label(input$industry, input$geography)

      plot_ly(
        data = df, x = ~Year, y = ~DisplayValue, name = subtitle,
        type = "scatter", mode = "lines+markers",
        line = list(color = line_color, width = 2),
        marker = list(color = line_color, size = 8),
        hovertemplate = paste0(
          "<b>%{y:", fmt$prefix, fmt$tickformat, "}", fmt$suffix, "</b><br>", subtitle, "<extra></extra>"
        )
      ) %>%
        layout(
          title = list(text = paste0(
            display_chart_title(input$variable, input$year_range),
            "<br><sup style='color:", INK_MUTED, "'>", subtitle, "</sup>"
          )),
          # nticks (not a fixed dtick) lets Plotly's own auto-tick engine
          # pick a clean step (2/5/10 years) that fits the actually-rendered
          # width, recalculated on every resize -- ~8 lands in the requested
          # 6-10 label range without hard-coding a tick-every-year step that
          # gets crowded over a long date range.
          xaxis = list(title = "Year", nticks = 8, tickformat = "d", gridcolor = GRIDLINE, color = INK_MUTED),
          yaxis = list(
            title = axis_title, gridcolor = GRIDLINE, color = INK_MUTED,
            tickformat = fmt$tickformat, tickprefix = fmt$prefix, ticksuffix = fmt$suffix
          ),
          paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
          font = list(color = INK_PRIMARY, family = FONT_FAMILY),
          showlegend = FALSE,
          hovermode = "x unified",
          # A visible zero line in growth mode -- "above/below flat" is the
          # first thing a reader wants out of an annual-% -change chart, and
          # Plotly's default zeroline is unstyled/easy to miss against the
          # gridlines otherwise.
          shapes = if (identical(input$view_mode, "growth")) {
            list(list(
              type = "line", xref = "paper", x0 = 0, x1 = 1,
              yref = "y", y0 = 0, y1 = 0,
              line = list(color = INK_MUTED, width = 1, dash = "dot")
            ))
          }
        )
    })

    output$variable_definition <- renderUI({
      def <- VARIABLE_DEFINITIONS[[input$variable]]
      if (is.null(def) || !nzchar(def)) return(NULL)
      p(class = "text-muted small", strong(paste0(input$variable, ": ")), def)
    })

    # raw_data() is the dependency, not the value used -- reading it just
    # ties this to the same reactiveFileReader invalidation as this tab's
    # own data, so the "as of" date updates the moment a new pipeline run
    # lands, without this needing its own poll loop.
    output$data_asof <- renderUI({
      raw_data()
      data_asof_ui()
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
ranking_tab_ui <- function(id, init_df, variable_choices, geography_choices) {
  ns <- NS(id)

  card(
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        selectInput(
          ns("variable"), "Variable",
          choices = variable_choices, selected = DEFAULT_VARIABLE
        ),
        uiOutput(ns("variable_definition")),
        # selectize (the default) makes this a type-to-filter searchable
        # dropdown rather than a native <select> -- worth it even with only
        # 14 provinces/territories, for consistency with every other picker
        # in the app.
        selectInput(
          ns("geography"), "Geography",
          choices = geography_choices, selected = DEFAULT_GEOGRAPHY,
          selectize = TRUE
        ),
        sliderInput(
          ns("year_range"), "Date range",
          min = min(init_df$Year), max = max(init_df$Year),
          value = c(min(init_df$Year), max(init_df$Year)),
          step = 1, sep = ""
        ),
        radioButtons(
          ns("industry_level"), "Industry classification level",
          choices = c(
            "Economy-wide and broad aggregates" = "Aggregate",
            "Sector level" = "2-digit",
            "Sub-sector level" = "3-digit"
          ),
          # Stacked rather than inline -- the longer labels above would
          # wrap awkwardly across a narrow sidebar as a horizontal row.
          selected = DEFAULT_INDUSTRY_LEVEL, inline = FALSE
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
          download_menu_ui(ns)
        )
      ),
      plotlyOutput(ns("chart"), height = "100%"),
      p(class = "text-muted small", "Source: Statistics Canada Table 36-10-0480-01"),
      uiOutput(ns("data_asof"))
    )
  )
}

ranking_tab_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {

    # Same DEFAULT_*-fallback sync pattern as the Trends tab -- Variable and
    # Geography are always-active single selections here too, never blank.
    # ignoreInit = TRUE -- see the matching comment on the Trends tab's own
    # sync observe(): ui() already built this session's initial widgets from
    # the same raw_data(), so this only needs to fire on a real data change.
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
    }) |> bindEvent(raw_data(), once = FALSE, ignoreInit = TRUE)

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
      joined$CAGR <- compute_cagr(joined$StartValue, joined$EndValue, end_year - start_year)
      joined
    })

    # A custom time frame can land on years some industries lack data for
    # (added/discontinued series) -- inner_join() above silently drops those
    # rows entirely. Separately, an industry can have a row at *both* years
    # but still end up with an undefined CAGR (e.g. a start value of exactly
    # 0 -- compute_cagr() returns NA rather than Inf for that). Both cases
    # mean "not shown", so both get surfaced the same way instead of one
    # silently shrinking the chart with no explanation.
    ranking_dropped_industries <- reactive({
      rd <- tryCatch(ranking_data(), error = function(e) NULL)
      if (is.null(rd)) return(character(0))
      all_industries <- unique(scoped_raw()$Industry)
      shown <- rd$Industry[!is.na(rd$CAGR)]
      setdiff(all_industries, shown)
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
      rd <- ranking_data() %>% filter(!is.na(CAGR)) %>% arrange(CAGR)
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

    # raw_data() is the dependency, not the value used -- reading it just
    # ties this to the same reactiveFileReader invalidation as this tab's
    # own data, so the "as of" date updates the moment a new pipeline run
    # lands, without this needing its own poll loop.
    output$data_asof <- renderUI({
      raw_data()
      data_asof_ui()
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
tab_module_ui <- function(id, init_df, kind, variable_choices, industry_tree) {
  ns <- NS(id)

  main_panel <- switch(
    kind,
    # fill = FALSE is the actual, sanctioned way to opt a DT table out of
    # bslib's fill-to-available-height system -- DTOutput() defaults to
    # fill = TRUE, which is what was pinning it to a fixed pixel height via
    # an actively-enforced resize observer (confirmed: neither CSS
    # !important nor a JS override applied after the fact could beat it,
    # since it just gets silently re-imposed on the next resize tick).
    table = DTOutput(ns("chart"), fill = FALSE),
    plotlyOutput(ns("chart"), height = "100%")
  )

  card(
    # Only the Data tab's card gets its own internal scrollbar (see the
    # .tab-pane.active > .card.table-tab-card CSS override below) -- a DT
    # table's true height (rows + its own "Showing X of Y"/pagination
    # footer) is content-driven and routinely exceeds the fixed
    # viewport-height budget every card is otherwise pinned to, which
    # silently clipped the Source/Data-last-refreshed lines after the table
    # instead of leaving them reachable by scrolling.
    class = if (kind == "table") "table-tab-card",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        selectInput(
          ns("variable"), "Variable",
          choices = variable_choices, selected = DEFAULT_VARIABLE
        ),
        uiOutput(ns("variable_definition")),
        tags$strong("Compare"),
        # Collapsible tree dropdown (see www/industry_tree.js) -- closed to
        # just the 3 root aggregates by default, arrow to expand a branch,
        # click a label to pick it. Natively supports an empty "nothing
        # selected" state (reported as ""), so unlike the old selectize
        # picker this replaces, no leading blank "" choice trick is needed
        # to make the box start empty.
        industryTreeInput(
          ns("pair_industry"), "Industry",
          tree_data = industry_tree, selected = NULL
        ),
        selectizeInput(
          ns("pair_geography"), "Geography",
          choices = c("", GEOGRAPHY_ORDER), selected = character(0),
          options = list(placeholder = "Search geographies...")
        ),
        # Starts disabled (both pickers are blank on load) -- the server's
        # observe() on input$pair_industry/input$pair_geography takes over
        # from here the moment the session connects, see tab_module_server().
        actionButton(ns("add_pair"), "Add series", icon = icon("plus"), disabled = NA),
        tags$strong("Series to compare"),
        # Chip list replaces the old checkboxGroupInput -- active_pairs()
        # drives this reactively via renderUI, so unlike the checkbox
        # widget there's no separate "sync the widget" step needed whenever
        # active_pairs() changes.
        uiOutput(ns("active_pairs_chips")),
        # Starts enabled -- active_pairs() has the default series in it on
        # load, unlike Add series' pickers which start blank. The server's
        # observe() on active_pairs() (see tab_module_server()) disables it
        # once the list is actually empty, same toggleDisabled mechanism
        # Add series uses.
        actionButton(ns("clear_pairs"), "Clear comparisons", icon = icon("trash-can"), class = "btn-sm"),
        p(
          class = "text-muted small",
          "Click × on a chip to remove it, or Clear comparisons to remove them all. ",
          "Charts stay readable up to about 6 series at once."
        ),
        if (kind == "table") {
          # Data tab: every control sits directly in the sidebar, always
          # visible -- no chevron disclosure (this is the one tab whose
          # sidebar is short enough not to need one). Download sits last,
          # after every control that shapes what it exports (Date
          # range/rebase/base year), same trailing spot it occupies inside
          # Compare's "More options". No "View values as" toggle -- level
          # vs. annual-growth only ever drove the *chart's* DisplayValue
          # (see transform_result()), which this tab's table export never
          # reads: build_export_df() always emits both the raw Value and
          # GrowthPct columns regardless.
          tagList(
            sliderInput(
              ns("year_range"), "Date range",
              min = min(init_df$Year), max = max(init_df$Year),
              value = c(min(init_df$Year), max(init_df$Year)),
              step = 1, sep = ""
            ),
            checkboxInput(ns("rebase_toggle"), "Set each series to 100 in a selected year", value = FALSE),
            conditionalPanel(
              "input.rebase_toggle == true", ns = ns,
              # A dropdown, not a slider, matching the Trends tab's Base
              # year picker -- a single specific year to jump to, not a
              # range to drag across.
              selectInput(
                ns("base_year"), "Base year",
                choices = sort(unique(init_df$Year)), selected = min(init_df$Year),
                selectize = FALSE
              )
            ),
            download_menu_ui(ns, include_png = FALSE, full_dataset = TRUE)
          )
        } else {
          # Compare (kind == "bar"): reuses the Trends/Rankings tabs'
          # .trend-more-options styling (chevron summary, no default
          # browser triangle) -- less-frequently-touched display controls
          # tucked away instead of always taking up sidebar space.
          tags$details(
            id = ns("more_options"), class = "trend-more-options",
            tags$summary("More options"),
            radioButtons(
              ns("chart_type"), "Chart type",
              choices = c("Trend line" = "line", "Bar chart" = "bar"),
              selected = "line", inline = TRUE
            ),
            radioButtons(
              ns("view_mode"), "View values as",
              choices = c("Level" = "level", "Annual percentage change" = "growth"),
              # Stacked rather than inline -- "Annual percentage change" is
              # too long to sit next to "Level" on one line in the sidebar.
              selected = "level", inline = FALSE
            ),
            sliderInput(
              ns("year_range"), "Date range",
              min = min(init_df$Year), max = max(init_df$Year),
              value = c(min(init_df$Year), max(init_df$Year)),
              step = 1, sep = ""
            ),
            conditionalPanel(
              "input.view_mode == 'level'", ns = ns,
              checkboxInput(ns("rebase_toggle"), "Set each series to 100 in a selected year", value = FALSE),
              conditionalPanel(
                "input.view_mode == 'level' && input.rebase_toggle == true", ns = ns,
                # A dropdown, not a slider, matching the Trends tab's Base
                # year picker -- a single specific year to jump to, not a
                # range to drag across.
                selectInput(
                  ns("base_year"), "Base year",
                  choices = sort(unique(init_df$Year)), selected = min(init_df$Year),
                  selectize = FALSE
                )
              )
            ),
            download_menu_ui(ns)
          )
        }
      ),
      main_panel,
      p(class = "text-muted small", "Source: Statistics Canada Table 36-10-0480-01"),
      uiOutput(ns("data_asof"))
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
    #
    # ignoreInit = TRUE -- see the matching comment on the Trends tab's own
    # sync observe(): ui() already built this session's initial widgets
    # (both Compare's and Data's -- this module runs once per kind) from the
    # same raw_data(), so this only needs to fire on a real data change, not
    # redundantly again for every new session.
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

      current_industry <- input$pair_industry
      new_industry <- if (is.null(current_industry) || !nzchar(current_industry) ||
                             !(current_industry %in% unique(df$Industry))) {
        ""
      } else {
        current_industry
      }
      updateIndustryTreeInput(session, "pair_industry", tree_data = industry_tree_nodes(df), selected = new_industry)

      year_min <- min(df$Year)
      year_max <- max(df$Year)
      current_range <- input$year_range
      range_value <- if (is.null(current_range)) {
        c(year_min, year_max)
      } else {
        c(max(current_range[1], year_min), min(current_range[2], year_max))
      }
      updateSliderInput(session, "year_range", min = year_min, max = year_max, value = range_value)

      # base_year is a selectInput (see tab_module_ui()) -- its choices are
      # numeric Years, but like every HTML <select> its reported value
      # comes back as a string, hence the as.numeric() round-trip before
      # comparing/reselecting against year_choices (a numeric vector).
      # Same pattern trend_tab_server() uses for its own Base year picker.
      year_choices <- sort(unique(df$Year))
      current_base <- suppressWarnings(as.numeric(input$base_year))
      new_base <- if (is.null(input$base_year) || is.na(current_base) || !(current_base %in% year_choices)) {
        min(year_choices)
      } else {
        current_base
      }
      updateSelectInput(session, "base_year", choices = year_choices, selected = new_base)
    }) |> bindEvent(raw_data(), once = FALSE, ignoreInit = TRUE)

    # The set of (Industry, Geography) pairs currently being compared --
    # replaces the old compare_mode-driven industries_multi/geos_multi/
    # geo_single/industry_single inputs with one free-form list that can mix
    # any industry with any geography.
    active_pairs <- reactiveVal(default_pair_row())

    # "Add series" only makes sense once both pickers hold a real value --
    # isTruthy() treats "" (pair_industry's "nothing selected" value) and
    # character(0)/NULL (pair_geography's) the same way req() elsewhere in
    # this module already does, so this and the req() inside the
    # add_pair observer below always agree on what counts as "ready".
    observe({
      ready <- isTruthy(input$pair_industry) && isTruthy(input$pair_geography)
      session$sendCustomMessage("toggleDisabled", list(id = session$ns("add_pair"), disabled = !ready))
    })

    observeEvent(input$add_pair, {
      req(input$pair_industry, input$pair_geography)
      key <- pair_key(input$pair_industry, input$pair_geography)
      current <- active_pairs()
      if (!(key %in% current$PairKey)) {
        # Same geography + a hierarchy relationship (parent or child, either
        # direction) means the two series aren't independent -- one already
        # includes the other's activity, so comparing their *levels* isn't
        # apples-to-apples for additive measures like jobs or compensation.
        # Informational only (not blocked): sometimes that comparison is
        # exactly the point (e.g. showing a subsector diverging from its
        # sector), so this just makes the overlap visible rather than
        # silently assuming it's a mistake.
        related <- current[
          current$Geography == input$pair_geography &
            mapply(
              function(i) industry_is_ancestor(i, input$pair_industry) || industry_is_ancestor(input$pair_industry, i),
              current$Industry
            ),
        ]
        if (nrow(related) > 0) {
          showNotification(
            paste0(
              "Heads up: \"", input$pair_industry, "\" overlaps with \"",
              paste(related$Industry, collapse = "\", \""), "\" in the industry hierarchy for ",
              input$pair_geography, " -- one contains the other, so comparing levels isn't ",
              "apples-to-apples for additive measures like jobs or compensation."
            ),
            type = "message"
          )
        }

        new_row <- data.frame(
          Industry = input$pair_industry, Geography = input$pair_geography,
          SeriesLabel = pair_label(input$pair_industry, input$pair_geography),
          PairKey = key, stringsAsFactors = FALSE
        )
        current <- rbind(current, new_row)
        active_pairs(current)
      }

      # Clear the pickers back to their empty/placeholder state after every
      # add -- so adding a series always ends with a clean search box ready
      # for the next pick, instead of leaving the just-added pick sitting
      # there looking like unconsumed input.
      updateSelectizeInput(session, "pair_geography", selected = character(0))
      updateIndustryTreeInput(session, "pair_industry", selected = "")
    })

    # Chip list for the active series -- reactive on active_pairs(), so
    # unlike the old checkboxGroupInput this needs no explicit "sync the
    # widget" push whenever active_pairs() changes.
    output$active_pairs_chips <- renderUI({
      pairs <- active_pairs()
      if (nrow(pairs) == 0) {
        return(p(class = "text-muted small", "No series selected."))
      }
      pal <- colors()
      tags$div(
        class = "pair-chip-list",
        lapply(seq_len(nrow(pairs)), function(i) {
          row <- pairs[i, ]
          tags$span(
            class = "pair-chip",
            tags$span(class = "pair-chip-swatch", style = paste0("background-color:", pal[[row$SeriesLabel]], ";")),
            tags$span(class = "pair-chip-label", row$SeriesLabel),
            tags$button(
              type = "button", class = "pair-chip-remove",
              `aria-label` = paste("Remove", row$SeriesLabel),
              onclick = sprintf(
                "Shiny.setInputValue('%s', %s, {priority: 'event'})",
                session$ns("remove_pair"), jsonlite::toJSON(row$PairKey)
              ),
              "×"
            )
          )
        })
      )
    })

    # Clicking a chip's × is how a pair gets removed now, replacing the old
    # "uncheck a box" mechanism.
    observeEvent(input$remove_pair, {
      current <- active_pairs()
      active_pairs(current[current$PairKey != input$remove_pair, ])
    })

    # "Clear comparisons" empties the list in one click instead of clicking
    # every chip's × individually -- current[0, ] keeps the same columns
    # (Industry/Geography/SeriesLabel/PairKey) with zero rows, which is
    # exactly what the chip renderUI's nrow() == 0 empty-state branch and
    # the downstream selected_series()/history_with_growth() req() gates
    # already expect from "nothing selected".
    observeEvent(input$clear_pairs, {
      active_pairs(active_pairs()[0, ])
    })

    # Mirrors the add_pair readiness toggle above -- "Clear comparisons"
    # only makes sense once there's something to clear.
    observe({
      session$sendCustomMessage(
        "toggleDisabled",
        list(id = session$ns("clear_pairs"), disabled = nrow(active_pairs()) == 0)
      )
    })

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
      # input$base_year comes back as a string from its selectInput (see
      # tab_module_ui()) -- coerce to numeric before comparing against the
      # numeric Year column, same as trend_tab_server()'s transform_result().
      base_year_num <- suppressWarnings(as.numeric(input$base_year))

      if (isTRUE(input$rebase_toggle) && !is.na(base_year_num)) {
        base_values <- df %>%
          filter(Year == base_year_num) %>%
          select(SeriesLabel, BaseValue = Value)
        df <- df %>% left_join(base_values, by = "SeriesLabel")
        df$RebasedValue <- safe_index_to_100(df$Value, df$BaseValue)
        # "Missing" covers both a series having no row at the base year at
        # all (BaseValue NA after the join) AND a base value of exactly 0
        # (rebasing to a 0 is undefined -- see safe_index_to_100()).
        missing <- df %>% filter(is.na(BaseValue) | BaseValue == 0) %>% pull(SeriesLabel) %>% unique()
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
    # Dash pattern / marker shape, indexed the same way as colors() -- the
    # colour-blind-safe secondary channel (see series_style_map()).
    styles <- reactive(series_style_map(active_pairs()$SeriesLabel))

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
        sty <- styles()
        series <- series_choices(df, "SeriesLabel")
        axis_title <- display_axis_title(input$view_mode, input$rebase_toggle, input$base_year, input$variable, variable_uom())
        fmt <- metric_format_spec(variable_uom(), input$view_mode, input$rebase_toggle)
        hover_spec <- paste0("%{y:", fmt$prefix, fmt$tickformat, "}", fmt$suffix)
        is_bar <- identical(input$chart_type, "bar")

        p <- plot_ly()
        for (s in series) {
          sd <- df %>% filter(SeriesLabel == s) %>% arrange(Year)
          if (nrow(sd) == 0) next
          # With hovermode "x unified" (below), Plotly shares one box across
          # every series at that X and keys each row with a colour/dash/
          # shape swatch drawn from the trace -- but *not* with the trace's
          # name as text, so the name still needs to be in the template
          # (confirmed empirically, not assumed). Value leads (bold), name
          # follows on its own line -- "the reader has the series [via the
          # swatch/legend] and wants the number" -- rather than the old
          # "name: value" ordering repeated as a single flat line.
          hovertemplate <- paste0("<b>", hover_spec, "</b><br>", s, "<extra></extra>")
          p <- if (is_bar) {
            p %>% add_trace(
              data = sd, x = ~Year, y = ~DisplayValue,
              type = "bar", name = s, legendgroup = s,
              marker = list(color = pal[[s]]),
              hovertemplate = hovertemplate
            )
          } else {
            p %>% add_trace(
              data = sd, x = ~Year, y = ~DisplayValue,
              type = "scatter", mode = "lines+markers", name = s, legendgroup = s,
              line = list(color = pal[[s]], width = 2, dash = sty$dash[[s]]),
              marker = list(color = pal[[s]], size = 8, symbol = sty$symbol[[s]]),
              hovertemplate = hovertemplate
            )
          }
        }

        # Direct end-of-line labels -- per the dataviz skill's rule these
        # *supplement* the legend (which always stays on for 2+ series),
        # they don't replace it, and only up to ~4 series before ends start
        # colliding. Line mode only (a bar chart's bars are already spatially
        # separated, nothing to label at "the end of"). Collision check is a
        # simplified numeric-proximity heuristic on the final data values,
        # not pixel-measured rendered positions (Shiny/R has no easy way to
        # read back actual rendered label geometry) -- any two series' last
        # points within 5% of the plotted y-range are treated as colliding,
        # and direct labels are skipped entirely for that render rather than
        # stacking illegibly; the legend + unified hover still carry identity.
        end_labels <- if (!is_bar && length(series) >= 2 && length(series) <= 4) {
          endpoints <- df %>%
            group_by(SeriesLabel) %>%
            filter(Year == max(Year)) %>%
            ungroup()
          y_range <- diff(range(df$DisplayValue, na.rm = TRUE))
          collide <- y_range > 0 && any(dist(endpoints$DisplayValue) < 0.05 * y_range)
          if (!collide) {
            lapply(seq_len(nrow(endpoints)), function(i) {
              row <- endpoints[i, ]
              list(
                x = row$Year, y = row$DisplayValue, xref = "x", yref = "y",
                text = paste0(" ", row$SeriesLabel), showarrow = FALSE,
                xanchor = "left", align = "left",
                font = list(color = pal[[row$SeriesLabel]], family = FONT_FAMILY, size = 11)
              )
            })
          }
        }

        p %>% layout(
          title = display_chart_title(input$variable, input$year_range),
          barmode = if (is_bar) "group" else NULL,
          xaxis = list(title = "Year", nticks = 8, tickformat = "d", gridcolor = GRIDLINE, color = INK_MUTED),
          yaxis = list(
            title = axis_title, gridcolor = GRIDLINE, color = INK_MUTED,
            tickformat = fmt$tickformat, tickprefix = fmt$prefix, ticksuffix = fmt$suffix
          ),
          paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
          font = list(color = INK_PRIMARY, family = FONT_FAMILY),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified",
          annotations = end_labels,
          shapes = if (identical(input$view_mode, "growth")) {
            list(list(
              type = "line", xref = "paper", x0 = 0, x1 = 1,
              yref = "y", y0 = 0, y1 = 0,
              line = list(color = INK_MUTED, width = 1, dash = "dot")
            ))
          }
        )
      })
    }

    output$variable_definition <- renderUI({
      def <- VARIABLE_DEFINITIONS[[input$variable]]
      if (is.null(def) || !nzchar(def)) return(NULL)
      p(class = "text-muted small", strong(paste0(input$variable, ": ")), def)
    })

    # raw_data() is the dependency, not the value used -- reading it just
    # ties this to the same reactiveFileReader invalidation as this tab's
    # own data, so the "as of" date updates the moment a new pipeline run
    # lands, without this needing its own poll loop.
    output$data_asof <- renderUI({
      raw_data()
      data_asof_ui()
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

    # Data-only (kind == "table", see the matching `if` in tab_module_ui()):
    # the entire underlying dataset -- every Variable/Geography/Industry/Year
    # combination, not just the currently selected series/variable/time
    # frame -- so this deliberately reads raw_data() directly rather than
    # any of the scoped_raw()/filtered_data() reactives the rest of this
    # module builds off of. No GrowthPct/RebasedValue columns here: those
    # are relative to *this tab's* current view-mode/rebase settings, which
    # don't have a single well-defined meaning across the whole dataset.
    if (kind == "table") {
      output$download_full <- downloadHandler(
        filename = function() {
          sprintf("productivity_full-dataset_%s.csv", format(Sys.Date(), "%Y%m%d"))
        },
        content = function(file) {
          out <- raw_data() %>%
            arrange(Variable, Geography, Industry, Year) %>%
            select(Year, Geography, Industry, Variable, Value, UOM)
          write.csv(out, file, row.names = FALSE)
        }
      )
    }
  })
}

ui <- function(request) {
  # ui() runs per HTTP
  # request, before any session/reactive context exists, so it can't read
  # a reactive value. Computed directly from the data at page-build time
  # (not via a server-side update*Input call) so the default selection is
  # already present in the first HTML the browser receives.
  # cached_load_lp_data(), not load_lp_data() -- see LP_DATA_CACHE -- so a
  # repeat page view doesn't pay the ~1.6s parse cost again; only the first
  # request after the file actually changes does.
  init_df <- cached_load_lp_data()
  # Computed once and threaded through to the 4 tab-UI builders below,
  # instead of each of them independently recomputing the same
  # series_choices()/industry_tree_nodes() result from the same init_df --
  # this collapses what was 6 series_choices() calls + 3 industry_tree_nodes()
  # calls (the latter each rebuilding + re-JSON-serializing a 139-node tree)
  # per page load down to 2 and 1 respectively.
  variable_choices <- series_choices(init_df, "Variable", VARIABLE_ORDER)
  geography_choices <- series_choices(init_df, "Geography", GEOGRAPHY_ORDER)
  industry_tree <- industry_tree_nodes(init_df)

  page_fillable(
    title = "Canadian Productivity Dashboard", # browser tab title only -- no on-page heading
    # Roboto -- the exact font csls.ca loads (see CSLS-Shiny-Style-Spec.md
    # section 3) -- at the 4 weights the theme actually uses (300/400/600/
    # 800). FONT_FAMILY's own fallback stack (Arial, sans-serif) covers the
    # case this request is blocked, so nothing here depends on it loading.
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;600;800&display=swap"
      )
    ),
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
       /* .card's own radius/padding/background come from csls-shiny-theme.css
          (section 8: 16px radius, 40px padding, white) -- no local override
          needed here now that that stylesheet is loaded (see tags$head()
          above and the trailing tags$link() below). */
       .nav-pills { margin-bottom: 1rem; }
       .nav-pills .nav-link {
         border: .5px solid rgba(0,0,0,.24); border-radius: 999px; margin: 0 0.5rem;
         color: %s; background-color: %s; font-weight: 400;
         transition: background-color 280ms ease, color 280ms ease, border-color 280ms ease;
       }
       /* csls.ca's interaction rule: every hover/focus state turns jets
          blue (see CSLS-Shiny-Style-Spec.md section 2) -- applied here to
          both the resting-tab hover and the persistent active tab, so the
          justified pill nav reads as the same control family as the rest
          of the page's links/buttons instead of keeping its own maple-only
          palette. */
       .nav-pills .nav-link:hover, .nav-pills .nav-link:focus-visible {
         border-color: %s; color: %s; background-color: rgba(5,152,216,.1); outline: 0;
       }
       .nav-pills .nav-link.active, .nav-pills .nav-link.active:hover {
         background-color: %s; color: %s; font-weight: 600; border-color: %s;
       }
       .trend-more-options > summary { cursor: pointer; list-style: none; display: flex; align-items: center;
         gap: 0.35rem; color: %s; font-size: 0.9rem; margin-bottom: 0.5rem; transition: color 280ms ease; }
       .trend-more-options > summary:hover { color: %s; }
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
      BRAND_MAPLE_BLUE, CHART_SURFACE,
      BRAND_JETS_BLUE, BRAND_JETS_BLUE,
      BRAND_JETS_BLUE, CHART_SURFACE, BRAND_JETS_BLUE,
      BRAND_MAPLE_BLUE,
      BRAND_JETS_BLUE
    ))),
    # A second, separate tags$style() -- sprintf() (used above and below for
    # the color-token substitutions) refuses to run on a format string past
    # 8192 characters, which one single CSS block spanning the whole page
    # eventually hit. This block has no %s color tokens of its own, so it's
    # plain HTML(), not sprintf(), and splitting it out here also keeps
    # each remaining sprintf() call comfortably under that limit.
    tags$style(HTML(
      "/* page_fillable() pins the whole page to viewport height with no
          page-level scrolling (by design, so Trends/Rankings/Compare's
          Plotly charts fill exactly one screen) -- fine for a chart sized
          to fill 100% of its card, but a DT table's true height (rows plus
          its own Showing-X-of-Y/pagination footer) is content-driven and
          routinely exceeds that fixed budget. Without this, anything past
          the budget was silently clipped, which is what made the Source/
          Data-last-refreshed lines after the table disappear or overlap
          DT's own footer instead of just being scrolled to. Scoped to just
          the Data tab's card (see class = if (kind == table) ... in
          tab_module_ui()) -- Trends/Rankings/Compare keep the plain
          clipped-to-viewport behavior, which is what makes their charts
          fill height instead of pushing the page taller. */
       .tab-pane.active > .card.table-tab-card { overflow-y: auto; }
       /* Every tab's Download dropdown (see download_menu_ui()) -- full
          width so the toggle button lines up with the other sidebar
          controls above it instead of sizing to its own label. */
       .download-dropdown { width: 100%; }
       .download-dropdown .dropdown-menu { width: 100%; }
       /* Bootstrap's .dropdown-item defaults to white-space: nowrap, sized
          for a normal wide dropdown -- fine there, but this menu is pinned
          to the sidebar's own (narrow) width above, so a label like
          'Download displayed data (.csv)' would run past the sidebar edge
          and get clipped instead of wrapping, the way every other sidebar
          control's label already does. */
       .download-dropdown .dropdown-item { white-space: normal; }"
    )),
    tags$style(HTML(sprintf(
      "/* Collapsible Industry tree dropdown (see www/industry_tree.js) --
          toggle styled to csls.ca's own form-control spec (42px tall, 8px
          radius, 14px/weight-300 text, rgba(0,0,0,.24) hairline border,
          jets blue on hover/focus -- CSLS-Shiny-Style-Spec.md section 4) so
          it reads as the same control family as the Geography selectize
          box next to it, not a separately hand-matched approximation of
          it. Chevron reuses .trend-more-options' rotate-on-open technique
          above. */
       .industry-tree { position: relative; width: 100%%; }
       .industry-tree-toggle {
         width: 100%%; height: 42px; min-height: 42px; display: flex; align-items: center;
         text-align: left; padding: 10px 36px 10px 16px;
         border: .5px solid rgba(0,0,0,.24); border-radius: 8px;
         background-color: %s; color: %s; position: relative;
         font-size: 14px; font-weight: 300; line-height: 1.4;
         transition: border-color 280ms ease;
       }
       .industry-tree-toggle:hover, .industry-tree-toggle:focus-visible,
       .industry-tree-toggle[aria-expanded=\"true\"] { border-color: %s; outline: 0; }
       /* Placeholder text keys off the same muted token the site's own
          .form-control::placeholder rule uses, rather than a hand-matched
          native-input grey -- this is a plain <span>, not a real
          placeholder attribute, so it doesn't get that colour for free the
          way an <input> would. */
       .industry-tree-toggle-label.industry-tree-empty { color: %s; }
       .industry-tree-toggle::after {
         content: '\\25B8'; position: absolute; right: 16px; top: 50%%;
         transform: translateY(-50%%) rotate(90deg); transition: transform 0.15s ease;
       }
       .industry-tree-toggle[aria-expanded=\"true\"]::after { transform: translateY(-50%%) rotate(-90deg); }
       .industry-tree-panel {
         position: absolute; z-index: 20; top: calc(100%% + 0.25rem); left: 0; width: 100%%;
         max-height: 320px; overflow-y: auto; background: %s; border: 0;
         border-radius: 8px; box-shadow: 0 14px 36px rgba(0,0,0,.16);
       }
       .industry-tree-search {
         position: sticky; top: 0; width: 100%%; padding: 10px 16px; box-sizing: border-box;
         border: none; border-bottom: .5px solid %s; background: %s;
         font-size: 14px; font-weight: 300;
       }
       .industry-tree-search:focus { outline: 0; border-bottom-color: %s; }
       .industry-tree-list, .industry-tree-children { list-style: none; margin: 0; padding: 0; }
       .industry-tree-row { display: flex; align-items: center; gap: 0.35rem; padding: 0.15rem 0.5rem; }
       .industry-tree-arrow { cursor: pointer; transition: transform 0.15s ease; color: %s; width: 1em; text-align: center; }
       .industry-tree-row.expanded > .industry-tree-arrow { transform: rotate(90deg); }
       .industry-tree-row.no-children > .industry-tree-arrow { visibility: hidden; }
       .industry-tree-label {
         cursor: pointer; flex: 1 1 auto; padding: 10px 16px; border-radius: 4px;
         font-size: 14px; font-weight: 300; transition: background-color 280ms ease, color 280ms ease;
       }
       /* Matches csls.ca's own dropdown-option treatment exactly (see
          .selectize-dropdown .active / .option:hover in
          csls-shiny-theme.css): a jets-blue tint background with maple-blue
          text, not a solid fill -- for both the row being hovered and the
          one currently selected. */
       .industry-tree-label:hover, .industry-tree-label:focus-visible,
       .industry-tree-row.selected > .industry-tree-label { background: rgba(5,152,216,.1); color: %s; }
       .industry-tree-node[hidden] { display: none; }
       /* Removable chip list for the Compare/Data 'Series to compare' list
          -- styled like csls.ca's own selectize multi-value chips
          (.selectize-control.multi .selectize-input > .item in
          csls-shiny-theme.css: tint background, maple text, 4px radius)
          rather than echoing the pill-shaped nav/button radius. */
       .pair-chip-list { display: flex; flex-wrap: wrap; gap: 0.4rem; margin: 0.5rem 0; }
       .pair-chip {
         display: inline-flex; align-items: center; gap: 0.35rem; padding: 2px 8px 2px 10px;
         border: 0; border-radius: 4px;
         background: rgba(5,152,216,.1); color: %s; font-size: 13px; font-weight: 400;
       }
       .pair-chip-swatch { width: 0.6rem; height: 0.6rem; border-radius: 50%%; flex-shrink: 0; }
       .pair-chip-remove {
         border: none; background: transparent; color: %s; border-radius: 50%%;
         width: 1.1rem; height: 1.1rem; line-height: 1; cursor: pointer; padding: 0;
         transition: background-color 280ms ease, color 280ms ease;
       }
       .pair-chip-remove:hover { background: rgba(5,152,216,.1); color: %s; }",
      CHART_SURFACE, INK_PRIMARY,
      BRAND_JETS_BLUE,
      INK_MUTED,
      CHART_SURFACE,
      GRIDLINE, CHART_SURFACE,
      BRAND_JETS_BLUE,
      INK_MUTED,
      BRAND_MAPLE_BLUE,
      BRAND_MAPLE_BLUE,
      INK_MUTED,
      BRAND_JETS_BLUE
    ))),
    tags$script(src = versioned_asset("industry_tree.js")),
    tags$script(src = versioned_asset("ui_helpers.js")),
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
        nav_panel("Trends", trend_tab_ui("trend", init_df, variable_choices, geography_choices, industry_tree)),
        nav_panel("Rankings", ranking_tab_ui("ranking", init_df, variable_choices, geography_choices)),
        nav_panel("Compare", tab_module_ui("bar", init_df, "bar", variable_choices, industry_tree)),
        nav_panel("Data", tab_module_ui("table", init_df, "table", variable_choices, industry_tree))
      )
    )$find("ul.nav")$addClass("nav-justified")$allTags(),
    # csls-shiny-theme.css (www/) is the drop-in stylesheet that makes every
    # Bootstrap control -- inputs, selects, checkboxes/radios, buttons, the
    # DT table, the ionRangeSlider year picker -- match csls.ca's own
    # metrics (42px/8px-radius controls, 24px checkboxes, pill buttons,
    # etc.) instead of Bootstrap's defaults. Placed as the LAST tag in the
    # whole page rather than up in tags$head() with the Google Fonts link
    # above: a <link rel="stylesheet"> works from anywhere in the document,
    # and putting it last in DOM order is what actually guarantees it wins
    # the cascade over bslib's own Bootstrap bundle for same-specificity
    # selectors (bslib injects that bundle's <link> into <head> itself, at a
    # point in the render pipeline this file doesn't control) -- no
    # !important needed anywhere in that stylesheet as a result. Loaded via
    # versioned_asset() like every other www/ asset here, so an edit to it
    # takes effect on reload instead of serving a stale cached copy.
    tags$link(rel = "stylesheet", type = "text/css", href = versioned_asset("csls-shiny-theme.css"))
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
