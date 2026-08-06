#install packages
library("cansim")
library("tidyr")
library("dplyr")

#define function that returns folder location of current script 
script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(f) f$ofile))
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]])))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    doc_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(doc_path)) {
      return(dirname(normalizePath(doc_path)))
    }
  }
  stop("Could not determine script location. Run this with Rscript or source(), or setwd() to this script's folder first.")
}

#retrieve data from statscan table 0480 
lp_data <- get_cansim("36-10-0480-01")

# Hierarchy for Industry is a dot-path of ancestor IDs (e.g. "1.323.2.3.4"), so its
# number of segments is the depth (1 = whole-economy total, 5 = 2-digit, 6 = 3-digit)
# Depth 3 holds 11 aggregates, but only "Business sector industries" and "Non-business sector industries"
# are parents of other 2-digit and 3-digit sectors so drop all other aggregates 
# Also drop any sectors more than 3-digits
industry_depth <- lengths(strsplit(as.character(lp_data$`Hierarchy for Industry`), "[.]"))
keep_industry <- industry_depth == 1 |
  lp_data$Industry %in% c("Business sector industries", "Non-business sector industries") |
  industry_depth %in% c(5, 6)

# Drop rows with "Northwest Territories including Nunavut"
keep_geo <- lp_data$GEO != "Northwest Territories including Nunavut"

# Row-subset lp_data (and industry_depth, kept in lockstep) using the
# logical vectors above, to keep only desired industries/depths + geographies
lp_data <- lp_data[keep_industry & keep_geo, ]
industry_depth <- industry_depth[keep_industry & keep_geo]

# Shape data frame to be more readable for app.R
lp_data <- lp_data %>%
  rename(Geography = GEO, Variable = `Labour productivity and related measures`) %>%
  mutate(
    Year = as.integer(REF_DATE),
    Value = as.numeric(VALUE),
    Geography = as.character(Geography),
    Variable = as.character(Variable),
    # A handful of 3-digit industries come back from Statistics Canada named
    # "X ==> Y" (e.g. "Ambulatory health care services ==> Non-profit
    # institutions") -- Y there is just the true parent under this table's
    # non-commercial-activity reclassification, which app.R's
    # INDUSTRY_PARENT already encodes for the plain name. Strip the
    # "==> ..." suffix so the plain name is what's stored/displayed.
    Industry = sub("\\s*==>.*$", "", as.character(Industry)),
    IndustryLevel = ifelse(
      industry_depth == 5, "2-digit",
      ifelse(industry_depth == 6, "3-digit", "Aggregate")
    )
  ) %>%
  select(Year, Geography, Variable, Industry, IndustryLevel, Value, UOM)

#save the labour productivity data frame next to this script
save(lp_data, file = file.path(script_dir(), "lp_data.RData"))




