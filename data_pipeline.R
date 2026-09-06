#install packages
library("cansim")
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

# STATCAN_TABLE_ID / RAW_STATCAN_CONTRACT / LP_DATA_CONTRACT / validate_data_contract()
# -- shared with app.R so both agree on exactly one definition of "what does
# this table's data look like" (see that file's own header comment).
# local = TRUE so these land in this script's own execution environment
# rather than unconditionally in .GlobalEnv (source()'s own default) -- see
# the matching comment on app.R's identical source() call for why that
# distinction matters even though, for a plain `Rscript data_pipeline.R` run
# specifically, the two happen to be the same environment.
source(file.path(script_dir(), "data_contract.R"), local = TRUE)

#retrieve data from statscan table 0480
lp_data <- get_cansim(STATCAN_TABLE_ID)

# Fail loudly, immediately, if StatCan has renamed/dropped a column this
# script depends on below -- before the rename()/mutate() chain gets a
# chance to fail with a more cryptic "object not found" partway through, or
# (worse) silently produce a wrong-shaped result that only surfaces as a
# strange chart in the app much later.
validate_data_contract(lp_data, RAW_STATCAN_CONTRACT, paste0("raw get_cansim(\"", STATCAN_TABLE_ID, "\") pull"))

# Hierarchy for Industry is a dot-path of ancestor IDs (e.g. "1.323.2.3.4"), so its
# number of segments is the depth (1 = whole-economy total, 5 = 2-digit, 6 = 3-digit)
# Depth 3 holds 11 aggregates, but only "Business sector industries" and "Non-business sector industries"
# are parents of other 2-digit and 3-digit sectors so drop all other aggregates
# Also drop any sectors more than 3-digits
#
# Stored as a real column -- not a bare vector kept "in lockstep" with
# lp_data's row order by convention -- so that subsetting lp_data below can
# never desync it from the rows it actually describes, however lp_data ends
# up filtered/reordered in the future.
lp_data$IndustryDepth <- lengths(strsplit(as.character(lp_data$`Hierarchy for Industry`), "[.]"))
keep_industry <- lp_data$IndustryDepth == 1 |
  lp_data$Industry %in% c("Business sector industries", "Non-business sector industries") |
  lp_data$IndustryDepth %in% c(5, 6)

# Drop rows with "Northwest Territories including Nunavut"
keep_geo <- lp_data$GEO != "Northwest Territories including Nunavut"

# Row-subset lp_data (IndustryDepth travels with it automatically, being a
# real column now) to keep only desired industries/depths + geographies.
lp_data <- lp_data[keep_industry & keep_geo, ]

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
      IndustryDepth == 5, "2-digit",
      ifelse(IndustryDepth == 6, "3-digit", "Aggregate")
    )
  ) %>%
  select(Year, Geography, Variable, Industry, IndustryLevel, Value, UOM)

# Fail loudly, before save(), if the cleaning above produced anything other
# than exactly the shape app.R's load_lp_data() is entitled to assume -- see
# LP_DATA_CONTRACT's own comment in data_contract.R. This is what stops a
# pipeline bug from silently overwriting a good lp_data.RData with a bad
# one: save() below never runs unless this passes.
validate_data_contract(lp_data, LP_DATA_CONTRACT, "data_pipeline.R output (pre-save)")

#save the labour productivity data frame next to this script
save(lp_data, file = file.path(script_dir(), "lp_data.RData"))
