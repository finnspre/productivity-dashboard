# ---------------------------------------------------------------------------
# Shared data-layer config + validation, sourced by both data_pipeline.R
# (the raw StatCan ingestion script) and app.R (the Shiny app) so the two
# processes can never drift onto two different ideas of "what does this
# table's data look like". Deliberately generic -- validate_data_contract()
# takes its expected shape as a plain `contract` argument, nothing
# LP-specific is baked into the function itself -- so this file, not just
# its pattern, is meant to be copied wholesale as the starting point for a
# future MFP (multifactor productivity) dashboard forked from this one: only
# the constants below (table ID, column list, enum values) would need to
# change, not validate_data_contract() itself.
# ---------------------------------------------------------------------------

# The one StatCan table this dashboard pulls from -- the single place this
# ID is defined. data_pipeline.R's get_cansim() call and every "Source:
# Statistics Canada Table ..." caption in app.R read this constant instead
# of each retyping the literal. To point the whole app at a new
# vintage/table number (e.g. StatCan retiring 36-10-0480-01 in favour of a
# successor table), change it here -- as long as the new table produces the
# same raw columns and the same IndustryLevel/UOM conventions as today's,
# nothing else needs to change.
#
# Not handled: pinning a specific historical *vintage* of the table (rather
# than always pulling whatever StatCan currently has live). cansim's
# get_cansim() doesn't offer a straightforward way to fetch a past vintage
# of a whole table (unlike its per-vector lookups), so this is left as a
# known limitation rather than worked around here.
STATCAN_TABLE_ID <- "36-10-0480-01"

# Raw columns data_pipeline.R's cleaning step (rename()/mutate()/the
# Hierarchy-depth logic) depends on existing, by their exact StatCan-assigned
# names, checked immediately after get_cansim() returns and before any
# renaming happens. Catches a StatCan schema change (a column renamed,
# dropped, or the classification-variable column given a different label)
# with one clear message naming the missing column, instead of a cryptic
# "object not found" / "can't rename a column that doesn't exist" error
# somewhere in the middle of the dplyr chain that follows. "any" -- not
# skipped entirely -- for every column here: presence is what this layer
# can meaningfully promise (types are cansim's own raw-pull types to define,
# not this pipeline's), but every one of these names still needs to exist
# for the checks below to even be able to say so.
RAW_STATCAN_CONTRACT <- list(
  columns = c(
    REF_DATE = "any", GEO = "any", "Labour productivity and related measures" = "any",
    Industry = "any", "Hierarchy for Industry" = "any", VALUE = "any", UOM = "any"
  )
)

# The shape data_pipeline.R promises to hand off in lp_data.RData, and the
# only shape app.R is ever allowed to assume when it reads that file back in
# (see load_lp_data()'s own contract note). A future table swap (a new
# vintage, or a successor table number in STATCAN_TABLE_ID above) needs
# nothing else to change here as long as it still produces exactly this:
# seven columns of these types, Year/Geography/Variable/Industry/
# IndustryLevel never NA, one row per (Year, Geography, Variable, Industry),
# and IndustryLevel restricted to these 3 values.
#
# Geography/Variable are deliberately NOT restricted to a fixed enum here --
# GEOGRAPHY_ORDER/VARIABLE_ORDER in app.R already tolerate a new value
# appearing (it's just appended after the preferred ones in every picker) or
# an old one disappearing, so a StatCan addition/removal to either is meant
# to flow through, not fail the whole app. Industry is similarly open-ended:
# a name not yet in app.R's INDUSTRY_PARENT lookup already soft-fails --
# shows up unindented in the picker, with a loud warning() -- by deliberate,
# existing design (see load_lp_data()); this contract doesn't second-guess
# that by hard-failing on an unrecognized Industry name.
LP_DATA_CONTRACT <- list(
  columns = c(
    Year = "integer", Geography = "character", Variable = "character",
    Industry = "character", IndustryLevel = "character",
    Value = "numeric", UOM = "character"
  ),
  key_columns = c("Year", "Geography", "Variable", "Industry", "IndustryLevel"),
  unique_key = c("Year", "Geography", "Variable", "Industry"),
  enum_columns = list(IndustryLevel = c("Aggregate", "2-digit", "3-digit")),
  # Recomputed at source() time, not a hardcoded literal -- "current year"
  # shouldn't need an annual edit here just to stay accurate.
  year_bounds = c(1900L, as.integer(format(Sys.Date(), "%Y")) + 1L)
)

# Generic contract check -- shared by data_pipeline.R (checks its own output
# before ever writing lp_data.RData, so a pipeline bug can't silently
# overwrite a good cached file with a bad one) and app.R's load_lp_data()
# (checks the file again on every read, since the RData file itself -- not
# just the pipeline run that produced it -- is a real process boundary that
# can independently drift or corrupt). Nothing LP-specific: every check
# below reads its expectations from `contract`, so this same function is
# meant to validate a future MFP `contract` object unchanged.
#
# Collects every problem found rather than stopping at the first, so a
# schema-drift investigation sees the whole picture in one run instead of
# fixing one mismatch only to hit the next on the next attempt. Always
# throws (never warns) when `problems` is non-empty -- every caller relies
# on that: data_pipeline.R lets it halt the script before save(), and
# app.R's safe_load_lp_data() already tryCatch()es load_lp_data() into a
# NULL sentinel for exactly this kind of thrown error (see its own comment).
# There's no separate "validation failed" UI state to build because a
# validation failure is deliberately indistinguishable, from the rest of the
# app's point of view, from any other failed load.
validate_data_contract <- function(df, contract, context) {
  if (!is.data.frame(df)) {
    stop(context, ": expected a data frame, got ", class(df)[1], call. = FALSE)
  }

  problems <- character(0)
  add_problem <- function(...) problems[length(problems) + 1] <<- paste0(...)
  present <- function(col) col %in% names(df)

  expected_cols <- names(contract$columns)
  missing_cols <- setdiff(expected_cols, names(df))
  if (length(missing_cols) > 0) {
    add_problem("missing column(s): ", paste(missing_cols, collapse = ", "))
  }

  # Type/NA/uniqueness/enum/range checks all need the column they check to
  # actually be present -- skip a column already flagged as missing above
  # rather than erroring a second, confusing time about the same thing.
  for (col in expected_cols) {
    if (!present(col)) next
    want_type <- contract$columns[[col]]
    ok <- switch(want_type,
      integer = is.numeric(df[[col]]),
      numeric = is.numeric(df[[col]]),
      character = is.character(df[[col]]),
      any = TRUE,
      TRUE # an unrecognized declared type is a contract-authoring mistake, not a data problem -- don't fail data over it
    )
    if (!isTRUE(ok)) {
      add_problem("column '", col, "' should be ", want_type, ", got ", class(df[[col]])[1])
    }
  }

  if (nrow(df) == 0) {
    add_problem("0 rows -- an empty pull/read is treated as invalid, not a valid-but-boring result")
  }

  for (col in intersect(contract$key_columns, names(df))) {
    n_na <- sum(is.na(df[[col]]))
    if (n_na > 0) {
      add_problem("column '", col, "' has ", n_na, " NA value(s) -- this is a key column, never expected to be NA")
    }
  }

  uk <- contract$unique_key
  if (!is.null(uk) && all(uk %in% names(df)) && nrow(df) > 0) {
    n_dupe <- sum(duplicated(df[uk]))
    if (n_dupe > 0) {
      add_problem(
        "found ", n_dupe, " duplicate row(s) on the (", paste(uk, collapse = ", "),
        ") key -- expected exactly one row per combination"
      )
    }
  }

  for (col in names(contract$enum_columns)) {
    if (!present(col)) next
    allowed <- contract$enum_columns[[col]]
    bad <- setdiff(unique(df[[col]]), allowed)
    if (length(bad) > 0) {
      add_problem(
        "column '", col, "' has value(s) outside its expected set (", paste(allowed, collapse = ", "),
        "): ", paste(bad, collapse = ", ")
      )
    }
  }

  if (!is.null(contract$year_bounds) && present("Year") && nrow(df) > 0) {
    yr <- suppressWarnings(as.integer(df$Year))
    out_of_range <- is.na(yr) | yr < contract$year_bounds[1] | yr > contract$year_bounds[2]
    if (any(out_of_range)) {
      add_problem(
        "column 'Year' has value(s) outside the plausible range ",
        contract$year_bounds[1], "-", contract$year_bounds[2], ": ",
        paste(sort(unique(df$Year[out_of_range])), collapse = ", ")
      )
    }
  }

  if (length(problems) > 0) {
    stop(
      context, ": data does not match the expected contract --\n  - ",
      paste(problems, collapse = "\n  - "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
