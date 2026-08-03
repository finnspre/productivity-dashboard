#install packages
library("cansim")
library("tidyr")
library("dplyr")

#finds the folder this script lives in, whether run via Rscript,
#source(), or RStudio's Source button -- so saved output always lands
#next to the script regardless of the current working directory
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
  stop("Could not determine script location. Run this with Rscript or source(), or setwd() to this script's folder first.")
}

#get labour productivity data for aggregate business sector
aggregate_business_lp <- get_cansim("36-10-0206-01")

#filter data to include only rows with labour productivity and after 1996
aggregate_business_lp <- aggregate_business_lp %>%
  filter(`Labour productivity measures and related measures` == "Labour productivity" &
           substr(aggregate_business_lp$REF_DATE, 1, 4) >= 1997)

#annualize quarterly data by averaging every quarter of the year
aggregate_business_lp <- aggregate_business_lp %>%
  mutate(Year = substr(aggregate_business_lp$REF_DATE, 1, 4)) %>%
  group_by(Year) %>%
  summarize(Value = mean(VALUE, na.rm = TRUE))

#add industry classification row 
aggregate_business_lp <- aggregate_business_lp %>%
  mutate(`North American Industry Classification System (NAICS)` = "Aggregate Business Sector")


#get labour productivity industry for each NAICS industry
industry_lp <- get_cansim("36-10-0207-01")

#filter data to include only rows with labour productivity 
industry_lp <- industry_lp %>%
  filter(`Labour productivity measures and related variables` == "Labour productivity")

#drop non-business activity, total economic activity rows
industry_lp <- industry_lp %>%
  filter(`North American Industry Classification System (NAICS)` != "Total economy" &
           `North American Industry Classification System (NAICS)` != "Non-business sector" &
           substr(industry_lp$REF_DATE, 1, 4) >= 1997)

#annualize quarterly data by averaging every quarter of the year
industry_lp <- industry_lp %>%
  mutate(Year = substr(industry_lp$Date, 1, 4)) %>%
  group_by(`North American Industry Classification System (NAICS)`, Year) %>%
  summarize(Value = mean(VALUE, na.rm = TRUE), .groups = "drop")

#join aggregate business sector df with industry specific df 
lp_data <- bind_rows(aggregate_business_lp, industry_lp)

#save the labour productivity data frame next to this script
save(lp_data, file = file.path(script_dir(), "lp_data.RData"))




