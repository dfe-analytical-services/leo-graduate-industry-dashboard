# Filename: read_data.R
# Created: 23/03/22
# Description: Functions for reading in and outputing data from/to files.

# Read in a cohort file.
read_cohort <- function(cohortfile = "data/pg_sankey_data_1_3_yag_dummy.csv") {
  cohort <- read.csv(cohortfile)
  cohort <- subset(cohort, select = -X)
  cohort$SECTIONNAME.x <- StrCap(tolower(cohort$SECTIONNAME.x))
  cohort$SECTIONNAME.y <- StrCap(tolower(cohort$SECTIONNAME.y))
  return(cohort)
}

# Read in tables data.
read_tables_data <- function(file) {
  tables_data <- fread(file)
  names(tables_data) <- c(
    "X", "YAG", "subject_name", "SECTIONNAME", "sex",
    "ethnicity", "current_region", "FSM", "prior_attainment",
    "count", "earnings_median", "threshold", "qualification_TR",
    "group_name"
  )
  tables_data$SECTIONNAME[tables_data$group_name == "Radio broadcasting"] <- "INFORMATION AND COMMUNICATION"
  tables_data$SECTIONNAME[tables_data$group_name == "Reproduction of recorded media"] <- "MANUFACTURING"
  tables_data$SECTIONNAME[is.na(tables_data$SECTIONNAME) == TRUE] <- "NOT KNOWN"
  tables_data$SECTIONNAME <- StrCap(tolower(tables_data$SECTIONNAME))
  return(tables_data)
}
