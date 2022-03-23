shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(plyr))
shhh(library(leaflet))
shhh(library(sp))
shhh(library(data.table))
shhh(library(RColorBrewer))
shhh(library(pander))
shhh(library(dplyr))
shhh(library(tidyr))
shhh(library(shinycssloaders))
shhh(library(plotly))
shhh(library(DT))
shhh(library(ggalt))
shhh(library(magrittr))
shhh(library(scales))
shhh(library(openxlsx))
shhh(library(sf))
shhh(library(shiny))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(networkD3))
shhh(library(reactable))
shhh(library(knitr))
shhh(library(Hmisc))
shhh(library(matrixStats))
shhh(library(sourcetools))
shhh(library(shinyBS))
shhh(library(readr))
shhh(library(metathis))
shhh(library(DescTools))
shhh(library(networkD3))
shhh(library(styler))

# tidy_code_function -------------------------------------------------------------------------------

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("R scripts")
  message("----------------------------------------")
  r_scripts <- eval(styler::style_dir("R/")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, r_scripts, test_scripts)
  return(script_changes)
}


read_cohort <- function(cohortfile='data/pg_sankey_data_1_3_yag_dummy.csv'){
  cohort <- read.csv(cohortfile)
  cohort <- subset(cohort, select = -X)
  cohort$SECTIONNAME.x <- StrCap(tolower(cohort$SECTIONNAME.x))
  cohort$SECTIONNAME.y <- StrCap(tolower(cohort$SECTIONNAME.y))
  return(cohort)
}

read_tables_data <- function(file){
  tables_data <- fread(file) %>% select(-V1)
  names(tables_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", 
                          "ethnicity", "current_region", "FSM", "prior_attainment",
                          "count", "earnings_median", "threshold", "qualification_TR", 
                          "group_name")
  tables_data$SECTIONNAME[tables_data$group_name == "Radio broadcasting"] <- "INFORMATION AND COMMUNICATION"
  tables_data$SECTIONNAME[tables_data$group_name == "Reproduction of recorded media"] <- "MANUFACTURING"
  tables_data$SECTIONNAME <- StrCap(tolower(tables_data$SECTIONNAME))
  return(tables_data)
}
