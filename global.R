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
shhh(library(shinyGovstyle))
shhh(library(shinyjs))
shhh(library(shinya11y))

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

source("R/read_data.R")
cohort1 <- read_cohort("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/19 - Spring 2022/Industry dashboard/CSVs/sankey data 1-3 YAG_FD_PG.csv")
cohort2 <- read_cohort("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/19 - Spring 2022/Industry dashboard/CSVs/sankey data 3-5 YAG_FD_PG.csv")
cohort3 <- read_cohort("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/19 - Spring 2022/Industry dashboard/CSVs/sankey data 1-5 YAG_FD_PG.csv")

tables_data <- read_tables_data("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/19 - Spring 2022/Industry dashboard/CSVs/tables_data_3digit_FD_PG.csv")

qual_subjects <- tables_data %>%
  select(qualification_TR, subject_name) %>%
  distinct()

industry_groups <- tables_data %>%
  select(SECTIONNAME, group_name) %>%
  distinct()
