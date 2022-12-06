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
# shhh(library(shinya11y))

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
cohort1 <- read_cohort("data/sankey data 1-3 YAG_FD_PG.csv")
cohort2 <- read_cohort("data/sankey data 3-5 YAG_FD_PG.csv")
cohort3 <- read_cohort("data/sankey data 1-5 YAG_FD_PG.csv")

tables_data <- read_tables_data("data/tables_data_3digit_FD_PG.csv")

qual_subjects <- tables_data %>%
  select(qualification_TR, subject_name) %>%
  distinct()

industry_groups <- tables_data %>%
  select(SECTIONNAME, group_name) %>%
  distinct()


# regional data
data <- read.csv("data/regional_data_FD_PG.csv")

regional_movement_data <- read.csv("data/regional_movement_FD_PG.csv")

ukRegions <- st_read("data/boundaries/Regions__December_2019__Boundaries_EN_BFE.shp", quiet = TRUE)

ukRegions <- ukRegions[order(ukRegions$rgn19nm), ]
ukRegions$rgn19nm[ukRegions$rgn19nm == "Yorkshire and The Humber"] <- "Yorkshire and the Humber"

data$SECTIONNAME <- StrCap(tolower(data$SECTIONNAME))
regional_movement_data$SECTIONNAME <- StrCap(tolower(regional_movement_data$SECTIONNAME))
