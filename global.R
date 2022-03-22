shhh <- suppressPackageStartupMessages # It's a library, so shhh!


shhh(library(styler))
shhh(library(plyr))
shhh(library(leaflet))
shhh(library(sp))
shhh(library(data.table))
shhh(library(RColorBrewer))
shhh(library(pander))
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
shhh(library(shinytest))
shhh(library(networkD3))
shhh(library(reactable))
shhh(library(knitr))
shhh(library(Hmisc))
shhh(library(matrixStats))
shhh(library(sourcetools))
shhh(library(shinyBS))
shhh(library(dplyr))
shhh(library(tidyr))
shhh(library(readr))
shhh(library(metathis))

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


cohort1 <- read.csv("data/pg_sankey_data_1_3_yag_dummy.csv")
cohort2 <- read.csv("data/pg_sankey_data_3_5_yag_dummy.csv")
cohort3 <- read.csv("data/pg_sankey_data_1_5_yag_dummy.csv")

cohort1 <- subset(cohort1, select = -X)
cohort2 <- subset(cohort2, select = -X)
cohort3 <- subset(cohort3, select = -X)

# earnings_data <- read.csv("earnings_data_with_PG.csv")

tables_data <- fread("data/pg_sic_crosstabs_underlying_data_cf_with_threshold_dummy.csv") %>% select(-V1)
tables_earnings_data <- fread("data/pg_sic_crosstabs_earnings_data_cf_with_threshold_dummy.csv") %>% select(-V1)
names(tables_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", "ethnicity", "current_region", "FSM", "prior_attainment", "count", "threshold", "qualification_TR")
names(tables_earnings_data) <- c("X", "YAG", "subject_name", "SECTIONNAME", "sex", "ethnicity", "current_region", "FSM", "prior_attainment", "count", "earnings_median", "threshold", "qualification_TR")

# Create sankey chart, inputs based on server logic from ui inputs.


source('R/sankey.R')


# REGIONAL ---------------------------------------------------------------------
source('R/regional.R')


# CROSSTABS ---------------------------------------------------------------
source('R/crosstabs.R')
source('R/crosstabs_reverse.R')




# Run the application -----------------------------------------------------

# shinyApp(ui = ui, server = server)
