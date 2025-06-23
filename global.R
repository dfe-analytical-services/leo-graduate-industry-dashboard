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
shhh(library(dfeshiny))
# shhh(library(shinya11y))
# shhh(library(chromote)) Cathie tried adding this when getting an error for running local tests, but it didn't help


# update years for text -------------------------------------------------

tax_year_dash <- "2022-23"
tax_year_slash <- "2022/23"
fiveyag_cohort_year <- "2016/17"


# read the scripts in the correct order ---------------------------------

# lapply(list.files("R/", full.names = TRUE), source)
# Source all files in the ui_panels folder
# lapply(list.files("R/ui_panels/", full.names = TRUE), source)

source("R/read_data_functions.R")
source("R/caveats_functions.R")
source("R/dashboard_text_functions.R")
source("R/general_functions.R")
source("R/industry_flow_functions.R")
source("R/regional_functions.R")
source("R/subject_industry_functions.R")
source("R/industry_subject_functions.R")
source("R/ui_panels/page1_homepage.R")
source("R/ui_panels/page2_industry_flow.R")
source("R/ui_panels/page3_regional.R")
source("R/ui_panels/page4_subject_industry.R")
source("R/ui_panels/page5_industry_subject.R")

# tidy_code_function -------------------------------------------------------------------------------

google_analytics_key <- "Q08W0Y4G5C"
dashboard_title <- "Longitudinal Education Outcomes - Graduate Industry dashboard"
dashboard_link <- paste0(
  "https://department-for-education.shinyapps.io/",
  "leo-graduate-industry-dashboard/"
)
ees_pub_name <- "LEO Graduate and Postgraduate Outcomes"
ees_pub_slug <- "leo-graduate-and-postgraduate-outcomes"
public_repo_link <- "https://github.com/dfe-analytical-services/leo-graduate-industry-dashboard"



# Read data:
# Use this when running locally pre-publication:
cohort1 <- read_cohort("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/24 - preparations for publication 2025/Industry dashboard data/industry_sankey_1to3yag.csv")
cohort2 <- read_cohort("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/24 - preparations for publication 2025/Industry dashboard data/industry_sankey_3to5yag.csv")
cohort3 <- read_cohort("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/24 - preparations for publication 2025/Industry dashboard data/industry_sankey_1to5yag.csv")

tables_data <- read_tables_data("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/24 - preparations for publication 2025/Industry dashboard data/industry_3digitSIC_dashboard_data.csv")

data <- read.csv("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/24 - preparations for publication 2025/Industry dashboard data/industry_regional_map.csv")
regional_movement_data <- read.csv("//vmt1pr-dhfs01/Working/EDUDEST-WKG-HE-FS/SFR/24 - preparations for publication 2025/Industry dashboard data/industry_regional_movement.csv")


# Change to this once data can be added to project data folder on morning of publication:
# cohort1 <- read_cohort("data/industry_sankey_1to3yag.csv")
# cohort2 <- read_cohort("data/industry_sankey_3to5yag.csv")
# cohort3 <- read_cohort("data/industry_sankey_1to5yag.csv")
# 
# tables_data <- read_tables_data("data/industry_3digitSIC_dashboard_data.csv")
# 
# data <- read.csv("data/industry_regional_map.csv")
# regional_movement_data <- read.csv("data/industry_regional_movement.csv")




qual_subjects <- tables_data %>%
  # filter line added by Cathie, so that only subject-qualification level combinations are available where
  # at least ten graduates have this combination
  #  filter(sex == "F+M" & ethnicity == "All" & current_region == "All" & FSM == "All" & prior_attainment == "All" & group_name == "All") %>%
  #  filter(as.numeric(count) >= 10) %>%
  # end of Cathie's addition
  select(qualification_TR, subject_name) %>%
  distinct()

industry_groups <- tables_data %>%
  select(SECTIONNAME, group_name) %>%
  distinct()


# Changes for regional function
# Cathie moved this to here.
# It was originally at the beginning of the page3_regional.R script.

ukRegions <- st_read("data/boundaries/RGN_DEC_2023_EN_BFE.shp", quiet = TRUE)

ukRegions <- ukRegions[order(ukRegions$RGN23NM), ]
ukRegions$RGN23NM[ukRegions$RGN23NM == "Yorkshire and The Humber"] <- "Yorkshire and the Humber"

data$SECTIONNAME <- StrCap(tolower(data$SECTIONNAME))
regional_movement_data$SECTIONNAME <- StrCap(tolower(regional_movement_data$SECTIONNAME))

# Line below was added by Cathie, as the final page
# (referred to in the script etc as industry by subject)
# didn't produce any results
# tables_data$SECTIONNAME <- StrCap(tolower(tables_data$SECTIONNAME))
