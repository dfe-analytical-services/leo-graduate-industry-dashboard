# File for storing the text content for the homepage of the dashboard

# Welcome text =========================================================================================

welcome_text <- function() {
  div(
    h2("Welcome"),
    "We have joined the IDBR (Inter-Departmental Business Register data) via SIC (Standard Industry
    Classification) codes to the LEO (Longitudinal Educational Outcomes) data for the 2014/15 to
    2018/19 tax years at one, three, five and ten years after graduation, which means we can now see
    what industry these graduates were working in at these points in time. For more detail see our ",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/graduate-outcomes-leo",
      "official statistics publication on Graduate Outcomes",
      .noWS = c("after")
    ),
    ".",
  )
}

# Contents (left panel) ================================================================================

## Industry flow text ----------------------------------------------------------------------------------

industry_flow_text <- function() {
  div(
    "This tab looks at the industries one longitudinal cohort (2012/13 academic year of graduation
    cohort) worked in and flowed between at one, three and five years after graduation. You can filter
    this analysis to look at graduates of a specific subject area, or filter by graduate sex."
  )
}

## Regional text ---------------------------------------------------------------------------------------

regional_text <- function() {
  div(
    "This tab compares where graduates studied to where they lived at one, three and five years after
   graduation, for each industry, and uses the latest tax year data, so does not follow the same cohort
   as the longitudinal tab. It can be filtered to look at graduates of a specific subject and provides
   context on the number of providers in each region that have graduates working in the selected
   industry, and the median earnings of graduates working in the selected industry in each region."
  )
}

## Subject by industry tables text --------------------------------------------------------------------

sub_by_ind_text <- function() {
  div(
    "The tables use the latest tax year data, so do not follow the same cohort as the longitudinal tab.
    Instead the table looks at the one, three, five and ten year after graduation cohorts from the
    2018/19 tax year. The tables show breakdowns of graduate industry for the filtered subject area
    and year after graduation. The following breakdowns are currently available:",
    br(),
    tags$ul(
      tags$li("sex"),
      tags$li("ethnicity"),
      tags$li("FSM status"),
      tags$li("current region"),
      tags$li("prior attainment"),
      tags$li("subject"),
      tags$li("qualification level")
    )
  )
}

## Industry by subject tables text ---------------------------------------------------------------------

ind_by_sub_text <- function() {
  div(
    "The tables use the latest tax year data, so do not follow the same cohort as the longitudinal tab.
    Instead the table looks at the one, three, five and ten year after graduation cohorts from the
    2018/19 tax year. The tables show breakdowns of graduate industry for the filtered subject area
    and year after graduation. The following breakdowns are currently available:",
    br(),
    tags$ul(
      tags$li("sex"),
      tags$li("ethnicity"),
      tags$li("FSM status"),
      tags$li("current region"),
      tags$li("prior attainment"),
      tags$li("subject"),
      tags$li("qualification level")
    )
  )
}

# SIC background (right panel) ===========================================================================

sic_groups_text <- function() {
  div(
    h3("IDBR (Inter-Departmental Business Register)"),
    "IDBR data is a comprehensive list of UK businesses used by government for statistical purposes.",
    h3("UK SIC (Standard Industrial Classification) code"),
    "The UK Standard Industrial Classification (SIC) of economic activties is used to classify
    businesses by the type of activity they do.",
    h3("Useful links"),
    a(
      href = "https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic",
      "Standard industrial classification of economic activities (SIC) - GOV.UK.(www.gov.uk)"
    ),
    br(),
    a(
      href = "https://siccode.com/sic-code-lookup-directory",
      "SIC Code Lookup | SIC Code Search Tool"
    ),
    h3("SIC Groups and sections"),
    "We have two SIC variables joined onto our data: SECTIONNAME and SIC2007. There are over 700
    detailed SIC codes in the SIC2007 variable, which are grouped into 21 industry sections in the
    SECTIONNAME variable. We have used the broader industry sections as the base for our analysis,
    which consist of the following:",
    br(),
    br(),
    tags$ol(
      tags$li("ACCOMMODATION AND FOOD SERVICE ACTIVITIES"),
      tags$li("ACTIVITITES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES"),
      tags$li("ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS - UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING
              ACTIVITIES OF HOUSEHOLDS FOR OWN USE"),
      tags$li("ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES"),
      tags$li("AGRICULTURE, FORESTRY AND FISHING"),
      tags$li("ARTS, ENTERTAINMENT AND RECREATION"),
      tags$li("CONSTRUCTION"),
      tags$li("EDUCATION"),
      tags$li("ELECTRICITY, GAS, STEAM AND AIR CONDITIONG SUPPLY"),
      tags$li("FINANCIAL AND INSURANCE ACTIVITIES"),
      tags$li("HUMAN HEALTH AND SOCIAL WORK ACTIVITIES"),
      tags$li("INFORMATION AND COMMUNICATION"),
      tags$li("MANUFACTURING"),
      tags$li("MINING AND QUARRYING"),
      tags$li("OTHER SERVICE ACTIVITIES"),
      tags$li("PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES"),
      tags$li("PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY"),
      tags$li("REAL ESTATE ACTIVITIES"),
      tags$li("TRANSPORTATION AND STORAGE"),
      tags$li("WATER SUPPLY - SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES"),
      tags$li("WHOLESALE AND RETAIL TRADE - REPAIR OF MOTOR VEHICLES AND MOTORCYCLES")
    )
  )
}
