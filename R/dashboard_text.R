# File for storing the text content for the homepage of the dashboard

# Welcome text =========================================================================================

welcome_text <- function() {
  div(
    h2("Welcome"),
    "For the first time, SIC (UK Standard Industrial Classification of economic activities) codes have been joined to
    LEO data using the IDBR (Inter-Departmental Business Register data) at one, three,
    five and ten years after graduation. This means we can now see what industry graduates were working in
    at these points in time. For more detail see our ",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/leo-graduate-and-postgraduate-outcomes/2020-21",
      "official statistics publication on LEO Graduate and Postgraduate Outcomes",
      .noWS = c("after")
    ),
    ".", br(), br(),
    "This dashboard has been produced by the Department for Education to support the aims of the",
    a(
      href = "https://www.gov.uk/government/groups/unit-for-future-skills",
      "Unit for Future Skills.", .noWS = c("after")
    ),
  )
}

# Contents (left panel) ================================================================================

## Industry flow text ----------------------------------------------------------------------------------

industry_flow_text <- function() {
  div(paste(
    "This tab takes a longitudinal view of the five year after graduation cohort (",fiveyag_cohort_year," academic year of graduation
    cohort), as it shows the industry sections they worked in and flowed between at one, three and five years after
    graduation. You can filter this analysis to look at graduates of a specific subject area, or filter by graduate sex."
    , sep = ""
  ))
}

## Regional text ---------------------------------------------------------------------------------------

regional_text <- function() {
  div(
    "This tab compares where graduates studied to where they lived at one, three, five and ten years after
   graduation, for each industry, and uses the ",tax_year_slash," tax year data (so does not follow the same cohort
   as the industry flow analysis). It can be filtered to look at graduates of a specific subject area, and provides
   context on the number of providers and median earnings for the selected options for each region."
  )
}

## Subject by industry tables text --------------------------------------------------------------------

sub_by_ind_text <- function() {
  div(
    "These tables look at the one, three, five and ten year after graduation cohorts from the
    ",tax_year_slash," tax year. The tables show which industries graduates of the selected subject area go on to work in.
    You can expand the industry sections in these tables to view a more detailed breakdown of the 3 digit SIC groups
    within that industry. The following breakdowns are currently available:",
    br(),
    tags$ul(
      tags$li("Sex"),
      tags$li("Ethnicity"),
      tags$li("Free School Meal (FSM) status"),
      tags$li("Current region"),
      tags$li("Prior attainment"),
      tags$li("Subject"),
      tags$li("Qualification level")
    )
  )
}

## Industry by subject tables text ---------------------------------------------------------------------

ind_by_sub_text <- function() {
  div(
    "These tables look at the one, three, five and ten year after graduation cohorts from the
    ",tax_year_slash," tax year. The tables show which subjects graduates working in a selected industry area previously studied.
    You can view this at the 3 digit SIC group level and may select multiple SIC groups within an industry area at one time.
    The following breakdowns are currently available:",
    br(),
    tags$ul(
      tags$li("Sex"),
      tags$li("Ethnicity"),
      tags$li("Free School Meal (FSM) status"),
      tags$li("Current region"),
      tags$li("Prior attainment"),
      tags$li("Industry section"),
      tags$li("Qualification level")
    )
  )
}

# SIC background (right panel) ===========================================================================

sic_groups_text <- function() {
  div(
    h3("IDBR (Inter-Departmental Business Register)"),
    "IDBR data is a comprehensive list of UK businesses used by government for statistical purposes.",
    h3("UK SIC (Standard Industrial Classification) code"),
    "The UK Standard Industrial Classification (SIC) of economic activities is used to classify
    businesses by the type of activity they do.",
    h3("Useful links"),
    a(
      href = "https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic",
      "Standard industrial classification of economic activities (SIC) - GOV.UK.(www.gov.uk)"
    ),
    br(),
    a(
      href = "https://onsdigital.github.io/dp-classification-tools/standard-industrial-classification/ONS_SIC_hierarchy_view.html",
      "Office for National Statistics interactive SIC hierarchy"
    ),
    h3("SIC Groups and sections"),
    "Using the Office for National Statistics (ONS) Standard Industrial Classification (SIC) of economic activities, there are over 700 detailed industry codes
    at the 5 digit level, which are then grouped hierarchically at the 4, 3 and 2 digit level before being grouped into
    21 broad industry sections (see the above link to the ONS interactive hierarchy). In this dashboard, the industry flow and regional analysis
    are both available only at the broadest level of the 21 industry sections. The tables go into more detail, with almost 250
    SIC groups available to view at the 3 digit level by expanding the broad sections which consist of the following:",
    br(),
    br(),
    tags$ol(
      tags$li("Accommodation and food service activities"),
      tags$li("Activities of extraterritorial organisations and bodies"),
      tags$li("Activities of households as employers - undifferentiated goods-and services-producing activities of households for own use"),
      tags$li("Administrative and support service activities"),
      tags$li("Agriculture, forestry and fishing"),
      tags$li("Arts, entertainment and recreation"),
      tags$li("Construction"),
      tags$li("Education"),
      tags$li("Electricity, gas, steam and air conditioning supply"),
      tags$li("Financial and insurance activities"),
      tags$li("Human health and social work activities"),
      tags$li("Information and communication"),
      tags$li("Manufacturing"),
      tags$li("Mining and quarrying"),
      tags$li("Other service activities"),
      tags$li("Professional, scientific and technical activities"),
      tags$li("Public administration and defence - compulsory social security"),
      tags$li("Real estate activities"),
      tags$li("Transportation and storage"),
      tags$li("Water supply - sewerage, waste management and remediation activities"),
      tags$li("Wholesale and retail trade - repair of motor vehicles and motorcycles")
    )
  )
}
