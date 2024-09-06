# File for storing the text content for the homepage of the dashboard

# Welcome text =========================================================================================

welcome_text <- function() {
  div(
    h2("Welcome"),
    "This dashboard provides information about the industries that graduates were working in
    one, three, five, and ten years after graduation. We are able to do this because we have joined
    the LEO data with SIC (UK Standard Industrial Classification of economic activities) codes
    using the Inter-Departmental Business Register data or IDBR. For more information, see our",

    #    "For the first time, SIC (UK Standard Industrial Classification of economic activities) codes have been joined to
    #   LEO data using the IDBR (Inter-Departmental Business Register data) at one, three,
    #  five and ten years after graduation. This means we can now see what industry graduates were working in
    # at these points in time. For more detail see our ",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/leo-graduate-and-postgraduate-outcomes/2021-22",
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
    "This page provides information about the industries that graduates worked in one, three and five years
    after graduation. The cohort graduated during the academic year of ", fiveyag_cohort_year, ".
    The page allows you to filter for graduates who studied a particular subject area, at a particular qualification
    level, and by graduate sex.",
    sep = ""
  ))
}

## Regional text ---------------------------------------------------------------------------------------

regional_text <- function() {
  div(
    "This page provides information about the regions in which graduates studied and subsequently
    lived one, three, five and ten years after graduation. A table provides additional contextual information
    about the number of providers in each region and median earnings of graduates living in that region.
    You can filter for graduates who studied a particular subject area, at a particular qualification
    level, who subsequently worked in a particular industry, for specific numbers of years after graduation.
    Information applies to different cohorts of graduates depending upon the number of years after graduation
    selected, as we use the most recent tax year, ", tax_year_slash, " to ascertain the region where graduates
    'currently' live."
  )
}

## Subject by industry tables text --------------------------------------------------------------------

sub_by_ind_text <- function() {
  div(
    "This page presents tables with information about the industries that graduates worked in up to ten years
    after graduation. This information applies to different cohorts of graduates depending upon the number of years after graduation
    selected, as we use the most recent tax year, ", tax_year_slash, " to ascertain the industry in which graduates
    are currently working. You can expand the industry sections in these tables to view a more detailed breakdown
    of the 3 digit SIC groups within that industry, and can filter the results by:",
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
    "This page presents tables with information about the subject areas studied by graduates who are currently working
    in a particular industry. Information is avaialable for cohorts currently working during the most recent tax year, ",
    tax_year_slash, ", one, three, five and ten year after graduation. You can filter the results by:",
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
    "The UK Standard Industrial Classification (SIC) of Economic Activities, abbreviated as SIC,
    is published by the Office for National Statistics (ONS). It provides a framework to classify
    economic activity.",
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
    h3("SIC groups and sections"),
    "The SIC framework includes over 700 detailed industry codes, which are grouped hierarchically
    into 615 classes, 272 groups, 88 divisions, and 21 sections. See the link above to the ONS interactive
    SIC hierarchy. In this dashboard, the industry flow and regional pages provide breakdowns
    by the 21 broad industry sections, while the industry by subject andsSubject by industry pages provide additional
    breakdowns by the SIC (3-digit code) groups. The 21 broad industry sections are as follows:",
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
