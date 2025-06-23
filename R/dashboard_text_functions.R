# File for storing the text content for the homepage of the dashboard

# Welcome text =========================================================================================

welcome_text <- function() {
  div(
    h2("Welcome"),
    "This dashboard provides information about the industries that graduates were working in
    one, three, five, and ten years after graduation. We are able to do this because we have joined
    the LEO data with the UK Standard Industrial Classification (SIC) of businesses
    using the Inter-Departmental Business Register data or IDBR. For more information, see our ",

    #    "For the first time, SIC (UK Standard Industrial Classification of economic activities) codes have been joined to
    #   LEO data using the IDBR (Inter-Departmental Business Register data) at one, three,
    #  five and ten years after graduation. This means we can now see what industry graduates were working in
    # at these points in time. For more detail see our ",

    # Cathie: Revamped the external link
    #        a(
    #        href = "https://explore-education-statistics.service.gov.uk/find-statistics/leo-graduate-and-postgraduate-outcomes/2021-22",
    #      "official statistics publication on LEO Graduate and Postgraduate Outcomes",
    #    #      .noWS = c("after")  This was the original code before Cathie changed it to be equivalent to the template code
    #  .noWS = "after" # no white space after the link text in the rendered HTML.
    # ),
    # Cathie: Revamped the external link
    external_link(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/leo-graduate-and-postgraduate-outcomes/2022-23",
      link_text = "official statistics publication on LEO Graduate and Postgraduate Outcomes",
      add_warning = TRUE
    ),
    ".", br(), br(),
    "This dashboard has been produced by the Department for Education to support the aims of ",
    #    a(
    #     href = "https://www.gov.uk/government/groups/unit-for-future-skills",
    #    "Unit for Future Skills.",
    #   #      .noWS = c("after")   This was the original code before Cathie changed it to be equivalent to the template code
    #  .noWS = "after" # no white space after the link text in the rendered HTML.
    # ),
    # Cathie: Revamped the external link
    external_link(
      href = "https://www.gov.uk/government/collections/skills-england",
      link_text = "Skills England",
      add_warning = TRUE
    ),
  )
}

# Contents (left panel) ================================================================================

## Industry flow text ----------------------------------------------------------------------------------

industry_flow_text <- function() {
  div(paste(
    "This page provides information about which industries graduates worked in one, three and five years
    after graduation. It tracks individuals who graduated during the academic year of 2016-17. You can
    filter to select graduates who studied in a particular subject area, at a particular qualification
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
    level, who subsequently worked in a particular industry, and for specific numbers of years after graduation.
    Information applies to different cohorts of graduates depending upon the number of years after graduation
    selected, as we use the most recent tax year, 2022-23, to ascertain the region where graduates
    'currently' live."
  )
}

## Subject by industry tables text --------------------------------------------------------------------

sub_by_ind_text <- function() {
  div(
    "This page presents tables with information about the industries that graduates worked in up to ten years
    after graduation. The information applies to different cohorts of graduates depending upon the number of years after graduation
    selected, as we use the most recent tax year, 2022-23, to ascertain the industry in which graduates
    are 'currently' working. You can expand the industry sections in these tables to view a more detailed breakdown
    of the 272 (3 digit) SIC groups within that industry, and can filter the results by:",
    br(),
    tags$ul(
      tags$li("Sex"),
      tags$li("Ethnicity"),
      tags$li("Free School Meal (FSM) status"),
      tags$li("Region of residence during 2022-23 tax year"),
      tags$li("Prior attainment"),
      tags$li("Subject"),
      tags$li("Qualification level")
    )
  )
}

## Industry by subject tables text ---------------------------------------------------------------------

ind_by_sub_text <- function() {
  div(
    "This page presents tables with information about the subject areas studied by graduates who were working
    in each broad industry section during the 2022-23 tax year. Information is available for cohorts that graduated
    one, three, five and ten years earlier, in 2021, 2019, 2017, and 2012, respectively. You can filter the results by:",
    br(),
    tags$ul(
      tags$li("Sex"),
      tags$li("Ethnicity"),
      tags$li("Free School Meal (FSM) status"),
      tags$li("Region of residence during 2022-22 tax year"),
      tags$li("Prior attainment"),
      tags$li("Industry section"),
      tags$li("Qualification level")
    )
  )
}

# SIC background (right panel) ===========================================================================

sic_groups_text <- function() {
  div(
    h3("Inter-Departmental Business Register (IDBR)"),
    "IDBR data is a comprehensive list of UK businesses used by government for statistical purposes.",
    br(),
    br(),
    h3("UK Standard Industrial Classification (SIC) code"),
    "The UK Standard Industrial Classification, abbreviated to SIC,
    is a hierarchical framework used to classify business establishments and other statistical units
    by the type of economic activity in which they are engaged.
    At the broadest level of classification, the SIC consists of
    21 sections, which are broken down into 88 divisions. These are then broken down into
    272 groups, 615 classes, and some of these 615 classes are further broken down into an additional 191 sub-classes.
    The UK SIC is published by the Office for National Statistics.",
    br(),
    br(),
    "Here are some useful links:",
    br(),
    #    a(
    #     href = "https://onsdigital.github.io/dp-classification-tools/standard-industrial-classification/ONS_SIC_hierarchy_view.html",
    #    "Office for National Statistics interactive SIC hierarchy"
    #   # Added by Cathie below
    #  , .noWS = "after"
    # # End of what Cathie added
    # ),
    # Cathie: Revamped the external link
    external_link(
      href = "https://www.ons.gov.uk/methodology/classificationsandstandards/ukstandardindustrialclassificationofeconomicactivities/uksic2007",
      link_text = "Office for National Statistics information about SIC",
      add_warning = TRUE
    ),
    br(),
    #    h3("Useful links"),
    #    a(
    #     href = "https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic",
    #    "Standard industrial classification of economic activities (SIC) - GOV.UK.(www.gov.uk)"
    #   # Added by Cathie below
    #  , .noWS = "after"
    # # End of what Cathie added
    # ),
    # Cathie: Revamped the external link
    external_link(
      href = "https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic",
      link_text = "Gov.uk guidance about SIC",
      add_warning = TRUE
    ),
    #    h3("SIC groups and sections"),
    #    "At the broadest level of classification, the SIC consists of 21 sections, which are broken down into 88 divisions. These are then broken down into
    #    272 groups, 615 classes, and over 700 sub-classes. In this dashboard, the industry flow and regional pages provide breakdowns
    #    by the 21 broad industry sections, while the industry by subject and subject by industry pages provide additional
    #    breakdowns by the 272 groups.",
    br(),
    br(),
    "In this dashboard, the industry flow and regional pages provide breakdowns
    by the 21 broad industry sections, and the industry by subject and subject by industry pages provide additional
    breakdowns by the 272 groups.",
    br(),
    br(),
    "The 21 industry sections are as follows:",
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
