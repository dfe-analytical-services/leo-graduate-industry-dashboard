caveats_box_flow <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Data for Level 7 (research) and Level 8 graduates are not provided in the sankey plot or
      table on this page, but are available in the underlying data files.",
      br(),
      "\U2022 The sankey plots and tables include only those graduates who were in sustained employment
      one and three years after graduation, and/or three and five years after graduation. Consequently,
      these figures are not comparable to the five year after graduation cohort in the 'Subject by
      industry' or 'Industry by subject' sections of this dashboard.",
      br(),
      "\U2022 Each graduate's industry is recorded as the industry in which they earned the most in the
      particular tax year.",
      br(),
      "\U2022 SIC codes provide information about the industry that each graduate worked in, but not
      their role or occupation within that industry."
    )
  )
}

caveats_box <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Coverage is graduates who were in sustained employment during the 2021-22 tax year.",
      br(),
      "\U2022 Each graduate's recorded industry is the industry in which they earned the most during the tax year.",
      br(),
      "\U2022 SIC codes provide information about the industry the graduate works in, but not their occupation or role within that industry.",
      br(),
      "\U2022 Please note that for a few cases, SIC section and group names conflict with one another. See the data quality section of ",
# Cathie revamped the link
      external_link(
        href = "https://explore-education-statistics.service.gov.uk/methodology/leo-graduate-and-postgraduate-outcomes",
        link_text = "the methodology",
        add_warning = TRUE
      ), " for further details."
#      ".", br(), br(),      
#      a(
 #       href = "https://explore-education-statistics.service.gov.uk/methodology/leo-graduate-and-postgraduate-outcomes",
  #      #       target = "_blank", # Cathie trying to get it to open on a new page - didn't work! as it tries to open in RStudio
   #     "the methodology"
#      ), " for further details."
    )
  )
}

caveats_box_regional <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Data for Level 7 (research) and Level 8 graduates are not provided in the map, sankey plot or
      table on this page, but are available in the underlying data files.",
      br(),
      "\U2022 Data only include graduates who were in sustained employment in the associated tax year.",
      br(),
      "\u2022 Each graduate's industry is recorded as the industry in which they earned most during the associated tax year.",
      br(),
      "\U2022 SIC codes tell us which industry section that the graduate worked in, but not their occupation or role in that industry."
    )
  )
}
