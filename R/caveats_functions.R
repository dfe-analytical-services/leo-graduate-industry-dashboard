caveats_box_flow <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Data for Level 7 (research) and Level 8 graduates are available in the underlying data files.",
      br(),
      "\U2022 The sankey plots and tables include graduates who were in sustained employment in the associated tax year, and a graduate's industry is recorded as the industry in which they earnt the most in the associated tax year.",
      br(),
      "\U2022 SIC codes provide information about the industry that each graduate worked in, but not their role or occupation within that industry.",
      br(),
      "\U2022 Graduates included in this page are those who were in sustained employment at either one and three years after graduation, or three and five years after graduation. Consequently, these figures are not comparable to the five year after graduation cohort in the 'Subject by industry' or 'Industry by subject' sections of this dashboard."
    )
  )
}

caveats_box <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Data only includes graduates who were in sustained employment in the associated tax year, and each graduate's industry is recorded as the one in which they earned the most in the particular tax year.",
      br(),
      "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us about the graduates occupation within that company.",
      br(),
      "\U2022 Please note there are a small number of cases where there are conflicting section names and group names – see the data quality section of ",
      a(
        href = "https://explore-education-statistics.service.gov.uk/methodology/leo-graduate-and-postgraduate-outcomes",
        "the methodology"
      ), " for further details"
    )
  )
}

caveats_box_regional <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Data for Level 7 (research) and Level 8 graduates are available in the underlying data files.",
      br(),
      "\U2022 Data only includes graduates who were in sustained employment in the associated tax year, and a graduate's industry is recorded as the industry in which they earnt the most in the associated tax year.",
      br(),
      "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us about the graduates occupation within that company."
    )
  )
}
