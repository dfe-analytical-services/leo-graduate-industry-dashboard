caveats_box_flow <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Data for Level 7 (research) and Level 8 graduates are available in the underlying data files.",
      br(),
      "\U2022 Data only includes graduates who were in sustained employment in the associated tax year, and a graduate's industry is recorded as the industry in which they earnt the most in the associated tax year.",
      br(),
      "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us about the graduates occupation within that company.",
      br(),
      "\U2022 For a graduate to be included in this section they must have been in sustained employment at either one and three years after graduation, or three and five years after graduation. These figures will therefore not be comparable to the five year after graduation cohort in the 'Subject by industry' or 'Industry by subject' sections in this dashboard."
    )
  )
}

caveats_box <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Data only includes graduates who were in sustained employment in the associated tax year, and a graduate's industry is recorded as the industry in which they earnt the most in the associated tax year.",
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
