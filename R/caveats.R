caveats_box <- function() {
  shinyGovstyle::insert_text(
    inputId = "caveats",
    text = paste0(
      strong("Caveats"),
      br(),
      "\U2022 Data only includes graduates who were in sustained employment in the associated tax year, and a graduate's industry is recorded as the industry in which they earnt the most in the associated tax year.",
      br(),
      "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us about the graduates occupation within that company."
      # br(),
      # "\U2022 Counts have been rounded to the nearest 5 and earnings are rounded to the nearest £100.",
      # br(),
      # "\U2022 c = data has been supressed due to small numbers. x = there is no result available (N/A)"
    )
  )
}

footnotes_box <- function() {
  shinyGovstyle::insert_text(
    inputId = "footnotes",
    text = paste0(
      strong("Footnotes"),
      br(),
      "\U2022 Counts have been rounded to the nearest 5 and earnings are rounded to the nearest £100.",
      br(),
      "\U2022 c = data has been supressed due to small numbers. x = there is no result available (N/A)"
    )
  )
}
