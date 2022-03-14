caveats_box <- function() {
  box(
    status = "warning", width = NULL, solidHeader = TRUE,
    strong("Caveats"),
    br(),
    "\U2022 Data only includes graduates who were in sustained employment in the associated tax year, and a graduate's industry is recorded as the industry in which they earnt the most in the associated tax year.",
    br(),
    "\U2022 Counts have been rounded to the nearest 5, however percentages are based on raw values.",
    br(),
    "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us about the graduates occupation within that company.",
    br()
  )
}
