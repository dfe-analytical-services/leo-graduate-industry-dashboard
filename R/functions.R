### Functions

# Colorders ==================================================================

colorders <- function(inputtable, countinput) {
  cols <- unique(tables_data[[paste0(countinput)]])
  for (column in cols) {
    inputtable[[column]] <- if (column %in% colnames(inputtable)) {
      inputtable[[column]]
    } else {
      NA
    }
  }
  return(inputtable)
}


# Expandable function ========================================================

expandable <- function(input_id, label, contents) {
  gov_details <- shiny::tags$details(
    class = "govuk-details", id = input_id,
    shiny::tags$summary(
      class = "govuk-details__summary",
      shiny::tags$span(
        class = "govuk-details__summary-text",
        label
      )
    ),
    shiny::tags$div(contents)
  )
}