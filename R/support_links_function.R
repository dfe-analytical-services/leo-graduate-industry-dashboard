support_links <- function() {
  div(
    h2("Give us feedback"),
    "This dashboard is a new service that we are developing. If you have any feedback or suggestions for improvements, please submit them using our ",
    #    a(
    #     href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-c6JT6ONG3lJtlg-5hU4A6xURUpQME1OUVZIMEFMUUdNMEVONkhEN0g1VSQlQCN0PWcu",
    #    "feedback form",
    #   #      .noWS = c("after")
    #  .noWS = "after" # No white space after the link - this version is the same as the DfE shiny template https://github.com/dfe-analytical-services/shiny-template/blob/main/R/ui_panels/accessibility_statement.R
    #  ),
    # Cathie - uses the external_link() function in dfeshiny
    external_link(
      href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-c6JT6ONG3lJtlg-5hU4A6xURUpQME1OUVZIMEFMUUdNMEVONkhEN0g1VSQlQCN0PWcu",
      link_text = "feedback form",
      add_warning = TRUE
    ),
    ".", br(),
    "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
    #    a(href = "mailto:he.leo@education.gov.uk", "he.leo@education.gov.uk", .noWS = c("after")), ".",
    # Cathie - uses the external_link() function in dfeshiny
    external_link(
      href = "mailto:he.leo@education.gov.uk",
      link_text = "he.leo@education.gov.uk",
      add_warning = TRUE
    ),
    br(),
    h2("Find more information on the data"),
    "The data used to produce the dashboard, along with methodological information can be found on ",
    #    a(
    #     href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics",
    #    #   .noWS = c("after")
    #   .noWS = "after"
    # ),
    # Cathie - uses the external_link() function in dfeshiny
    external_link(
      href = "https://explore-education-statistics.service.gov.uk/",
      link_text = "Explore Education Statistics",
      add_warning = TRUE
    ),
    ".",
    br(),
    h2("Contact us"),
    "If you have questions about the dashboard or data within it, please contact us at ",
    #    a(href = "mailto:he.leo@education.gov.uk", "he.leo@education.gov.uk", .noWS = c("after")), br(),
    # Cathie - uses the external_link() function in the dfeshiny package
    external_link(
      href = "mailto:he.leo@education.gov.uk",
      link_text = "he.leo@education.gov.uk",
      add_warning = TRUE
    ),
    "This dashboard has been produced by the Department for Education to support the aims of the ",
    #    a(
    #     href = "https://www.gov.uk/government/groups/unit-for-future-skills",
    #    "Unit for Future Skills",
    #   #      .noWS = c("after")
    #  .noWS = "after"
    # ),
    # Cathie - uses the external_link() function in the dfeshiny package
    external_link(
      href = "https://www.gov.uk/government/groups/unit-for-future-skills",
      link_text = "Unit for Future Skills",
      add_warning = TRUE
    ),
    ".",
    br(),
    h2("See the source code"),
    "The source code for this dashboard is available in our ",
    #    a(
    #     href = "https://github.com/dfe-analytical-services/leo-graduate-industry-dashboard", "GitHub repository",
    #    #      .noWS = c("after")
    #   .noWS = "after"
    # ),
    # Cathie - uses the external_link() function in the dfeshiny package
    external_link(
      href = "https://github.com/dfe-analytical-services/leo-graduate-industry-dashboard",
      link_text = "GitHub repository",
      add_warning = TRUE
    ),
    ".",
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  )
}
