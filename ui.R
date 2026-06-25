fluidPage(
  # lines below added by Cathie
  # tags$head(HTML("<title>Longitudinal Education Outcomes (LEO): Graduate Industry, Tax year ", tax_year_dash, "</title>")),
  # use_shiny_title(),
  # tags$html(lang = "en"),
  tags$head(includeHTML(("google-analytics.html"))),
  shinyGovstyle::skip_to_main(),
  title = tags$head(
    tags$link(
      rel = "shortcut icon",
      href = "dfefavicon.png"
    ),
    # Add title for browser tabs
    tags$title("Longitudinal Educational Outcomes - Graduate Industry dashboard")
  ),
  use_shiny_title(),
  tags$html(lang = "en"),
  # 1. Set metadata for browser ==================================================
  meta() %>%
    meta_general(
      application_name = "LEO Graduate Industry dashboard",
      description = "Longitudinal Education Outcomes for graduates by industry",
      robots = "index,follow",
      generator = "R-Shiny",
      subject = "Outcomes for graduates",
      rating = "General",
      referrer = "no-referrer"
    ),
  shinyjs::useShinyjs(),
  dfeshiny::custom_disconnect_message(
    links = "https://department-for-education.shinyapps.io/leo-graduate-industry-dashboard/",
    publication_name = ees_pub_name,
    publication_link = paste0(
      "https://explore-education-statistics.service.gov.uk/find-statistics/",
      ees_pub_slug
    )
  ),
  # Setting up cookie consent based on a cookie recording the consent:
  # https://book.javascript-for-r.com/shiny-cookies.html
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "dfe_shiny_gov_style.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "additional_style.css"
    )
  ),
  # 2. Cookie banner ==================================================
  #    copied over from template
  dfeshiny::dfe_cookies_script(),
  dfeshiny::cookies_banner_ui(
    name = dashboard_title
  ),

  # 3. Header =========================================================
  dfeshiny::header(dashboard_title),

  # 4. Beta banner=====================================================
  #  copied from template
  shinyGovstyle::banner(
    "beta banner",
    "beta",
    paste0(
      "This dashboard is a new service that we are developing. If you have any feedback or
    suggestions for improvements, please submit them using our ",
      shinyGovstyle::external_link(
        href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-c6JT6ONG3lJtlg-5hU4A6xURUpQME1OUVZIMEFMUUdNMEVONkhEN0g1VSQlQCN0PWcu",
        link_text = "feedback form",
        add_warning = TRUE
      )
      # ),
      # ".</b><br>"
    )
  ),

  # 5. Navbar ====================================================================

  shiny::navlistPanel(
    id = "navlistPanel",
    # widths = c(2, 8),
    shinyGovstyle::full_width_overrides(),
    well = FALSE,
    shiny::tabPanel("Home", homepage()),
    shiny::tabPanel("Industry flow", industry_flow_page()),
    shiny::tabPanel("Regional", regional_page()),
    shiny::tabPanel("Subject by industry", subject_by_industry_page()),
    shiny::tabPanel("Industry by subject", industry_by_subject_page()),
    shiny::tabPanel(
      "Support and feedback",
      support_panel(
        team_email = "he.leo@education.gov.uk",
        repo_name = "https://github.com/dfe-analytical-services/leo-graduate-industry-dashboard",
        form_url = paste0(
          "https://forms.office.com/Pages/ResponsePage.aspx?id=",
          "yXfS-grGoU2187O4s0qC-c6JT6ONG3lJtlg-5hU4A6xURUpQME1OUVZIMEFMUUdNMEVONkhEN0g1VSQlQCN0PWcu"
        ),
        publication_name = ees_pub_name,
        publication_slug = ees_pub_slug
      )
    ),
    shiny::tabPanel(
      "Accessibility",
      dfeshiny::a11y_panel(
        dashboard_title = dashboard_title,
        dashboard_url = dashboard_link,
        date_tested = "8th October 2024",
        date_prepared = "1st July 2024",
        date_reviewed = "8th October 2024",
        issues_contact = public_repo_link,
        publication_slug = ees_pub_slug,
        publication_name = ees_pub_name,
        non_accessible_components = c(
          "Keyboard navigation through the interactive charts is currently limited, and some features are unavailable for keyboard only users",
          "Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)"
        ),
        specific_issues = c(
          "Charts have non-accessible components that are inaccessible for keyboard users.",
          "Chart tooltips are not compativle with screen reader use.",
          "Some decorative images are not labelled appropriately as yet.",
          "Some links are not appropriately labelled."
        )
      )
    ),
    shiny::tabPanel(
      value = "cookies_panel_ui",
      "Cookies",
      dfeshiny::cookies_panel_ui(
        google_analytics_key = google_analytics_key
      )
    )
  ),
  tags$script(
    src = "script.js"
  ),

  # 6. Footer ====================================================================

  shinyGovstyle::footer(TRUE)
) # end of fluid page
