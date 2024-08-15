fluidPage(
  # lines below added by Cathie
  # tags$head(HTML("<title>Longitudinal Education Outcomes (LEO): Graduate Industry, Tax year ", tax_year_dash, "</title>")),
  # use_shiny_title(),
  # tags$html(lang = "en"),

  shinyjs::useShinyjs(),
  includeCSS("www/dfe_shiny_gov_style.css"),
  title = "LEO Graduate Industry dashboard",
  # use_tota11y(), # accessibility layer for local testing

  # 1. Set metadata for browser ==================================================

  tags$html(lang = "en"),
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
  # Set title for search engines
  HTML("<title>Longitudinal Education Outcomes (LEO): Graduate Industry, Tax year ", tax_year_dash, "</title>"),


  # 2. Cookie banner ==================================================
  #    copied over from template
  dfeshiny::dfe_cookie_script(),
  dfeshiny::cookie_banner_ui(
    "cookie-banner",
    "Longitudinal Education Outcomes - Graduate Industry dashboard"
  ),


  # 3. Header =========================================================
  shinyGovstyle::header(
    main_text = "",
    main_link = "https://www.gov.uk/government/organisations/department-for-education",
    secondary_text = "Longitudinal Education Outcomes - Graduate Industry dashboard",
    logo = "images/DfE_logo_landscape.png",
    logo_width = 150,
    logo_height = 32
  ),


  # 4. Beta banner=====================================================
  #  copied from template
  shinyGovstyle::banner(
    "beta banner",
    "beta",
    paste0(
      "This dashboard is a new service that we are developing. If you have any feedback or
    suggestions for improvements, please submit them using our ",
      a(
        href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-c6JT6ONG3lJtlg-5hU4A6xURUpQME1OUVZIMEFMUUdNMEVONkhEN0g1VSQlQCN0PWcu",
        "feedback form", .noWS = c("after")
      ),
      ".</b><br>"
    )
  ),


  # 5. Navbar ====================================================================

  shiny::navlistPanel(
    "",
    id = "navlistPanel",
    widths = c(2, 8),
    well = FALSE,
    homepage(),
    industry_flow_page(),
    regional_page(),
    subject_by_industry_page(),
    industry_by_subject_page(),
    tabPanel(
      "Accessibility",
      accessibility_statement() # defined in R/accessibility_statement.R
    ),
    dfeshiny::cookies_panel_ui(
      id = "cookie-panel",
      google_analytics_key = google_analytics_key
    )
    #  accessibility_page(),
    #  cookies_page(),
    #  support_and_feedback_page(
    #   team_email = "statistics.development@education.gov.uk",
    #  repo_name = "https://github.com/dfe-analytical-services/shiny-template",
    # form_url = "https://forms.office.com"
    #  )
  ),




  # 6. Footer ====================================================================

  shinyGovstyle::footer(TRUE)
) # end of fluid page
