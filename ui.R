fluidPage(
  # lines below added by Cathie
  # tags$head(HTML("<title>Longitudinal Education Outcomes (LEO): Graduate Industry, Tax year ", tax_year_dash, "</title>")),
  # use_shiny_title(),
  # tags$html(lang = "en"),

  shinyjs::useShinyjs(),
  includeCSS("www/dfe_shiny_gov_style.css"),
  title = dashboard_title,
  # shinya11y::use_tota11y(), # accessibility layer for local testing

  dfeshiny::custom_disconnect_message(
    links = "https://department-for-education.shinyapps.io/leo-graduate-industry-dashboard/",
    publication_name = ees_pub_name,
    publication_link = paste0(
      "https://explore-education-statistics.service.gov.uk/find-statistics/",
      ees_pub_slug
    )
  ),

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
  dfeshiny::dfe_cookies_script(),
  dfeshiny::cookies_banner_ui(
    name = "Longitudinal Education Outcomes - Graduate Industry dashboard"
  ),


  # 3. Header =========================================================
  shinyGovstyle::header(
    main_text = "",
    main_link = "https://www.gov.uk/government/organisations/department-for-education",
    secondary_text = "Longitudinal Education Outcomes - Graduate Industry dashboard",
    logo = "images/DfE_logo.png",
    logo_width = 85,
    logo_height = 80
  ),


  # 4. Beta banner=====================================================
  #  copied from template
  shinyGovstyle::banner(
    "beta banner",
    "beta",
    paste0(
      "This dashboard is a new service that we are developing. If you have any feedback or
    suggestions for improvements, please submit them using our ",
      external_link(
        href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-c6JT6ONG3lJtlg-5hU4A6xURUpQME1OUVZIMEFMUUdNMEVONkhEN0g1VSQlQCN0PWcu",
        link_text = "feedback form",
        add_warning = TRUE
      ),
      ".</b><br>"
    )
  ),


  # 5. Navbar ====================================================================

  shiny::navlistPanel(
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
      a11y_panel()
    ),
    tabPanel(
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
    tabPanel(
      value = "cookies_panel_ui",
      "Cookies",
      dfeshiny::cookies_panel_ui(
        google_analytics_key = google_analytics_key
      )
    )
  ),


  # 6. Footer ====================================================================

  shinyGovstyle::footer(TRUE)
) # end of fluid page
