# Homepage tab ============================================================
homepage <- function() {
  tabPanel(
    value = "homepage",
    title = "Homepage",
    # noti_banner(
    #   "notId",
    #   title_txt = "Known issue",
    #   body_txt = "We are aware of and working to address performance issues with the dashboard, during this time some interactive elements may be slow.",
    #   type = "standard"
    # ),

    ## Tab content ----------------------------------------------------------

    gov_main_layout(
      gov_row(
        column(
          12,
          h1(
            "Longitudinal Education Outcomes: Graduate Industry dashboard, Tax year 2023-24"
          ),
          welcome_text(), # defined in R/dashboard_text.R
          br(),
          br()
        ),

        ## Left panel -------------------------------------------------------
        bslib::layout_columns(
          bslib::card(
            bslib::card_header(
              heading_text("Contents", level = 2, size = "m")
            ),
            bslib::card_body(
              tags$div(
                title = "This page provides information about the industries that graduates work in one, three and five years after graduation.",
                h3(actionLink(
                  "link_to_industryFlow_tab",
                  "Industry flow page",
                  disabled = F
                ))
              ),
              industry_flow_text(), # defined in R/dashboard_text.R
              tags$div(
                title = "This page provides information about where graduates studied and lived after graduating.",
                h3(actionLink("link_to_regional_tab", "Regional page"))
              ),
              regional_text(), # defined in R/dashboard_text.R
              tags$div(
                title = "This page provides information about the industries that graduates of different subjects have worked in.",
                h3(actionLink(
                  "link_to_subjectByIndustry_tab",
                  "Industry by subject page"
                ))
              ),
              sub_by_ind_text(), # defined in R/dashboard_text.R
              tags$div(
                title = "This page provides information about the subjects studied by graduates who subsequently worked in a particular industry.",
                h3(actionLink(
                  "link_to_industryBySubject_tab",
                  "Subject by industry page"
                ))
              ),
              ind_by_sub_text(), # defined in R/dashboard_text.R
            )
          ),

          ## Right panel ------------------------------------------------------
          bslib::card(
            bslib::card_header(
              heading_text("IDBR and SIC background", level = 2, size = "m")
            ),
            bslib::card_body(
              sic_groups_text(), # defined in R/dashboard_text.R
            )
          ),
          widths = c(6, 6)
        )
      )
    )
  )
}
# End of homepage tabPanel()
