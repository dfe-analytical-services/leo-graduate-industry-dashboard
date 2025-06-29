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

    fluidPage(
      fluidRow(
        column(
          12,
          h1("Longitudinal Education Outcomes: Graduate Industry dashboard, Tax year 2022-23"),
          welcome_text(), # defined in R/dashboard_text.R
          br(),
          br()
        ),

        ## Left panel -------------------------------------------------------
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Contents")
              ),
              div(
                class = "panel-body",
                tags$div(
                  title = "This page provides information about the industries that graduates work in one, three and five years after graduation.",
                  h3(actionLink("link_to_industryFlow_tab", "Industry flow page", disabled = F))
                ),
                industry_flow_text(), # defined in R/dashboard_text.R
                br(),
                tags$div(
                  title = "This page provides information about where graduates studied and lived after graduating.",
                  h3(actionLink("link_to_regional_tab", "Regional page"))
                ),
                regional_text(), # defined in R/dashboard_text.R
                br(),
                tags$div(
                  title = "This page provides information about the industries that graduates of different subjects have worked in.",
                  h3(actionLink("link_to_subjectByIndustry_tab", "Industry by subject page"))
                ),
                sub_by_ind_text(), # defined in R/dashboard_text.R
                tags$div(
                  title = "This page provides information about the subjects studied by graduates who subsequently worked in a particular industry.",
                  h3(actionLink("link_to_industryBySubject_tab", "Subject by industry page"))
                ),
                ind_by_sub_text(), # defined in R/dashboard_text.R
              )
            )
          ),
        ),

        ## Right panel ------------------------------------------------------

        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("IDBR and SIC background")
              ),
              div(
                class = "panel-body",
                sic_groups_text(), # defined in R/dashboard_text.R
              )
            )
          )
        )
      )
    )
  )
}
# End of homepage tabPanel()
