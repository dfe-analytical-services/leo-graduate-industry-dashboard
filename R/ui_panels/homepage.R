# Homepage tab ============================================================
homepage <- function() {
  tabPanel(
    value = "homepage", title = "Homepage",
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
          h1("Longitudinal Education Outcomes: Graduate Industry dashboard, Tax year 2021-22"),
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
                  title = "This section is useful if you want to understand how well different industries retain graduates.",
                  h3(actionLink("link_to_industryFlow_tab", "Industry flow analysis"))
                ),
                industry_flow_text(), # defined in R/dashboard_text.R
                br(),
                tags$div(
                  title = "This section is useful if you want to understand which parts of the country graduates move to in order to find roles in particular industries.",
                  h3(actionLink("link_to_regional_tab", "Regional analysis"))
                ),
                regional_text(), # defined in R/dashboard_text.R
                br(),
                tags$div(
                  title = "This section is useful if you want to understand which industries your subject of study can lead to.",
                  h3(actionLink("link_to_subjectByIndustry_tab", "Subject by industry tables"))
                ),
                sub_by_ind_text(), # defined in R/dashboard_text.R
                tags$div(
                  title = "This section is useful if you want to understand which subject to study to access certain industries.",
                  h3(actionLink("link_to_industryBySubject_tab", "Industry by subject tables"))
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
