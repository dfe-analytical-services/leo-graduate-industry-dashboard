### This code writes a function that sets up the industry flow tab

industry_flow_page <- function() {
  # Industry flow tab =======================================================

  tabPanel(
    value = "industryFlow",
    tags$div(title = "This section shows how graduates of each subject move into different industries.", "Industry flow"),

    ## Heading ==================================================================
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Industries in which graduates are employed one, three and five years after graduation"),
          #          h3("... by qualification, subject area studied, and sex."),
          h3("Graduates from 2015-16 academic year"),
        )
      ),

      ## Expandable section ===================================================
      column(
        width = 12,
        expandable(
          input_id = "details", label = textOutput("dropdown_label"),
          contents =
            div(
              id = "div_a",
              ### User selection dropdowns ========================================
              gov_row(
                column(
                  width = 6,
                  selectizeInput(
                    "qualinput",
                    label = "Select a qualification",
                    choices = list(
                      "First degree",
                      "Level 7 (taught)"
                    ),
                    selected = "First degree"
                  )
                ),
                column(
                  width = 6,
                  selectizeInput(
                    "indflow.subjectinput",
                    label = "Select a subject area studied",
                    choices = unique(c("All", sort(qual_subjects$subject_name))),
                    selected = "All"
                  )
                ),
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "sexinput",
                    label = "Select graduate sex",
                    choices = list(
                      "Female & male" = "F+M",
                      "Female" = "F",
                      "Male" = "M"
                    ),
                    selected = "F+M"
                  )
                ),
              )
            )
        ),


        ## Side bar =============================================================

        #     sidebarLayout(
        #      sidebarPanel(
        #       width = 2,

        ### Help text -------------------------------------------------------

        #          helpText("Create a Sankey chart using the dropdowns below."),
        #         helpText("Switch between the Sankey and the proportions table using the tabs on the right."),

        ### Degree input ----------------------------------------------------

        #          selectInput("qualinput",
        #           label = "Choose graduate qualification level",
        #          choices = list(
        #           "First degree",
        #          "Level 7 (taught)"
        #       ),
        #      selected = "First degree"
        #   ),

        ### Subject input ---------------------------------------------------

        #          selectizeInput("indflow.subjectinput",
        #           label = "Select a subject area",
        #          choices = unique(c("All", sort(qual_subjects$subject_name))),
        #         selected = "All"
        #      ),

        ### Sex input -------------------------------------------------------

        #          selectInput("sexinput",
        #           label = "View by graduate sex",
        #          choices = list(
        #           "Female & Male" = "F+M",
        #          "Female" = "F",
        #         "Male" = "M",
        #      selected = "F+M"
        #    ),
        #   )
        #        )
        #       ), # end of sidebar

        ## Main panel =========================================================

        mainPanel(
          #          width = 10,
          width = 25,
          style = "height: 90vh; overflow-y: auto;",

          ### Title  ----------------------------------------------------------

          #          htmlOutput("sankey_title"),
          #         br(), # this makes a line break
          # strong("Most popular industry"),
          htmlOutput("sankeytext1"),
          br(),
          # strong("Movement between industries"),
          htmlOutput("sankeytext2"),
          br(),

          ### Tabs ------------------------------------------------------------

          tabsetPanel(

            #### Sankey plot --------------------------------------------------
            tabPanel(
              "Industry flow sankey plot",
              br(),
              details(
                inputId = "sankeyhelp",
                label = "How to read this sankey",
                help_text = "The coloured bars represent numbers of graduates in each
                industry one, three and five years after graduation. The grey flow lines
                represent movement of graduates between industries between the same years
                after graduation. You can hover
                your mouse over a bar or flow line to see the number of
                graduates it represents. Note that this chart displays the
                nine industries that most graduates work in and the rest are grouped as 'Other'.
                To see the full breakdown of industries please view the Industry proportions table."
              ),
              column(
                4,
                "1 year after graduation"
              ),
              column(4, div(
                "3 years after graduation",
                style = "text-align: center"
              )),
              column(
                4,
                div("5 years after graduation", style = "text-align: right")
              ),
              # conditionalPanel(
              #   condition = "!is.null(output$sankey_flag)",
              # withSpinner(
              #  uiOutput("sankey_flag")
              #   )
              # ),
              # conditionalPanel(
              #  condition = "is.null(output$sankey_flag)",
              withSpinner(
                sankeyNetworkOutput(outputId = "sankey", height = 800)
              ), br(),
              strong("Footnotes"),
              br(),
              paste("1. All populations are rounded to the nearest 5 full-person equivalent (FPE) individuals. This reflects the level of precision in our data. It is also easier to read.")
              # )
            ),

            #### Table --------------------------------------------------------

            tabPanel(
              "Industry proportions table",
              withSpinner(reactableOutput("sankey_table")),
              br(),
              strong("Footnotes"), ### defined
              br(),
              paste("1. Outcome percentages are rounded to the nearest 0.1%."),
              br(),
              paste("2. All populations are rounded to the nearest 5 full-person equivalent (FPE) individuals. This reflects the level of precision of our data and makes for easier reading."),
              br(),
              paste("3. c = data has been supressed due to small numbers. x = there is no result available (N/A)"),
              height = 1500
            )
          ),

          ### Caveats ---------------------------------------------------------

          caveats_box_flow() # defined in R/caveats.R
        ),
      ),
    ),
  )
}
