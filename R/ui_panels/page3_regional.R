###
### This code writes the function that sets up the Regions page
###


regional_page <- function() {
  tabPanel(
    value = "regional",
    tags$div(title = "This page provides information about where graduates studied and lived after graduating.", "Regional"),

    ## Heading ==================================================================
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Regions in which graduates studied and lived after graduating."),
          br(),
          h3("Coverage is graduates who were in sustained employment during the 2021-22 tax year.")
        )
      )
    ),

    ## Expandable section ===================================================
    column(
      width = 12,
      expandable(
        input_id = "details", label = textOutput("regional_dropdown_label"),
        contents =
          div(
            id = "div_a",
            ### User selection dropdowns ========================================
            gov_row(
              column(
                width = 6,
                selectizeInput(
                  "regional_input_qual",
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
                  "regional_input_YAG",
                  label = "Select year after graduation",
                  choices = list(1, 3, 5, 10),
                  selected = 5
                )
              ),
              column(
                width = 6,
                selectizeInput("regional_input_industry",
                  label = "Choose an industry area",
                  list(
                    "Accommodation and food service activities",
                    "Activities of extraterritorial organisations and bodies",
                    "Activities of households as employers - undifferentiated goods-and services-producing activities of households for own use",
                    "Administrative and support service activities",
                    "Agriculture, forestry and fishing",
                    "Arts, entertainment and recreation",
                    "Construction",
                    "Education",
                    "Electricity, gas, steam and air conditioning supply",
                    "Financial and insurance activities",
                    "Human health and social work activities",
                    "Information and communication",
                    "Manufacturing",
                    "Mining and quarrying",
                    "Other service activities",
                    "Professional, scientific and technical activities",
                    "Public administration and defence - compulsory social security",
                    "Real estate activities",
                    "Transportation and storage",
                    "Water supply - sewerage, waste management and remediation activities",
                    "Wholesale and retail trade - repair of motor vehicles and motorcycles"
                  ),
                  selected = "Education"
                ),
                column(
                  width = 6,
                  selectizeInput(
                    "regional_input_subject",
                    label = "Select a subject area studied",
                    choices = unique(c("All", sort(qual_subjects$subject_name))),
                    selected = "All"
                  )
                ),
              )
            )
          )
      )
    ),















    ## Side bar =============================================================

    #    sidebarLayout(
    #     sidebarPanel(
    #      width = 2,

    ### Help text -------------------------------------------------------

    #        helpText("Create a map and Sankey chart to show graduate movement between study region and current region
    #                  using the dropdowns below."),

    ### Degree input ----------------------------------------------------

    #        selectInput("qualinput2",
    #         label = "Choose graduate qualification level",
    #        choices = list(
    #         "First degree",
    #        "Level 7 (taught)"
    #     ),
    #    selected = "First degree"
    # ),

    ### YAG input -------------------------------------------------------

    #        selectInput("YAGinput",
    #         label = "Select year after graduation",
    #        choices = list(1, 3, 5, 10),
    #       selected = 5
    #    ),

    ### Industry input --------------------------------------------------

    #        selectInput("sectionnameinput",
    #         label = "Choose an industry area",
    #        choices = list(
    #            "Accommodation and food service activities",
    #           "Activities of extraterritorial organisations and bodies",
    #          "Activities of households as employers - undifferentiated goods-and services-producing activities of households for own use",
    #         "Administrative and support service activities",
    #        "Agriculture, forestry and fishing",
    #       "Arts, entertainment and recreation",
    #      "Construction",
    #     "Education",
    #    "Electricity, gas, steam and air conditioning supply",
    #   "Financial and insurance activities",
    #  "Human health and social work activities",
    # "Information and communication",
    #            "Manufacturing",
    #           "Mining and quarrying",
    #          "Other service activities",
    #         "Professional, scientific and technical activities",
    #        "Public administration and defence - compulsory social security",
    #       "Real estate activities",
    #      "Transportation and storage",
    #     "Water supply - sewerage, waste management and remediation activities",
    #          "Wholesale and retail trade - repair of motor vehicles and motorcycles"
    #          ),
    #         selected = "Education"
    #      ),

    ### Subject input ---------------------------------------------------

    #        selectInput("regions.subjectinput",
    #         label = "Select a subject area",
    #          choices = unique(c("All", sort(qual_subjects$subject_name))),
    #         selected = "All"
    #      ),
    #   ),

    ## Main panel =========================================================

    mainPanel(
      width = 12,
      style = "height: 90vh; overflow-y: auto;",

      ### Summary text ----------------------------------------------------

      div(
        htmlOutput("maptext"),
        br(),
        htmlOutput("maptext2"),
        br()
      ),

      ### Tabs ------------------------------------------------------------

      tabsetPanel(

        #### Map ----------------------------------------------------------
        tabPanel(
          "Map and sankey",
          column(
            6,
            h3(htmlOutput("map_title")),
            div(
              # Set as well but override sidebar defaults
              class = "well",
              style = "min-height: 100%; height: 100%; overflow-y: visible",
              div(selectInput("countinput",
                label = "Select statistic to view in the map below",
                choices = list(
                  "Studied in region" = "trained_in_region",
                  "Currently living in region" = "living_in_region",
                  "Difference (n)" = "difference",
                  "Difference (%)" = "difference_prop"
                ),
                selected = "trained_in_region"
              ))
            ),

            # details(
            #   inputId = "maphelp",
            #   label = "How to read this map",
            #   help_text = "Click on a region to see all of the information for
            #    that region, including the number of providers and the
            #    median earnings for the selected industry, subject section and year after graduation."
            # ),

            withSpinner(leafletOutput(outputId = "map", height = 470)),
            #          div(
            #            # Set as well but override sidebar defaults
            #            class = "well",
            #            style = "min-height: 100%; height: 100%; overflow-y: visible",
            strong("Footnotes"),
            br(),
            paste("1. Outcome percentages are rounded to the nearest 0.1%. Rounding reflects the precision of our data and makes it easier to read figures."),
            br(),
            paste("2. Earnings figures are rounded to the nearest £100."),
            br(),
            paste("3. All populations are rounded to the nearest 5 full-person equivalent (FPE) individuals."),
            br(),
            paste("4. c = data has been supressed due to small numbers."),
            br(),
            paste("5. x = there is no result available (N/A)"),
            br(),
            br()
            #          )
          ),
          column(
            6,
            h3(htmlOutput("regional_sankey_title")),
            details(
              inputId = "sankeyhelp",
              label = "How to read this sankey",
              help_text = "The coloured bars represent the numbers of graduates in each
                region. The grey flow lines show the movement of graduates from the regions where they studied
                (on the left) to the region where they are living in the current tax year (on the right).
                You can hover your mouse over a bar or flow line to see the number of
                graduates it represents."
            ),
            column(
              6,
              "Region of study"
            ),
            column(6, div(
              "Current region",
              style = "text-align: right"
            )),
            withSpinner(sankeyNetworkOutput("regional_sankey")),
            br(),
            br()
          ),
        ),


        #### Table --------------------------------------------------------

        tabPanel(
          "Regional table",
          h3("Regional table"),
          div(
            # Set as well but override sidebar defaults
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",

            #### Regional input -------------------------------------------

            div(
              selectizeInput("regioninput",
                label = "Select multiple regions from the dropdown below to compare.",
                choices = list(
                  "North East",
                  "North West",
                  "Yorkshire and the Humber",
                  "East Midlands",
                  "West Midlands",
                  "East of England",
                  "London",
                  "South East",
                  "South West"
                ),
                selected = "North East", multiple = FALSE,
                options = list(maxItems = 9, placeholder = "Start typing a region")
              )
            )
          ),

          #### Table ------------------------------------------------------

          strong("Click a column header to sort the table"),
          br(),
          withSpinner(reactableOutput("maptable")),
          br(),
          strong("Please note that the table only shows results for the selected industry, subject and year after graduation."),
          br(), br(),
          strong("Footnotes"),
          br(),
          paste("1. Outcome percentages are rounded to the nearest 0.1%. Rounding reflects the precision of our data and makes figures easier to read"),
          br(),
          paste("2. Earnings figures are rounded to the nearest £100."),
          br(),
          paste("3. All populations are rounded to the nearest 5 full-person equivalent (FPE) individuals."),
          br(),
          paste("4. c = data has been supressed due to small numbers."),
          br(),
          paste("5. x = there is no result available (N/A)")
        )
      ),

      ## Caveats ----------------------------------------------------------

      caveats_box_regional() # defined in R/caveats.R
    ) # end of mainPanel layout on line 181
  ) # end of tabPanel on line 6
}
# end of regional tab
