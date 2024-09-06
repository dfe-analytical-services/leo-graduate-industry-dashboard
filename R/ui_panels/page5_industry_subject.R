### Creates the function that makes the industry by subject pate

industry_by_subject_page <- function() {
  tabPanel(
    #    tags$div(title = "This section is useful if you want to understand which subject to study to access certain industries.", "Industry by subject"),
    # Cathie changed the name that's displayed from Industry by subject to Subject by industry
    tags$div(title = "This section provides information about the subjects that graduates studied before working in a particular industry.", "Subject by industry"),
    value = "industryBySubject",


    ## Heading ==================================================================
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Subject areas that graduates studied before working in a particular industry."),
          h3("Coverage is all graduates from HE providers in England who were in sustained employment during the 2021-22 tax year.")
        )
      )
    ),

    ## Expandable section ===================================================
    column(
      width = 12,
      expandable(
        input_id = "details", label = textOutput("industry_by_subject_dropdown_label"),
        contents =
          div(
            id = "div_a",
            ### User selection dropdowns ========================================
            gov_row(
              column(
                width = 6,
                selectizeInput(
                  "earningsbutton2",
                  label = "View the proportion of graduates in each industry, or the median earnings of these graduates",
                  choices = list("Proportions", "Median earnings"),
                  selected = "Proportions"
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  "qualinput4",
                  label = "Select a qualification",
                  choices = list(
                    "First degree",
                    "Level 7 (taught)",
                    "Level 7 (research)",
                    "Level 8"
                  ),
                  selected = "First degree"
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  "YAGinput3",
                  label = "Select year after graduation",
                  choices = list(1, 3, 5, 10),
                  selected = 5
                )
              ),
              column(
                width = 6,
                selectizeInput("sectionnameinput2",
                  label = "Choose an industry area",
                  choices = list(
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
                )
              ),
              column(
                width = 6,
                selectizeInput("groupinput",
                  label = "Select area within industry (3 digit SIC group)",
                  choices = unique(c("All", sort(industry_groups$group_name))),
                  selected = "All"
                )
              ),
              column(
                width = 6,
                selectizeInput("countinput3",
                  label = "Choose a breakdown",
                  choices = list(
                    "Sex" = "sex",
                    "Ethnicity" = "ethnicity",
                    "Current region" = "current_region",
                    "Free school meals (FSM)" = "FSM",
                    "Prior attainment" = "prior_attainment",
                    "Industry" = "SECTIONNAME",
                    "Qualification level" = "qualification_TR"
                  ),
                  selected = "sex"
                )
              ),

              ### Download button ---------------------------------------------------

              column(
                width = 12,
                paste("Download the current data as a csv file"),
                br(),
                tags$p("Note that the downloaded data is ordered alphabetically by subject area studied, whereas the table below can be ordered by any column."),
                br(),
                downloadButton(
                  outputId = "downloadData",
                  label = "Download table",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                )
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

    #     helpText("Create your own table by selecting from the drop down boxes below."),

    ### Proportions / earnings input ------------------------------------

    #    selectInput("earningsbutton2",
    #     label = "View the proportion of graduates in each industry, or the median earnings of these graduates",
    #    choices = list("Proportions", "Median earnings"),
    #   selected = "Proportions"
    # ),

    ### Degree input ----------------------------------------------------

    #        selectInput("qualinput4",
    #         label = "Select qualification level",
    #        choices = list(
    #         "First degree",
    #        "Level 7 (taught)",
    #       "Level 7 (research)",
    #      "Level 8"
    #   ),
    #  selected = "First degree"
    #        ),

    ### YAG input -------------------------------------------------------

    #        selectInput("YAGinput3",
    #         label = "Select a year after graduation",
    #        choices = list(1, 3, 5, 10),
    #       selected = 5
    #    ),

    ### Industry input -------------------------------------------------

    #        selectInput("sectionnameinput2",
    #         label = "Choose an industry area",
    #        choices = list(
    #         "Accommodation and food service activities",
    #        "Activities of extraterritorial organisations and bodies",
    #       "Activities of households as employers - undifferentiated goods-and services-producing activities of households for own use",
    #      "Administrative and support service activities",
    #     "Agriculture, forestry and fishing",
    #    "Arts, entertainment and recreation",
    #   "Construction",
    #  "Education",
    # "Electricity, gas, steam and air conditioning supply",
    #            "Financial and insurance activities",
    #           "Human health and social work activities",
    #          "Information and communication",
    #         "Manufacturing",
    #        "Mining and quarrying",
    #       "Other service activities",
    #      "Professional, scientific and technical activities",
    #     "Public administration and defence - compulsory social security",
    #    "Real estate activities",
    #   "Transportation and storage",
    #  "Water supply - sewerage, waste management and remediation activities",
    # "Wholesale and retail trade - repair of motor vehicles and motorcycles"
    #          ),
    #         selected = "Education"
    #      ),

    ### Group input -----------------------------------------------------

    #        selectizeInput("groupinput",
    #         label = "View 3 digit SIC groups within the selected industry",
    #        choices = unique(c("All", sort(industry_groups$group_name))),
    #       selected = "All", multiple = FALSE
    #    ),


    ### Breakdown input -------------------------------------------------

    #   selectInput("countinput3",
    #    label = "Choose a breakdown",
    #   choices = list(
    #    "Sex" = "sex",
    #   "Ethnicity" = "ethnicity",
    #  "Current region" = "current_region",
    # "Free school meals (FSM)" = "FSM",
    # "Prior attainment" = "prior_attainment",
    #            "Industry" = "SECTIONNAME",
    #           "Qualification level" = "qualification_TR"
    #        ),
    #       selected = "sex"
    #    ),

    ### Download data ---------------------------------------------------

    #   helpText("Download the current table as a csv"),
    #  downloadButton("IndSubjDownload", "Download table"),
    # helpText("Note that the downloaded data will not retain the ordering in the displayed table and will instead order the data alphabetically by subject.")
    #      ), # end of sidebar

    ## Main panel =========================================================

    #    mainPanel(
    #     width = 15,
    #    style = "height: 90vh; overflow-y: auto; overflow-x: auto;",

    ### Backwards crosstab ----------------------------------------------

    #    htmlOutput("backwards_crosstab_title"),
    # Cathie adds the next rows (fluidRow, column, and width to wrap around the original line of code)
    fluidRow(
      column(
        width = 12,
        withSpinner(reactableOutput("crosstab_backwards"))
      )
    ),

    ### Caveats ---------------------------------------------------------
    br(),
    strong("Footnotes"),
    br(),
    paste("1. Outcome percentages are rounded to the nearest 0.1%. Rounding reflects the precision of our data, and makes it easier to read figures."),
    br(),
    paste("2. Earnings figures are rounded to the nearest £100."),
    br(),
    paste("3. All populations are rounded to the nearest 5 full-person equivalent (FPE) individuals."),
    br(),
    paste("4. c = data has been supressed due to small numbers."),
    br(),
    paste("5. x = there is no result available (N/A)."),
    caveats_box() # defined in R/caveats.R
  )
}
