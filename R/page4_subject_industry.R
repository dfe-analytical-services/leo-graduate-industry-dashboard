### This code writes a function that sets up the subject by industry page

# Subject by industry tab =================================================

subject_by_industry_page <- function() {
  tabPanel(
    #  tags$div(title = "This section is useful if you want to understand which industries your subject of study can lead to.", "Subject by industry"),
    # Cathie changed the name that's displayed from Subject by industry to Industry by subject
    tags$div(title = "This section provides information about which industries graduates of different subjects worked in.", "Industry by subject"),
    value = "subjectByIndustry",

    ## Heading =====================================================
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Industries that graduates work in by the subject area they studied, 2021-22 tax year")
        )
      )
    ),


    ## Expandable section =======================================================
    column(
      width = 12,
      expandable(
        input_id = "details", label = textOutput("subject_by_industry_dropdown_label"),
        contents =
          div(
            id = "div_a",
            ### User selection dropdowns ========================================
            gov_row(
              column(
                width = 6,
                selectizeInput(
                  "earningsbutton",
                  label = "View the proportion of graduates in each industry, or the median earnings of these graduates",
                  choices = list("Proportions", "Median earnings"),
                  selected = "Proportions"
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  "qualinput3",
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
                  "YAGinput2",
                  label = "Select year after graduation",
                  choices = list(1, 3, 5, 10),
                  selected = 5
                )
              ),
              column(
                width = 6,
                selectizeInput("crosstabs.subjectinput",
                  label = "Select a subject area",
                  choices = unique(c("All", sort(qual_subjects$subject_name))),
                  selected = "All"
                )
              ),
              column(
                width = 6,
                selectizeInput("countinput2",
                  label = "Choose a breakdown",
                  choices = list(
                    "Sex" = "sex",
                    "Ethnicity" = "ethnicity",
                    "Current region" = "current_region",
                    "Free school meals (FSM)" = "FSM",
                    "Prior attainment" = "prior_attainment",
                    "Subject" = "subject_name",
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




    ## Main panel =========================================================

    #  mainPanel(
    #   width = 10,
    #  style = "height: 90vh; overflow-y: auto; overflow-x: auto;",



    #     htmlOutput("crosstab_title"),   ### this is defined in the server.R script


    ## Side bar =============================================================

    #    sidebarLayout(
    #     sidebarPanel(
    #      width = 2,

    ### Helptext --------------------------------------------------------

    #     helpText("Create your own table by selecting from the drop down boxes below."),

    ### Proportions / earnings input ------------------------------------

    #      selectInput("earningsbutton",
    #       label = "View the proportion of graduates in each industry, or the median earnings of these graduates",
    #      choices = list("Proportions", "Median earnings"),
    #     selected = "Proportions"
    #  ),
    #
    #       ### Degree input ----------------------------------------------------
    #
    #
    #       selectInput("qualinput3",
    #        label = "Select qualification level",
    #       choices = list(
    #        "First degree",
    #       "Level 7 (taught)",
    #      "Level 7 (research)",
    #     "Level 8"
    #  ),
    # selected = "First degree"
    #        ),

    ### YAG input -------------------------------------------------------

    #       selectInput("YAGinput2",
    #        label = "Select a year after graduation",
    #       choices = list(1, 3, 5, 10),
    #      selected = 5
    #   ),

    ### Subject input ---------------------------------------------------


    #        selectInput("crosstabs.subjectinput",
    #         label = "Select a subject area",
    #        choices = unique(c("All", sort(qual_subjects$subject_name))),
    #       selected = "All"
    #    ),
    #
    ### Breakdown input -------------------------------------------------

    #       selectInput("countinput2",
    #        label = "Choose a breakdown",
    #       choices = list(
    #        "Sex" = "sex",
    #       "Ethnicity" = "ethnicity",
    #      "Current region" = "current_region",
    #     "Free school meals (FSM)" = "FSM",
    #    "Prior attainment" = "prior_attainment",
    #   "Subject" = "subject_name",
    #  "Qualification level" = "qualification_TR"
    #          ),
    #         selected = "sex"
    #      ),

    ### Download data ---------------------------------------------------

    #        helpText("Download the current table as a csv"),
    #       downloadButton("downloadData", "Download table"),
    #      helpText("Note that the downloaded data will not retain the ordering in the displayed table and will instead order the data alphabetically by industry and sub-industry."),
    #   ),

    ### Crosstab table --------------------------------------------------
    ### Crosstab text ---------------------------------------------------

    div(
      "Please note that summed figures in the following text may differ slightly from figures in the table due to rounding.",
      br(),
      br(),
      htmlOutput("crosstab_text")
    ),
    withSpinner(reactableOutput("crosstab")),

    ### Caveats ---------------------------------------------------------
    br(),
    strong("Footnotes"),
    br(),
    paste("1. Outcome percentages are rounded to the nearest 0.1%."),
    br(),
    paste("2. Earnings figures are rounded to the nearest £100"),
    br(),
    paste("3. All populations are rounded to the nearest 5 full-person equivalent (FPE) individuals."),
    br(),
    paste("4. c = data has been supressed due to small numbers. x = there is no result available (N/A)"),
    caveats_box() # defined in R/caveats.R
  )
  #  ) # end of tabPanel
  #  ) # end of subject by industry tab
}
