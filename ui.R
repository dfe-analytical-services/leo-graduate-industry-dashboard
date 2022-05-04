fluidPage(
  shinyjs::useShinyjs(),
  includeCSS("www/dfe_shiny_gov_style.css"),
  title = "LEO Graduate Industry dashboard",
  # use_tota11y(), # accessibility layer for local testing

  # Set metadata for browser ==================================================

  tags$html(lang = "en"),
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
  HTML("<title>Longitudinal Education Outcomes (LEO): Graduate Industry, Tax year 2018-19</title>"),

  # Navbar ====================================================================

  # This CSS sets the 7th item on the navbar to the right
  tagList(
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(7) {
                           float: right;
                           }
                           ")))
  ),
  navbarPage("",
    id = "navbar",

    # Homepage tab ============================================================

    tabPanel(
      value = "homepage", title = "Homepage",

      ## Tab content ----------------------------------------------------------

      fluidPage(
        fluidRow(
          column(
            12,
            h1("Longitudinal Education Outcomes (LEO): Graduate Industry, Tax year 2018-19"),
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
                  h3(actionLink("link_to_industryFlow_tab", "Industry flow analysis")),
                  industry_flow_text(), # defined in R/dashboard_text.R
                  br(),
                  h3(actionLink("link_to_regional_tab", "Regional analysis")),
                  regional_text(), # defined in R/dashboard_text.R
                  br(),
                  h3(actionLink("link_to_subjectByIndustry_tab", "Subject by industry tables")),
                  sub_by_ind_text(), # defined in R/dashboard_text.R
                  h3(actionLink("link_to_industryBySubject_tab", "Industry by subject tables")),
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
    ), # End of homepage tabPanel()

    # Industry flow tab =======================================================

    tabPanel(
      value = "industryFlow", title = "Industry flow",

      ## Side bar =============================================================

      sidebarLayout(
        sidebarPanel(
          width = 2,

          ### Help text -------------------------------------------------------

          helpText("Create a Sankey chart using the dropdowns below."),
          helpText("Switch between the Sankey and the proportions table using the tabs on the right."),

          ### Degree input ----------------------------------------------------

          selectInput("qualinput",
            label = "Choose graduate qualification level",
            choices = list(
              "First degree",
              "Level 7 (taught)",
              "Level 7 (research)",
              "Level 8"
            ),
            selected = "First degree"
          ),

          ### Subject input ---------------------------------------------------

          selectizeInput("indflow.subjectinput",
            label = "Select a subject area",
            choices = unique(c("All", sort(qual_subjects$subject_name))),
            selected = "All"
          ),

          ### Sex input -------------------------------------------------------

          selectInput("sexinput",
            label = "View by graduate sex",
            choices = list(
              "Female & Male" = "F+M",
              "Female" = "F",
              "Male" = "M"
            ),
            selected = "F+M"
          )
        ), # end of sidebar

        ## Main panel =========================================================

        mainPanel(
          width = 10,
          style = "height: 90vh; overflow-y: auto;",

          ### Title  ----------------------------------------------------------

          htmlOutput("sankey_title"),
          br(),
          strong("Most popular industry"),
          htmlOutput("sankeytext1"),
          br(),
          strong("Movement between industries"),
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
                help_text = "The coloured bars represent graduates in that
                industry at each year after graduation, and the grey flow lines
                show the movement of these graduates from one year after
                graduation on the left, to three years of graduation in the
                middle, to five years after graduation on the right side. Hover
                your mouse over a bar or flow line to see the number of
                graduates it represents."
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
              withSpinner(
                sankeyNetworkOutput(outputId = "sankey", height = 800)
              )
            ),

            #### Table --------------------------------------------------------

            tabPanel(
              "Industry proportions table",
              withSpinner(reactableOutput("sankey_table")),
              height = 1500
            )
          ),

          ### Caveats ---------------------------------------------------------

          caveats_box() # defined in R/caveats.R
        ),
      ),
    ),

    # Regional analysis tab ===================================================

    tabPanel(
      value = "regional", title = "Regional",

      ## Side bar =============================================================

      sidebarLayout(
        sidebarPanel(
          width = 2,

          ### Help text -------------------------------------------------------

          helpText("Create a map and Sankey chart to show graduate movement between study region and current region
                   using the dropdowns below."),

          ### Degree input ----------------------------------------------------

          selectInput("qualinput2",
            label = "Choose graduate qualification level",
            choices = list(
              "First degree",
              "Level 7 (taught)",
              "Level 7 (research)",
              "Level 8"
            ),
            selected = "First degree"
          ),

          ### YAG input -------------------------------------------------------

          selectInput("YAGinput",
            label = "Select year after graduation",
            choices = list(1, 3, 5, 10),
            selected = 5
          ),

          ### Industry input --------------------------------------------------

          selectInput("sectionnameinput",
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
          ),

          ### Subject input ---------------------------------------------------

          selectInput("regions.subjectinput",
            label = "Select a subject area",
            choices = unique(c("All", sort(qual_subjects$subject_name))),
            selected = "All"
          ),
        ),

        ## Main panel =========================================================

        mainPanel(
          width = 10,
          style = "height: 90vh; overflow-y: auto;",

          ### Summary text ----------------------------------------------------

          div(
            h3("Regional summary"),
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
                  style = "height: 100%; overflow-y: visible",
                  div(selectInput("countinput",
                    label = "Select which statistic to view in the map below",
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

                withSpinner(leafletOutput(outputId = "map", height = 470))
              ),
              column(
                6,
                h3(htmlOutput("regional_sankey_title")),
                details(
                  inputId = "sankeyhelp",
                  label = "How to read this sankey",
                  help_text = "The coloured bars represent graduates in that
                region, and the grey flow lines show the movement of these graduates from region of study
                on the left, to current region of residence on the right. Hover
                your mouse over a bar or flow line to see the number of
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
              )
            ),

            #### Table --------------------------------------------------------

            tabPanel(
              "Regional table",
              h3("Regional table"),
              div(
                # Set as well but override sidebar defaults
                class = "well",
                style = "height: 100%; overflow-y: visible",

                #### Regional input -------------------------------------------

                div(selectizeInput("regioninput",
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
                  selected = "London", multiple = FALSE,
                  options = list(maxItems = 9, placeholder = "Start typing a region")
                ))
              ),

              #### Table ------------------------------------------------------

              strong("Click a column header to sort the table"),
              br(),
              withSpinner(reactableOutput("maptable")),
              br(),
              strong("Please note that the table only shows results for the selected industry, subject and year after graduation.")
            )
          ),

          ## Caveats ----------------------------------------------------------

          caveats_box() # defined in R/caveats.R
        ) # end of main panel
      ), # end of sidebar layout
    ), # end of regional tab

    # Subject by industry tab =================================================

    tabPanel(
      title = "Subject by industry", value = "subjectByIndustry",

      ## Side bar =============================================================

      sidebarLayout(
        sidebarPanel(
          width = 2,

          ### Helptext --------------------------------------------------------

          helpText("Create your own table by selecting from the drop down boxes below."),

          ### Proportions / earnings input ------------------------------------

          selectInput("earningsbutton",
            label = "View the proportion of graduates in each industry, or the median earnings of these graduates",
            choices = list("Proportions", "Median earnings"),
            selected = "Proportions"
          ),

          ### Degree input ----------------------------------------------------

          conditionalPanel(
            condition = "input.countinput2 == 'sex' || input.countinput2 == 'subject_name'",
            selectInput("qualinput3",
              label = "Select qualification level",
              choices = list(
                "First degree",
                "Level 7 (taught)",
                "Level 7 (research)",
                "Level 8"
              ),
              selected = "First degree"
            )
          ),

          ### YAG input -------------------------------------------------------

          selectInput("YAGinput2",
            label = "Select a year after graduation",
            choices = list(1, 3, 5, 10),
            selected = 5
          ),

          ### Subject input ---------------------------------------------------

          conditionalPanel(
            condition = "input.countinput2 != 'subject_name'",
            selectInput("crosstabs.subjectinput",
              label = "Select a subject area",
              choices = unique(c("All", sort(qual_subjects$subject_name))),
              selected = "All"
            )
          ),

          ### Breakdown input -------------------------------------------------

          selectInput("countinput2",
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
          ),

          ### Download data ---------------------------------------------------

          helpText("Download the current table as a csv"),
          downloadButton("downloadData", "Download table"),
          br(),
          br()
        ),

        ## Main panel =========================================================

        mainPanel(
          width = 10,
          style = "height: 90vh; overflow-y: auto; overflow-x: auto;",

          ### Crosstab table --------------------------------------------------

          htmlOutput("crosstab_title"),

          ### Crosstab text ---------------------------------------------------

          div(
            htmlOutput("crosstab_text")
          ),
          withSpinner(reactableOutput("crosstab")),

          ### Caveats ---------------------------------------------------------

          caveats_box() # defined in R/caveats.R
        )
      ),
    ), # end of subject by industry tab

    # Industry by subject tab =================================================
    tabPanel(
      title = "Industry by subject", value = "industryBySubject",

      ## Side bar =============================================================

      sidebarLayout(
        sidebarPanel(
          width = 2,

          ### Help text -------------------------------------------------------

          helpText("Create your own table by selecting from the drop down boxes below."),

          ### Proportions / earnings input ------------------------------------

          selectInput("earningsbutton2",
            label = "View the proportion of graduates in each industry, or the median earnings of these graduates",
            choices = list("Proportions", "Median earnings"),
            selected = "Proportions"
          ),

          ### Degree input ----------------------------------------------------

          conditionalPanel(
            condition = "input.countinput3 == 'sex' || input.countinput3 == 'subject_name'",
            selectInput("qualinput4",
              label = "Select qualification level",
              choices = list(
                "First degree",
                "Level 7 (taught)",
                "Level 7 (research)",
                "Level 8"
              ),
              selected = "First degree"
            )
          ),

          ### YAG input -------------------------------------------------------

          selectInput("YAGinput3",
            label = "Select a year after graduation",
            choices = list(1, 3, 5, 10),
            selected = 5
          ),

          ### Industry input -------------------------------------------------

          conditionalPanel(
            condition = "input.countinput3 != 'SECTIONNAME'",
            selectInput("sectionnameinput2",
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

          ### Group input -----------------------------------------------------

          conditionalPanel(
            condition = "input.countinput3 != 'SECTIONNAME'",
            selectizeInput("groupinput",
              label = "View 3-digit SIC groups within the selected industry",
              choices = unique(c("All", sort(industry_groups$group_name))),
              selected = "All", multiple = FALSE
            )
          ),


          ### Breakdown input -------------------------------------------------

          selectInput("countinput3",
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
          ),
          
          ### Download data ---------------------------------------------------
          
          helpText("Download the current table as a csv"),
          downloadButton("IndSubjDownload", "Download table"),
          br(),
          br()

        ), # end of sidebar

        ## Main panel =========================================================

        mainPanel(
          width = 10,
          style = "height: 90vh; overflow-y: auto; overflow-x: auto;",

          ### Backwards crosstab ----------------------------------------------

          htmlOutput("backwards_crosstab_title"),
          withSpinner(reactableOutput("crosstab_backwards")),

          ### Caveats ---------------------------------------------------------

          caveats_box() # defined in R/caveats.R
        )
      ),
    ), # end of industry by subject tab

    # Accessibility ===========================================================

    tabPanel(
      "Accessibility",
      warning_text(inputId = "accessWarn", text = "THIS IS A DRAFT STATEMENT - NEEDS UPDATING AFTER TESTING"),
      accessibility_statement() # defined in R/accessibility_statement.R
    ),

    # Support links ===========================================================

    tabPanel(
      "Support and feedback",
      support_links() # defined in R/supporting_links.R
    )
  ), # end of navbar page

  # Footer ====================================================================

  shinyGovstyle::footer(TRUE)
) # end of fluid page
