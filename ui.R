navbarPage("",
  id = "navbar", # No title so there's no fake clickable link that isn't actually clickable, will need to check for accessibility

  # Homepage tab ------------------------------------------------------------------------------------

  tabPanel(
    value = "homepage", title = "Homepage",

    ## Style sheet ------------------------------------------------------------------------------

    includeCSS("www/shiny_gov_style.css"),

    ## Set metadata for browser ---------------------------------------------------------------------

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

    ## Tab content ---------------------------------------------------------------------------------

    fluidPage(
      titlePanel("Longitudinal Education Outcomes Graduate Industry Dashboard, Tax year 2018-19"),
      fluidRow(
        column(
          12,
          h2("Welcome"),
          paste(welcome_text, " For more detail see our official statistics "), # stored in www/text, read in via R/dashboard_text.R
          a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/graduate-outcomes-leo", "publication on Graduate Outcomes."),
        ),

        ## Left panel ------------------------------------------------------------------------------

        column(
          6,
          box(
            status = "primary", width = NULL, solidHeader = TRUE,
            h2("Contents"),
            h4(actionLink("link_to_industryFlow_tab", "Industry flow analysis")),
            paste(industryFlow_text), # stored in www/text, read in via R/dashboard_text.R
            br(),
            h4(actionLink("link_to_regional_tab", "Regional analysis")),
            paste(regional_text), # stored in www/text, read in via R/dashboard_text.R
            br(),
            h4("Tables"),
            h4(actionLink("link_to_subjectByIndustry_tab", "(1) Subject by industry")),
            h4(actionLink("link_to_industryBySubject_tab", "(2) Industry by subject")),
            paste(tables_text), # stored in www/text, read in via R/dashboard_text.R
            br(),
            tags$ul(
              tags$li("sex"),
              tags$li("ethnicity"),
              tags$li("FSM status"),
              tags$li("current region"),
              tags$li("prior attainment"),
              tags$li("subject"),
              tags$li("qualification level")
            )
          )
        ),

        ## Right panel ---------------------------------------------------------------------------

        column(
          6,
          box(
            status = "primary", width = NULL, solidHeader = TRUE,
            h2("IDBR and SIC background"),
            h4("IDBR (Inter-Departmental Business Register)"),
            "IDBR data is a comprehensive list of UK businesses used by government for statistical purposes.",
            h4("UK SIC (Standard Industrial Classification) code"),
            "The UK Standard Industrial Classification (SIC) of economic activties is used to classify businesses by the type of activity they do.",
            h4("Useful links"),
            a(
              href = "https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic",
              "Standard industrial classification of economic activities (SIC) - GOV.UK.(www.gov.uk)"
            ),
            br(),
            a(
              href = "https://siccode.com/sic-code-lookup-directory",
              "SIC Code Lookup | SIC Code Search Tool"
            ),
            h4("SIC Groups and sections"),
            paste(sicGroups_text), # stored in www/text, read in via R/dashboard_text.R
            br(),
            br(),
            tags$ol(
              tags$li("ACCOMMODATION AND FOOD SERVICE ACTIVITIES"),
              tags$li("ACTIVITITES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES"),
              tags$li("ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS - UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE"),
              tags$li("ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES"),
              tags$li("AGRICULTURE, FORESTRY AND FISHING"),
              tags$li("ARTS, ENTERTAINMENT AND RECREATION"),
              tags$li("CONSTRUCTION"),
              tags$li("EDUCATION"),
              tags$li("ELECTRICITY, GAS, STEAM AND AIR CONDITIONG SUPPLY"),
              tags$li("FINANCIAL AND INSURANCE ACTIVITIES"),
              tags$li("HUMAN HEALTH AND SOCIAL WORK ACTIVITIES"),
              tags$li("INFORMATION AND COMMUNICATION"),
              tags$li("MANUFACTURING"),
              tags$li("MINING AND QUARRYING"),
              tags$li("OTHER SERVICE ACTIVITIES"),
              tags$li("PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES"),
              tags$li("PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY"),
              tags$li("REAL ESTATE ACTIVITIES"),
              tags$li("TRANSPORTATION AND STORAGE"),
              tags$li("WATER SUPPLY - SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES"),
              tags$li("WHOLESALE AND RETAIL TRADE - REPAIR OF MOTOR VEHICLES AND MOTORCYCLES")
            )
          )
        )
      )
    )
  ), # End of homepage tabPanel()

  # Industry flow tab ---------------------------------------------------------------

  tabPanel(
    value = "industryFlow", title = "Industry flow analysis",

    ## Set metadata for browser ----------------------------------------------------

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

    ## Caveats ---------------------------------------------------------------------

    caveats_box(), # defined in R/caveats.R
    br(),

    ## Side bar --------------------------------------------------------------------

    sidebarLayout(
      sidebarPanel(
        helpText("Create sankey charts for each subject showing one, three and five years after graduation (YAG)."),
        helpText("Switch between the sankey and the proportions table using the tabs on the right."),
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
        selectizeInput("indflow.subjectinput",
          label = "Select a subject area",
          choices = unique(c("All", sort(qual_subjects$subject_name))),
          selected = "All"
        ),
        selectInput("sexinput",
          label = "View by graduate sex",
          choices = list(
            "Female & Male" = "F+M",
            "Female" = "F",
            "Male" = "M"
          ),
          selected = "F+M"
        ),
        div(
          style = "color:#ffffff",
          h4("Summary"),
          strong("Most popular industry"),
          htmlOutput("sankeytext1"),
          br(),
          strong("Movement between industries"),
          htmlOutput("sankeytext2"),
          br()
        )
      ),

      ## Main panel -------------------------------------------

      mainPanel(
        htmlOutput("sankey_title"),
        tabsetPanel(

          ### Sankey plot -------------------------------------

          tabPanel(
            "Sankey Plot",
            htmlOutput("sankeyhelp"),
            bsTooltip("sankeyhelp",
              "The coloured bars represent graduates in that industry at each year after graduation, and the grey flow lines show the movement of these graduates. Hover your mouse over a bar or flow line to see the number of graduates it represents.",
              placement = "left",
              trigger = "hover",
              options = NULL
            ),
            absolutePanel(strong("1 YAG"), top = "15%", left = 25),
            absolutePanel(strong("5 YAG"), top = "15%", right = 25),
            absolutePanel(strong("3 YAG"), top = "15%", left = "48%"),
            br(),
            withSpinner(sankeyNetworkOutput(outputId = "sankey", height = 800))
          ),

          ### Table ------------------------------------------

          tabPanel(
            "Industry proportions table",
            withSpinner(reactableOutput("sankey_table")),
            height = 1500
          )
        )
      )
    )
  ),

  # Regional analysis tab ------------------------------------------

  tabPanel(
    value = "regional", title = "Regional analysis",

    ## Set metadata for browser ----------------------------------------------------

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

    ## Caveats ---------------------------------------------------------------------

    caveats_box(), # defined in R/caveats.R
    br(),

    ## Side bar ----------------------------------------------------

    sidebarLayout(
      sidebarPanel(
        width = 3,
        helpText("Create a heat map to show graduate movement from study region to current region for the selected industry."),
        helpText("Click on a region to see all of the information for that region, including the number of providers and the median earnings for the selected industry."),
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
        selectInput("regions.subjectinput",
          label = "Select a subject area",
          choices = unique(c("All", sort(qual_subjects$subject_name))),
          selected = "All"
        ),
        selectInput("countinput",
          label = "View different statistics",
          choices = list(
            "Studied in region" = "trained_in_region",
            "Currently living in region" = "living_in_region",
            "Difference (n)" = "difference",
            "Difference (%)" = "difference_prop"
          ),
          selected = "trained_in_region"
        ),
        selectInput("YAGinput",
          label = "Select year after graduation",
          choices = list(1, 3, 5, 10),
          selected = 5
        ),
        div(
          style = "color:#ffffff",
          strong("Summary"),
          htmlOutput("maptext"),
          br(),
          htmlOutput("maptext2")
        )
      ),

      ## Main panel -------------------------------------------------------------

      mainPanel(
        htmlOutput("map_title"),
        width = 9, box(withSpinner(leafletOutput(outputId = "map", height = 800)), width = 5),
        box(
          width = 6, selectizeInput("regioninput",
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
          ),
          strong("Click a column header to sort the table"),
          br(),
          withSpinner(reactableOutput("maptable")),
          br(),
          strong("Please note that the table only shows results for the selected industry, subject and year after graudation."),
          br(),
          br(),
          htmlOutput("regional_sankey_title"),
          withSpinner(sankeyNetworkOutput("regional_sankey"))
        )
      )
    )
  ),

  # Subject by industry tab ---------------------------------------------
  tabPanel(
    title = "Subject by industry tables", value = "subjectByIndustry",

    ## Set metadata for browser ----------------------------------------------------

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

    ## Caveats ---------------------------------------------------------------------

    caveats_box(), # defined in R/caveats.R
    br(),

    ## Side bar ---------------------------------------------------------

    sidebarLayout(
      sidebarPanel(
        width = 3,
        helpText("Using the drop down boxes below, create your own table by selecting the breakdown, year after graduation, qualification level (available for sex and subject treakdowns only) and subject area you would like to view."),
        radioGroupButtons("earningsbutton",
          label = "View the proportion of graduates in each industry, or the median earnings of these graduates",
          choices = list("Proportions", "Median earnings"),
          selected = "Proportions"
        ),
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
        selectInput("YAGinput2",
          label = "Select a year after graduation",
          choices = list(1, 3, 5, 10),
          selected = 5
        ),
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
        conditionalPanel(
          condition = "input.countinput2 != 'subject_name'",
          selectInput("crosstabs.subjectinput",
            label = "Select a subject area",
            choices = unique(c("All", sort(qual_subjects$subject_name))),
            selected = "All"
          )
        ),
        helpText("Download the current table as a csv"),
        downloadButton("downloadData", label = "Download table"), br(), br(),
        div(
          style = "color:#ffffff",
          strong("Summary"),
          htmlOutput("crosstab_text")
        )
      ),

      ## Main panel ------------------------------------------------

      mainPanel(
        width = 9,
        htmlOutput("crosstab_title"),
        withSpinner(reactableOutput("crosstab"))
      )
    )
  ),

  # Industry by subject tab -------------------------------------
  tabPanel(
    title = "Industry by subject tables", value = "industryBySubject",

    ## Set metadata for browser ----------------------------------------------------

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

    ## Caveats ---------------------------------------------------------------------

    caveats_box(), # defined in R/caveats.R
    br(),

    ## Side bar ----------------------------------------------------

    sidebarLayout(
      sidebarPanel(
        width = 3,
        helpText("Using the drop down boxes below, create your own table by selecting the breakdown, year after graduation, qualification level (available for sex and subject breakdowns only) and industry area you would like to view."),
        radioGroupButtons("earningsbutton2",
          label = "View the proportion of graduates that studied each subject, or the median earnings of these graduates",
          choices = list("Proportions", "Median earnings"),
          selected = "Proportions"
        ),
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
        selectInput("YAGinput3",
          label = "Select a year after graduation",
          choices = list(1, 3, 5, 10),
          selected = 5
        ),
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
        )
      ),

      ## Main panel -------------------------------------

      mainPanel(
        width = 9,
        htmlOutput("backwards_crosstab_title"),
        withSpinner(reactableOutput("crosstab_backwards"))
      )
    )
  ),

  # Footer ----------------------------------------------

  footer = includeHTML("www/footer.html")
)
