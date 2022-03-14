# UI ----------------------------------------------------------------------


ui <- navbarPage("LEO Graduate Industry Dashboard",id="tabs",
                 
                 # titlePanel(div("Unpublished analysis - do not forward", style = "color: #FF0000")),
                 
                 tabPanel("Home/Contents",id="tab_contents",
                          tags$head(tags$script(HTML('
                            var fakeClick = function(tabName) {
                              var dropdownList = document.getElementsByTagName("a");
                                for (var i = 0; i < dropdownList.length; i++) {
                                 var link = dropdownList[i];
                                 if(link.getAttribute("data-value") == tabName) {
                                  link.click();
                                 };
                                }
                              };
                            '))),
                          tags$style(HTML("

                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#F2F2F2
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#C7E1FF ;
                    border-left-color:#C7E1FF ;
                    border-right-color:#C7E1FF ;
                    border-top-color:#C7E1FF ;
                    background:#DAECFF 
                    }

                    ")),
                    
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: visible; content: 'Loading...'; }"
                          ),
                          
                          
                          titlePanel('LEO Graduate Industry Dashboard'),
                          h2('Welcome'),
                          h4("Unpublished analysis - do not forward", style = "color: #FF0000"),
                          paste('Welcome to the LEO Graduate Industry Dashboard. We have joined the IDBR 
                          (Inter-Departmental Business Register data) via SIC (Standard Industry Classification ) 
                          codes to the LEO (Longitudinal Educational Outcomes) data for the 2014/15 to 2018/19 
                          tax years at one, three, five and ten years after graduation, which means we can now see 
                          what industry these graduates were working in at these points in time.
'),
                          fluidPage(
                            fluidRow(column(6, 
                                            box(status = "primary", width = NULL, solidHeader = TRUE,
                                            h2('Contents'),
                                            a(h4('IDBR and SIC background'), onclick = "fakeClick('IDBR and SIC background')"),
                                            paste(' - This tab provides information on the data used in this dashboard.'),br(),
                                            a(h4('Industry flow analysis'), onclick = "fakeClick('Industry flow analysis')"),
                                            paste(' - This tab looks at the industries one longitudinal cohort (2012/13 acadmeic year of 
                                                  graduation cohort) worked in and flowed between at one, three and five years after 
                                                  graduation. You can filter this analysis to look at graduates of
                                                  a specific subject area, or filter by graduate sex.'),br(),
                                            a(h4('Regional analysis'), onclick = "fakeClick('Regional analysis')"),
                                            paste(' - This tab compares where graduates studied to where they lived 
                                                  at one, three and five years after graduation, for each industry, and uses the latest tax year data, 
                                                  so does not follow the same cohort as the longitudinal tab. It can be filtered 
                                                  to look at graduates of a specific subject and provides context on the number of providers in each
                                                  region that have graduates working in the selected industry, and the median earnings of graduates working
                                                  in the selected industry in each region.'),br(),
                                            h4('Tables - '),a(h4('(1) Subject by industry'), onclick = "fakeClick('Subject by industry tables')"),
                                            a(h4('(2) Industry by subject'), onclick = "fakeClick('Industry by subject tables')"),
                                            paste(' - These tabs provides tables for multiple breakdowns. The tables use the latest tax year data, 
                                                  so do not follow the same cohort as the longitudinal tab. Instead the table looks at the one, three,
                                                  five and ten year after graduation cohorts from the 2018/19 tax year. The tables show breakdowns of
                                                  graduate industry for the filtered subject area and year after graduation. The following breakdowns are 
                                                  currently available:'),br(),
                                            "\U2022 sex",br(), "\U2022 ethnicity",br(), "\U2022 FSM status",br(), "\U2022 current region",br(),
                                            "\U2022 prior attainment",br(),"\U2022 subject",br(),"\U2022 qualification level")),
                                           
                                     column(6, 
                                            box(status = "primary", width = NULL, solidHeader = TRUE,
                                                
                                                h2('Important Caveats to note:'),
                                                "\U2022 Data only includes graduates who were in sustained employment in the associated
                                            tax year, and a graduate's industry is recorded as the industry in which they earnt
                                            the most in the associated tax year.",
                                                br(),br(),
                                                "\U2022 Counts have been rounded to the nearest 5, however percentages are based on raw values.",
                                                br(),br(),
                                                
                                                "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us
                                                about the graduates occupation within that company.")

                                            )
                                     ))),
                 
                 # What is SIC tab
                 tabPanel("IDBR and SIC background",
                          
                          titlePanel('IDBR and SIC background'),
                          h4('IDBR (Inter-Departmental Business Register)'),
                          paste('IDBR data is a comprehensive list of UK businesses used by gonernment for statistical purposes.'),br(),
                          h4('UK SIC (Standard Industrial Classification) code'),
                          paste('The UK Standard Industrial Classification (SIC) of economic activties is used to classify
                                businesses by the type of activity thet do.'),br(),
                          h4('Useful links'),
                          a(href = 'https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic',
                            'Standard industrial classification of economic activities (SIC) - GOV.UK.(www.gov.uk)'
                          ), br(),
                          a(href = 'https://siccode.com/sic-code-lookup-directory', 'SIC Code Lookup | SIC Code Search Tool'),br(),
                          h4('SIC Groups and sections'),
                          
                          paste("We have two SIC variables joined onto our data: SECTIONNAME and SIC2007. There are over 700 detailed SIC
                                 codes in the SIC2007 variable, which are grouped into 21 industry sections in the SECTIONNAME variable. We
                                 have used the broader industry sections as the base for our analysis, which consist of the following:"),br(),br(),
                          "1. ACCOMMODATION AND FOOD SERVICE ACTIVITIES",br(),
                          "2. ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES",br(),
                          "3. ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS - UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE",br(),
                          "4. ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",br(),
                          "5. AGRICULTURE, FORESTRY AND FISHING",br(),
                          "6. ARTS, ENTERTAINMENT AND RECREATION",br(),
                          "7. CONSTRUCTION",br(),
                          "8. EDUCATION",br(),
                          "9. ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",br(),
                          "10. FINANCIAL AND INSURANCE ACTIVITIES",br(),
                          "11. HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",br(),
                          "12. INFORMATION AND COMMUNICATION",br(),
                          "13. MANUFACTURING",br(),
                          "14. MINING AND QUARRYING",br(),
                          "15. OTHER SERVICE ACTIVITIES",br(),
                          "16. PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",br(),
                          "17. PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY",br(),
                          "18. REAL ESTATE ACTIVITIES",br(),
                          "19. TRANSPORTATION AND STORAGE",br(),
                          "20. WATER SUPPLY - SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES",br(),
                          "21. WHOLESALE AND RETAIL TRADE - REPAIR OF MOTOR VEHICLES AND MOTORCYCLES"
                 ),
                 
                 

                 # Sankey tab
                 tabPanel("Industry flow analysis",
                          strong("Unpublished analysis - do not forward", style = "color: #FF0000"),
                          box(status = "primary", width = NULL, solidHeader = TRUE,
                              strong("Caveats"),br(),
                              "\U2022 Data only includes graduates who were in sustained employment in the associated
                                            tax year, and a graduate's industry is recorded as the industry in which they earnt
                                            the most in the associated tax year.",
                              br(),
                              "\U2022 Counts have been rounded to the nearest 5, however percentages are based on raw values.",
                              br(),
                             
                              "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us
                                                about the graduates occupation within that company."
                          ),br(),
                         
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              helpText("Create sankey charts for each subject showing one, three and five years after graduation (YAG)."),
                              helpText('Switch between the sankey and the proportions table using the tabs on the right.'),
                              
                              selectInput("qualinput",
                                          label = "Choose graduate qualification level",
                                          choices = list("First degree",
                                                         "Level 7 (taught)",
                                                         "Level 7 (research)",
                                                         "Level 8"),
                                          selected = 'First degree'),
                              
                              uiOutput('sankeysubjectlist'),
                              
                              selectInput("sexinput",
                                          label = "View by graduate sex",
                                          choices = list("Female & Male" = "F+M",
                                                         "Female" = "F",
                                                         "Male" = "M"),
                                          selected = "F+M"),
                              
                              h4("Summary"),
                              strong('Most popular industry'),
                              htmlOutput("sankeytext1"),
                              br(),
                              strong('Movement between industries'),
                              htmlOutput("sankeytext2"),
                              br(),
                              # strong('Earnings'),
                              # htmlOutput("earningstext"),
                             
                            ),
                          
                            mainPanel(
                                       htmlOutput('sankey_title'),
                                       
                                       # absolutePanel(htmlOutput('sankeyhelp'), top = '6%', right = '6%'),
                                      
                              tabsetPanel(
                                tabPanel("Sankey Plot",
                                         
                                         htmlOutput('sankeyhelp'),
                                         
                                         bsTooltip('sankeyhelp',
                                                   'The coloured bars represent graduates in that industry at each year after graduation, and the grey flow lines show the movement of these graduates. Hover your mouse over a bar or flow line to see the number of graduates it represents.',
                                                   placement = 'left',
                                                   trigger = 'hover',
                                                   options = NULL),
                                         absolutePanel(strong("1 YAG"), top = '15%', left = 25),
                                         absolutePanel(strong("5 YAG"), top = '15%', right = 25),
                                         absolutePanel(strong("3 YAG"), top = '15%', left = '48%'),
                                         br(),
                                         sankeyNetworkOutput(outputId = 'sankey', height = 800)),
                                
                                tabPanel("Industry proportions table", reactableOutput("sankey_table"), height = 1500)
                                
                                # tabPanel("Earnings table", 
                                #          selectInput("Earningsinput", 
                                #                      label = "Choose 1 YAG industry",
                                #                      choices = list("ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
                                #                                     "ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES",
                                #                                     "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS - UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE",
                                #                                     "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
                                #                                     "AGRICULTURE, FORESTRY AND FISHING",
                                #                                     "ARTS, ENTERTAINMENT AND RECREATION",
                                #                                     "CONSTRUCTION",
                                #                                     "EDUCATION",
                                #                                     "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
                                #                                     "FINANCIAL AND INSURANCE ACTIVITIES",
                                #                                     "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",
                                #                                     "INFORMATION AND COMMUNICATION",
                                #                                     "MANUFACTURING",
                                #                                     "MINING AND QUARRYING",
                                #                                     "OTHER SERVICE ACTIVITIES",
                                #                                     "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",
                                #                                     "PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY",
                                #                                     "REAL ESTATE ACTIVITIES",
                                #                                     "TRANSPORTATION AND STORAGE",
                                #                                     "WATER SUPPLY - SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES",
                                #                                     "WHOLESALE AND RETAIL TRADE - REPAIR OF MOTOR VEHICLES AND MOTORCYCLES"),
                                #                      selected = "EDUCATION"),
                                #          reactableOutput("earnings_table"),
                                #          absolutePanel(strong("Table is initially ordered by number of graduates. Click on
                                #                               a column to reorder the table."), br(), 
                                #                        strong("Number of graduates is rounded to the nearest 5, and rows that round to 0 have been removed."), 
                                #                        top = "10%", left = "30%"),
                                         
                                )
                             
                              
                              
                            ))),
                 
                 # Map tab
                 tabPanel("Regional analysis",
                          strong("Unpublished analysis - do not forward", style = "color: #FF0000"),
                          box(status = "primary", width = NULL, solidHeader = TRUE,
                              strong("Caveats"),br(),
                              "\U2022 Data only includes graduates who were in sustained employment in the associated
                                            tax year, and a graduate's industry is recorded as the industry in which they earnt
                                            the most in the associated tax year.",
                              br(),
                              "\U2022 Counts have been rounded to the nearest 5, however percentages are based on raw values.",
                              br(),
                             
                              "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us
                                                about the graduates occupation within that company."
                          ),br(),
                          
                          sidebarLayout(
                            sidebarPanel(width = 3,
                              helpText("Create a heat map to show graduate movement from study region to current region for 
                                       the selected industry."),
                              helpText("Click on a region to see all of the information for that region, including the
                                       number of providers and the median earnings for the selected industry."),
                              
                              selectInput('qualinput2',
                                          label = "Choose graduate qualification level",
                                          choices = list("First degree",
                                                         "Level 7 (taught)",
                                                         "Level 7 (research)",
                                                         "Level 8"),
                                          selected = 'First degree'),
                              
                              selectInput("sectionnameinput", 
                                          label = "Choose an industry area",
                                          choices = list("ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
                                                         "ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES",
                                                         "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS - UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE",
                                                         "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
                                                         "AGRICULTURE, FORESTRY AND FISHING",
                                                         "ARTS, ENTERTAINMENT AND RECREATION",
                                                         "CONSTRUCTION",
                                                         "EDUCATION",
                                                         "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
                                                         "FINANCIAL AND INSURANCE ACTIVITIES",
                                                         "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",
                                                         "INFORMATION AND COMMUNICATION",
                                                         "MANUFACTURING",
                                                         "MINING AND QUARRYING",
                                                         "OTHER SERVICE ACTIVITIES",
                                                         "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",
                                                         "PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY",
                                                         "REAL ESTATE ACTIVITIES",
                                                         "TRANSPORTATION AND STORAGE",
                                                         "WATER SUPPLY - SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES",
                                                         "WHOLESALE AND RETAIL TRADE - REPAIR OF MOTOR VEHICLES AND MOTORCYCLES"),
                                          selected = "EDUCATION"),
                              
                              uiOutput('mapsubjectlist'),
                              
                              selectInput("countinput",
                                          label = "View different statistics",
                                          choices = list("Studied in region" = "trained_in_region",
                                                         "Currently living in region"="living_in_region",
                                                         "Difference (n)"="difference",
                                                         "Difference (%)"="difference_prop"),
                                          selected = "trained_in_region"),

                              selectInput("YAGinput",
                                          label = "Select year after graduation",
                                          choices = list(1,3,5,10),
                                          selected = 5),
                              strong('Summary'),
                              textOutput("maptext"),
                              br(),
                              textOutput('maptext2'),
                              
                            ),
                            
                            mainPanel(
                                       htmlOutput('map_title'),
                              width = 9, box(leafletOutput(outputId = 'map', height = 800), width = 5),
                                       box(width = 6, selectizeInput('regioninput', 
                                                          label = 'Select multiple regions from the dropdown below to compare.', 
                                                          choices = list('North East',
                                                                         'North West',
                                                                         'Yorkshire and the Humber',
                                                                         'East Midlands',
                                                                         'West Midlands',
                                                                         'East of England',
                                                                         'London',
                                                                         'South East',
                                                                         'South West'), 
                                                          selected = 'London', multiple = FALSE,
                                                          options = list(maxItems = 9, placeholder = 'Start typing a region')),
                                           strong('Click a column header to sort the table'),
                                           br(),
                                           reactableOutput('maptable'),
                                           br(),
                                           strong('Please note that the table only shows results for the selected industry, subject and year after graudation.'),
                                           br(),
                                           br(),
                                           htmlOutput('regional_sankey_title'),
                                           sankeyNetworkOutput('regional_sankey')
                                           )
                             )
                          )),
                 #Tables panel
                 tabPanel("Subject by industry tables",
                          strong("Unpublished analysis - do not forward", style = "color: #FF0000"),
                          box(status = "primary", width = NULL, solidHeader = TRUE,
                              strong("Caveats"),br(),
                              "\U2022 Data only includes graduates who were in sustained employment in the associated
                                            tax year, and a graduate's industry is recorded as the industry in which they earnt
                                            the most in the associated tax year.",
                              br(),
                              "\U2022 Counts have been rounded to the nearest 5, however percentages are based on raw values.",
                              br(),
                            
                              "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us
                                                about the graduates occupation within that company."
                          ),br(),
                            
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         helpText('Using the drop down boxes below, create your own table by selecting
                                                  the breakdown, year after graduation, qualification level (available for sex and subject
                                                  breakdowns only) and subject area you would like to view.'),
                                         
                                         radioGroupButtons('earningsbutton',
                                                           label = 'View the proportion of graduates in each industry, or the median earnings
                                                           of these graduates',
                                                           choices = list('Proportions', 'Median earnings'),
                                                           selected = 'Proportions'), 
                                         
                                         radioGroupButtons('thresholdinput',
                                                           label = 'View for only above or below the student loan
                                                           repayment threshold (plan 2 in 2018/19: £25,000)',
                                                           choices = list('All','Above','Below'),
                                                           selected = 'All'),
                             
                            selectInput("countinput2",
                                        label = "Choose a breakdown",
                                        choices = list('Sex' = 'sex',
                                                       'Ethnicity' = 'ethnicity',
                                                       'Current region' = 'current_region',
                                                       'Free school meals (FSM)' = 'FSM', 
                                                       'Prior attainment' = 'prior_attainment',
                                                       'Subject' = 'subject_name',
                                                       'Qualification level' = 'qualification_TR'),
                                        selected = 'sex'),
                            
                            selectInput("YAGinput2",
                                        label = "Select a year after graduation",
                                        choices = list(1,3,5,10),
                                        selected = 5),
                            
                            conditionalPanel(condition = "input.countinput2 == 'sex' || input.countinput2 == 'subject_name'",
                                             selectInput("qualinput3",
                                                         label = 'Select qualification level',
                                                         choices = list('First degree',
                                                                        'Level 7 (taught)',
                                                                        'Level 7 (research)',
                                                                        'Level 8'),
                                                         selected = 'First degree')),
                            
                            conditionalPanel(condition = "input.countinput2 != 'subject_name'",
                                             uiOutput('subjectlist3')),
                            
                            helpText('Download the current table as a csv'),
                            downloadButton("downloadData", label = "Download table"),br(),br(),
                            strong('Summary'),
                            htmlOutput("crosstab_text")
                            

                          ),
                          mainPanel( 
                            width = 9,
                                    htmlOutput('crosstab_title'),
                            reactableOutput("crosstab")))),
                 
                 tabPanel('Industry by subject tables',
                          strong("Unpublished analysis - do not forward", style = "color: #FF0000"),
                          box(status = "primary", width = NULL, solidHeader = TRUE,
                              strong("Caveats"),br(),
                              "\U2022 Data only includes graduates who were in sustained employment in the associated
                                            tax year, and a graduate's industry is recorded as the industry in which they earnt
                                            the most in the associated tax year.",
                              br(),
                              "\U2022 Counts have been rounded to the nearest 5, however percentages are based on raw values.",
                              br(),
                              
                              "\U2022 SIC codes tell us the industry of the company that the graduate works for, and does NOT tell us
                                                about the graduates occupation within that company."
                          ),br(),
                          
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         helpText('Using the drop down boxes below, create your own table by selecting
                                                  the breakdown, year after graduation, qualification level (available for sex and subject
                                                  breakdowns only) and industry area you would like to view.'),
                                         
                                         radioGroupButtons('earningsbutton2',
                                                           label = 'View the proportion of graduates that studied each subject, or the median earnings
                                                           of these graduates',
                                                           choices = list('Proportions', 'Median earnings'),
                                                           selected = 'Proportions'),    
                                          
                                         selectInput("countinput3",
                                                     label = "Choose a breakdown",
                                                     choices = list('Sex' = 'sex',
                                                                    'Ethnicity' = 'ethnicity',
                                                                    'Current region' = 'current_region',
                                                                    'Free school meals (FSM)' = 'FSM', 
                                                                    'Prior attainment' = 'prior_attainment',
                                                                    'Industry' = 'SECTIONNAME',
                                                                    'Qualification level' = 'qualification_TR'),
                                                     selected = 'sex'),
                                         
                                         selectInput("YAGinput3",
                                                     label = "Select a year after graduation",
                                                     choices = list(1,3,5,10),
                                                     selected = 5),
                                         
                                         conditionalPanel(condition = "input.countinput3 == 'sex' || input.countinput3 == 'subject_name'",
                                                          selectInput("qualinput4",
                                                                      label = 'Select qualification level',
                                                                      choices = list('First degree',
                                                                                     'Level 7 (taught)',
                                                                                     'Level 7 (research)',
                                                                                     'Level 8'),
                                                                      selected = 'First degree')),
                                         
                                         conditionalPanel(condition = "input.countinput3 != 'SECTIONNAME'",
                                                          selectInput('sectionnameinput2', 
                                                                      label = "Choose an industry area",
                                                                      choices = list("ACCOMMODATION AND FOOD SERVICE ACTIVITIES",
                                                                                     "ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES",
                                                                                     "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS - UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE",
                                                                                     "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
                                                                                     "AGRICULTURE, FORESTRY AND FISHING",
                                                                                     "ARTS, ENTERTAINMENT AND RECREATION",
                                                                                     "CONSTRUCTION",
                                                                                     "EDUCATION",
                                                                                     "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
                                                                                     "FINANCIAL AND INSURANCE ACTIVITIES",
                                                                                     "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",
                                                                                     "INFORMATION AND COMMUNICATION",
                                                                                     "MANUFACTURING",
                                                                                     "MINING AND QUARRYING",
                                                                                     "OTHER SERVICE ACTIVITIES",
                                                                                     "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES",
                                                                                     "PUBLIC ADMINISTRATION AND DEFENCE - COMPULSORY SOCIAL SECURITY",
                                                                                     "REAL ESTATE ACTIVITIES",
                                                                                     "TRANSPORTATION AND STORAGE",
                                                                                     "WATER SUPPLY - SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES",
                                                                                     "WHOLESALE AND RETAIL TRADE - REPAIR OF MOTOR VEHICLES AND MOTORCYCLES"),
                                                                      selected = "EDUCATION")
                                         ),
                                         
                                         # helpText('Download the current table as a csv'),
                                         # downloadButton("downloadData", label = "Download table"),br(),br(),
                                         # strong('Summary'),
                                         # htmlOutput("crosstab_text")
                                         
                                         
                            ),
                            mainPanel( 
                                       width = 9,
                                       htmlOutput('backwards_crosstab_title'),
                                       reactableOutput("crosstab_backwards"))))
                          
                                     )
