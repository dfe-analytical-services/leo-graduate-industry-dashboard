server <- function(input, output, session) {
  # Close session after closing app --------------------------

  session$onSessionEnded(stopApp) # commenting out to test using lighthouse

  # Cookies ====================================================

  output$cookie_status <- cookie_banner_server(
    "cookie-banner",
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key,
    cookie_link_panel = "cookies_panel_ui"
  )

  cookies_panel_server(
    id = "cookies_panel",
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key # # nolint: [object_usage_linter]
  )

  # Links to tabs --------------------------------------------

  observeEvent(input$link_to_industryFlow_tab, {
    #    updateTabsetPanel(session, "navbar", selected = "industryFlow")
    #    Cathie changed navbar to navlistPanel for the four links
    updateTabsetPanel(session, "navlistPanel", selected = "industryFlow")
  })

  observeEvent(input$link_to_regional_tab, {
    #    updateTabsetPanel(session, "navbar", selected = "regional")
    updateTabsetPanel(session, "navlistPanel", selected = "regional")
  })

  observeEvent(input$link_to_subjectByIndustry_tab, {
    #    updateTabsetPanel(session, "navbar", selected = "subjectByIndustry")
    updateTabsetPanel(session, "navlistPanel", selected = "subjectByIndustry")
  })

  observeEvent(input$link_to_industryBySubject_tab, {
    #    updateTabsetPanel(session, "navbar", selected = "industryBySubject")
    updateTabsetPanel(session, "navlistPanel", selected = "industryBySubject")
  })

  # Added by Cathie - makes the drop down in industry flow page have a reactive title
  #  observe({
  #   if(input$navlistPanel == "industryFlow") {
  #    change_window_title(
  #     session,
  #    paste0(
  #     site_title, " - ",
  #    input$qualinput, ", ",
  #   input$indflow.subjectinput, ", ",
  #  input$sexinput))
  #      }
  #   else{
  #    change_window_title(
  #     session,
  #    paste0(
  #     site_title, " - ",
  #    input$navlistPanel
  # )
  #      )
  #   }})


  # p2: Industry flow page -----------------------------

  ### Label for filters selected --------------------------------

  ## First, create an object for male and female, as the values of input$sexinput are M,F,F&M

  #  sexselection <- reactive(ifelse(input$sexinput == "F", "females",
  #                           ifelse(input$sexinput == "M", "males",
  #                                 "females and males")))

  #  sexselection <- reactive(list(
  #   "Female & male" = "F+M",
  #  "Female" = "F",
  # "Male" = "M"))

  output$dropdown_label <- renderText({
    paste0(
      "Current selections: Qualification is ", input$qualinput,
      ", Subject area is ", input$indflow.subjectinput,
      ", Graduates incuded are ", input$sexinput
    )
  })





  ### Sankey functions --------------------------------------------------------

  # Update the select input box in the Industry Flow analysis based on the
  # selected qualification.
  observe({
    if (input$qualinput != "All") {
      data_filtered <- qual_subjects %>%
        filter(qualification_TR == input$qualinput) %>%
        distinct()
    } else {
      data_filtered <- qual_subjects
    }
    updateSelectizeInput(session, "indflow.subjectinput",
      choices = unique(c("All", data_filtered$subject_name))
    )
  })

  # Here's the sankey network plot. It's calculated using reactive, which only
  # updates if any of the three input variables have changed since the last time
  # it was called.
  reactiveSankey <- reactive(
    sankey_chart(
      input$indflow.subjectinput,
      input$sexinput,
      input$qualinput
    )
  )

  output$sankey <- renderSankeyNetwork({
    reactiveSankey()
  })

  output$sankey_flag <- renderUI({
    if (!type_sum(reactiveSankey()) == "snkyNtwr") {
      tagList(br(), h3("No data found for the selected filters."))
    } else {
      NULL
    }
  })


  output$sankey_title <- renderText({
    sankey_title(input$indflow.subjectinput, input$sexinput, input$qualinput)
  })

  # Here's the sankey table render.
  reactiveSankeyTable <- reactive({
    sankey_table(input$indflow.subjectinput, input$sexinput, input$qualinput)
  })
  output$sankey_table <- renderReactable({
    reactiveSankeyTable()
  })

  # Putting both the text outputs in a reactive container as they've got some
  # calculations embedded in them.
  reactiveSankeyText1 <- reactive({
    sankeytext1(input$indflow.subjectinput, input$sexinput, input$qualinput)
  })
  output$sankeytext1 <- renderText({
    reactiveSankeyText1()
  })
  reactiveSankeyText2 <- reactive({
    sankeytext2(input$indflow.subjectinput, input$sexinput, input$qualinput)
  })
  output$sankeytext2 <- renderText({
    reactiveSankeyText2()
  })


  # output$earningstext <- renderText({
  #   earnings_text(input$indflow.subjectinput, input$sexinput)
  # })

  # output$earnings_sankey <- renderSankeyNetwork({
  #   earnings_sankey(input$indflow.subjectinput, input$sexinput, input$Earningsinput)
  # })
  #
  # output$earnings_table <- renderReactable({
  #   earnings_table(input$indflow.subjectinput, input$sexinput, input$Earningsinput)
  # })


  # p3: Regional page =================================

  ### Dropdown filters ===============================

  output$regional_dropdown_label <- renderText({
    paste0(
      "Current selections: Qualification is ", input$regional_input_qual,
      ", Years after graduation: ", input$regional_input_YAG,
      ", Industry is ", input$regional_input_industry,
      ", Subject area studied is ", input$regional_input_subject
    )
  })


  # p4: Subject by industry page (Industry by subject on the updated dashboard) =======================

  ### Cathie, August/Sept 2024.
  ### The names of subject by industry and industry by subject are potentially very confusing.
  ### The original code presented page 4 as Subject by industry, and page 5 as Industry by subject.
  ### All the code is written this way.
  ### However, the breakdowns in the dashboard are actually the other way around and so
  ### on the dashboard, the names of these pages have been switched. The code remains as it was.

  ### Dropdown filters ------------------------

  output$subject_by_industry_dropdown_label <- renderText({
    paste0(
      "Current selections: Output is ", input$earningsbutton,
      ", Qualification is ", input$qualinput3,
      ", Years after graduation  is ", input$YAGinput2,
      ", Subject area studied is ", input$crosstabs.subjectinput,
      ", Breakdown is by ", input$countinput2
    )
  })


  # p5: Industry by subject page =======================

  ### Dropdown filters ------------------------

  output$industry_by_subject_dropdown_label <- renderText({
    paste0(
      "Current selections: Output is ", input$earningsbutton2,
      ", Qualification is ", input$qualinput4,
      ", Years after graduation  is ", input$YAGinput3,
      ", Industry is ", input$sectionnameinput2, ", ", input$groupinput,
      ", Breakdown is by ", input$countinput3
    )
  })


  # Map functions -----------------------------------------------------------

  observe({
    data_filtered <- regional_movement_data %>%
      filter(
        qualification_TR == input$regional_input_qual,
        SECTIONNAME == input$regional_input_industry,
        count >= 3,
        YAG == input$regional_input_YAG
      ) %>%
      distinct()
    updateSelectizeInput(
      session, "regions.subjectinput",
      choices = unique(c("All", data_filtered$subject_name))
    )
  })

  reactiveRegionTable <- reactive({
    create_maptabledata(
      data, regional_movement_data,
      input$regional_input_industry, input$regional_input_subject, input$regional_input_YAG,
      input$regional_input_qual
    )
  })

  reactiveMapInput <- reactive({
    map_chart(reactiveRegionTable(), input$countinput)
  })

  output$map <- renderLeaflet({
    reactiveMapInput()
  })

  output$map_title <- renderText({
    map_title(
      input$regional_input_industry, input$regional_input_subject,
      input$countinput, input$regional_input_YAG, input$regional_input_qual
    )
  })

  output$maptext <- renderText({
    map_text(
      reactiveRegionTable(), input$regional_input_industry,
      input$regional_input_subject, input$regional_input_YAG, input$regional_input_qual
    )
  })

  output$maptext2 <- renderText({
    map_text2(
      reactiveRegionTable(), input$regional_input_industry,
      input$regional_input_subject, input$regional_input_YAG, input$regional_input_qual
    )
  })

  output$maptable <- renderReactable(
    create_regions_table(reactiveRegionTable(), input$regioninput)
  )

  # Putting the regional sankey in a reactive as well as it's a bit intensive.
  reactiveRegionalSankey <- reactive({
    data <- create_regionalsankeyframe(input$regional_input_industry, input$regional_input_subject, input$regional_input_YAG, input$regional_input_qual)
    validate(
      need(nrow(data$nodes) >= 1, "
           There is no data available.")
    )
    regional_sankey(as.data.frame(data$links), as.data.frame(data$nodes))
  })

  output$regional_sankey <- renderSankeyNetwork({
    reactiveRegionalSankey()
  })

  output$regional_sankey_title <- renderText({
    regional_sankey_title(input$regional_input_industry, input$regional_input_subject, input$regional_input_YAG, input$regional_input_qual)
  })


  # Tables functions --------------------------------------------------------
  reactiveSubjbyIndGroupedSummary <- reactive({
    subjbyind_grouped_summary(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3)
  })
  reactiveSubjbyIndGroupedData <- reactive({
    subjbyind_grouped_data(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3)
  })

  reactiveSubjbyIndText <- reactive({
    crosstab_text(reactiveSubjbyIndGroupedSummary(), input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3)
  })


  output$crosstab_text <- renderText({
    x <- reactiveSubjbyIndText()
    return(x)
  })

  reactiveSubjIndTable <- reactive({
    crosstabs(
      reactiveSubjbyIndGroupedData,
      input$crosstabs.subjectinput,
      input$YAGinput2,
      input$countinput2,
      input$qualinput3,
      input$earningsbutton
    )
  })

  output$crosstab <- renderReactable({
    table_data <- reactiveSubjIndTable()
    crosstabs_reactable(
      table_data$crosstabs_data,
      table_data$nested_crosstabs,
      table_data$numeric_cols_def,
      table_data$nested_numeric_cols_def,
      table_data$script
    )
  })

  # Downloads of data from p4 and p5 ----------------------------

  # Cathie work on this bit
  # Download p4 data with current selections from the dropdown menu
  output$downloadData_p4 <- downloadHandler(
    filename = function() {
      prefix <- "DfE_LEO-SIC"
      suffix <- "IndustryBySubject.csv"
      if (input$countinput2 == "subject_name") {
        paste(prefix,
          gsub(" ", "-", input$earningsbutton),
          input$countinput2,
          paste0(input$YAGinput2, "YAG"),
          gsub(" ", "-", input$qualinput3),
          suffix,
          sep = "_"
        )
      } else if (input$countinput2 == "sex") {
        paste(prefix,
          gsub(" ", "-", input$earningsbutton),
          input$countinput2,
          paste0(input$YAGinput2, "YAG"),
          gsub(" ", "-", input$qualinput3),
          input$crosstabs.subjectinput,
          suffix,
          sep = "_"
        )
      } else {
        paste(prefix,
          gsub(" ", "-", input$earningsbutton),
          input$countinput2,
          paste0(input$YAGinput2, "YAG"),
          input$crosstabs.subjectinput,
          suffix,
          sep = "_"
        )
      }
    },
    content = function(file) {
      table_data <- reactiveSubjIndTable()
      out_columns <- colnames(table_data$crosstabs_data)

      footsum <- table_data$footer_crosstabs %>%
        select(-SECTIONNAME, -group_name) %>%
        summarise_all(sum) %>%
        mutate(SECTIONNAME = "TOTAL (N)", group_name = "TOTAL (N)") %>%
        select(out_columns)
      dfDownload <- rbind(
        table_data$crosstabs_data,
        table_data$nested_crosstabs %>%
          select(out_columns)
      )
      if (input$earningsbutton == "Proportions") {
        dfDownload <- dfDownload %>%
#  Cathie changes mutate_if() to mutate(across()) as mutate_if isn't supported by more recent version of dplyr          
#          mutate_if(is.numeric, list(~ (100.0 * .)))
          mutate(across(where(is.numeric), ~ if_else(.==0, 0, . / sum(., na.rm = TRUE))))
      }
      dfDownload <- dfDownload %>%
#  Cathie changes mutate_if() and mutate_all() to mutate(across()) as mutate_if isn't supported by more recent version of dplyr          
#        mutate_if(is.numeric, list(~ gsub(" ", "", format(., scientific = FALSE)))) %>%
        mutate(across(where(is.numeric), list(~ gsub(" ", "", format(., scientific = FALSE))))) %>%
#        mutate_all(list(~ gsub("-10000", "c", .))) %>%
#        mutate_all(list(~ ifelse(. %in% c("NA", "NaN"), "x", .))) %>%
        mutate(across(list(~ gsub("-10000", "c", .)))) %>%
        mutate(across(list(~ ifelse(. %in% c("NA", "NaN"), "x", .)))) %>%
        arrange(SECTIONNAME, group_name) %>%
        rbind(footsum)
      write.csv(dfDownload, file, row.names = FALSE)
    }
  )

  # p5: IndSubj panel server code ============================

  ### Create the reactive Industry by Subject data in a data frame ===================
  reactiveIndSubjTable <- reactive({
    backwards_crosstabs(input$sectionnameinput2, input$YAGinput3, input$countinput3, input$qualinput4, input$earningsbutton2, input$groupinput)
  })

  # Render the reactive industry by subject data frame into a ReacTable element.
  output$crosstab_backwards <- renderReactable({
    table_data <- reactiveIndSubjTable()
    indsubj_reactable(
      table_data$data,
      table_data$coldefs
    )
  })

  ### Download the reactive industry by subject data.=========================
  #  output$IndSubjDownload <- downloadHandler(
  output$downloadData_p5 <- downloadHandler(
    filename = function() {
      prefix <- "DfE_LEO-SIC"
      suffix <- "IndustryBySubject.csv"
      if (input$countinput3 == "SECTIONNAME") {
        paste(prefix,
          gsub(" ", "-", input$earningsbutton2),
          "industry",
          paste0(input$YAGinput3, "YAG"),
          suffix,
          sep = "_"
        )
      } else if (input$countinput3 == "sex") {
        paste(prefix,
          gsub(" ", "-", input$earningsbutton2),
          input$countinput3,
          paste0(input$YAGinput3, "YAG"),
          gsub(" ", "-", input$qualinput4),
          input$sectionnameinput2,
          input$groupinput,
          suffix,
          sep = "_"
        )
      } else {
        paste(prefix,
          gsub(" ", "-", input$earningsbutton2),
          input$countinput3,
          paste0(input$YAGinput3, "YAG"),
          input$sectionnameinput2,
          input$groupinput,
          suffix,
          sep = "_"
        )
      }
    },
    content = function(file) {
      table_data <- reactiveIndSubjTable()

      out_columns <- colnames(table_data$data)

      footsum <- table_data$footer %>%
        select(-subject_name) %>%
        summarise_all(sum) %>%
        mutate(subject_name = "TOTAL (N)") %>%
        select(out_columns)
      dfDownload <- table_data$data
      if (input$earningsbutton2 == "Proportions") {
        dfDownload <- dfDownload %>%
    #    mutate_if(is.numeric, list(~ (100.0 * .)))
        mutate(across(where(is.numeric), ~ list(~ (100.0 * .))))
      }
      dfDownload <- dfDownload %>%
#        mutate_if(is.numeric, list(~ gsub(" ", "", format(., scientific = FALSE)))) %>%
#        mutate_all(list(~ gsub("-10000", "c", .))) %>%
#        mutate_all(list(~ ifelse(. %in% c("NA", "NaN"), "x", .))) %>%
        mutate(across(where(is.numeric), list(~ gsub(" ", "", format(., scientific = FALSE))))) %>%
        mutate(across(list(~ gsub("-10000", "c", .)))) %>%
        mutate(across(list(~ ifelse(. %in% c("NA", "NaN"), "x", .)))) %>%
        arrange(subject_name) %>%
        rbind(footsum)
      write.csv(dfDownload, file, row.names = FALSE)
    }
  )


  #  output$crosstab_title <- renderText({
  #   crosstab_title(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3)
  # })

  # output$crosstab_title <- renderText(
  # paste("<h2>Industries that graduates work in by the subject area they studied, ",
  #        tax_year_slash, " tax year.</h2>")
  # )


  output$backwards_crosstab_title <- renderText({
    backwards_crosstab_title(input$sectionnameinput2, input$YAGinput3, input$countinput3, input$qualinput4, input$groupinput)
  })

  output$sankeyhelp <- renderText({
    paste(a(h4("How to read this sankey?")))
  })

  # Conditional panel replacements ####

  # Qualification input condition (sub by ind) ----
  observeEvent(input$countinput2, {
    x <- input$countinput2

    if (!x %in% c("sex", "subject_name")) {
      updateSelectInput(
        session,
        "qualinput3",
        label = "Select qualification level",
        choices = list(
          "First degree"
        ),
        selected = "First degree"
      )
    }
    if (x %in% c("sex", "subject_name")) {
      updateSelectInput(
        session,
        "qualinput3",
        label = "Select qualification level",
        choices = list(
          "First degree",
          "Level 7 (taught)",
          "Level 7 (research)",
          "Level 8"
        )
      )
    }
  })

  # Subject input condition (sub by ind) ----

  observeEvent(input$qualinput3, {
    if (input$qualinput3 != "All") {
      data_filtered <- qual_subjects %>%
        filter(qualification_TR == input$qualinput3) %>%
        distinct()
    } else {
      data_filtered <- qual_subjects
    }
    updateSelectInput(
      session, "crosstabs.subjectinput",
      choices = na.exclude(unique(c("All", data_filtered$subject_name)))
    )
  })

  observeEvent(input$countinput2, {
    x <- input$countinput2

    if (x == "subject_name") {
      updateSelectInput(
        session,
        "crosstabs.subjectinput",
        label = "Select a subject area",
        choices = list(
          "All"
        ),
        selected = "All"
      )
    } else {
      if (input$qualinput3 != "All") {
        data_filtered <- qual_subjects %>%
          filter(qualification_TR == input$qualinput3) %>%
          distinct()
      } else {
        data_filtered <- qual_subjects
      }
      updateSelectInput(
        session, "crosstabs.subjectinput",
        choices = na.exclude(unique(c("All", data_filtered$subject_name)))
      )
    }
  })

  # Qualification input condition (ind by sub) ----
  observeEvent(input$countinput3, {
    x <- input$countinput3

    if (!x %in% c("sex", "subject_name")) {
      updateSelectInput(
        session,
        "qualinput4",
        label = "Select qualification level",
        choices = list(
          "First degree"
        ),
        selected = "First degree"
      )
    }
    if (x %in% c("sex", "subject_name")) {
      updateSelectInput(
        session,
        "qualinput4",
        label = "Select qualification level",
        choices = list(
          "First degree",
          "Level 7 (taught)",
          "Level 7 (research)",
          "Level 8"
        )
      )
    }
  })

  # Industry input condition (ind by sub) ----

  observeEvent(input$countinput3, {
    x <- input$countinput3

    if (x == "SECTIONNAME") {
      updateSelectInput(
        session,
        "sectionnameinput2",
        label = "Choose an industry area",
        choices = list(
          "Education"
        ),
        selected = "Education"
      )
    } else {
      updateSelectInput(
        session,
        "sectionnameinput2",
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
    }
  })

  # Group input condition (ind by sub) ----

  observeEvent(input$sectionnameinput2, {
    if (input$sectionnameinput2 != "All") {
      data_filtered <- industry_groups %>%
        filter(SECTIONNAME == input$sectionnameinput2) %>%
        distinct()
    } else {
      data_filtered <- industry_groups
    }
    updateSelectizeInput(
      session, "groupinput",
      choices = na.exclude(unique(c("All", data_filtered$group_name)))
    )
  })

  observeEvent(input$countinput3, {
    x <- input$countinput3

    if (x == "SECTIONNAME") {
      updateSelectInput(
        session,
        "groupinput",
        label = "View 3 digit SIC groups within the selected industry",
        choices = list(
          "All"
        ),
        selected = "All"
      )
    } else {
      if (input$sectionnameinput2 != "All") {
        data_filtered <- industry_groups %>%
          filter(SECTIONNAME == input$sectionnameinput2) %>%
          distinct()
      } else {
        data_filtered <- industry_groups
      }
      updateSelectizeInput(
        session, "groupinput",
        label = "View 3 digit SIC groups within the selected industry",
        choices = na.exclude(unique(c("All", data_filtered$group_name)))
      )
    }
  })




  # output$subjecttable <- renderReactable({
  #   subjecttable(input$sectionnameinput2, input$YAGinput3, input$countinput3)
  # })
  #
  # output$subjecttable_title <- renderText({
  #   subjecttable_title(input$sectionnameinput2, input$YAGinput3, input$countinput3)
  # })
}
