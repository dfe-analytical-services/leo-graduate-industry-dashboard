server <- function(input, output, session) {
  # Close session after closing app --------------------------

  session$onSessionEnded(stopApp) # commenting out to test using lighthouse

  # Links to tabs --------------------------------------------

  observeEvent(input$link_to_industryFlow_tab, {
    updateTabsetPanel(session, "navbar", selected = "industryFlow")
  })

  observeEvent(input$link_to_regional_tab, {
    updateTabsetPanel(session, "navbar", selected = "regional")
  })

  observeEvent(input$link_to_subjectByIndustry_tab, {
    updateTabsetPanel(session, "navbar", selected = "subjectByIndustry")
  })

  observeEvent(input$link_to_industryBySubject_tab, {
    updateTabsetPanel(session, "navbar", selected = "industryBySubject")
  })

  # Sankey functions --------------------------------------------------------

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


  # Map functions -----------------------------------------------------------

  observe({
    data_filtered <- regional_movement_data %>%
      filter(
        qualification_TR == input$qualinput2,
        SECTIONNAME == input$sectionnameinput,
        count >= 3,
        YAG == input$YAGinput
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
      input$sectionnameinput, input$regions.subjectinput, input$YAGinput,
      input$qualinput2
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
      input$sectionnameinput, input$regions.subjectinput,
      input$countinput, input$YAGinput, input$qualinput2
    )
  })

  output$maptext <- renderText({
    map_text(
      reactiveRegionTable(), input$sectionnameinput,
      input$regions.subjectinput, input$YAGinput, input$qualinput2
    )
  })

  output$maptext2 <- renderText({
    map_text2(
      reactiveRegionTable(), input$sectionnameinput,
      input$regions.subjectinput, input$YAGinput, input$qualinput2
    )
  })

  output$maptable <- renderReactable(
    create_regions_table(reactiveRegionTable(), input$regioninput)
  )

  # Putting the regional sankey in a reactive as well as it's a bit intensive.
  reactiveRegionalSankey <- reactive({
    data <- create_regionalsankeyframe(input$sectionnameinput, input$regions.subjectinput, input$YAGinput, input$qualinput2)
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
    regional_sankey_title(input$sectionnameinput, input$regions.subjectinput, input$YAGinput, input$qualinput2)
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

  # Download current Subject by Industry view
  output$downloadData <- downloadHandler(
    filename = function() {
      prefix <- "DfE_LEO-SIC"
      suffix <- "SubjectbyIndustry.csv"
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
          mutate_if(is.numeric, list(~ (100.0 * .)))
      }
      dfDownload <- dfDownload %>%
        mutate_if(is.numeric, list(~ gsub(" ", "", format(., scientific = FALSE)))) %>%
        mutate_all(list(~ gsub("-10000", "c", .))) %>%
        mutate_all(list(~ ifelse(. %in% c("NA", "NaN"), "x", .))) %>%
        arrange(SECTIONNAME, group_name) %>%
        rbind(footsum)
      write.csv(dfDownload, file, row.names = FALSE)
    }
  )

  ### IndSubj panel server code
  #############################

  # Create the reactive Industry by Subject data in a data frame
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

  # Download the reactive industry by subject data.
  output$IndSubjDownload <- downloadHandler(
    filename = function() {
      prefix <- "DfE_LEO-SIC"
      suffix <- "IndustrybySubject.csv"
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
          mutate_if(is.numeric, list(~ (100.0 * .)))
      }
      dfDownload <- dfDownload %>%
        mutate_if(is.numeric, list(~ gsub(" ", "", format(., scientific = FALSE)))) %>%
        mutate_all(list(~ gsub("-10000", "c", .))) %>%
        mutate_all(list(~ ifelse(. %in% c("NA", "NaN"), "x", .))) %>%
        arrange(subject_name) %>%
        rbind(footsum)
      write.csv(dfDownload, file, row.names = FALSE)
    }
  )


  output$crosstab_title <- renderText({
    crosstab_title(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3)
  })


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
