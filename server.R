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

  reactiveMapData <- reactive({
    map_chart(
      input$sectionnameinput, input$regions.subjectinput,
      input$countinput, input$YAGinput, input$qualinput2
    )
  })
  output$map <- renderLeaflet({
    reactiveMapData()
  })

  output$map_title <- renderText({
    map_title(input$sectionnameinput, input$regions.subjectinput, input$countinput, input$YAGinput, input$qualinput2)
  })

  output$maptext <- renderText({
    map_text(input$sectionnameinput, input$regions.subjectinput, input$countinput, input$YAGinput, input$qualinput2)
  })

  output$maptext2 <- renderText({
    map_text2(input$sectionnameinput, input$regions.subjectinput, input$countinput, input$YAGinput, input$qualinput2)
  })

  output$maptable <- renderReactable(
    maptable(input$sectionnameinput, input$regions.subjectinput, input$countinput, input$YAGinput, input$regioninput, input$qualinput2)
  )

  # Putting the regional sankey in a reactive as well as it's a bit intensive.
  reactiveRegionalSankey <- reactive({
    regional_sankey(input$sectionnameinput, input$regions.subjectinput, input$YAGinput, input$qualinput2)
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

  output$crosstab_text <- renderText({
    crosstab_text(reactiveSubjbyIndGroupedSummary(), input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3)
  })
  reactiveSubjIndTable <- reactive({
    crosstabs(reactiveSubjbyIndGroupedData, input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3, input$earningsbutton)
  })

  output$crosstab <- renderReactable({
    reactiveSubjIndTable()
  })

  reactiveIndSubjTable <- reactive({
    backwards_crosstabs(input$sectionnameinput2, input$YAGinput3, input$countinput3, input$qualinput4, input$earningsbutton2, input$groupinput)
  })
  output$crosstab_backwards <- renderReactable({
    reactiveIndSubjTable()
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$crosstabs.subjectinput, input$YAGinput2, "YAG", input$countinput2, "LEO_SIC.csv", sep = "_")
    },
    content = function(file) {
      write.csv(downloadcrosstabs(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3), file)
    }
  )


  output$crosstab_title <- renderText({
    crosstab_title(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3)
  })


  output$backwards_crosstab_title <- renderText({
    crosstab_title(input$sectionnameinput2, input$YAGinput3, input$countinput3, input$qualinput4)
  })

  output$sankeyhelp <- renderText({
    paste(a(h4("How to read this sankey?")))
  })

  observe({
    if (input$qualinput != "All") {
      data_filtered <- qual_subjects %>%
        filter(qualification_TR == input$qualinput3) %>%
        distinct()
    } else {
      data_filtered <- qual_subjects
    }
    updateSelectInput(
      session, "subjInd.subjectinput",
      unique(c("All", data_filtered$subject_name))
    )
  })

  observe({
    if (input$sectionnameinput != "All") {
      data_filtered <- industry_groups %>%
        filter(SECTIONNAME == input$sectionnameinput2) %>%
        distinct()
    } else {
      data_filtered <- industry_groups
    }
    updateSelectizeInput(
      session, "groupinput",
      choices = unique(c("All", data_filtered$group_name))
    )
  })
  
  
  # output$subjecttable <- renderReactable({
  #   subjecttable(input$sectionnameinput2, input$YAGinput3, input$countinput3)
  # })
  #
  # output$subjecttable_title <- renderText({
  #   subjecttable_title(input$sectionnameinput2, input$YAGinput3, input$countinput3)
  # })
}
