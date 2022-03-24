server <- function(input, output, session) {

  # Closes session after closing app (saves time)

  session$onSessionEnded(stopApp)

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
      choices = unique(data_filtered$subject_name)
    )
  })

  reactiveSankey <- reactive(sankey_chart(input$indflow.subjectinput, input$sexinput, input$qualinput))

  output$sankey <- renderSankeyNetwork({
    reactiveSankey()
  })

  reactiveSankeyTitle <- reactive(sankey_title(input$indflow.subjectinput, input$sexinput, input$qualinput))
  output$sankey_title <- renderText({
    reactiveSankeyTitle()
  })

  reactiveSankeyTable <- reactive({
    sankey_table(input$indflow.subjectinput, input$sexinput, input$qualinput)
  })
  output$sankey_table <- renderReactable({
    reactiveSankeyTable()
  })


  output$sankeytext1 <- renderText({
    sankeytext1(input$indflow.subjectinput, input$sexinput, input$qualinput)
  })

  output$sankeytext2 <- renderText({
    sankeytext2(input$indflow.subjectinput, input$sexinput, input$qualinput)
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
    updateSelectizeInput(session, "regions.subjectinput",
      choices = unique(data_filtered$subject_name)
    )
  })

  output$map <- renderLeaflet({
    map_chart(input$sectionnameinput, input$regions.subjectinput, input$countinput, input$YAGinput, input$qualinput2)
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

  output$regional_sankey <- renderSankeyNetwork({
    regional_sankey(input$sectionnameinput, input$regions.subjectinput, input$YAGinput, input$qualinput2)
  })

  output$regional_sankey_title <- renderText({
    regional_sankey_title(input$sectionnameinput, input$regions.subjectinput, input$YAGinput, input$qualinput2)
  })


  # Tables functions --------------------------------------------------------


  output$crosstab <- renderReactable({
    crosstabs(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3, input$earningsbutton, input$thresholdinput)
  })

  output$crosstab_backwards <- renderReactable({
    backwards_crosstabs(input$sectionnameinput2, input$YAGinput3, input$countinput3, input$qualinput4, input$earningsbutton2)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$crosstabs.subjectinput, input$YAGinput2, "YAG", input$countinput2, "LEO_SIC.csv", sep = "_")
    },
    content = function(file) {
      write.csv(downloadcrosstabs(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3, input$thresholdinput), file)
    }
  )


  output$crosstab_title <- renderText({
    crosstab_title(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3)
  })

  output$backwards_crosstab_title <- renderText({
    crosstab_title(input$sectionnameinput2, input$YAGinput3, input$countinput3, input$qualinput4)
  })

  output$crosstab_text <- renderText({
    crosstab_text(input$crosstabs.subjectinput, input$YAGinput2, input$countinput2, input$qualinput3, input$thresholdinput)
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
    updateSelectInput(session, "subjInd.subjectinput",
      choices = unique(data_filtered$subject_name)
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
