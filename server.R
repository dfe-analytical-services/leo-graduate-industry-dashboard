server <- function(input, output, session) {
  
  # Closes session after closing app (saves time)
  
  session$onSessionEnded(stopApp)
  
# Links to tabs --------------------------------------------
  
  observeEvent(input$link_to_industryFlow_tab, {
    updateTabsetPanel(session, "navbar", selected = "industryFlow")
  })
  
  
# Sankey functions --------------------------------------------------------
  
  output$sankeysubjectlist <- renderUI({
    selectInput("subjectinput",
                label = "Select a subject area",
                choices = c(unique(tables_data$subject_name[which(tables_data$qualification_TR == input$qualinput)])),
                selected = 'All'
    )
  })
  
  output$sankey <- renderSankeyNetwork({
    sankey_chart(input$subjectinput,input$sexinput, input$qualinput)
  })

  
  output$sankey_title <- renderText({
    sankey_title(input$subjectinput, input$sexinput, input$qualinput)
  })
  
  output$sankey_table <- renderReactable({
    sankey_table(input$subjectinput, input$sexinput, input$qualinput)
  })
  

  output$sankeytext1 <- renderText({
    sankeytext1(input$subjectinput, input$sexinput, input$qualinput)
  })
  
  output$sankeytext2 <- renderText({
    sankeytext2(input$subjectinput, input$sexinput, input$qualinput)
  })
  
  # output$earningstext <- renderText({
  #   earnings_text(input$subjectinput, input$sexinput)
  # })
  
  # output$earnings_sankey <- renderSankeyNetwork({
  #   earnings_sankey(input$subjectinput, input$sexinput, input$Earningsinput)
  # })
  # 
  # output$earnings_table <- renderReactable({
  #   earnings_table(input$subjectinput, input$sexinput, input$Earningsinput)
  # })
  

# Map functions -----------------------------------------------------------

  output$mapsubjectlist <- renderUI({
    selectInput("subjectinput2",
                label = "Select a subject area",
                choices = c(unique(regional_movement_data$subject_name[which(regional_movement_data$qualification_TR == input$qualinput2 & regional_movement_data$SECTIONNAME == input$sectionnameinput & regional_movement_data$count >= 3 & regional_movement_data$YAG == input$YAGinput)])),
                selected = 'All'
    )
  })

  output$map <- renderLeaflet({
    map_chart(input$sectionnameinput, input$subjectinput2, input$countinput, input$YAGinput, input$qualinput2)
  })

  output$map_title <- renderText({
    map_title(input$sectionnameinput, input$subjectinput2, input$countinput, input$YAGinput, input$qualinput2)
  })
  
  output$maptext <- renderText({
    map_text(input$sectionnameinput, input$subjectinput2, input$countinput, input$YAGinput, input$qualinput2)
  })
  
  output$maptext2 <- renderText({
    map_text2(input$sectionnameinput, input$subjectinput2, input$countinput, input$YAGinput, input$qualinput2)
  })
  
  output$maptable <- renderReactable(
    maptable(input$sectionnameinput, input$subjectinput2, input$countinput, input$YAGinput, input$regioninput, input$qualinput2)
  )
  
  output$regional_sankey <- renderSankeyNetwork({
    regional_sankey(input$sectionnameinput, input$subjectinput2, input$YAGinput, input$qualinput2)
  })
  
  output$regional_sankey_title <- renderText({
    regional_sankey_title(input$sectionnameinput, input$subjectinput2, input$YAGinput, input$qualinput2)
  })
  

# Tables functions --------------------------------------------------------

  
  output$crosstab <- renderReactable({
    crosstabs(input$subjectinput3, input$YAGinput2, input$countinput2, input$qualinput3, input$earningsbutton, input$thresholdinput)
    
  })
  
  output$crosstab_backwards <- renderReactable({
    backwards_crosstabs(input$sectionnameinput2, input$YAGinput3, input$countinput3, input$qualinput4, input$earningsbutton2)  
  })

  output$downloadData <- downloadHandler(
    filename = function(){
      paste(input$subjectinput3, input$YAGinput2,'YAG', input$countinput2,'LEO_SIC.csv', sep = '_')
    },
    content = function(file){
      write.csv(downloadcrosstabs(input$subjectinput3, input$YAGinput2, input$countinput2, input$qualinput3, input$thresholdinput), file)
    }
  )


  output$crosstab_title <- renderText({
    crosstab_title(input$subjectinput3, input$YAGinput2, input$countinput2, input$qualinput3)
  })
  
  output$backwards_crosstab_title <- renderText({
    crosstab_title(input$sectionnameinput2, input$YAGinput3, input$countinput3, input$qualinput4)
  })
  
  output$crosstab_text <- renderText({
    crosstab_text(input$subjectinput3, input$YAGinput2, input$countinput2, input$qualinput3, input$thresholdinput)
  })
  
  output$sankeyhelp <- renderText({
    paste(a(h4('How to read this sankey?')))
  })

 output$subjectlist3 <- renderUI({
   selectInput("subjectinput3",
               label = "Select a subject area",
               choices = c(unique(tables_data$subject_name[which(tables_data$qualification_TR == input$qualinput3)])),
               selected = 'All'
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