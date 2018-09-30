# Define server logic required to draw a histogram
shinyServer(function(input, output) {
###########################################  
  #GRAPH 1: TOTAL RESEARCH STUDIES  
  output$Fig1 <- renderPlotly({
    ggplotly(test) 
  }) 
  
#Number of Studies VALUE BOXES
  
#Reactive Practice Dataset 
  reactive_df1 <- reactive({
    Fig1 %>% 
      filter(Year==input$date_input) %>% 
      filter(Status=="Active") %>%
      arrange(desc(Year))
    })
 #creating the valueBoxOutput content (based on the reactive_df above)
  output$value1 <- renderValueBox({
    valueBox(
      paste0(reactive_df1()$Count),
      'Total Number of Active Studies'
      ,icon = icon("book")
      ,color = "light-blue")  
  })
  
  
  #Reactive Practice Dataset 
  reactive_df2 <- reactive({
    Fig1 %>% 
      filter(Year==input$date_input) %>% 
      filter(Status=="Completed") %>%
      arrange(desc(Year))
  })  
  #Value box based on reactive values above
  output$value2 <- renderValueBox({
    valueBox(
      paste0(reactive_df2()$Count),
      'Total Number of Completed Studies'
      ,icon = icon("star",lib='glyphicon')
      ,color = "light-blue")   
  })  
  
#Reactive Practice Dataset 
  reactive_df3 <- reactive({
    Fig1 %>% 
      filter(Year==input$date_input) %>% 
      summarise(Total=sum(Count)) 
  })  
  #Value box based on reactive values above
  output$value3 <- renderValueBox({
    valueBox(
      paste0(reactive_df3()$Total),
      'Total Number of Studies'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")   
  })  
  
######################################  
  
  #TABLE 1- Research Studies By Lab
  #Displaying Hyperlinks
  TABLE1_Input<- reactive({
    switch(input$radio_TABLE1, 
           "No"= Table1[,-8],
           "Yes"= Table1)
  })  
  
  #({output$table1<-renderDataTable(expr=datatable(TABLE1_Input(), escape=FALSE, options= list(paging = FALSE, searching= FALSE))
  #                )
  #})
  output$table1<-renderDataTable({
    datatable(TABLE1_Input(), options=list(paging=FALSE), rownames=FALSE, escape=FALSE) 
    
  })
  
  #TABLE 1a- SUM of TABLE1
  output$Table1a<-renderDataTable({
    datatable(Table1a, options=list(paging=FALSE, searching=FALSE, ordering=FALSE,
                                    columnDefs = list(list(
                                      className = 'dt-center', targets=0:6))), rownames=FALSE) %>% 
      formatStyle('Summary', color='white', backgroundColor='#354377') %>% 
      formatStyle('2012-2013', color='white', backgroundColor='#354377') %>%  
      formatStyle('2013-2014', color='white', backgroundColor='#354377') %>% 
      formatStyle('2014-2015', color='white', backgroundColor='#354377') %>% 
      formatStyle('2015-2016', color='white', backgroundColor='#354377') %>% 
      formatStyle('2016-2017', color='white', backgroundColor='#354377') %>% 
      formatStyle('2017-2018', color='white', backgroundColor='#354377')  
  })
  
######################################################  
  #STUDENT ENGAGMENT 
  output$Fig2 <- renderPlotly({
    ggplotly(test1) 
  })   

  
  #Number of Students Value Box
  
  #Reactive Practice Dataset: Undergraduates
  reactive_df4 <- reactive({
    Fig2 %>% 
      filter(Year==input$date_input1) %>% 
      filter(Student=="Undergraduate Students") %>%
      arrange(desc(Year))
  })
  #creating the valueBoxOutput content (based on the reactive_df above)
  output$value4 <- renderValueBox({
    valueBox(
      paste0(reactive_df4()$Count),
      '# of Undergraduate Students'
      ,icon = icon("users")
      ,color = "light-blue")  
  })
  
  
  #Reactive Practice Dataset 
  reactive_df5 <- reactive({
    Fig2 %>% 
      filter(Year==input$date_input1) %>% 
      filter(Student=="Master's Students") %>%
      arrange(desc(Year))
  })  
  #Value box based on reactive values above
  output$value5 <- renderValueBox({
    valueBox(
      paste0(reactive_df5()$Count),
      '# of Master Students'
      ,icon = icon("graduation-cap")
      ,color = "light-blue")   
  })  
  
  #Reactive Practice Dataset 
  reactive_df6 <-  reactive({
    Fig2 %>% 
      filter(Year==input$date_input1) %>% 
      filter(Student=="Doctoral Students") %>%
      arrange(desc(Year))
  })  
  #Value box based on reactive values above
  output$value6 <- renderValueBox({
    valueBox(
      paste0(reactive_df6()$Count),
      '# of PhD Students'
      ,icon = icon("institution")
      ,color = "light-blue")   
  })      
  

  
  #Reactive Practice Dataset 
  reactive_df7 <-  reactive({
    Fig2 %>% 
      filter(Year==input$date_input1) %>% 
      filter(Student=="Post Doctoral Fellows") %>%
      arrange(desc(Year))
  })  
  #Value box based on reactive values above
  output$value7 <- renderValueBox({
    valueBox(
      paste0(reactive_df7()$Count),
      '# of Post-Doctoral Fellows'
      ,icon = icon("book")
      ,color = "light-blue")   
  })        
  
########################################################  
  #GRAPH 2: Figure 2- 2017-18: Engagement in Collaborative Research Studies by SSRL Laboratory 
  output$Fig1a <- renderPlotly({
    plot_ly(CollabStudies20172018, x=~Count, y=~Unit, type='bar', text=~Lab, orientation='h',
            marker=list(color='#354377')) %>% 
     layout(xaxis=x, yaxis=y)  
  })   
 
#TABLE 3- Number of Researchers by Origin
  output$table3<-renderDataTable({
   datatable(Table3, options=list(paging=FALSE, searching=TRUE, ordering=TRUE,
                                  columnDefs = list(list(
                                    className = 'dt-center', targets=1:6))),rownames=FALSE)
  })

  
  #TABLE 3a- SUM of TABLE3
  output$Table3a<-renderDataTable({
    datatable(Table3a, options=list(paging=FALSE, searching=FALSE, ordering=FALSE,
                                    columnDefs = list(list(
                                      className = 'dt-center', targets=1:6))), rownames=FALSE)%>% 
      formatStyle('Summary', color='white', backgroundColor='#354377') %>% 
      formatStyle('2012-2013', color='white', backgroundColor='#354377') %>%  
      formatStyle('2013-2014', color='white', backgroundColor='#354377') %>% 
      formatStyle('2014-2015', color='white', backgroundColor='#354377') %>% 
      formatStyle('2015-2016', color='white', backgroundColor='#354377') %>% 
      formatStyle('2016-2017', color='white', backgroundColor='#354377') %>% 
      formatStyle('2017-2018', color='white', backgroundColor='#354377')
  })

######
#Research Outputs GRAPH

  output$Fig3a <- renderPlotly({
    plot_ly(Fig3, x=~Year, y=~get(input$yvar), type='bar', text="Total Research Outputs", 
            marker=list(color='#354377')) %>% 
      layout(xaxis=y, yaxis=y)  
  })    
  #TABLE 4- Number of Researchers by Origin
  output$table4<-DT::renderDataTable({
    DT::datatable(options = list(paging = FALSE, searching= FALSE, 
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '500px', targets = "_all"))),
                  Fig3[,input$show_vars, drop = FALSE]
                  )  
                 })   
    
 
  
  #NETWORK ANALYSIS
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
        render.d3movie(net3.dyn, usearrows = F,
                       displaylabels = F, label=net3 %v% "group",
                       bg="#ffffff", vertex.border="#333333",
                       vertex.cex = net3 %v% "size"/10,
                       vertex.col = net3.dyn %v% "col",
                       edge.lwd = (net3.dyn %e% "value")/10,
                       edge.col = '#55555599',
                       vertex.tooltip = paste("<b>Name:</b>", (net3.dyn %v% "name") , "<br>",
                                              "<b>Group:</b>", (net3.dyn %v% "group.name")),
                       edge.tooltip = paste("<b>Number of Collaborations:</b>", (net3.dyn %e% "value" )),
                       #launchBrowser=T, filename="SSRL SNA 2017 and 2018.html",
                       render.par=list(tween.frames = 30, show.time = F), output.mode = 'htmlWidget')
  }
  )
})

