# Number of Studies Value Boxes
frow1 <- fluidRow(
  #making a drop-down of the different Years
  selectInput("date_input", label = "Select Year", choices = levels(factor(Fig1$Year)),
              selected="2017-2018"
  ), #selectInput 
  HTML ('</br>'),
  valueBoxOutput("value1", width=4)  #Active Studies
  ,valueBoxOutput("value2", width=4) #Completed Studies
  ,valueBoxOutput("value3", width=4) #Total Studies
  ,HTML ('</br>')
)#fluidRow


# STUDENT ENGAGEMENT
frow2 <- fluidRow(
  #making a drop-down of the different Years
  selectInput("date_input1", label = "Select Year", choices = levels(factor(Fig2$Year)),
              selected="2017-2018"
  ), #selectInput 
  HTML ('</br>'),
  valueBoxOutput("value4", width=4)  #Undergrad
  ,valueBoxOutput("value5", width=4) #Master
  ,valueBoxOutput("value6", width=4) #PhD
  #,valueBoxOutput("value7", width=3) #Post Doc
  ,HTML ('</br>')
)#fluidRow


dashboardPage(skin = "blue", title = 'SSRL Annual Report',     
              dashboardHeader(title= "2017-2018 Report"), #Insert the Main Title
              
              
              dashboardSidebar(  
                sidebarMenu(
                  menuItem("# of Studies", tabName= "first", icon= icon("info-circle")),
                  menuItem("Collaborations", tabName="second", icon=icon("group")),
                  menuItem("Researcher Involvement", tabName="third", icon=icon("institution")),
                  menuItem("Student Engagement", tabName="fourth", icon=icon("graduation-cap")),
                  menuItem("Research Outputs", tabName="fifth", icon=icon("globe")),
                  menuItem("Network Analysis", tabName="sixth", icon=icon("sitemap")),
                  menuItem("Past Annual Reports", icon = icon("folder"), 
                           
                  menuSubItem('2016-2017',href = "http://ssrl.usask.ca/ssrl-2016-2017-annual-report.php"
                              ),
                  menuSubItem('2015-2016',href = "https://ssrl.usask.ca/documents/SSRL_2015-2016_Annual_Report_-_Building_Bridges.pdf"
                  ),
                  menuSubItem('2014-2016',href = "https://ssrl.usask.ca/documents/SSRL%202014-2015%20Annual%20Report%20-%20Enhancing%20Capacity.pdf"
                  ),
                  menuSubItem('2013-2014',href = "https://ssrl.usask.ca/documents/SSRL%202013-2014%20Annual%20Report%20-%20Enhancing%20Capacity.pdf"
                  ),
                  menuSubItem('2012-2013',href = "https://ssrl.usask.ca/documents/SSRL%202012-2013%20Annual%20Report%20-%20Enhancing%20Capacity.pdf"
                  )
                  )
                )),
              dashboardBody(  
                tabItems(
                  tabItem(tabName= "first",
                           frow1
                          ,HTML ('</br>')
                          ,h3("Figure 1- Total Research Studies"),
                          plotlyOutput("Fig1",width = "100%", height = "100%")
                          ),
                  tabItem(tabName="second",
                          tabsetPanel(
                            tabPanel("2012-2013 to 2017-2018",
                                     #HTML ('</br>'),           
                                     h3("Table 1- Number of Research Studies by Lab"),
                                     HTML ('</br>'),
                                     #Inserting Radio Buttons
                                     radioButtons("radio_TABLE1", label= h5 ("Show Hyperlinks:"),
                                                  choices= c ("Yes", "No"), selected="No", inline=TRUE),
                                     dataTableOutput('table1'),
                                     h6("* Blank cells indicates the lab was not fully operable at any time during the fiscal year"),
                                     #HTML ('</br>'),
                                     #HTML ('</br>'),
                                     #HTML ('</br>'),
                                     #HTML ('</br>'),
                                     #HTML ('</br>'),
                                     #SUMMATION TABLE
                                     dataTableOutput("Table1a")
                            ),
                            tabPanel("2017-2018 Collaborative Research Studies by Lab",
                                     h3("Figure 2- 2017-18: Engagement in Collaborative Research Studies by SSRL Laboratory"),
                                     plotlyOutput("Fig1a",width = "100%", height = "100%")
                          )
                          )),
                  tabItem(tabName="third",
                          tabsetPanel(
                            tabPanel("Overview",
                          h3("Table 2- Number of Researchers by Origin"),
                          dataTableOutput("Table3a")
                          ),
                          tabPanel("2012-2013 to 2017-2018",
                          #HTML ('</br>'),
                         # htmlOutput('table3'),
                          dataTableOutput('table3'),
                          h6("*Blank cells indicates college, school or supporting unit was not affiliated with the SSRL at any time during the fiscal year. Previous years' data for these colleges, schools or supporting units are reported in the row 'Other University of Saskatchewan' in the 'Overview' tab")
                          )
                          )),
                  tabItem(tabName="fourth"
                          ,frow2
                          ,HTML ('</br>')
                          ,h3("Figure 3- Number of Students Trained or Employed"),
                          plotlyOutput("Fig2",width = "100%", height = "100%")
                          ),
                  tabItem(tabName="fifth",
                          tabsetPanel(
                            tabPanel("Graph",
                          h3("Figure 4- Number of Research Outputs per Year"),
                          HTML ('</br>'),
                          #Inserting Drop-Down Menu
                          selectInput("yvar", "Choose a Research Output:", choices= c("Books", "Book Chapters", "Conference Papers and Presentations",
                                                                                      "Media Articles", "Non-Refereed Journal Articles",
                                                                                      "Peer-Reviewed Journal Articles", "Review Articles", "Technical Reports",
                                                                                      "Theses", "Websites")),
                          plotlyOutput("Fig3a",width = "100%", height = "100%")
                          ),
                          tabPanel("Summary Table",
                                   HTML ('</br>'),
                                   checkboxGroupInput('show_vars', 'Columns to show in the table:',
                                                      choices= c("Year","Books", "Book Chapters", "Conference Papers and Presentations",
                                                                 "Media Articles", "Non-Refereed Journal Articles",
                                                                 "Peer-Reviewed Journal Articles", "Review Articles", "Technical Reports",
                                                                 "Theses", "Websites"), selected = c("Year","Peer-Reviewed Journal Articles","Books",
                                                                                                             "Book Chapters","Conference Papers and Presentations"), inline=TRUE),
                                   DT::dataTableOutput('table4') 
                          )
                          )),
                  tabItem(tabName="sixth",
                          h5("Note: This will take a few minutes to load"),
                          HTML ('</br>'),
                          ndtv:::ndtvAnimationWidgetOutput("netPlot")
                          )
                )) 
) 