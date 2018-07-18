library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
library(highcharter)
library(plotly)
library(ggmap)
library(shinyjs)
library(maps)
library(leaflet)
library(lattice)
library(tm)
library(wordcloud)
ui <- dashboardPage(skin=c("red"),
                    
       dashboardHeader(title ="Retail Dashboard"),
                
       dashboardSidebar(width = 0),## dashboardSidebar
                    
       dashboardBody(
                      
           fluidRow(
                        
             div(div(style="display: inline-block;vertical-align:top; width: 100px;",
                  selectInput("year_inptdr1", "Year", selected=2017, choices = list("All",2017,2016,2015,2014), width = "100px")),
                            
                 div(style="display: inline-block;vertical-align:top; width: 100px;",
                   selectInput("month_inptdr2", "Month", selected='All', choices = list("All","Jan"=1,"Feb"=2,"Mar"=3,"Apr"=4,"May"=5,"Jun"=6,"Jul"=7,"Aug"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dec"=12),
                                            width = "100px"))
                 # ,div(style="display: inline-block;vertical-align:top; width: 300px; height=300px",
                 #     tags$head(tags$style(HTML(".small-box {height: 50px}"))),
                 #     valueBoxOutput("avg_pur_val",width="300px"))
                 
                 )),
           fluidRow(
              box(tabsetPanel(tabPanel("Revenue", highchartOutput("year_sales_graph", height="300px", width="700px")),
                              tabPanel("Wordcloud", plotOutput("word_cloud", height = "300px", width ="700px")),
                              tabPanel("Branch",
                                       div(
                                         div(style="display: inline-block;vertical-align:top; width: 150px;",
                                             selectInput("branch_inptdr6", "Branch:", selected = 'Ave West', choices = branches,
                                                         width = "150px")),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                             selectInput("category_inptdr7", "Category:", selected = 'Casual', choices = categories,
                                                        width = "100px")),
                                        div(style="display: inline-block;vertical-align:top; width: 150px;",
                                             selectInput("brand_inptdr8", "Brand:", selected = 'Evisu', choices = brands,
                                                         width = "150px"))),
                                       
                                       highchartOutput("Filter_inter_graph", height = "220px", width ="700px"))),##tabsetPanel
                      title=strong("Revenue"),
                      status="primary",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      height = "400px",
                      width=6 ), ##box
                  
                    box(tabsetPanel(tabPanel("Category", plotlyOutput("category_analysis_graph", height="300px", width="700px")),
                                    tabPanel("Age Groups", plotlyOutput("age_analysis_graph", height="300px", width="700px"))),
                      title=strong("Category"),
                      status="primary",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      height = "400px",
                      width=6 )  ##box
                  ), ##fluidrow
                  
          fluidRow(
              box(tabsetPanel(tabPanel("Brand", htmlOutput("brand_sales_graph", height="400px", width="285px"))),
                      title=strong("Brand"),
                      status="primary",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      height = "400px",
                      width=2 ), ##box
                    
              box(tabsetPanel(tabPanel("Gender", highchartOutput("gender_sales_graph", height="325px", width="450px")),
                              tabPanel("Registered vs Unregistered",
                                            highchartOutput("registered_sales_graph", height="250px", width="450px")), 
                              tabPanel("Weekday vs Weekends", plotlyOutput("week_sales_graph", height="300px", width="450px"))),
                      title=strong("Customer"),
                      status="primary",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      height = "400px",
                      width = 4 ),
           
              box(
              tabsetPanel(
                tabPanel("Branch",leafletOutput("branch_analysis_graph",height="300px",width="700px")),
                tabPanel("Group",
                         
                         div(
                      

                      #     div(style="display: inline-block;vertical-align:top; width: 100px;",
                      #         selectInput("gender_inptdr4","Gender:", selected= 'All', choices = gender,
                      #         width = "100px")),
                      #     
                           div(style="display: inline-block;vertical-align:top; width: 100px;",
                              selectInput("category_inptdr3","Category:", selected = 'Casual', choices = categories,
                              width = "100px")),
                           
                           div(style="display: inline-block;vertical-align:top; width: 150px;",
                               selectInput("branch_inptdr4","Branch:", selected = 'Ave West', choices = branches,
                                           width = "150px")),

                           div(style="display: inline-block;vertical-align:top; width: 150px;",
                              selectInput("brand_inptdr5","Brand:", selected = 'Addidas', choices = brands,
                              width = "150px"))),
                      
                              highchartOutput("group_analysis_graph",height="200px",width="700px")
                          
                          )),## tabsetpanel
                    
                      title=strong("Location"),
                      status="primary",
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      height = "400px",
                      width=6 ) ##box
            
            ) ##fluidrow
            
      ) ## dashboardBody
            
) ## dashboardPage