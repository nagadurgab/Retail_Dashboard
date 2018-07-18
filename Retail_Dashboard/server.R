library(RODBC)
library(lubridate)

server <- shinyServer(function(input, output, session) {
  
  observe({
    
    con <- odbcDriverConnect('driver={SQL Server};server=AXONML-SERVER;database=Retail_Dashboard;uid=axonmluser;pwd=axon*2016BZA;',rows_at_time = 100)
    
    str_input1 <- input$year_inptdr1
    str_input2 <- input$month_inptdr2
    str_input3 <- input$category_inptdr3
    str_input4 <- input$branch_inptdr4
    str_input5 <- input$brand_inptdr5
    str_input6 <- input$branch_inptdr6
    str_input7 <- input$category_inptdr7
    str_input8 <- input$brand_inptdr8
    
    if(str_input1 == 'All'){
      
      if(str_input2 == 'All'){
        
        
        ################   Revenue Graph 1 1  ###########################
        
        yRevenueDF <- sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                         from sales_data 
                                         group by year(sold_date) 
                                         order by year(sold_date)"))
        # yavg_pur_val <- sqlQuery(con, paste0("select sum(net_pay)/count(*) as avg_pur_val
        #                                  from sales_data"))
        # yavg_pur_val <- yavg_pur_val$avg_pur_val
        # output$avg_pur_val <- renderValueBox({
        #   valueBox(
        #     paste(round(yavg_pur_val,2)," $"), "Average Purchase Value", 
        #     color = "fuchsia")
        #   
        # }) ## valuebox()
        
        yRevenueDF$year_labels <- list("2014","2015","2016","2017")
        
        # output$year_sales_graph <- renderPlotly({
        # 
        #   f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
        #   x <- list(title = "Year",titlefont = f)
        #   y <- list(title = "Revenue($)",titlefont = f)
        # 
        #   revenuechart<-plot_ly(x=yRevenueDF$Years, y=yRevenueDF$Revenue, type="area", showlegend=FALSE)%>%
        #     layout(xaxis = x, yaxis = y, showlegend=FALSE)
        # 
        #   year_sales_graph<-plotly_build(qplot(1:10))
        #   year_sales_graph$elementId <- NULL
        #   return(revenuechart)
        # }) ## Output$year_sales_graph()
        
        output$year_sales_graph <- renderHighchart({

          highchart() %>%
            hc_chart(type="line") %>%

            hc_xAxis(title=list(text = "Year"),categories = yRevenueDF$Years)%>%
            hc_yAxis(title = list(text = "Revenue($)")) %>%
            hc_add_series(data = yRevenueDF$Revenue, type = "line",color="orange")

        })

        
        
        ###################   Wordcloud 1 2 ####################################################
        
        ywordcloud <-sqlQuery(con, paste0("select item_name as Item from item_data, sales_data 
                                          where sales_data.item_id = item_data.item_id"))
        
        
        item_name = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", ywordcloud$Item )
        
        # item_name = gsub("@\\w+", "", item_name)   # remove at people
        
        item_name = gsub("[[:punct:]]", "", item_name)   # remove punctuation
        
        item_name = gsub("[[:digit:]]", "", item_name)   # remove numbers
        
        item_df <- data.frame(item_name) ## dataframe
        
        itemcorpus <- Corpus(VectorSource(item_df$item_name))   ## corpus
        
        output$word_cloud<- renderPlot({
          pal <- brewer.pal(9,"Spectral")
          pal <- pal[(1:9)]
          set.seed(123)
          wordcloud::wordcloud(words = itemcorpus,scale=c(3.5,.5),max.words=75,
                               random.order=FALSE,rot.per=0.35, use.r.layout=FALSE, colors=pal)
        }) ### output$word_cloud()
        
        ##################### Category Graph  1 3 #############################################
        
        ycategoryDF<-sqlQuery(con, "select item_category as Category,
                              sum(sales_data.net_pay) as Revenue 
                              FROM item_data,sales_data 
                              where sales_data.item_id = item_data.item_id 
                              group by  item_category
                              order by Category")
        
       
        
        output$category_analysis_graph <- renderPlotly({
          
          f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
          x <- list(title = "Category",titlefont = f)
          y <- list(title = "Revenue ($)",titlefont = f)
          
          categorychart<-plot_ly(x=ycategoryDF$Category,y=ycategoryDF$Revenue,type="bar",color = ycategoryDF$Category, showlegend=FALSE)%>%
            layout(xaxis = x, yaxis = list(range = c(150000000, 165000000)), showlegend=FALSE)
          
          
          
          return(categorychart)
          
        }) ## output$category_analysis_graph()
        
        ###################   Brand Table 1 4  ####################################################################
        
        ybrandDF<- sqlQuery(con,paste0("select top 10 brand_name as Brand,
                                       sum(net_pay) as Revenue 
                                       from sales_data,brand_data,item_data 
                                       where brand_data.brand_id = item_data.brand_id 
                                       and item_data.item_id = sales_data.item_id 
                                       group by brand_name 
                                       order by Revenue DESC"))
        
        output$brand_sales_graph<- renderGvis({
          BrandRevenue<- select(ybrandDF,Brand,Revenue)
          BrandRevenuechart<-gvisTable(BrandRevenue,options=list(height="250px"))
          return(BrandRevenuechart)
        }) ## output$brand_sales_graph()
        
        #################   Branch Map   1 5  ########################################################################
        
        ybranchDF<-sqlQuery(con,paste0("select branch_name as Branch,sum(net_pay) as Revenue,
                                       branch_data.lat as latitude,
                                       branch_data.lon as longitude 
                                       from branch_data,sales_data 
                                       where branch_data.branch_id = sales_data.branch_id 
                                       group by branch_name, branch_data.lat,branch_data.lon
                                       order by Branch"))
        
        output$branch_analysis_graph <- renderLeaflet({
          leaflet(data=ybranchDF) %>%
            addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
            
            addMarkers(data = ybranchDF, lng = ybranchDF$longitude, lat = ybranchDF$latitude, 
                       popup = paste( "<div class='leaflet-popup-scrolled' style='max-height:200px'>",
                                      "<h5 style='color:black'><b>",ybranchDF$Branch,":",ybranchDF$Revenue,"</b></h5>"))%>%
            setView(lng = ybranchDF$longitude, lat = ybranchDF$latitude, zoom = 7) %>%
            fitBounds(lng1 = max(ybranchDF$longitude),lat1 = max(ybranchDF$latitude),
                      lng2 = min(ybranchDF$longitude),lat2 = min(ybranchDF$latitude))%>%
            clearBounds()
          
        }) ##output$branch_analysis_graph()
        
        
        ##################### Registered Vs Un Registered  1 6  ###############################################
        
        yregisterVsUnregistered <- sqlQuery(con, paste0("select customer_type,sum(net_pay) as Revenue  from 
                                                        (select case when 
                                                        customer_id = 0 then 'Unregistered'
                                                        else 'Registered' end as customer_type, 
                                                        net_pay from sales_data) table1 group by customer_type"))
        
        output$registered_sales_graph<- renderHighchart({
          
          highchart(width = 100, height = 100) %>%
            hc_title(text = "") %>%
            hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
            hc_plotOptions(pie = list(depth = 70)) %>%
            hc_add_series_labels_values(yregisterVsUnregistered$customer_type,yregisterVsUnregistered$Revenue) %>%
            hc_tooltip(pointFormat = "$: {point.y}")%>%
            hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
        }) ### Output$registered_sales_graph()
        
        ########################## Gender Chart 1 7  #############################################################
        
        ygenderDF <- sqlQuery(con, paste0("select Gender, sum(Revenue) as Revenue from (select case 
                                          when item_for = 'boy' then 'Male'
                                          when item_for = 'Men' then 'Male'
                                          when item_for = 'girl' then 'Female' 
                                          when item_for = 'Women' then 'Female' 
                                          when item_for = 'home-utility' then 'Home-Utility' end as Gender, 
                                          sum(net_pay) as Revenue from item_data,sales_data
                                          where item_data.item_id = sales_data.item_id
                                          group by  item_for) table1 group by Gender"))
        
        output$gender_sales_graph<- renderHighchart({
          
          highchart(width = 100, height = 100) %>%
            hc_title(text = "") %>%
            
            hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
            hc_plotOptions(pie = list(depth = 70)) %>%
            hc_add_series_labels_values(namme = NULL,ygenderDF$Gender,ygenderDF$Revenue) %>%
            hc_tooltip(pointFormat = "$: {point.y}")%>%
            hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
        }) ### output$gender_sales_graph()
        
        ########################## Age Group Chart 1 8 #############################
        
        yagesDF<-sqlQuery(con, paste0("select item_for, 
                                      sum(net_pay) as Revenue from item_data,sales_data
                                      where item_data.item_id = sales_data.item_id
                                      group by  item_for"))
        
        output$age_analysis_graph <- renderPlotly({
          
          f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
          x <- list(title = "Category",titlefont = f)
          y <- list(title = "Revenue ($)",titlefont = f)
          
          ageschart<-plot_ly(x=yagesDF$item_for,y=yagesDF$Revenue,type="bar",color = yagesDF$item_for, showlegend=FALSE)%>%
            layout(xaxis = x, yaxis = y, showlegend=FALSE)
          
          return(ageschart)
          
        }) ## output$age_analysis_graph()
        
        
        ############################ Group Graph 1 9 ########################################################
        
         yRevenueDF1 <- sqlQuery(con, paste0("select year(sold_date) as Year,
                                            sum(net_pay) as Revenue
                                            from sales_data 
                                            group by year(sold_date) 
                                            order by year(sold_date)"))
        
        ybranchDF1 <- sqlQuery(con,paste0("select year(sales_data.sold_date) as Year,
                                          sum(net_pay) as Revenue
                                          from branch_data,sales_data 
                                          where branch_data.branch_id = sales_data.branch_id and
                                          branch_data.branch_name = '", str_input4, "'
                                          group by branch_name, year(sales_data.sold_date) 
                                          order by Year"))
        
        ycategoryDF1 <- sqlQuery(con,paste0("SELECT year(sales_data.sold_date) AS Year,
                                            sum(sales_data.net_pay) AS Revenue 
                                            FROM item_data,sales_data 
                                            WHERE sales_data.item_id = item_data.item_id and
                                            item_category = '", str_input3, "'
                                            GROUP BY  year(sales_data.sold_date) 
                                            ORDER BY Year"))
        
        ybrandDF1 <- sqlQuery(con,paste0("select year(sales_data.sold_date) as Year,
                                         sum(net_pay) as Revenue 
                                         from sales_data,brand_data,item_data 
                                         where brand_data.brand_id = item_data.brand_id 
                                         and item_data.item_id = sales_data.item_id and
                                         brand_data.brand_name = '", str_input5, "'
                                         group by year(sales_data.sold_date)
                                         order by Year"))
        
        # print(str_input1,", ",str_input2,", ",str_input3,", ",str_input4,", ",str_input5)
       output$group_analysis_graph<- renderHighchart({
          highchart() %>%
            hc_chart(type="grouped") %>%
            
            hc_xAxis(title=list(text = "Year"), categories = ycategoryDF1$Year) %>%
            hc_yAxis(title = list(text = "Revenue($)")) %>%
            hc_add_series(name = "Category", data = ycategoryDF1$Revenue, type = "column", color="orange") %>%
            hc_add_series(name = "Branch", data = ybranchDF1$Revenue, type = "line", color="green") %>%
            hc_add_series(name = "Brand", data = ybrandDF1$Revenue, type = "line", color="red")
        }) ## output$group_analysis_graph
        
       #################################### Branch 1 10 ##############################################################################
       
       ybranchFilterDF <- sqlQuery(con,paste0("select year(sold_date) as WYear, sum(net_pay) as Revenue  
                                              From sales_data,branch_data,item_data,brand_data 
                                              where item_data.item_id = sales_data.item_id and
                                              item_data.brand_id = brand_data.brand_id and
                                              branch_data.branch_id = sales_data.branch_id and
                                              branch_name = '", str_input6, "' and 
                                              item_category = '", str_input7, "' and 
                                              brand_name = '",str_input8,"'  
                                              group by year(sold_date)
                                              order by year(sold_date)"))
       
       output$Filter_inter_graph <- renderHighchart({
         
         highchart() %>%
           hc_chart(type="line") %>%
           
           hc_xAxis(title=list(text = "Year"),categories=ybranchFilterDF$WYear)%>%
           hc_yAxis(title = list(text = "Revenue($)")) %>%
           hc_add_series(data = ybranchFilterDF$Revenue,type = "line",color="orange") 
         
       }) ## output$Filter_inter_graph
       
       
       #################################### Weekday 1 11 ##################################################
       yweekdayDF <- sqlQuery(con,paste0("SELECT Day_type,sum(net_pay) Revenue From (SELECT transaction_id, CASE WHEN Day_Name = 'Sunday' THEN 'Weekend'
			                                    WHEN Day_Name = 'Saturday' THEN 'Weekend'
                                          ELSE 'Weekday' END Day_type FROM 
                                          (SELECT transaction_id, sales_data.sold_date, FORMAT(sales_data.sold_date,'dddd') 'Day_Name' 
                                          FROM sales_data) table1) table2,sales_data WHERE table2.transaction_id = sales_data.transaction_id
                                          GROUP BY Day_type"))
       
       output$week_sales_graph <- renderPlotly({
         
         colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')
         
         weekday_chart <- plot_ly(yweekdayDF, labels = ~Day_type, values = ~Revenue, type = 'pie',
                      textposition = 'inside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
                      hoverinfo = 'text',
                      text = ~paste('$', Revenue),
                      marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                      #The 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE) %>%
           layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
           
           return(weekday_chart)
         
       }) ### output$week_sales_graph
       
      ##################################### 2 0 ################################################
        
      }else{       ### if-2 close
        
        str_input2 <- as.integer(str_input2)
        
        ################### Revenue Graph 2 1   #######################################################################
        
        yRevenueDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                         from sales_data where month(sold_date)=",str_input2,"
                                         group by year(sold_date) 
                                         order by year(sold_date)"))
        
        # output$year_sales_graph<- renderPlotly({
        #   
        #   f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
        #   x <- list(title = "Year",titlefont = f)
        #   y <- list(title = "Revenue ($)",titlefont = f)
        #   
        #   revenuechart<-plot_ly(x=as.integer(yRevenueDF$Years),y=yRevenueDF$Revenue,type="area",showlegend=FALSE)%>%
        #     layout(xaxis = x, yaxis = y, showlegend=FALSE)
        #   
        #   year_sales_graph<-plotly_build(qplot(1:10))
        #   year_sales_graph$elementId <- NULL
        #   return(revenuechart)
        # }) ## Output$year_sales_graph()
        
        output$year_sales_graph <- renderHighchart({
          
          highchart() %>%
            hc_chart(type="line") %>%
            
            hc_xAxis(title=list(text = "Year"),categories = yRevenueDF$Years)%>%
            hc_yAxis(title = list(text = "Revenue($)")) %>%
            hc_add_series(data = yRevenueDF$Revenue, type = "line",color="orange")
          
        })
        
        ################## Wordcloud 2 2  ###########################################################
        
        ywordcloud <-sqlQuery(con, paste0("select item_name as Item from item_data, sales_data 
                                          where sales_data.item_id = item_data.item_id
                                          and month(sales_data.sold_date)=",str_input2))
        
        
        item_name = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", ywordcloud$Item )
        
        # item_name = gsub("@\\w+", "", item_name)   # remove at people
        
        item_name = gsub("[[:punct:]]", "", item_name)   # remove punctuation
        
        item_name = gsub("[[:digit:]]", "", item_name)   # remove numbers
        
        item_df <- data.frame(item_name) ## dataframe
        
        itemcorpus <- Corpus(VectorSource(item_df$item_name))   ## corpus
        
        output$word_cloud<- renderPlot({
          pal <- brewer.pal(9,"Spectral")
          pal <- pal[(1:9)]
          set.seed(123)
          wordcloud::wordcloud(words = itemcorpus,scale=c(3,.5),max.words=75,
                               random.order=FALSE,rot.per=0.35, use.r.layout=FALSE, colors=pal)
        }) ### output$word_cloud()
        
        #####################  Category Graph 2 3   ##################################################
        
        ycategoryDF<-sqlQuery(con, paste0("select item_category as Category,
                                          sum(sales_data.net_pay) as Revenue 
                                          FROM item_data,sales_data 
                                          where sales_data.item_id = item_data.item_id
                                          and month(sold_date)=",str_input2," 
                                          group by  item_category
                                          order by Category"))
        
        output$category_analysis_graph <- renderPlotly({
          
          f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
          x <- list(title = "Category",titlefont = f)
          y <- list(title = "Revenue ($)",titlefont = f)
          
          categorychart<-plot_ly(x=ycategoryDF$Category,y=ycategoryDF$Revenue,type="bar",color = ycategoryDF$Category, showlegend=FALSE)%>%
            layout(xaxis = x, yaxis = list(range = c(12000000, 14000000)), showlegend=FALSE)
          
          return(categorychart)
          
        }) ## output$category_analysis_graph
        
        
        ###############  Brand Table 2 4 #################################
        
        ybrandDF<- sqlQuery(con,paste0("select top 10 brand_name as Brand,
                                       sum(net_pay) as Revenue 
                                       from sales_data,brand_data,item_data 
                                       where brand_data.brand_id = item_data.brand_id 
                                       and item_data.item_id = sales_data.item_id
                                       and month(sales_data.sold_date) =",str_input2,"
                                       group by brand_name 
                                       order by Revenue DESC"))
        
        output$brand_sales_graph<- renderGvis({
          BrandRevenue<- select(ybrandDF,Brand,Revenue)
          BrandRevenuechart<-gvisTable(BrandRevenue,options=list(height="250px"))
          return(BrandRevenuechart)
          
        }) ### output$brand_sales_graph()
        
        ######################### Branch Map 2 5   ##################################################
        
        ybranchDF<-sqlQuery(con,paste0("select branch_name as Branch,
                                       sum(net_pay) as Revenue,
                                       branch_data.lat as latitude,
                                       branch_data.lon as longitude 
                                       from branch_data,sales_data 
                                       where branch_data.branch_id = sales_data.branch_id 
                                       and month(sales_data.sold_date) =",str_input2,"
                                       group by branch_name, branch_data.lat,branch_data.lon
                                       order by Branch"))    
        output$branch_analysis_graph <- renderLeaflet({
          leaflet(data=ybranchDF) %>%
            addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
            
            addMarkers(data = ybranchDF, lng = ybranchDF$longitude, lat = ybranchDF$latitude, 
                       popup = paste( "<div class='leaflet-popup-scrolled' style='max-height:200px'>",
                                      "<h5 style='color:black'><b>",ybranchDF$Branch,":",ybranchDF$Revenue,"</b></h5>"))%>%
            setView(lng = ybranchDF$longitude, lat = ybranchDF$latitude, zoom = 7) %>%
            fitBounds(lng1 = max(ybranchDF$longitude),lat1 = max(ybranchDF$latitude),
                      lng2 = min(ybranchDF$longitude),lat2 = min(ybranchDF$latitude))%>%
            clearBounds()
          
        }) ##output$branch_analysis_graph
        
        ##################### Registered Vs Unregistered Graph 2 6  ###########################
        
        yregisterVsUnregistered <- sqlQuery(con, paste0("select customer_type,sum(net_pay) as Revenue  from 
                                                        (select case when 
                                                        customer_id = 0 then 'Unregistered'
                                                        else 'Registered' end as customer_type, 
                                                        net_pay from sales_data where month(sold_date) =",str_input2," ) table1 group by customer_type"))
        
        output$registered_sales_graph<- renderHighchart({
          
          highchart(width = 100, height = 100) %>%
            hc_title(text = "") %>%
            hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
            hc_plotOptions(pie = list(depth = 70)) %>%
            hc_add_series_labels_values(yregisterVsUnregistered$customer_type,yregisterVsUnregistered$Revenue) %>%
            hc_tooltip(pointFormat = "$: {point.y}")%>%
            hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
        })  ###  Output$registered_sales_graph
        
        #####################  Gender Chart 2 7  ##########################################################
        
        ygenderDF <- sqlQuery(con, paste0("select Gender, sum(Revenue) as Revenue from (select case 
                                          when item_for = 'boy' then 'Male'
                                          when item_for = 'Men' then 'Male'
                                          when item_for = 'girl' then 'Female' 
                                          when item_for = 'Women' then 'Female' 
                                          when item_for = 'home-utility' then 'Home-Utility' end as Gender, 
                                          sum(net_pay) as Revenue from item_data,sales_data
                                          where item_data.item_id = sales_data.item_id and
                                          month(sales_data.sold_date) = ",str_input2,"
                                          group by  item_for) table1 group by Gender"))
        
        output$gender_sales_graph<- renderHighchart({
          
          s1 <-round(100/sum(ygenderDF$Revenue),3)
          
          highchart(width = 120, height = 140) %>%
            hc_title(text = "") %>%
            
            hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
            hc_plotOptions(pie = list(depth = 70)) %>%
            hc_add_series_labels_values(name = 'Revenue($)',ygenderDF$Gender,ygenderDF$Revenue) %>%
            hc_tooltip(pointFormat = "$: {point. percentage: .1f}%")%>%
            hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
        })  ####  output#gender_sales_graph()
        
        ##################  Age Group chart 2 8  ###################################
        
        yagesDF<-sqlQuery(con, paste0("select item_for,sum(net_pay) as Revenue from item_data,sales_data
                                      where item_data.item_id = sales_data.item_id and
                                      month(sales_data.sold_date) =",str_input2," 
                                      group by  item_for"))
        
        output$age_analysis_graph <- renderPlotly({
          
          f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
          x <- list(title = "Category",titlefont = f)
          y <- list(title = "Revenue ($)",titlefont = f)
          
          ageschart<-plot_ly(x=yagesDF$item_for,y=yagesDF$Revenue,type="bar",color = yagesDF$item_for, showlegend=FALSE)%>%
            layout(xaxis = x, yaxis = list(range = c(25000000, 40000000)), showlegend=FALSE)
          
          return(ageschart)
          
        }) ## output$age_analysis_graph
        
        
        ################ Group Graph 2 9 ################################################################
        
        yRevenueDF1 <- sqlQuery(con, paste0("select year(sold_date) as Year,
                                            sum(net_pay) as Revenue
                                            from sales_data where month(sales_data.sold_date)=",str_input2,"
                                            group by year(sold_date)  
                                            order by year(sold_date)"))
        
        ybranchDF1 <- sqlQuery(con,paste0("select year(sales_data.sold_date) as Year,
                                          sum(net_pay) as Revenue
                                          from branch_data,sales_data 
                                          where branch_data.branch_id = sales_data.branch_id and
                                          month(sales_data.sold_date)=",str_input2," and
                                          branch_data.branch_name = '", str_input4, "'
                                          group by branch_name, year(sales_data.sold_date) 
                                          order by Year"))
        
        
        ycategoryDF1 <- sqlQuery(con,paste0("select year(sales_data.sold_date) as Year,
                                            sum(sales_data.net_pay) as Revenue 
                                            FROM item_data,sales_data 
                                            where sales_data.item_id = item_data.item_id and
                                            month(sales_data.sold_date)=",str_input2," and
                                            item_category = '", str_input3, "'
                                            group by  year(sales_data.sold_date) 
                                            order by Year"))
        
        
        ybrandDF1 <- sqlQuery(con,paste0("select year(sales_data.sold_date) as Year,
                                         sum(net_pay) as Revenue 
                                         from sales_data,brand_data,item_data 
                                         where brand_data.brand_id = item_data.brand_id 
                                         and item_data.item_id = sales_data.item_id and
                                         month(sales_data.sold_date)=",str_input2," and
                                         brand_data.brand_name = '", str_input5, "'
                                         group by year(sales_data.sold_date)
                                         order by Year"))
        
        output$group_analysis_graph<- renderHighchart({
          
          highchart() %>%
            hc_chart(type="grouped") %>%
            
            hc_xAxis(title=list(text = "Year"),categories=ybranchDF1$Year) %>%
            hc_yAxis(title = list(text = "Revenue")) %>%
            hc_add_series(name = "Category",data = ycategoryDF1$Revenue,type = "column",color="orange") %>%
            hc_add_series(name = "Branch",data = ybranchDF1$Revenue,type = "line",color="green") %>%
            hc_add_series(name = "Brand",data = ybrandDF1$Revenue,type = "line",color="red")
        }) ## output$group_analysis_graph
        
        #################################### Branch 2 10 ##################################################
        
        ybranchFilterDF <- sqlQuery(con,paste0("select year(sold_date) as WYear, sum(net_pay) as Revenue  
                                              From sales_data,branch_data,item_data,brand_data 
                                               where item_data.item_id = sales_data.item_id and
                                               item_data.brand_id = brand_data.brand_id and
                                               branch_data.branch_id = sales_data.branch_id and
                                               branch_name = '", str_input6, "' and 
                                               item_category = '", str_input7 ,"' and 
                                               brand_name = '",str_input8,"' and
                                               month(sold_date) = ", str_input2, "
                                               group by year(sold_date)
                                               order by year(sold_date)"))
        
        if(nrow(ybranchFilterDF) == 1)  
          {
            ybranchFilterDF <- sqlQuery(con,paste0("select year(sold_date) as WYear, net_pay as Revenue  
                                                  From sales_data,branch_data,item_data,brand_data 
                                                  where item_data.item_id = sales_data.item_id and
                                                  item_data.brand_id = brand_data.brand_id and
                                                  branch_data.branch_id = sales_data.branch_id and
                                                  branch_name = 'Ave West' and 
                                                  item_category = 'Casual' and 
                                                  brand_name = 'Evisu' and
                                                  month(sold_date) = 1"))
        }
        
      
      output$Filter_inter_graph<- renderHighchart({
        
        highchart() %>%
          hc_chart(type="line") %>%
          hc_xAxis(title=list(text = "Year"),categories=ybranchFilterDF$WYear)%>%
          hc_yAxis(title = list(text = "Revenue($)")) %>%
          hc_add_series(name = "Revenue", data = ybranchFilterDF$Revenue,type = "line",color="orange") 
        
      }) ## output$Filter_inter_graph
      
      ####################################### Weekday Chart 2 11 ##################################
      
      yweekdayDF <- sqlQuery(con,paste0("SELECT Day_type,sum(net_pay) Revenue From (SELECT transaction_id, CASE WHEN Day_Name = 'Sunday' THEN 'Weekend'
			                                    WHEN Day_Name = 'Saturday' THEN 'Weekend'
                                        ELSE 'Weekday' END Day_type FROM 
                                        (SELECT transaction_id, sales_data.sold_date, FORMAT(sales_data.sold_date,'dddd') 'Day_Name' 
                                        FROM sales_data) table1) table2,sales_data WHERE table2.transaction_id = sales_data.transaction_id and
                                        month(sales_data.sold_date) = ",str_input2,"
                                        GROUP BY Day_type"))
      
      output$week_sales_graph <- renderPlotly({
        
        colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')
        
        weekday_chart <- plot_ly(yweekdayDF, labels = ~Day_type, values = ~Revenue, type = 'pie',
                                 textposition = 'inside',
                                 textinfo = 'label+percent',
                                 insidetextfont = list(color = '#FFFFFF'),
                                 hoverinfo = 'text',
                                 text = ~paste('$', Revenue),
                                 marker = list(colors = colors,
                                               line = list(color = '#FFFFFF', width = 1)),
                                 #The 'pull' attribute can also be used to create space between the sectors
                                 showlegend = FALSE) %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        return(weekday_chart)
        
      }) ### output$week_sales_graph
      
      } ### else1
      
      ################################################### 3 0 #######################################
      
  }else{    ### if-1
    
    str_input1 <- as.integer(str_input1)
    
    if(str_input2=='All'){
      
      ############################  Revenue Graph 3 1 #########################################
      
      
      yRevenueDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,month(sold_date) as Months
                                       from sales_data where year(sold_date)=",str_input1," 
                                       group by month(sold_date) 
                                       order by month(sold_date)"))
      
      yRevenueDF$month_labels <- list("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      # output$year_sales_graph<- renderPlotly({
      #   
      #   f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
      #   x <- list(title = "Month",titlefont = f)
      #   y <- list(title = "Revenue ($)",titlefont = f)
      #   
      #   revenuechart<-plot_ly(x=yRevenueDF$month_labels,y=yRevenueDF$Revenue,type="area", showlegend = FALSE)%>%
      #     layout(xaxis = x, yaxis = y, showlegend = FALSE)
      #   
      #   year_sales_graph<-plotly_build(qplot(1:10))
      #   year_sales_graph$elementId <- NULL
      #   return(revenuechart)
      # }) ## Output$year_sales_graph
      
      output$year_sales_graph <- renderHighchart({
        
        highchart() %>%
          hc_chart(type="line") %>%
          
          hc_xAxis(title=list(text = "Month"),categories = yRevenueDF$month_labels)%>%
          hc_yAxis(title = list(text = "Revenue($)")) %>%
          hc_add_series(data = yRevenueDF$Revenue, type = "line",color="green")
        
      })
      
      ######################  Wordcloud 3 2   #################################################
      
      ywordcloud <-sqlQuery(con, paste0("select item_name as Item from item_data, sales_data 
                                        where sales_data.item_id = item_data.item_id
                                        and year(sales_data.sold_date)=",str_input1))
      
      
      item_name = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", ywordcloud$Item )
      
      # item_name = gsub("@\\w+", "", item_name)   # remove at people
      
      item_name = gsub("[[:punct:]]", "", item_name)   # remove punctuation
      
      item_name = gsub("[[:digit:]]", "", item_name)   # remove numbers
      
      item_df <- data.frame(item_name) ## dataframe
      
      itemcorpus <- Corpus(VectorSource(item_df$item_name))   ## corpus
      
      
      output$word_cloud<- renderPlot({
        pal <- brewer.pal(9,"Spectral")
        pal <- pal[(1:9)]
        set.seed(123)
        wordcloud::wordcloud(words = itemcorpus,scale=c(3.5,.5),max.words=75,
                             random.order=FALSE,rot.per=0.35, use.r.layout=FALSE, colors=pal)
      }) ### wordcloud
      
      ########################  Categoory Graph 3 3 ########################################
      
      ycategoryDF<-sqlQuery(con, paste0("select item_category as Category,
                                        sum(sales_data.net_pay) as Revenue 
                                        FROM item_data,sales_data 
                                        where sales_data.item_id = item_data.item_id
                                        and year(sold_date)=",str_input1," 
                                        group by  item_category
                                        order by Category"))
      
      output$category_analysis_graph <- renderPlotly({
        
        f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
        x <- list(title = "Category",titlefont = f)
        y <- list(title = "Revenue ($)",titlefont = f)
        
        categorychart<-plot_ly(x=ycategoryDF$Category,y=ycategoryDF$Revenue,type="bar",color = ycategoryDF$Category, showlegend=FALSE)%>%
          layout(xaxis = x, yaxis = list(range = c(35000000, 42000000)), showlegend=FALSE)
        
        return(categorychart)
        
      }) ## output$category_analysis_graph
      
      #######################  Brand Table 3 4  #######################################
      
      ybrandDF<- sqlQuery(con,paste0("select top 10 brand_name as Brand,
                                     sum(net_pay) as Revenue 
                                     from sales_data,brand_data,item_data 
                                     where brand_data.brand_id = item_data.brand_id 
                                     and item_data.item_id = sales_data.item_id
                                     and year(sales_data.sold_date) = ",str_input1,"
                                     group by brand_name 
                                     order by Revenue DESC"))
      
      output$brand_sales_graph<- renderGvis({
        BrandRevenue<- select(ybrandDF,Brand,Revenue)
        BrandRevenuechart<-gvisTable(BrandRevenue,options=list(height="250px"))
        return(BrandRevenuechart)
        
      })##output$brand_sales_graph
      
      #################  Branch Map 3 5  ###############################################
      
      ybranchDF<-sqlQuery(con,paste0("select branch_name as Branch,
                                     sum(net_pay) as Revenue,
                                     branch_data.lat as latitude,
                                     branch_data.lon as longitude 
                                     from branch_data,sales_data 
                                     where branch_data.branch_id = sales_data.branch_id 
                                     and year(sales_data.sold_date) = ",str_input1,"
                                     group by branch_name, branch_data.lat,branch_data.lon
                                     order by Branch"))
      
      output$branch_analysis_graph <- renderLeaflet({
        leaflet(data=ybranchDF) %>%
          addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
          
          addMarkers(data = ybranchDF, lng = ybranchDF$longitude, lat = ybranchDF$latitude, 
                     popup = paste( "<div class='leaflet-popup-scrolled' style='max-height:200px'>",
                                    "<h5 style='color:black'><b>",ybranchDF$Branch,":",ybranchDF$Revenue,"</b></h5>"))%>%
          setView(lng = ybranchDF$longitude, lat = ybranchDF$latitude, zoom = 7) %>%
          fitBounds(lng1 = max(ybranchDF$longitude),lat1 = max(ybranchDF$latitude),
                    lng2 = min(ybranchDF$longitude),lat2 = min(ybranchDF$latitude))%>%
          clearBounds()
        
      }) ##output$branch_analysis_graph
      
      ########################### Registered Vs Unregistered Chart 3 6  ##############################
      
      yregisterVsUnregistered <- 
        sqlQuery(con, paste0("select customer_type,sum(net_pay) as Revenue  from 
                             (select case when 
                             customer_id = 0 then 'Unregistered'
                             else 'Registered' end as customer_type, 
                             net_pay from sales_data where year(sold_date) =",str_input1,") table1
                             group by customer_type"))
      
      output$registered_sales_graph<- renderHighchart({
        
        highchart(width = 100, height = 100) %>%
          hc_title(text = "") %>%
          
          hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
          hc_plotOptions(pie = list(depth = 70)) %>%
          hc_add_series_labels_values(yregisterVsUnregistered$customer_type,yregisterVsUnregistered$Revenue) %>%
          hc_tooltip(pointFormat = "$: {point.y}")%>%
          hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
      })  ### Output$registered_sales_graph()
      
      ##################### Gender Chart 3 7 ######################################################
      
      ygenderDF <- sqlQuery(con, paste0("select Gender, sum(Revenue) as Revenue from (select case 
                                        when item_for = 'boy' then 'Male'
                                        when item_for = 'Men' then 'Male'
                                        when item_for = 'girl' then 'Female' 
                                        when item_for = 'Women' then 'Female' 
                                        when item_for = 'home-utility' then 'Home-Utility' end as Gender, 
                                        sum(net_pay) as Revenue from item_data,sales_data
                                        where item_data.item_id = sales_data.item_id and
                                        year(sales_data.sold_date) = ",str_input1,"
                                        group by  item_for) table1 group by Gender"))
      
      output$gender_sales_graph<- renderHighchart({
        
        highchart(width = 100, height = 100) %>%
          hc_title(text = "") %>%
          
          hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
          hc_plotOptions(pie = list(depth = 70)) %>%
          hc_add_series_labels_values(ygenderDF$Gender,ygenderDF$Revenue) %>%
          hc_tooltip(pointFormat = "$: {point.y}")%>%
          hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
      })
      
      #################### Ages Graph 3 8 ########################################################
      
      yagesDF<-sqlQuery(con, paste0("select item_for,sum(net_pay) as Revenue from item_data,sales_data
                                    where item_data.item_id = sales_data.item_id and
                                    year(sales_data.sold_date) =",str_input1," 
                                    group by  item_for"))
      
      output$age_analysis_graph <- renderPlotly({
        
        f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
        x <- list(title = "Category",titlefont = f)
        y <- list(title = "Revenue ($)",titlefont = f)
        
        ageschart<-plot_ly(x=yagesDF$item_for,y=yagesDF$Revenue,type="bar",color = yagesDF$item_for, showlegend=FALSE)%>%
          layout(xaxis = x, yaxis = list(range = c(45000000, 50000000)), showlegend=FALSE)
        
        return(ageschart)
        
      }) ## output$gender_analysis_graph
      
      ##################### Group Graph 3 9 ################################################################
      
      yRevenueDF1 <- sqlQuery(con, paste0("select month(sold_date) as Month,
                                          sum(net_pay) as Revenue
                                          from sales_data where year(sales_data.sold_date)=",str_input1,"
                                          group by month(sold_date)  
                                          order by month(sold_date)"))
      
      ybranchDF1 <- sqlQuery(con,paste0("select month(sales_data.sold_date) as Month,
                                        sum(net_pay) as Revenue
                                        from branch_data,sales_data 
                                        where branch_data.branch_id = sales_data.branch_id and
                                        year(sales_data.sold_date) = ",str_input1," and
                                        branch_data.branch_name = '", str_input4, "'
                                        group by branch_name, month(sales_data.sold_date) 
                                        order by Month"))
      
      
      ycategoryDF1 <- sqlQuery(con,paste0("select month(sales_data.sold_date) as Month,
                                          sum(sales_data.net_pay) as Revenue 
                                          FROM item_data,sales_data 
                                          where sales_data.item_id = item_data.item_id and
                                          year(sales_data.sold_date)=",str_input1," and
                                          item_category = '", str_input3, "'
                                          group by  month(sales_data.sold_date) 
                                          order by Month"))
      
      #print(str_input5)
      ybrandDF1 <- sqlQuery(con,paste0("select month(sales_data.sold_date) as Month,
                                       sum(net_pay) as Revenue 
                                       from sales_data,brand_data,item_data 
                                       where brand_data.brand_id = item_data.brand_id 
                                       and item_data.item_id = sales_data.item_id and
                                       year(sales_data.sold_date)=",str_input1," and
                                       brand_data.brand_name = '",str_input5,"'
                                       group by month(sales_data.sold_date)
                                       order by Month"))
     # print(str_input5)
      
      ybranchDF1$month_labels <- list("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      output$group_analysis_graph<- renderHighchart({
        
        highchart() %>%
          hc_chart(type="grouped") %>%
          
          hc_xAxis(title=list(text = "Month"),categories=ybranchDF1$month_labels) %>%
          hc_yAxis(title = list(text = "Revenue")) %>%
          hc_add_series(name = "Category",data = ycategoryDF1$Revenue,type = "column",color="orange") %>%
          hc_add_series(name = "Branch",data = ybranchDF1$Revenue,type = "line",color="green") %>%
          hc_add_series(name = "Brand",data = ybrandDF1$Revenue,type = "line",color="red")
      }) ## output$group_analysis_graph
      
      ################################ Branch 3 10 #################################################
      
      ybranchFilterDF <- sqlQuery(con,paste0("select DateName(month, DateAdd(month, month(sold_date), -1)) as WMonth, sum(net_pay) as Revenue  
                                             From sales_data,branch_data,item_data,brand_data 
                                             where item_data.item_id = sales_data.item_id and
                                             item_data.brand_id = brand_data.brand_id and
                                             branch_data.branch_id = sales_data.branch_id and
                                             branch_name = '", str_input6, "' and 
                                             item_category = '", str_input7, "' and 
                                             brand_name = '",str_input8,"'  and
                                             year(sold_date) = ",str_input1," 
                                             group by month(sold_date)"))
      
      output$Filter_inter_graph<- renderHighchart({
        
        highchart() %>%
          hc_chart(type="line") %>%
          
          hc_xAxis(title=list(text = "Month"),categories=ybranchFilterDF$WMonth)%>%
          hc_yAxis(title = list(text = "Revenue ($)")) %>%
          hc_add_series(name = "Revenue($)", data = ybranchFilterDF$Revenue,type = "line",color="orange") 
        
      }) ## output$Filter_inter_graph
      
      ####################################### Weekday Chart 3 11 ##################################
      
      yweekdayDF <- sqlQuery(con,paste0("SELECT Day_type,sum(net_pay) Revenue From (SELECT transaction_id, CASE WHEN Day_Name = 'Sunday' THEN 'Weekend'
                                        WHEN Day_Name = 'Saturday' THEN 'Weekend'
                                        ELSE 'Weekday' END Day_type FROM 
                                        (SELECT transaction_id, sales_data.sold_date, FORMAT(sales_data.sold_date,'dddd') 'Day_Name' 
                                        FROM sales_data) table1) table2,sales_data WHERE table2.transaction_id = sales_data.transaction_id and
                                        year(sales_data.sold_date) = ",str_input1,"
                                        GROUP BY Day_type"))
      
      output$week_sales_graph <- renderPlotly({
        
        colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')
        
        weekday_chart <- plot_ly(yweekdayDF, labels = ~Day_type, values = ~Revenue, type = 'pie',
                                 textposition = 'inside',
                                 textinfo = 'label+percent',
                                 insidetextfont = list(color = '#FFFFFF'),
                                 hoverinfo = 'text',
                                 text = ~paste('$', Revenue),
                                 marker = list(colors = colors,
                                               line = list(color = '#FFFFFF', width = 1)),
                                 #The 'pull' attribute can also be used to create space between the sectors
                                 showlegend = FALSE) %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        return(weekday_chart)
        
      }) ### output$week_sales_graph
      
    }else{       ### if-3 
      
      str_input2 <- as.integer(str_input2)
      
      ######################## Revenue Graph  4 1 ########################################################
      
yRevenueDF<-sqlQuery(con,paste0(" WITH Calendar AS 
  (
  SELECT CONVERT(datetime, (select DATEADD(month,",str_input2,"-1,DATEADD(year,",str_input1,"-1900,0)))) AS CalendarDate
  UNION ALL SELECT CalendarDate + 1 FROM Calendar
  WHERE CalendarDate + 1 <= CONVERT(datetime, (select DATEADD(day,-1,DATEADD(month,",str_input2,",DATEADD(year,",str_input1,"-1900,0)))))
  )Select Day(CalendarDate) ADay, ISNULL(Revenue,0) Revenue from Calendar c
  Left Join (select day(sold_date) as WDay, sum(net_pay) as Revenue
  from sales_data where year(sold_date)=",str_input1,"
  and month(sold_date)=",str_input2,"
  group by day(sold_date) 
  
  ) d ON Day(c.CalendarDate) = d.WDay"))
      
      # output$year_sales_graph<- renderPlotly({
      #   
      #   f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
      #   x <- list(title = "Day",titlefont = f)
      #   y <- list(title = "Revenue ($)",titlefont = f)
      #   
      #   revenuechart<-plot_ly(x=as.integer(yRevenueDF$ADay),y=yRevenueDF$Revenue,type="area", showlegend=FALSE)%>%
      #     layout(xaxis = x, yaxis = y, showlegend=FALSE)
      #   year_sales_graph<-plotly_build(qplot(1:10))
      #   year_sales_graph$elementId <- NULL
      #   return(revenuechart)
      # }) ## Output$year_sales_graph

output$year_sales_graph <- renderHighchart({
  
  highchart() %>%
    hc_chart(type="line") %>%
    
    hc_xAxis(title=list(text = "Day"),categories = yRevenueDF$ADay)%>%
    hc_yAxis(title = list(text = "Revenue($)")) %>%
    hc_add_series(data = yRevenueDF$Revenue, type = "line",color="green")
  
})


      
      ####################### Wordcloud 4 2 #####################################################
      
      ywordcloud <-sqlQuery(con, paste0("select item_name as Item from item_data, sales_data 
                                        where sales_data.item_id = item_data.item_id
                                        and year(sales_data.sold_date)=",str_input1,"and
                                        month(sales_data.sold_date)=",str_input2))
      
      
      item_name = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", ywordcloud$Item )
      
      item_name = gsub("@\\w+", "", item_name)   # remove at people
      
      item_name = gsub("[[:punct:]]", "", item_name)   # remove punctuation
      
      item_name = gsub("[[:digit:]]", "", item_name)   # remove numbers
      
      item_df <- data.frame(item_name) ## dataframe
      
      itemcorpus <- Corpus(VectorSource(item_df$item_name))   ## corpus
      
      
      output$word_cloud<- renderPlot({
        pal <- brewer.pal(9,"Spectral")
        pal <- pal[(1:9)]
        set.seed(123)
        wordcloud::wordcloud(words = itemcorpus,scale=c(3.5,.5),max.words=75,
                             random.order=FALSE,rot.per=0.35, use.r.layout=FALSE, colors=pal)
      }) ### output$wordcloud()
      
      ########################### Category Graph 4 3 #####################################
      
      ycategoryDF<-sqlQuery(con,paste0("select item_category as Category,
                                       sum(sales_data.net_pay) as Revenue 
                                       FROM item_data,sales_data 
                                       where sales_data.item_id = item_data.item_id
                                       and year(sold_date)=",str_input1," 
                                       and month(sold_date)=",str_input2,"
                                       group by  item_category
                                       order by Category"))
      
      output$category_analysis_graph <- renderPlotly({
        
        f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
        x <- list(title = "Category",titlefont = f)
        y <- list(title = "Revenue ($) ",titlefont = f)
        
        categorychart<-plot_ly(x = ycategoryDF$Category,y = ycategoryDF$Revenue,type="bar",
                               text = y, textposition = 'auto',
                               color = ycategoryDF$Category, showlegend=FALSE)%>%
          layout(xaxis = x, yaxis = list(range = c(2500000, 4000000)), showlegend=FALSE)
        
        return(categorychart)
        
      }) ## output$category_analysis_graph
      
      ##########################  Brand Table 4 4 #############################
      
      ybrandDF<- sqlQuery(con,paste0("select top 10 brand_name as Brand,
                                     sum(net_pay) as Revenue 
                                     from sales_data,brand_data,item_data 
                                     where brand_data.brand_id = item_data.brand_id 
                                     and item_data.item_id = sales_data.item_id
                                     and year(sales_data.sold_date) = ",str_input1,"
                                     and month(sales_data.sold_date) = ",str_input2,"
                                     group by brand_name 
                                     order by Revenue DESC"))
      
      output$brand_sales_graph<- renderGvis({
        BrandRevenue<- select(ybrandDF,Brand,Revenue)
        BrandRevenuechart<-gvisTable(BrandRevenue,options=list(height="250px"))
        return(BrandRevenuechart)
        
      }) ### output$brand_sales_graph
      
      ################## Branch Map 4 5 ################################################
      
      ybranchDF<-sqlQuery(con,paste0("select branch_name as Branch,
                                     sum(net_pay) as Revenue,
                                     branch_data.lat as latitude,
                                     branch_data.lon as longitude 
                                     from branch_data,sales_data 
                                     where branch_data.branch_id = sales_data.branch_id 
                                     and year(sales_data.sold_date) = ",str_input1,"
                                     and month(sales_data.sold_date) = ",str_input2,"
                                     group by branch_name, branch_data.lat,branch_data.lon
                                     order by Branch"))
      
      output$branch_analysis_graph <- renderLeaflet({
        leaflet(data=ybranchDF) %>%
          addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
          
          addMarkers(data = ybranchDF, lng = ybranchDF$longitude, lat = ybranchDF$latitude,
                     popup = paste( "<div class='leaflet-popup-scrolled' style='max-height:200px'>",
                                    "<h5 style='color:black'><b>",ybranchDF$Branch,":",ybranchDF$Revenue,"</b></h5>"))%>%
          setView(lng = ybranchDF$longitude, lat = ybranchDF$latitude, zoom = 7) %>%
          fitBounds(lng1 = max(ybranchDF$longitude),lat1 = max(ybranchDF$latitude),
                    lng2 = min(ybranchDF$longitude),lat2 = min(ybranchDF$latitude))%>%
          clearBounds()
        
      }) ##output$branch_analysis_graph
      
      ###################### Registered Vs Unregistered Chart 4 6 ################################
      
      yregisterVsUnregistered <- sqlQuery(con, paste0("select customer_type,sum(net_pay) as Revenue  from 
                                                      (select case when 
                                                      customer_id = 0 then 'Unregistered'
                                                      else 'Registered' end as customer_type, 
                                                      net_pay from sales_data where
                                                      year(sold_date) =",str_input1," and
                                                      month(sold_date) =",str_input2," ) table1 
                                                      group by customer_type"))
      
      output$registered_sales_graph<- renderHighchart({
        
        highchart(width = 100, height = 100) %>%
          hc_title(text = "") %>%
          hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
          hc_plotOptions(pie = list(depth = 70)) %>%
          hc_add_series_labels_values(yregisterVsUnregistered$customer_type,yregisterVsUnregistered$Revenue) %>%
          hc_tooltip(pointFormat = "$: {point.y}")%>%
          hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
      }) ### output$registered_sales_graph()
      
      ####################### Gender Graph 4 7  ###########################################
      
      ygenderDF <- sqlQuery(con, paste0("select Gender, sum(Revenue) as Revenue from (select case 
                                        when item_for = 'boy' then 'Male'
                                        when item_for = 'Men' then 'Male'
                                        when item_for = 'girl' then 'Female' 
                                        when item_for = 'Women' then 'Female' 
                                        when item_for = 'home-utility' then 'Home-Utility' end as Gender, 
                                        sum(net_pay) as Revenue from item_data,sales_data
                                        where item_data.item_id = sales_data.item_id
                                        group by  item_for) table1 group by Gender"))
      output$gender_sales_graph<- renderHighchart({
        
        highchart(width = 100, height = 100) %>%
          hc_title(text = "") %>%
          hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
          hc_plotOptions(pie = list(depth = 70)) %>%
          hc_add_series_labels_values(ygenderDF$Gender,ygenderDF$Revenue) %>%
          hc_tooltip(pointFormat = "$: {point.y}")%>%
          hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
        
      }) ### gendergraph
      
      ################################# Age Group Chart 4 8  ############################################
      
      yagesDF<-sqlQuery(con, paste0("select item_for,sum(net_pay) as Revenue from item_data,sales_data
                                    where item_data.item_id = sales_data.item_id and
                                    year(sales_data.sold_date) = ",str_input1," and
                                    month(sales_data.sold_date) =",str_input2," 
                                    group by  item_for"))
      
      output$age_analysis_graph <- renderPlotly({
        
        f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
        x <- list(title = "Category",titlefont = f)
        y <- list(title = "Revenue ($)",titlefont = f)
        
        ageschart<-plot_ly(x=yagesDF$item_for,y=yagesDF$Revenue,type="bar",color = yagesDF$item_for, showlegend=FALSE)%>%
          layout(xaxis = x, yaxis = list(range = c(2500000, 4000000)), showlegend=FALSE)
        
        return(ageschart)
        
      }) ## output$age_analysis_graph
      
      
      ##################### Group Graph 4 9 ################################################################
      
ybranchDF1 <- sqlQuery(con,paste0("WITH Calendar AS 
  (
  SELECT CONVERT(datetime, (select DATEADD(month,",str_input2,"-1,DATEADD(year,",str_input1,"-1900,0)))) AS CalendarDate
  UNION ALL SELECT CalendarDate + 1 FROM Calendar
  WHERE CalendarDate + 1 <= CONVERT(datetime, (select DATEADD(day,-1,DATEADD(month,",str_input2,",DATEADD(year,",str_input1,"-1900,0)))))
      )Select Day(CalendarDate) ADay, ISNULL(Revenue,0) Revenue from Calendar c
  Left Join (select day(sales_data.sold_date) as WDay,
  sum(net_pay) as Revenue
  from branch_data,sales_data 
  where branch_data.branch_id = sales_data.branch_id and
  year(sales_data.sold_date)=",str_input1," and
  month(sales_data.sold_date) = ",str_input2," and
  branch_data.branch_name = '", str_input4, "'
  group by branch_name, day(sales_data.sold_date)
  ) d ON Day(c.CalendarDate) = d.WDay"))
      # View(ybranchDF1)
      
ycategoryDF1 <- sqlQuery(con,paste0("WITH Calendar AS 
  (
  SELECT CONVERT(datetime, (select DATEADD(month,",str_input2,"-1,DATEADD(year,",str_input1,"-1900,0)))) AS CalendarDate
  UNION ALL SELECT CalendarDate + 1 FROM Calendar
  WHERE CalendarDate + 1 <= CONVERT(datetime, (select DATEADD(day,-1,DATEADD(month,",str_input2,",DATEADD(year,",str_input1,"-1900,0)))))
      )Select Day(CalendarDate) ADay, ISNULL(Revenue,0) Revenue from Calendar c
  Left Join (select day(sales_data.sold_date) as WDay,
  sum(sales_data.net_pay) as Revenue 
  FROM item_data,sales_data 
  where sales_data.item_id = item_data.item_id and
  year(sales_data.sold_date) = ",str_input1," and
  month(sales_data.sold_date) = ",str_input2," and
  item_category = '",str_input3,"'
  group by  Day(sales_data.sold_date)
  ) d ON Day(c.CalendarDate) = d.WDay"))
      # View(ycategoryDF1)
      
ybrandDF1 <- sqlQuery(con,paste0("WITH Calendar AS 
  (SELECT CONVERT(datetime, (select DATEADD(month,",str_input2,"-1,DATEADD(year,",str_input1,"-1900,0)))) AS CalendarDate
  UNION ALL SELECT CalendarDate + 1 FROM Calendar
  WHERE CalendarDate + 1 <= CONVERT(datetime, (select DATEADD(day,-1,DATEADD(month,",str_input2,",DATEADD(year,",str_input1,"-1900,0)))))
  )Select Day(CalendarDate) ADay, ISNULL(Revenue,0) Revenue from Calendar c
  Left Join (select Day(sales_data.sold_date) as WDay,
  sum(net_pay) as Revenue 
  from sales_data,brand_data,item_data 
  where brand_data.brand_id = item_data.brand_id 
  and item_data.item_id = sales_data.item_id and
  year(sales_data.sold_date)=",str_input1," and 
  month(sales_data.sold_date) = ",str_input2," and
  brand_data.brand_name = '", str_input5, "'
  group by Day(sales_data.sold_date)
  ) d ON Day(c.CalendarDate) = d.WDay"))
      
      # View(ybrandDF1)
      
      output$group_analysis_graph<- renderHighchart({
        
        highchart() %>%
          hc_chart(type="grouped") %>%
          
          hc_xAxis(title=list(text = "Day"),categories=ybrandDF1$ADay)%>%
          hc_yAxis(title = list(text = "Revenue ($)")) %>%
          hc_add_series(name = "Category", data = ycategoryDF1$Revenue,type = "column",color="orange") %>%
          hc_add_series(name = "Branch", data = ybranchDF1$Revenue,type = "line",color="green") %>%
          hc_add_series(name = "Brand", data = ybrandDF1$Revenue,type = "line",color="red")
      }) ## output$group_analysis_graph
      
      ################################ Branch 4 10 #################################################
      
 ybranchFilterDF <- sqlQuery(con,paste0("WITH Calendar AS 
  (SELECT CONVERT(datetime, (select DATEADD(month,",str_input2,"-1,DATEADD(year,",str_input1,"-1900,0)))) AS CalendarDate
     UNION ALL SELECT CalendarDate + 1 FROM Calendar
     WHERE CalendarDate + 1 <= CONVERT(datetime, (select DATEADD(day,-1,DATEADD(month,",str_input2,",DATEADD(year,",str_input1,"-1900,0)))))
      )Select Day(CalendarDate) ADay, ISNULL(Revenue,0) Revenue from Calendar c
     Left Join (select day(sold_date) as WDay, sum(net_pay) as Revenue  
     From sales_data,branch_data,item_data,brand_data 
     where item_data.item_id = sales_data.item_id and
     item_data.brand_id = brand_data.brand_id and
     branch_data.branch_id = sales_data.branch_id and
     branch_name = '", str_input6, "' and item_category = '", str_input7, "' and brand_name = '",str_input8,"'  and
     year(sold_date) = ",str_input1," and month(sold_date) = ",str_input2,"
     group by day(sold_date))  d ON Day(c.CalendarDate) = d.WDay"))
      
      output$Filter_inter_graph<- renderHighchart({
        
        highchart() %>%
          hc_chart(type="line") %>%
          
          hc_xAxis(title=list(text = "Day"),categories = ybranchFilterDF$ADay)%>%
          hc_yAxis(title = list(text = "Revenue($)")) %>%
          hc_add_series(name = "Revenue",data = ybranchFilterDF$Revenue,type = "line",color="orange") 
        
      }) ## output$Filter_inter_graph
      
      ####################################### Weekday Chart 4 11 ##################################
      
      yweekdayDF <- sqlQuery(con,paste0("SELECT Day_type,sum(net_pay) Revenue From (SELECT transaction_id, CASE WHEN Day_Name = 'Sunday' THEN 'Weekend'
                                        WHEN Day_Name = 'Saturday' THEN 'Weekend'
                                        ELSE 'Weekday' END Day_type FROM 
                                        (SELECT transaction_id, sales_data.sold_date, FORMAT(sales_data.sold_date,'dddd') 'Day_Name' 
                                        FROM sales_data) table1) table2,sales_data WHERE table2.transaction_id = sales_data.transaction_id and
                                        year(sales_data.sold_date) = ",str_input1," and
                                        month(sales_data.sold_date) = ",str_input2,"
                                        GROUP BY Day_type"))
      
      output$week_sales_graph <- renderPlotly({
        
        colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')
        
        weekday_chart <- plot_ly(yweekdayDF, labels = ~Day_type, values = ~Revenue, type = 'pie',
                                 textposition = 'inside',
                                 textinfo = 'label+percent',
                                 insidetextfont = list(color = '#FFFFFF'),
                                 hoverinfo = 'text',
                                 text = ~paste('$', Revenue),
                                 marker = list(colors = colors,
                                               line = list(color = '#FFFFFF', width = 1)),
                                 #The 'pull' attribute can also be used to create space between the sectors
                                 showlegend = FALSE) %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        return(weekday_chart)
        
      }) ### output$week_sales_graph
      
    } ### else - 3 
    
    } ### else - 2
    
    odbcCloseAll()
    
    })### Observe()
  
  })### Server()