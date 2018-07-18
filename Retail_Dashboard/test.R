


ycategoryDF1$Day <- sort(ycategoryDF1$Day)

no_days <- nrow(yRevenueDF1)

for (i in 1:no_days){
  
  if(i != ybrandDF1$Day[i]){
    
    
  }
}



categories <- c('Casual','Formal','Night-Wear','Others','Party','Sports')

str_input5<-categories
print(str_input5)

str_input5<- sQuote(str_input5)

str_input1 <- 2017
str_input2 <- 12
str_input3 <- c('Casual')
str_input4 <- c('Ave West')
str_input5 <- c('Addidas')
str_input7 <- c('Casual')
str_input6 <- c('Ave West')
str_input8 <- c('Addidas')
library(RODBC)
con <- odbcDriverConnect('driver={SQL Server};server=AXONML-SERVER;database=Retail_Dashboard;uid=axonmluser;pwd=axon*2016BZA;',rows_at_time = 100)

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

con <- odbcDriverConnect('driver={SQL Server};server=AXONML-SERVER;database=Retail_Dashboard;uid=axonmluser;pwd=axon*2016BZA;',rows_at_time = 100)

ybranchFilterDF <- sqlQuery(con,paste0("select month(sold_date) as WMonth, sum(net_pay) as Revenue  
                                             From sales_data,branch_data,item_data,brand_data 
                                             where item_data.item_id = sales_data.item_id and
                                             item_data.brand_id = brand_data.brand_id and
                                             branch_data.branch_id = sales_data.branch_id and
                                             branch_name = '", str_input6, "' and 
                                             item_category = '", str_input7, "' and 
                                             brand_name = '",str_input8,"'  and
                                             year(sold_date) = ",str_input1," 
                                             group by month(sold_date)"))
                                        
yRevenueDF1 <- sqlQuery(con,paste0(" WITH Calendar AS 
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
                        


if(str_input6 == 'All')
{
  if(str_input7 == 'All')
  {
    if(str_input8 == 'All')
    {
      ybranchFilterDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                       from sales_data 
                                       group by year(sold_date) 
                                       order by year(sold_date)"))
      
    }else{
      ybranchFilterDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                       from sales_data,item_data,brand_data 
                                       where sales_data.item_id = item_data.item_id and
                                       item_data.brand_id = brand_data.brand_id and
                                       brand_data.brand_name = '",str_input8,"'
                                       group by year(sold_date) 
                                       order by year(sold_date)"))
      }
    
    }else{
      
      if(str_input8 == 'All')
      {
        ybranchFilterDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                         from sales_data,item_data
                                         where sales_data.item_id = item_data.item_id and
                                         item_category = '",str_input7,"'
                                         group by year(sold_date) 
                                         order by year(sold_date)"))
        
      }else{
        ybranchFilterDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                         from sales_data,item_data,brand_data 
                                         where sales_data.item_id = item_data.item_id and
                                         item_data.brand_id = brand_data.brand_id and
                                         item_category = '",str_input7,"' and
                                         brand_data.brand_name = '",str_input8,"'
                                         group by year(sold_date) 
                                         order by year(sold_date)"))
      }  ## end else brand
      
    }  ## end else category
  
}else{
    
  if(str_input7 == 'All')
  {
    if(str_input8 == 'All')
    {
      ybranchFilterDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                       from sales_data,branch_data
                                       where sales_data.branch_id = branch_data.branch_id and
                                       branch_name = '",str_input6,"'
                                       group by year(sold_date) 
                                       order by year(sold_date)"))
      
    }else{
      ybranchFilterDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                       from sales_data,item_data,brand_data 
                                       where sales_data.item_id = item_data.item_id and
                                       sales_data.branch_id = branch_data.branch_id and
                                       item_data.brand_id = brand_data.brand_id and
                                       branch_name = '",str_input6,"' and
                                       brand_data.brand_name = '",str_input8,"'
                                       group by year(sold_date) 
                                       order by year(sold_date)"))
    }
    
    }else{
      
      if(str_input8 == 'All')
      {
        ybranchFilterDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                         from sales_data,item_data
                                         where sales_data.item_id = item_data.item_id and
                                         sales_data.branch_id = branch_data.branch_id and
                                         branch_name = '",str_input6,"' and
                                         item_category = '",str_input7,"'
                                         group by year(sold_date) 
                                         order by year(sold_date)"))
        
      }else{
        ybranchFilterDF<-sqlQuery(con, paste0("select sum(net_pay) as Revenue,year(sold_date) as Years
                                         from sales_data,item_data,brand_data 
                                         where sales_data.item_id = item_data.item_id and
                                         item_data.brand_id = brand_data.brand_id and
                                         sales_data.branch_id = branch_data.branch_id and
                                         branch_name = '",str_input6,"' and
                                         item_category = '",str_input7,"' and
                                         brand_data.brand_name = '",str_input8,"'
                                         group by year(sold_date) 
                                         order by year(sold_date)"))
      }  ## end else brand
    }  ## end else category
} ## end else branch

library(plotly)
library(dplyr)
library(RODBC)
con <- odbcDriverConnect('driver={SQL Server};server=AXONML-SERVER;database=Retail_Dashboard;uid=axonmluser;pwd=axon*2016BZA;',rows_at_time = 100)
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

p <- plot_ly() %>%
  add_pie(data = yweekdayDF, labels = ~Day_type, values = ~Revenue,
          name = "Day_type", domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  
  layout(showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 


colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')

p <- plot_ly(yweekdayDF, labels = ~Day_type, values = ~Revenue, type = 'pie',
           textposition = 'inside',
           textinfo = 'label+percent',
           insidetextfont = list(color = '#FFFFFF'),
           hoverinfo = 'text',
           text = ~paste('$', Revenue),
           marker = list(colors = colors,
                         line = list(color = '#FFFFFF', width = 1)),
           #The 'pull' attribute can also be used to create space between the sectors
           showlegend = FALSE) %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


library(highcharter)
library(RODBC)
con <- odbcDriverConnect('driver={SQL Server};server=AXONML-SERVER;database=Retail_Dashboard;uid=axonmluser;pwd=axon*2016BZA;',rows_at_time = 100)

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

highchart() %>% 
  hc_add_series_labels_values(ygenderDF$Gender, ygenderDF$Revenue,
                              type = "pie",
                              name = "Bar", colorByPoint = TRUE,
                              size = 250, dataLabels = list(enabled = FALSE)) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_tooltip(pointFormat = "{point.y}%")


x = 
y = paste(round((ygenderDF$Revenue/sum(ygenderDF$Revenue))*100,3)," %")

highchart(width = 100, height = 100) %>%
  hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0,color = 'rgb(255, 110, 176)')) %>%
  hc_plotOptions(pie = list(depth = 70)) %>%
  hc_add_series_labels_values(name = 'Revenue($)',ygenderDF$Gender,ygenderDF$Revenue) %>%
  hc_tooltip(pointFormat = '{point.y}')%>%
  hc_add_theme(hc_theme(chart = list(backgroundColor = NULL)))
                        
                        
                        
                       
                        
                        

                        
                        
                        

                     


