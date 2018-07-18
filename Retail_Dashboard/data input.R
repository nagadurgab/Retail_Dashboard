library(RODBC)
library(lubridate)

Data<-read.csv("sales_data.csv")

colnames(Data)
Data$item_id<-as.integer(Data$item_id)
Data$customer_id<-as.integer(Data$customer_id)
Data$branch_id<-as.integer(Data$branch_id)
Data$mrp<-as.integer(Data$mrp)
Data$discount_amount<-as.integer(Data$discount_amount)
Data$price<-as.integer(Data$price)
Data$Quantity<-as.integer(Data$Quantity)
Data$net_pay<-as.integer(Data$net_pay)
Data$transaction_id<-as.integer(Data$transaction_id)
Data$sold_date<-as.Date(Data$sold_date)
View(Data)



con <- odbcDriverConnect('driver={SQL Server};server=AXONML-SERVER;database=Retail_Dashboard;uid=axonmluser;pwd=axon*2016BZA',rows_at_time = 1)



sqlSave(con, Data,tablename ="sales_data", append = TRUE,rownames = FALSE)

