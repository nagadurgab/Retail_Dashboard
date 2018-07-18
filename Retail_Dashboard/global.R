library(RODBC)

con <- odbcDriverConnect('driver={SQL Server};server=AXONML-SERVER;database=Retail_Dashboard;uid=axonmluser;pwd=axon*2016BZA',rows_at_time = 100)

branches <- sqlQuery(con, paste0("SELECT branch_name FROM branch_data order by branch_name"))

branches <- c(paste0(sort(branches$branch_name)))

branches1 <- c('All',branches)

brands <- sqlQuery(con, paste0("SELECT brand_name FROM brand_data order by brand_name"))

brands <- c(paste0(sort(brands$brand_name)))

brands1 <- c('All',brands)

gender <- c("Male","Female")

categories <- c('Casual','Formal','Night Wear','Others','Party','Sports')

categories1 <- c('All',categories)
