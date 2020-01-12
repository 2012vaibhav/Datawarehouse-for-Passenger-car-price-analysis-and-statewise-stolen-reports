library(rvest)
library(purrr)
library('gmp')
library(xml2)
library(tidyr)
library(readr)
library(readxl)
library(stringi)
library(Hmisc)

# Structured Dataset for USA statewise Number of stolen Cars by Model from https://www.iii.org/table-archive/20900 Website 
Theft_Car_Data <- html_text(html_nodes(read_html("https://www.iii.org/table-archive/20900"),'td'))

#Removing string that contains brief description of Dataset
value <- "Most"
value2 <- "Highest"
Theft_Car_Data<-Theft_Car_Data[-(grep(value2,Theft_Car_Data))]
Theft_Car_Data<-Theft_Car_Data[-(grep(value,Theft_Car_Data))]

#Merge All values in to matrix and split matrix in to sub Matrices 1st Matrix is for 2017, 8th = 2015, 9th = 2016
for(i in 2:length(Theft_Car_Data)) output=rbind(output,matrix(Theft_Car_Data[[i]],ncol=3,byrow=T))
output <- matrix(unlist(Theft_Car_Data), ncol = 3, byrow = TRUE)
My_Matrix <- split(as.data.frame(output), rep(1:9, each = 20))

Theft_Car_2017 <- My_Matrix$`1`[-1]
Theft_Car_2016 <- My_Matrix$`9`[-1]
Theft_Car_2015 <- My_Matrix$`8`[-1]
Theft_Car_2017$Year <- 2017
Theft_Car_2016$Year <- 2016
Theft_Car_2015$Year <- 2015

names(Theft_Car_2017)[1] <- paste("State")
names(Theft_Car_2017)[2] <- paste("Car Stolen")
names(Theft_Car_2016)[1] <- paste("State")
names(Theft_Car_2016)[2] <- paste("Car Stolen")
names(Theft_Car_2015)[1] <- paste("State")
names(Theft_Car_2015)[2] <- paste("Car Stolen")

Theft_Car_2017$`Car Stolen` <- extract_numeric(Theft_Car_2017$`Car Stolen`)
Theft_Car_2016$`Car Stolen` <- extract_numeric(Theft_Car_2016$`Car Stolen`)
Theft_Car_2015$`Car Stolen`<- extract_numeric(Theft_Car_2015$`Car Stolen`)

Theft_Car_2017 <- Theft_Car_2017[order(Theft_Car_2017$`Car Stolen`,decreasing = TRUE),]
Theft_Car_2016 <- Theft_Car_2016[order(Theft_Car_2016$`Car Stolen`,decreasing = TRUE),]
Theft_Car_2015 <- Theft_Car_2015[order(Theft_Car_2015$`Car Stolen`,decreasing = TRUE),]

Theft_Car <- rbind(c,Theft_Car_2015,Theft_Car_2016,Theft_Car_2017)
Theft_Car <- Theft_Car[!is.na(Theft_Car$State),]
rownames(Theft_Car) <- NULL
df_Theft_Cars <- data.frame(Theft_Car)

names(df_Theft_Cars)[1] <- paste("St_State")
names(df_Theft_Cars)[2] <- paste("St_TotalNumber")
names(df_Theft_Cars)[3] <- paste("St_Year")

#Export cleaned Used Data to csv
write.csv(df_Theft_Cars, file = "C:\\Users\\Vaibhav\\Desktop\\DWBI Project\\df_Theft_Cars.csv",row.names = FALSE)

