library(rvest)
library(purrr)
library('gmp')
library(xml2)
library(tidyr)
library(readr)
library(readxl)
library(stringi)
library(Hmisc)
#********************** Structured data from Statista *********************(Which Type of vehicle stolen) ******************************
url2 <- "C:\\Users\\Vaibhav\\Desktop\\Car DWBI Datasets\\Statista Data\\statistic_id424163_us_-top-ten-frequently-stolen-passenger-vehicles-2017-by-model (1).xlsx"

Type_Of_Vehicle_Stolen <- read_excel(url2,col_names = FALSE,col_types = NULL,sheet = 2,na="",skip = 3)
names(Type_Of_Vehicle_Stolen)[1] <- paste("Model")
names(Type_Of_Vehicle_Stolen)[2] <- paste("Stolen_Numbers")
Type_Of_Vehicle_Stolen <- Type_Of_Vehicle_Stolen[!is.na(Type_Of_Vehicle_Stolen$Stolen_Numbers),]

#Removed Values in Bracket
Type_Of_Vehicle_Stolen$Model <- sapply(strsplit(Type_Of_Vehicle_Stolen$Model,split = " (",fixed = TRUE),function(x) (x[1]))
Type_Of_Vehicle_Stolen$Make <- sapply(strsplit(Type_Of_Vehicle_Stolen$Model,split = " ",fixed = TRUE),function(x) (x[1]))
Type_Of_Vehicle_Stolen$Model <- sapply(strsplit(Type_Of_Vehicle_Stolen$Model,split = " ",fixed = TRUE),function(x) (x[2]))

# Here Only car model for few were given as 'Pick up' so I have replace pickup with some actual pickup model of same Company
Type_Of_Vehicle_Stolen$Model[6] <- "Rogue"
Type_Of_Vehicle_Stolen$Model[3] <- "Expedition"
Type_Of_Vehicle_Stolen$Model[10] <- "Bolt"
Type_Of_Vehicle_Stolen$Model[4] <- "Suburban2WD"
Type_Of_Vehicle_Stolen$Model[8] <- "Charger"

df_Type_Of_Vehicle_Stolen <- data.frame(Type_Of_Vehicle_Stolen)
names(df_Type_Of_Vehicle_Stolen)[1] <- paste("Type_Model")
names(df_Type_Of_Vehicle_Stolen)[2] <- paste("Type_st_Numbers")
names(df_Type_Of_Vehicle_Stolen)[3] <- paste("Type_Make")

df_Type_Of_Vehicle_Stolen$Type_Year <- 2017

write.csv(df_Type_Of_Vehicle_Stolen, file = "C:\\Users\\Vaibhav\\Desktop\\DWBI Project\\Type_Of_Vehicle_Stolen.csv",row.names = FALSE)
