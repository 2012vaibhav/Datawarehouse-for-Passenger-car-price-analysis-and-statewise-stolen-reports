library(rvest)
library(purrr)
library('gmp')
library(xml2)
library(tidyr)
library(readr)
library(readxl)
library(stringi)
library(Hmisc)

#*************** Structured data for Number of vehicles woned statewise**************************************
Path <- "C:\\Users\\Vaibhav\\Desktop\\Car DWBI Datasets\\Total STates Vehicle\\mv7.xlsx"
data <- read_excel(path = Path,sheet = 1,skip = 6,na = "",col_names = FALSE,col_types = NULL)
Number_Vehicle_Owned <- data[14]
States_Name <- data[1]
Number_Vehicle_Owned <- data.frame(States_Name,Number_Vehicle_Owned)
Number_Vehicle_Owned <- Number_Vehicle_Owned[c(2:52),]
row.names(Number_Vehicle_Owned) <- NULL
names(Number_Vehicle_Owned)[1] <- paste("States")
names(Number_Vehicle_Owned)[2] <- paste("Total_Cars_Owned")
write.csv(Number_Vehicle_Owned, file = "C:\\Users\\Vaibhav\\Desktop\\DWBI Project\\Number_Vehicle_Owned.csv",row.names = FALSE)

