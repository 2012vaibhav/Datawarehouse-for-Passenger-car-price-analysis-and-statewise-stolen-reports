library(rvest)
library(purrr)
library('gmp')
library(xml2)
library(tidyr)
library(readr)
library(readxl)
library(stringi)
library(Hmisc)
#********************** Structured Data set of Second Hand Cars and its Prices **************************#
print("Importing Sencond Hand cars data will take time as it contains more than 8 lacs rows")
Used_Cars <- read.csv("C:\\Users\\Vaibhav\\Desktop\\Car DWBI Datasets\\true_car_listings.csv\\true_car_listings.csv") # Import xlsx

# Removed VIN column
Used_Cars <- Used_Cars[-6] 

#Remove all data older than 2015
Used_Cars <- Used_Cars[!Used_Cars$Year< 2015,]

# Cleaning with respect to selected brands like "Gmc","Honda","Chevrolet","Toyota","Nissan","Dodge"
Used_Cars <- Used_Cars[Used_Cars$Make == "Ford" | Used_Cars$Make == "GMC"
                       | Used_Cars$Make == "Honda"   | Used_Cars$Make == "Chevrolet"
                       | Used_Cars$Make == "Toyota"  | Used_Cars$Make == "Nissan"
                       | Used_Cars$Make == "Dodge"| Used_Cars$Make == "Audi"| Used_Cars$Make == "Mitsubishi" ,]

# Give full names to states from Abbrevation here state.Name is inbuild dataset
names(state.name) <-state.abb
Used_Cars$State <-state.name[Used_Cars$State]

#Removed the states with Unkwon Abbrevations
Used_Cars <- Used_Cars[!is.na(Used_Cars$State),]

#Remove All NA columns from Dataset
Used_Cars <- Used_Cars[!is.na(Used_Cars$Price) |!is.na(Used_Cars$Year)    |
                         !is.na(Used_Cars$City)   |!is.na(Used_Cars$Mileage) |
                         !is.na(Used_Cars$Make)   |!is.na(Used_Cars$Model),]

#Rearrange the row sequence number and cast list to data frame
rownames(Used_Cars) <- NULL
df_Used_Cars <- as.data.frame(Used_Cars)

names(df_Used_Cars)[1] <- paste("Old_Price")
names(df_Used_Cars)[2] <- paste("Old_Year")
names(df_Used_Cars)[3] <- paste("Old_Milage")
names(df_Used_Cars)[4] <- paste("Old_City")
names(df_Used_Cars)[5] <- paste("Old_State")
names(df_Used_Cars)[6] <- paste("Old_Make")
names(df_Used_Cars)[7] <- paste("Old_Model")

#Removed Sub category of Charger SE car to Charger
k = 0
for (val in df_Used_Cars$Old_Model) {
  k = k+1
  if(val =="ChargerSE"){
    df_Used_Cars$Old_Model[k] <- "Charger"
  }
}

#Export cleaned Used Data to csv
write.csv(df_Used_Cars, file = "C:\\Users\\Vaibhav\\Desktop\\DWBI Project\\Cleaned_Used_Cars_Prices.csv",row.names = FALSE)
