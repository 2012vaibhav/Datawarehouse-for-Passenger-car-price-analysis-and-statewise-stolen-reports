library(rvest)
library(purrr)
library('gmp')
library(xml2)
library(tidyr) 
library(readr)
library(readxl)
library(stringi)
library(Hmisc)


#*************** Unstructured Data of New Cars and Its prices *******************************************#

#Iterating through webpages
Vehicle <- c("ford/","gmc/","honda/","chevrolet/","toyota/","nissan/","dodge/","audi/","mitsubishi/") 
url_base <- "https://www.cars.com/research"

#Addind Extracted data in to the Data frame
map_df(Vehicle[1:9], function(i) {
  pg1 <- paste(url_base,i,sep = "/") 
  pg <-  WS1 <- html_session(pg1)
  model <- capitalize(gsub("/","",i))

  data.frame(Model=stri_trim(gsub(model,"",html_text(html_nodes(pg, "#make-model-list .cui-heading-4"))),side = c("both","left","right")),
             Price=extract_numeric(html_text(html_nodes(pg, ".make-model-card--msrp-range"))),
             Year = "2018",
             Make= model)
}) -> Vehicle_df

#Removing rows with NA Values
Vehicle_df <- Vehicle_df[!is.na(Vehicle_df$Price),]
names(Vehicle_df)[1] <- paste("New_Model")
names(Vehicle_df)[2] <- paste("New_Price")
names(Vehicle_df)[3] <- paste("New_Year")
names(Vehicle_df)[4] <- paste("New_Make")

#For Some car Model Car with extra model name was given like GMC Acadia 1500 for that we are considering model as Acadia only 
#Because in Used old car data model is given only as Acadia engine capacity not mentioned so removed it while cleaning
i = 31
while(i <41){
  Vehicle_df$New_Model[i] <- strsplit(Vehicle_df$New_Model[i], "\\s+")[[1]][2]
  i = i+1
}
#While web scrapping some models are comes with extra Values or with features seperated from during scrapping
#To it has been replaced with original one

Basic_Car <- c("C-Max","FiestaSedan","Express","TaurusSE","EdgeSE","Escape4WD","Explorer4WD",
               "FlexLimited","Transit","TerrainAWD","CanyonCrew","FitLX","Pilot2WD","RidgelineRTS",
               "OdysseyEX","Bolt","CamaroCoupe","SSSedan","SonicSedan","VoltLT","EquinoxFWD",
               "Suburban2WD","TahoeLS","TraverseFWD","ColoradoCrew","86Manual","Tacoma2WD","SiennaL","Altima3.5","LEAFS","MaximaS","SentraS","JUKES","MuranoAWD","PathfinderS")
N <- c( 1,3,76,13,15,16,20,21,30,33,38,48,51,52,53,54,56,61,62,64,66,67,68,69,71,77,101,103,105,107,108,109,113,115,116)
Vehicle_df$New_Model[N] <- Basic_Car


#Export cleaned New Car Prices Data to csv
write.csv(Vehicle_df, file = "C:\\Users\\Vaibhav\\Desktop\\DWBI Project\\Cleaned_New_Car_Prices.csv",row.names = FALSE)

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


