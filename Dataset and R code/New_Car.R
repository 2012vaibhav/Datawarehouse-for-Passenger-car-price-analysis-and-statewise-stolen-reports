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
