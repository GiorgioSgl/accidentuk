# Library
library("readxl")
library("plyr")
library("dplyr")
library("Hmisc")
library("data.table")

# Load csv

accident <- read.csv("../data/Accidents0514.csv",na.strings = "-1")
casualties <- read.csv("../data/Casualties0514.csv",na.strings = "-1")
#info <- read_excel("ukmidyearestimates20182019ladcodes.xls")
vehicles <- read.csv("../data/Vehicles0514.csv",na.strings = "-1")


## Wrangling

#accident
accident_ <- accident %>%
  select(ï..Accident_Index,Longitude,Latitude,Accident_Severity,Number_of_Vehicles,Date,Day_of_Week,Time,Light_Conditions,Weather_Conditions,Local_Authority_.Highway.) %>%
  mutate(index=ï..Accident_Index) %>%
  select(-c(ï..Accident_Index)) %>%
  na.omit()

accident_$Accident_Severity <- revalue(factor(accident_$Accident_Severity),c("1"="fatal","2"="serious","3"="slight"))

new <- c("1"="sunday","2"="monday","3"="tuesday","4"="wednesday","5"="thursday","6"="friday","7"="saturday")
accident_$Day_of_Week <- revalue(factor(accident_$Day_of_Week),new)

new <- c("1"="Fine","2"="Raining","3"="Snowing","4"="Fine","5"="Raining","6"="Snowing","7"="Fog","8"="-1","9"="-1")
accident_$Weather_Conditions <- revalue(factor(accident_$Weather_Conditions),new)
accident_[accident_=="-1"] <- NA

new <- c("1"="Daylight","4"="Darkness","5"="Darkness","6"="Darkness","7"="Darkness")
accident_$Light_Conditions <- revalue(factor(accident_$Light_Conditions),new)

# get the city with a merging on another dataset
map <- read.csv("../data/postcodetocity.csv",na.strings = "")
map <- map %>% select(geo_code, geo_label)
accident_ <- merge(x = accident_, y = map, by.x = "Local_Authority_.Highway.", by.y="geo_code", all = TRUE)

accident_ <- accident_ %>% 
  mutate(city=geo_label,geo_code=Local_Authority_.Highway.) %>%
  select(-c(geo_label,Local_Authority_.Highway.))

# casualties (vittima)
casualties_ <- casualties %>%
  select(ï..Accident_Index,Casualty_Class,Sex_of_Casualty,Age_of_Casualty,Casualty_Severity,Casualty_Home_Area_Type) %>%
  mutate(index=ï..Accident_Index) %>%
  select(-c(ï..Accident_Index)) %>%
  na.omit()

casualties_$Sex_of_Casualty <- revalue(factor(casualties_$Sex_of_Casualty),c("1"="male","2"="female"))
casualties_$Casualty_Severity <- revalue(factor(casualties_$Casualty_Severity),c("1"="fatal","2"="serious","3"="slight"))
casualties_$Casualty_Class <- revalue(factor(casualties_$Casualty_Class),c("1"="driver","2"="passenger","3"="pedestrian"))
casualties_$Casualty_Home_Area_Type <- revalue(factor(casualties_$Casualty_Home_Area_Type),c("1"="urban area","2"="small town","3"="rural"))

# vehicles

vehicles_ <- vehicles %>%
  select(ï..Accident_Index,Vehicle_Type,Sex_of_Driver,Age_of_Driver,Age_of_Driver,Age_of_Vehicle,Driver_Home_Area_Type,Engine_Capacity_.CC.) %>%
  mutate(index=ï..Accident_Index,
         engine=Engine_Capacity_.CC.) %>%
  select(-c(ï..Accident_Index,Engine_Capacity_.CC.))

new <- c("1"="Bicycle","2"="Moto","3"="Moto","4"="Moto","5"="Moto","8"="Taxi","9"="Car","10"="Bus","11"="Bus",
         "19"="Goods","20"="Goods","21"="Goods","22"="Moto","23"="Moto","97"="Moto","98"="Goods")
vehicles_$Vehicle_Type <- revalue(factor(vehicles_$Vehicle_Type),new)
vehicles_ <- vehicles_ %>% mutate(Vehicle_Type = na_if(Vehicle_Type, "90"),
                                  Vehicle_Type = na_if(Vehicle_Type, "16"),
                                  Vehicle_Type = na_if(Vehicle_Type, "17"),
                                  Vehicle_Type = na_if(Vehicle_Type, "18"))
vehicles_$Sex_of_Driver <- revalue(factor(vehicles_$Sex_of_Driver),c("1"="male","2"="female"))
vehicles_$Driver_Home_Area_Type <- revalue(factor(vehicles_$Driver_Home_Area_Type),c("1"="urban area","2"="small town","3"="rural"))



# outer join
accidents <- merge(x = vehicles_, y = casualties_, by = "index", all = TRUE)
accidents <- merge(x = accidents, y = accident_, by = "index", all = TRUE)
accidents$Age_Interval <- cut2(accidents$Age_of_Driver, g=5)

#two variable are the same we merge it
accidents <- accidents %>%
  mutate(severity = Accident_Severity) %>%
  select(-c(Accident_Severity,Casualty_Severity))

# get year and month from date
accidents <- accidents %>%
  mutate(month = month.name[month(as.POSIXlt(Date, format="%d/%m/%Y"))],
         year = year(as.POSIXlt(Date, format="%d/%m/%Y")))



#clean memory (if you want)
memory.size()
rm(vehicles)
rm(vehicles_)
rm(accident)
rm(accident_)
rm(casualties)
rm(casualties_)
gc()
memory.size()





