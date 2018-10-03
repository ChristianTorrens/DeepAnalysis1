#https://en.selectra.info/energy-france/guides/electricity-cost#
#Mutate etc# #https://jules32.github.io/2016-07-12-Oxford/dplyr_tidyr/#

#### Installing and Calling libraries####

install.packages("chron")

source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
devtools::install_github("hrbrmstr/taucharts")
source("calendarHeat.R")
install.packages("chron")
library("chron")
install.packages("tidyr")
install.packages("lubridate")
install.packages("hydroTSM")
install.packages("ggplot2")
install.packages("bdvis")
install.packages("magrittr")
install.packages("RColorBrewer")
install.packages("grid")
install.packages("zoo")
install.packages("padr")
install.packages("ggalt")
install.packages("taucharts")
install.packages("doBy")
install.packages("highcharter")

library(highcharter)
library(doBy)
library(taucharts)
library(ggalt)
library(padr)
library(zoo)
library(caret)
library(tidyr)
library(dplyr)
library(timeDate)
library(lubridate)
library(readr)
library(hydroTSM)
library(ggplot2)
library(caret)
library(magrittr)
library(RColorBrewer)
library(chron)
library(bdvis)
library(grid)

#### Importing DataSet####
options(digits=5)
setwd("~/Dropbox/Ubiqum Master/Deep Analytics and Visualization/Task1_DefineDataScienceProject")
household <- read.csv("~/Dropbox/Ubiqum Master/Deep Analytics and Visualization/Task1_DefineDataScienceProject/household_power_consumption.txt", 
                      header=TRUE,sep=";",na.strings = c("?"))

View(household)

####Dplyr DateTime####
household$Date<- as.character(household$Date)
household$Time<- as.character(household$Time)
household$DateTime<- paste(household$Date, household$Time)
household$DateTime <- dmy_hms(household$DateTime)
household$DateTime <- with_tz(household$DateTime, "Europe/Paris")
#household$DateTime <- strptime(household$DateTime, "%d/%m/%Y %H:%M:%S", tz= "Europe/Paris")
#household$Date<-as.POSIXct(household$DateTime,tz= "Europe/Paris" )
str(household)




####APPLY DAYLIGHT SAVINGS####
#https://cran.r-project.org/web/packages/padr/vignettes/padr_implementation.html
#Define a time Period#
SET_2007_SummerTime  <- interval(ymd_hms('2007-03-25 2:00:00'), ymd_hms('2007-10-28 2:59:00'))
SET_2008_SummerTime  <- interval(ymd_hms('2008-03-25 2:00:00'), ymd_hms('2008-10-28 2:59:00'))
SET_2009_SummerTime  <- interval(ymd_hms('2009-03-25 2:00:00'), ymd_hms('2009-10-28 2:59:00'))
SET_2010_SummerTime  <- interval(ymd_hms('2010-03-25 2:00:00'), ymd_hms('2010-10-28 2:59:00'))
SET_2011_SummerTime  <- interval(ymd_hms('2011-03-25 2:00:00'), ymd_hms('2011-10-28 2:59:00'))

####THE NEW FUNCTION WAS TAKING WAS TOO LONG TO RUN####
#Daylight_func<- function(dia){
  #if( dia %within% SET_2007_SummerTime) { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} 
  #if( dia %within% SET_2007_WinterTime) {household$DateTime <- with_tz(household$DateTime,tz= "CET")}   
  #if ( dia %within% SET_2008_SummerTime) { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} 
  #if( dia %within% SET_2008_WinterTime) {household$DateTime <- with_tz(household$DateTime,tz= "CET")}  
  #if ( dia %within% SET_2009_SummerTime) { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} 
  #if( dia %within% SET_2009_WinterTime) {household$DateTime <- with_tz(household$DateTime,tz= "CET")}  
  #if ( dia %within% SET_2010_SummerTime) { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} 
  #if( dia %within% SET_2010_WinterTime) {household$DateTime <- with_tz(household$DateTime,tz= "CET")}  
  #if ( dia %within% SET_2011_SummerTime) { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} 
  #}

#household$DateTime <- sapply(household$DateTime, Daylight_func)
household$SDateTime <- if(( household$DateTime %in% SET_2007_SummerTime) | (household$DateTime %in% SET_2008_SummerTime)
| (household$DateTime %in% SET_2009_SummerTime) |(household$DateTime %in% SET_2010_SummerTime)){household$DateTime +dhours(1)}
else{household$DateTime +dhours(0)}
                           


#Now the times changed into seconds#
####CHANGING BACK THE FORMAT OF DATE TIME####
household$DateTime<- as.POSIXct(household$DateTime, origin = "1970-01-01")


####Checking that Changes summer to winter Hours and winter to Summer were properly made####
household[141577,]$DateTime #Ok
household[141576,]$DateTime
as.POSIXct(1193536800, origin = "1970-01-01", tz = "Europe/Paris") #To see what the Seconds strip of time means in Date#
as.POSIXct(1193536860, origin = "1970-01-01", tz = "Europe/Paris") #https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html

####DAYLIGHT SAVINGS ONLY WORKING ON THE FIRST CHANFE
which(household$DateTime == "2008-03-25 2:00:00")
household[668617,]$DateTime
household[668618,]$DateTime

which(household$DateTime == "2008-10-28 3:00:00")
household[981157,]$DateTime
household[,]$DateTime

which(household$DateTime == "2009-03-25 2:00:00")
household[1194217,]$DateTime
household[1194240,]$DateTime
as.POSIXct(1237944180, origin = "1970-01-01", tz = "Europe/Paris")

which(household$DateTime == "2009-10-28 3:00:00")
household[1506757,]$DateTime
household[,]$DateTime

which(household$DateTime == "2010-03-25 2:00:00")
household[1719817,]$DateTime
household[,]$DateTime


which(household$DateTime == "2010-10-28 3:00:00")
household[2032297,]$DateTime
household[,]$DateTime




####Finding NAs####
MatrixOfNAs<- filter(household, is.na(Global_active_power))
RowsOfNAs<- which(is.na(household$Global_active_power))
write.csv(RowsOfNAs, "typeofna.csv")

sum(is.na(household))

####Replacing NA´s of less than 3 hours. THOSE THAT ARE JUST ENERGY CUTS####
household$Global_active_power<- na.locf(household$Global_active_power, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
sum(is.na(household$Global_active_power))#to see how many NA
household$Global_reactive_power<- na.locf(household$Global_reactive_power, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
household$Voltage<- na.locf(household$Voltage, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
household$Global_intensity<- na.locf(household$Global_intensity, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
household$Sub_metering_1<- na.locf(household$Sub_metering_1, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
household$Sub_metering_2<- na.locf(household$Sub_metering_2, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
household$Sub_metering_3<- na.locf(household$Sub_metering_3, na.rm = FALSE, fromLast = FALSE, maxgap = 180)



####Replacing rest of NA´s of less than 3 hours. THOSE THAT ARE JUST ENERGY CUTS####
household$Global_active_power[is.na(household$Global_active_power)]<-0
sum(is.na(household$Global_active_power))
household$Global_reactive_power[is.na(household$Global_reactive_power)]<-0
household$Global_intensity[is.na(household$Global_intensity)]<-0
household$Voltage[is.na(household$Voltage)]<-0
household$Sub_metering_1[is.na(household$Sub_metering_1)]<-0
household$Sub_metering_2[is.na(household$Sub_metering_2)]<-0
household$Sub_metering_3[is.na(household$Sub_metering_3)]<-0

sum(is.na(household))


####Columns with SAME ENERGY MEASURING METRICS####
household<-household %>% mutate(Global_ConsumptionKWh=((household$Global_active_power)/60))
household<-household %>% mutate(Global_Consumption_reactiveKWh=((household$Global_reactive_power)/60))
household<-household %>% mutate(Submetter1_kwh=(household$Sub_metering_1/1000))
household<-household %>% mutate(Submetter2_kwh=(household$Sub_metering_2/1000))
household<-household %>% mutate(Submetter3_kwh=(household$Sub_metering_3/1000))





####Create Month,Week, Day, WeekDay,Season column####
household$Hora <- hour(household$DateTime)
sum(is.na(household$Hora))

household$Mes <- month(household$Date)
sum(is.na(household$Mes))

household$Semana <- week(household$DateTime)
sum(is.na(household$Semana))

household$Dia <- day(household$DateTime)
sum(is.na(household$Dia))

household$DiaSemana <- wday(household$DateTime, label = TRUE, abbr = FALSE)
sum(is.na(household$DiaSemana))

household$Season<- quarter(household$DateTime)
sum(is.na(household$Season))


###Creating a Column with the Names of the Season###
household$SeasonWNames <-""
household$SeasonWNames[household$Season == "1"] <- "Winter"
household$SeasonWNames[household$Season == "2"] <- "Spring"
household$SeasonWNames[household$Season == "3"] <- "Summer"
household$SeasonWNames[household$Season == "4"] <- "Fall"

View(household)
sum(is.na(household$SeasonWNames))

####Creating a Year columne###
household$Any<- year(household$DateTime)
sum(is.na(household$Any))

####LINEAR CORRELATION BETWEEN ATTRIBUTES TO DELETE SOME ALREADY AND SIMPLIFY ALL COMPUTING PROCESSES####
#TO DO HERE: Correlation Matrix#











####CREATING A HOUSEHOLD 2 DATASET####
#1s Merge the hours to reduce input#
household2<- household #creating a new exact subset to be able to step backwards easily if needed
household2$Global_active_power<- NULL
household2$Global_reactive_power<- NULL
household2$Global_intensity<- NULL
household2$Voltage<- NULL
household2$Sub_metering_1<- NULL
household2$Sub_metering_2<- NULL
household2$Sub_metering_3<- NULL

View(household2)
str(household2)

####HOUSEHOLD HISTOGRAM PER SEASON ####
householdSeason<- household2

householdSeason<- householdSeason%>% group_by(SeasonWNames)%>% mutate(SumaSeasonKW = sum(Global_ConsumptionKWh))

householdSeason<- householdSeason%>% group_by(SeasonWNames)%>%mutate(SumaSeasonReactiveKW = sum(Global_Consumption_reactiveKWh))

householdSeason<- householdSeason%>% group_by(SeasonWNames)%>%mutate(SumaSeasonKitchenKW = sum(Submetter1_kwh))

householdSeason<- householdSeason%>% group_by(SeasonWNames)%>%mutate(SumaSeasonLaundryKW = sum(Submetter2_kwh))

householdSeason<- householdSeason%>% group_by(SeasonWNames)%>%mutate(SumaSeasonHeaterKW = sum(Submetter3_kwh))

householdSeasonGood<- householdSeason

  
householdSeasonGood<- select(householdSeasonGood, Any, SeasonWNames,SumaSeasonKW,SumaSeasonReactiveKW,SumaSeasonLaundryKW,SumaSeasonHeaterKW )

householdSeasonGood<- distinct(householdSeasonGood)

View(householdSeasonGood)

####QUICK PLOTTING OF TOTAL CONSUMPTION PER SEASON### #http://www.cookbook-r.com
ggplot(data=householdSeasonGood, aes(x=SeasonWNames, y=SumaSeasonKW, fill=Any)) +
  facet_wrap( ~ Any)+ geom_bar(stat="identity", position=position_dodge())


####HOUSEHOLD HISTOGRAM PER MONTH ####

householdMonth<- household2

householdMonth<- householdMonth%>% group_by(Mes)%>% mutate(SumaMonthKW = sum(Global_ConsumptionKWh))

householdMonth<- householdMonth%>% group_by(Mes)%>%mutate(SumaMonthReactiveKW = sum(Global_Consumption_reactiveKWh))

householdMonth<- householdMonth%>% group_by(Mes)%>%mutate(SumaMonthKitchenKW = sum(Submetter1_kwh))

householdMonth<- householdMonth%>% group_by(Mes)%>%mutate(SumaMonthLaundryKW = sum(Submetter2_kwh))

householdMonth<- householdMonth%>% group_by(Mes)%>%mutate(SumaMonthHeaterKW = sum(Submetter3_kwh))

householdMonthGood<- householdMonth

householdMonthGood$MonthWNames <-""
householdMonthGood$MonthWNames[household$Mes == "1"] <- "Jan"
householdMonthGood$MonthWNames[household$Mes == "2"] <- "Feb"
householdMonthGood$MonthWNames[household$Mes == "3"] <- "Mar"
householdMonthGood$MonthWNames[household$Mes == "4"] <- "Apr"
householdMonthGood$MonthWNames[household$Mes == "5"] <- "May"
householdMonthGood$MonthWNames[household$Mes == "6"] <- "Jun"
householdMonthGood$MonthWNames[household$Mes == "7"] <- "Jul"
householdMonthGood$MonthWNames[household$Mes == "8"] <- "Aug"
householdMonthGood$MonthWNames[household$Mes == "9"] <- "Sep"
householdMonthGood$MonthWNames[household$Mes == "10"] <- "Oct"
householdMonthGood$MonthWNames[household$Mes == "11"] <- "Nov"
householdMonthGood$MonthWNames[household$Mes == "12"] <- "Dec"

householdMonthGood<- select(householdMonthGood, Any, MonthWNames, Mes, SumaMonthKW,SumaMonthReactiveKW,SumaMonthLaundryKW,SumaMonthHeaterKW )
householdMonthGood<- distinct(householdMonthGood)

View(householdMonthGood)



####QUICK PLOTTING OF TOTAL CONSUMPTION PER SEASON### #http://www.cookbook-r.com
ggplot(data=householdMonthGood, aes(x=MonthWNames, y=SumaMonthKW, fill=Any)) +
  facet_wrap( ~ Any)+ geom_bar(stat="identity", position=position_dodge())

ggplot(data=householdMonthGood, aes(x=MonthWNames , y=SumaMonthKW, group=Any, colour=Any)) +
  geom_line()+theme_bw()+ geom_point()+facet_wrap(facets = Any ~ .)#, margins = FALSE)

####HOUSEHOLD HISTOGRAM PER WEEK ####
householdWeek<- household2

householdWeek<- householdWeek%>% group_by(Semana)%>% mutate(SumaWeekKW = sum(Global_ConsumptionKWh))

householdWeek<- householdWeek%>% group_by(Semana)%>%mutate(SumaWeekReKW = sum(Global_Consumption_reactiveKWh))

householdWeek<- householdWeek%>% group_by(Semana)%>%mutate(SumaWeekKitchenKW = sum(Submetter1_kwh))

householdWeek<- householdWeek%>% group_by(Semana)%>%mutate(SumaWeekLaundryKW = sum(Submetter2_kwh))

householdWeek<- householdWeek%>% group_by(Semana)%>%mutate(SumaWeekHeaterKW = sum(Submetter3_kwh))

householdWeekGood<- householdWeek

householdWeekGood<- select(householdWeekGood, Any, Semana, Mes, SumaWeekKW,SumaWeekReKW,SumaWeekLaundryKW,SumaWeekHeaterKW )

householdWeekGood<- distinct(householdWeekGood)

View(householdWeekGood)

unique(householdWeekGood$Semana)

####QUICK PLOTTING OF TOTAL CONSUMPTION PER SEASON### #http://www.cookbook-r.com
ggplot(data=householdWeekGood, aes(x=Semana, y=SumaWeekKW, fill=Any)) +
  facet_wrap( ~ Any)+ geom_bar(stat="identity", position=position_dodge())

ggplot(data=householdWeekGood, aes(x= Semana, y=SumaWeekKW, group=Any, colour=Any)) +
  geom_line()+theme_bw()+ geom_point()+facet_wrap(facets = Any ~ .)#, margins = FALSE)


####HOUSEHOLD HISTOGRAM WEEKDAY/PER MONTH ####
householdWDayMonth<- household2

householdWDayMonth<- householdWDayMonth%>% group_by(DiaSemana)%>% mutate(SumaWeekDayKW = sum(Global_ConsumptionKWh))

householdWDayMonth<- householdWDayMonth%>% group_by(DiaSemana)%>%mutate(SumaWeekDayReKW = sum(Global_Consumption_reactiveKWh))

householdWDayMonth<- householdWDayMonth%>% group_by(DiaSemana)%>%mutate(SumaWeekDayKitchenKW = sum(Submetter1_kwh))

householdWDayMonth<- householdWDayMonth%>% group_by(DiaSemana)%>%mutate(SumaWeekDayLaundryKW = sum(Submetter2_kwh))

householdWDayMonth<- householdWDayMonth%>% group_by(DiaSemana)%>%mutate(SumaWeekDayHeaterKW = sum(Submetter3_kwh))

householdWDayMonthGood<- householdWDayMonth

householdWDayMonthGood<- select(householdWDayMonthGood, Any, DiaSemana, Mes, SumaWeekDayKW,SumaWeekDayReKW,SumaWeekDayKitchenKW,SumaWeekDayLaundryKW,SumaWeekDayHeaterKW )

householdWDayMonthGood<- distinct(householdWDayMonthGood)

View(householdWDayMonthGood)

unique(householdWDayMonthGood$DiaSemana)

####QUICK PLOTTING OF TOTAL CONSUMPTION PER SEASON### #http://www.cookbook-r.com
ggplot(data=householdWDayMonthGood, aes(x= DiaSemana, y=SumaWeekDayKW, group= Mes, colour=Mes)) +
  geom_line()+theme_bw()+ geom_point()+facet_wrap(facets = Mes ~ .)#, margins = FALSE)

####Prep: Collapsing Consumptions into Hour. (Suma horas here: Called HouseholdHOURComplete) #### 
householdHOUR<- household2
householdHOUR<- householdHOUR%>% group_by(Any,Mes,Dia,Hora)%>% mutate(SumaHoraKW = sum(Global_ConsumptionKWh))
                                                           
householdHOUR<- householdHOUR%>% group_by(Any,Mes,Dia,Hora)%>%mutate(SumaHoraReKW = sum(Global_Consumption_reactiveKWh))

householdHOUR<- householdHOUR%>% group_by(Any,Mes,Dia,Hora)%>%mutate(SumaKitchenKW = sum(Submetter1_kwh))

householdHOUR<- householdHOUR%>% group_by(Any,Mes,Dia,Hora)%>%mutate(SumaLaundryKW = sum(Submetter2_kwh))

householdHOUR<- householdHOUR%>% group_by(Any,Mes,Dia,Hora)%>%mutate(SumaHeaterKW = sum(Submetter3_kwh))

unique(householdHOUR$SumaHoraKW)
unique(householdHOUR$SumaHoraReKW)
unique(householdHOUR$SumaKitchenKW)
unique(householdHOUR$SumaLaundryKW)
unique(householdHOUR$SumaHeaterKW)

householdHOURComplete<- householdHOUR

str(householdHOUR)

View(householdHOUR)

householdHOUR<- select(householdHOUR, Any, Hora, Mes, Dia,DiaSemana,SumaHoraKW,SumaHoraReKW,SumaKitchenKW,SumaLaundryKW,SumaHeaterKW)
householdHOUR<-distinct(householdHOUR)
str(householdHOUR)
View(householdHOUR)



####CHECKING PER HOUR WITHIN DAY OF WEEK####
#warning. Solve this!
DIAMESHORA<- householdHOUR
DIAMESHORA$DateTime<- format(as.POSIXct(DIAMESHORA$DateTime,format='%m/%d/%Y %H'),format='%m/%d/%Y')  #To give DateTime a time format again
DIAMESHORA<- select(DIAMESHORA,DateTime, Any, Hora, Mes, Dia,DiaSemana,SumaHoraKW,SumaHoraReKW,SumaKitchenKW,SumaLaundryKW,SumaHeaterKW)
DIAMESHORA<- distinct(DIAMESHORA) 

DIAMESHORAGOOD<-DIAMESHORA3

DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaKW = sum(SumaHoraKW))
DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaReKW = sum(SumaHoraReKW))
DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaKitchen = sum(SumaKitchenKW))
DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaLaundry = sum(SumaLaundryKW))
DIAMESHORAGOOD<- DIAMESHORAGOOD%>% group_by(Any,Mes,Dia)%>% mutate(SumaDiaHeater = sum(SumaHeaterKW))


####GRAPHS CONSUMPTION PER HOUR ON EACH DAY OF THE WEEK####
DIASEMANACTIVE<-household2%>% group_by(DiaSemana) %>% summarise(NewGlobalActive= sum(Global_ConsumptionKWh))
DIASEMANACTIVE

DIASEMANHORAACTIVE<-household2%>% group_by(DiaSemana, Hora) %>% summarise(NewGlobalActive= sum(Global_ConsumptionKWh))
DIASEMANAHORACTIVE

HORAACTIVE<-household2%>% group_by(Hora) %>% summarise(NewGlobalActive= sum(Global_ConsumptionKWh))
HORAACTIVE

household2$HourKWSum<-household2%>% group_by(Hora) %>% mutate(ActivePerHour= sum(Global_ConsumptionKWh))
HORAACTIVE


#2 Variables Bar plot
ggplot(data=DIASEMANAHORAACTIVE, aes(x=DiaSemana, y=NewGlobalActive, fill = DiaSemana)) +
  geom_bar(stat="identity")


#3 Variables Bar plot
ggplot(data=DIASEMANHORAACTIVE, aes(x=Hora, y=NewGlobalActive, fill=DiaSemana)) +
  geom_bar(stat="identity", position=position_dodge())

##ultiple Graphs
#Line Graph WeekDay, Hour, Active Consumption#
p1 <- ggplot(DIASEMANHORAACTIVE, aes(x=Hora, y=NewGlobalActive, colour=DiaSemana, group=DiaSemana)) +
  geom_line() 
p1

#Line Graph WeekDay, Hour, Active Consumption with trends#
p2 <- ggplot(DIASEMANHORAACTIVE, aes(x=Hora, y=NewGlobalActive, colour=DiaSemana)) +
  geom_point(alpha=.3) +
  geom_smooth(alpha=.2, size=1) 
p2




####365 Days per Year####
DIAMES<- householdHOURComplete
DIAMES$DiaDelAny<-""

DIAMES$DiaDelAny<- strftime(DIAMES$DateTime, format = "%j", tz = "Europe/Paris")####to see which Day of the year it is
DIAMES$DiaDelAny<-as.numeric(DIAMES$DiaDelAny)

DIAMES<- DIAMES%>% group_by(DiaDelAny)%>% mutate(SumaDayKW = sum(SumaHoraKW))

DIAMES<- DIAMES%>% group_by(DiaDelAny)%>%mutate(SumaDayReKW = sum(SumaHoraReKW))

DIAMES<- DIAMES%>% group_by(DiaDelAny)%>%mutate(SumaDayKitchenKW = sum(SumaKitchenKW))

DIAMES<- DIAMES%>% group_by(DiaDelAny)%>%mutate(SumaDayLaundryKW = sum(SumaLaundryKW))

DIAMES<- DIAMES%>% group_by(DiaDelAny)%>%mutate(SumaDayHeaterKW = sum(SumaHeaterKW))


View(DIAMES)

unique(DIAMES$SumaDayKW)
unique(DIAMES$SumaDayReKW)
unique(DIAMES$SumaDayKitchenKW)
unique(DIAMES$SumaDayLaundryKW)
unique(DIAMES$SumaDayHeaterKW)
unique(DIAMES$DiaDelAny)


DIAMES$SumaHoraKW<- NULL
DIAMES$SumaHoraReKW<- NULL
DIAMES$SumaKitchenKW<- NULL
DIAMES$SumaLaundryKW<- NULL
DIAMES$SumaHeaterKW<- NULL
DIAMES$Hora<- NULL
DIAMES$Time<- NULL
DIAMES$DateTime<- NULL
DIAMES$Global_ConsumptionKWh<- NULL
DIAMES$Global_Consumption_reactiveKWh<- NULL
DIAMES$Submetter1_kwh<- NULL
DIAMES$Submetter2_kwh<- NULL
DIAMES$Submetter3_kwh<- NULL

DIAMES<- distinct(DIAMES) 

str(DIAMES)
View(DIAMES)




ggplot(data=DIAMES, aes(x=DiaDelAny , y=SumaDayKW, group=Any, colour=Any)) +
  geom_line()+theme_bw()+ geom_point()+facet_wrap(facets = Any ~ .)#, margins = FALSE)

p1 <- ggplot(DIAMES, aes(x=DiaDelAny, y=SumaDayKW, colour=Any, group=Any)) +
  geom_line() 
p1




####SET RULES OF OUTLIERS PER DAY####
boxplot(DIAMES$SumaDayKW) #Consumption Outlier Rule
boxplot(DIAMES$SumaDayReKW) #Reactive Outlier Rule
boxplot(DIAMES$SumaDayKitchenKW) #Kitchen Outlier Rule
boxplot(DIAMES$SumaDayLaundryKW) #Laundry Outlier Rule
boxplot(DIAMES$SumaDayHeaterKW) #Heater Outlier Rule

####DETECTING WHICH ARE THE ROWS OF THE OUTLIERS###
#For Rective Power
unique(DIAMES$SumaDayReKW)

OutliersRE <- which(DIAMES$SumaDayReKW >= 1000)
OutliersRE

#Checking one of those days
DIAMES[384,]$Date
DIAMES[384,]


#WANT TO CREATE A SUBSET WITH ALL THE INFO THOSE..


####REACTIVE vs ACTIVE ENERGY####







####ENERGY CONSUMPTION PER MONTH####
ggplot(data=household_MONTHYEAR, aes(household_MONTHYEAR$MonthAbb, group=1))+
  #geom_line(aes(y = HPC_My2006$Submetter1_kwh, color="Kitchen")) + 
  #geom_line(aes(y = HPC_My2006$Submetter2_kwh, color="Laundry Room")) + 
  #geom_line(aes(y = HPC_My2006$Submetter3_kwh, color="Heater")) + 
  geom_line(aes(y = household_MONTHYEAR$Global_ConsumptionKWh, color="Active_Power"))+
  geom_line(aes(y = household_MONTHYEAR$Global_Consumption_reactiveKWh, color="Reactive_Power"))+
  xlab("Year")+
  ylab("KWh")+
  ggtitle("Energy Consumption by Month")+
  scale_y_continuous(labels = function(x) format(x, scientific =FALSE))+
  facet_wrap( ~ Any)




####To make two Columns of graphs### http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#multiplot(p1, p2, p3, p4, cols=2)
#> `geom_smooth()` using method = 'loess'


#Submetering Graph#
#plot(data_sub$DateTime, data_sub$Sub_metering_1,
    # "n",
    # xlab = "",
     #ylab = "Energy sub metering")

#points(data_sub$DateTime, data_sub$Sub_metering_1, type = "line")

#points(data_sub$DateTime, data_sub$Sub_metering_2, type = "line", col = "red")

#points(data_sub$DateTime, data_sub$Sub_metering_3, type = "line", col = "blue")

#legend("topright",
       #legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       #col = c("black", "red", "blue"),
       #lty = c(1, 1, 1))

#Line Graph WeekDay, Hour, Active Consumption with trends#



# This must go after theme_b



#####CONSUMPTION REACTIVE PER SUM###
household_MONTHYEAR<-household2 %>% select(Any,Mes,Global_active_power, Global_reactive_power,Global_ConsumptionKWh ,Global_Consumption_reactiveKWh,Submetter3_kwh,
                         Submetter2_kwh,Submetter1_kwh) %>%
  group_by(Any,Mes)%>% 
  #Global_Consumption_reactive=sum(Global_Consumption_reactive))
  summarise_at(vars(Global_ConsumptionKWh,Global_Consumption_reactiveKWh,Submetter3_kwh,
                    Submetter2_kwh,Submetter1_kwh),
               funs(sum))

####June per hour each Year####
DIAMESHORA3$SumaHoraKW<- NULL
DIAMESHORA3$SumaHoraReKW<- NULL
DIAMESHORA3$SumaKitchenKW<- NULL
DIAMESHORA3$SumaLaundryKW<- NULL
DIAMESHORA3$SumaHeaterKW<- NULL
DIAMESHORA3$Hora<- NULL
DIAMESHORA3$DateTime<- NULL
DIAMESHORA3<- distinct(DIAMESHORA3) 
str(DIAMESHORA3)
View(DIAMESHORA3)









household3_hour<-household2 %>% select(Any,Mes,Hora,Global_ConsumptionKWh,Global_Consumption_reactiveKWh,
                           Sub_metering_3,Submetter3_kwh,
                           Submetter2_kwh,Submetter1_kwh) %>%
  filter(Mes==6 & Any!=2006) %>%
  group_by(Any,Mes,Hora)%>% 
  #Global_Consumption_reactive=sum(Global_Consumption_reactive)
  summarise_at(vars(Global_ConsumptionKWh, Global_Consumption_reactiveKWh ,Submetter3_kwh,Submetter2_kwh,Submetter1_kwh),funs(sum))

####Gives you the name of the month in the graphs as a factor####
household_MONTHYEAR<- transform(household_MONTHYEAR, MonthAbb = month.abb[Mes])

####Puts them in order####
household_MONTHYEAR$MonthAbb <-factor(household_MONTHYEAR$MonthAbb, 
                                    levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
####plots####
hist(household$Global_ConsumptionKWh,
     col="red", 
     main="Global Active Power", 
     xlab="Global Active Power (kilowatts/h)")

ggplot(data=household_MONTHYEAR, aes(x= Mes, y=Global_ConsumptionKWh, group=Any, colour=Any)) +
  geom_line()+theme_bw()+ geom_point()+facet_wrap(facets = Any ~ .)#, margins = FALSE)


####ENERGY CONSUMPTION PER MONTH####
ggplot(data=household_MONTHYEAR, aes(household_MONTHYEAR$MonthAbb, group=1))+
  #geom_line(aes(y = HPC_My2006$Submetter1_kwh, color="Kitchen")) + 
  #geom_line(aes(y = HPC_My2006$Submetter2_kwh, color="Laundry Room")) + 
  #geom_line(aes(y = HPC_My2006$Submetter3_kwh, color="Heater")) + 
  geom_line(aes(y = household_MONTHYEAR$Global_ConsumptionKWh, color="Active_Power"))+
  geom_line(aes(y = household_MONTHYEAR$Global_Consumption_reactiveKWh, color="Reactive_Power"))+
  xlab("Year")+
  ylab("KWh")+
  ggtitle("Energy Consumption by Month")+
  scale_y_continuous(labels = function(x) format(x, scientific =FALSE))+
  facet_wrap( ~ Any)
#facet_grid(facets = Any ~ ., margins = FALSE) 


####Calendar Heat####
library("chron")
calendarHeat(household$Date, household$Global_Consumption_reactiveKWh, varname="Global_reActive_Power")
summary(household)


####9####
ggplot(data=household3_My2006, aes(household3_My2006$MonthAbb,group=1))+
  #geom_line(aes(y = HPC_My2006$Submetter1_kwh, color="Kitchen")) + 
  #geom_line(aes(y = HPC_My2006$Submetter2_kwh, color="Laundry Room")) + 
  #geom_line(aes(y = HPC_My2006$Submetter3_kwh, color="Heater")) + 
  geom_line(aes(y = household3_My2006$Global_ConsumptionKWh, color="Active_Power"))+
  geom_line(aes(y = household3_My2006$Global_reactiveKWh, color="Reactive_Power"))+
  xlab("Year")+
  ylab("KWh")+
  ggtitle("Energy Consumption by Month")+
  #scale_x_discrete(labels=  month.abb) + 
  #scale_x_date(labels = date_format("%b"))+
  #theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  #theme_bw()+
  scale_y_continuous(labels = function(x) format(x, scientific =FALSE))+
  #scale_colour_manual(name='', 
  #  values=c('Active_Power'="#CC6666"), # Kitchen',
  #'Reactive_Power'="blue"), #Laundry Room'="blue", 
  #'Heater'="darkgreen"), 
  #guide='legend') +
  facet_wrap( ~ Any )
#facet_grid(facets = Year ~ ., margins = FALSE) 








####DO NOT USE THE CODE BELOW YET###

####SEASON SUBSETS#
household %>% group_by(SeasonWNames) %>% summarise(mean(Global_active_power))
NA2<- which(is.na(household$Global_active_power))
NA2

####AHORA GRÁFICOS####
ggplot(data=Data_Month, aes(Month)) +
  facet_wrap( ~ Year) +
  geom_line(aes(y = Laundry, color="red")) +
  geom_line(aes(y = Kitchen, color="green"))


####to show graph of the 3 submeterings####
library(ggplot2)
library(lubridate)
theme_set(theme_bw())

df <- economics_long[economics_long$variable %in% c("psavert", "uempmed"), ]
df <- df[lubridate::year(df$date) %in% c(1967:1981), ]

# labels and breaks for X axis text
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)

# plot
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Time Series of Returns Percentage", 
       subtitle="Drawn from Long Data format", 
       caption="Source: Economics", 
       y="Returns %", 
       color=NULL) +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  scale_color_manual(labels = c("psavert", "uempmed"), 
                     values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank()) 



####CREATING SUBSETS####
### 1. If it is Day of Week and Season###


####WORK DAYS VS HOLIDAYS#### #Based on NAs#
####Filtering Dates#### #https://blog.exploratory.io/filter-with-date-function-ce8e84be680#
WeekendsOut<-which(household$DiaSemana == 1 | 
                     household$xDiaSemana == 2) 


household<-household[-WeekendsOut,]
