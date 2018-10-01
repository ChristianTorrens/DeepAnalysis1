#https://en.selectra.info/energy-france/guides/electricity-cost#
#Mutate etc# #https://jules32.github.io/2016-07-12-Oxford/dplyr_tidyr/#

#### Installing and Calling libraries####

install.packages("chron")

source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
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

options(digits=5)
household <- read.csv("Dropbox/Ubiqum Master/Deep Analytics and Visualization/Task1_DefineDataScienceProject/household_power_consumption.txt", 
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
SET_2007_WinterTime  <- interval(ymd_hms('2007-10-28 3:00:00'), ymd_hms('2008-03-25 1:59:00'))
SET_2008_SummerTime  <- interval(ymd_hms('2008-03-25 2:00:00'), ymd_hms('2008-10-28 2:59:00'))
SET_2008_WinterTime  <- interval(ymd_hms('2008-10-28 3:00:00'), ymd_hms('2009-03-25 1:59:00'))
SET_2009_SummerTime  <- interval(ymd_hms('2009-03-25 2:00:00'), ymd_hms('2009-10-28 2:59:00'))
SET_2009_WinterTime  <- interval(ymd_hms('2009-10-28 3:00:00'), ymd_hms('2010-03-25 1:59:00'))
SET_2010_SummerTime  <- interval(ymd_hms('2010-03-25 2:00:00'), ymd_hms('2010-10-28 2:59:00'))
SET_2010_WinterTime  <- interval(ymd_hms('2010-10-28 3:00:00'), ymd_hms('2011-03-25 1:59:00'))
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


household$DateTime<- ifelse(household$DateTime %in% SET_2007_SummerTime, { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} , 
                            ifelse(household$DateTime %in% SET_2007_WinterTime,{household$DateTime <- with_tz(household$DateTime,tz= "CET")} ,
                                   ifelse(household$DateTime %in% SET_2008_SummerTime, { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} ,
                                          ifelse(household$DateTime %in% SET_2008_WinterTime,{household$DateTime <- with_tz(household$DateTime,tz= "CET")},
                                                 ifelse(household$DateTime %in% SET_2009_SummerTime, { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} ,
                                                        ifelse(household$DateTime %in% SET_2009_WinterTime, {household$DateTime <- with_tz(household$DateTime,tz= "CET")},
                                                               ifelse(household$DateTime %in% SET_2010_SummerTime, { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} ,
                                                                      ifelse(household$DateTime %in% SET_2010_WinterTime, {household$DateTime <- with_tz(household$DateTime,tz= "CET")},
                                                                             { household$DateTime <- with_tz(household$DateTime,tz= "CEST")} ))))))))


####Checking that Changes summer to winter Hours and winter to Summer were properly made####
household[141577,]$DateTime #Ok
household[141576,]$DateTime


which(household$DateTime == "2007-10-28 3:00:00") #OK
household[454117,]$DateTime
household[454056,]$DateTime
as.POSIXct(1193536800, origin = "1970-01-01", tz = "Europe/Paris") #To see what the Seconds strip of time means in Date#
as.POSIXct(1193533140, origin = "1970-01-01", tz = "Europe/Paris")

which(household$DateTime == "2008-03-25 2:00:00")
household[668617,]$DateTime
household[668677,]$DateTime

which(household$DateTime == "2008-10-28 3:00:00")
household[981157,]$DateTime
household[,]$DateTime

which(household$DateTime == "2009-03-25 2:00:00")
household[1194217,]$DateTime
household[,]$DateTime

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



####Create Month, Day, WeekDay, Season column####
household$Hora <- hour(household$DateTime)
sum(is.na(household$Hora))

household$Mes <- month(household$DateTime)
sum(is.na(household$Mes))

household$Dia <- day(household$DateTime)
sum(is.na(household$Dia))

household$DiaSemana <- wday(household$DateTime, label = TRUE, abbr = FALSE)
sum(is.na(household$DiaSemana))

household$Season<- quarter(household$DateTime)
sum(is.na(household$Season))


####Creating a Column with the Names of the Season####
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


####PRE-GRAPHS####
###New Columns###
household$Date <- as.Date(household$Date, "%d/%m/%Y")


###Columns with same Energy Measuring Metrics###
household<-household %>% mutate(Global_ConsumptionKWh=((household$Global_active_power)/60))
household<-household %>% mutate(Global_Consumption_reactiveKWh=((household$Global_reactive_power)/60))
household<-household %>% mutate(Submetter1_kwh=(household$Sub_metering_1/1000))
household<-household %>% mutate(Submetter2_kwh=(household$Sub_metering_2/1000))
household<-household %>% mutate(Submetter3_kwh=(household$Sub_metering_3/1000))



#####CONSUMPTION REACTIVE PER SUM###
household_MONTHYEAR<-household2 %>% select(Any,Mes,Global_active_power, Global_reactive_power,Global_ConsumptionKWh ,Global_Consumption_reactiveKWh,Submetter3_kwh,
                         Submetter2_kwh,Submetter1_kwh) %>%
  group_by(Any,Mes)%>% 
  #Global_Consumption_reactive=sum(Global_Consumption_reactive))
  summarise_at(vars(Global_ConsumptionKWh,Global_Consumption_reactiveKWh,Submetter3_kwh,
                    Submetter2_kwh,Submetter1_kwh),
               funs(sum))

####June per hour each Year####
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
