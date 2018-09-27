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

####Finding NAs####
MatrixOfNAs<- filter(household, is.na(Global_active_power))
RowsOfNAs<- which(is.na(household$Global_active_power))
write.csv(RowsOfNAs, "typeofna.csv")

sum(is.na(household))

####Replacing NA´s of less than 3 hours. THOSE THAT ARE JUST ENERGY CUTS####
household$Global_active_power<- na.locf(household$Global_active_power, na.rm = FALSE, fromLast = FALSE, maxgap = 180)
sum(is.na(household$Global_active_power))#to see how many NA

####Replacing rest of NA´s of less than 3 hours. THOSE THAT ARE JUST ENERGY CUTS####
household$Global_active_power[is.na(household$Global_active_power)]<-0
sum(is.na(household$Global_active_power))

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

####Exclude NAs from the graphs####
na.exclude(household)


####PRE-GRAPHS####
###New Columns###
household$Date <- as.Date(household$Date, "%d/%m/%Y")


###Columns with same Energy Measuring Metrics###
household<-household %>% mutate(Global_ConsumptionKWh=((household$Global_active_power)/60))
household<-household %>% mutate(Global_Consumption_reactiveKWh=((household$Global_reactive_power)/60))
household<-household %>% mutate(Submetter1_kwh=(household$Sub_metering_1/1000))
household<-household %>% mutate(Submetter2_kwh=(household$Sub_metering_2/1000))
household<-household %>% mutate(Submetter3_kwh=(household$Sub_metering_3/1000))

household2<- na.exclude(household)


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

####Cambio de hora#### #there is a function for that#
household$DateTime<- dst(household$DateTime)


####Finding NAs####
OutOfHome<- filter(household, is.na(Global_active_power))
TYPEOFNA<- which(is.na(household$Global_active_power))
write.csv(TYPEOFNA, "typeofna.csv")

sum(is.na(household))

####Replacing NA´s of less than 3 hours. THOSE THAT ARE JUST ENERGY CUTS####
household$Global_active_power<- na.locf(household$Global_active_power, na.rm = FALSE, fromLast = FALSE, maxgap = 180)

####Replacing rest of NA´s of less than 3 hours. THOSE THAT ARE JUST ENERGY CUTS####
is.na(household$Global_active_power)<- 0



####Replacing NAs of several days out with 0####
household[190498:194220,]$Global_active_power<- 0
household[190498:194220,]$Global_reactive_power<- 0
household[190498:194220,]$Global_intensity<- 0
household[190498:194220,]$Voltage<- 0
household[190498:194220,]$Sub_metering_1<- 0
household[190498:194220,]$Sub_metering_2<- 0
household[190498:194220,]$Sub_metering_3<- 0

household[1309389:1312691,]$Global_active_power<- 0
household[1309389:1312691,]$Global_reactive_power<- 0
household[1309389:1312691,]$Global_intensity<- 0
household[1309389:1312691,]$Voltage<- 0
household[1309389:1312691,]$Sub_metering_1<- 0
household[1309389:1312691,]$Sub_metering_2<- 0
household[1309389:1312691,]$Sub_metering_3<- 0

household[1397498:1398387,]$Global_active_power<- 0
household[1397498:1398387,]$Global_reactive_power<- 0
household[1397498:1398387,]$Global_intensity<- 0
household[1397498:1398387,]$Voltage<- 0
household[1397498:1398387,]$Sub_metering_1<- 0
household[1397498:1398387,]$Sub_metering_2<- 0
household[1397498:1398387,]$Sub_metering_3<- 0

household[1616874:1620098,]$Global_active_power<- 0
household[1616874:1620098,]$Global_reactive_power<- 0
household[1616874:1620098,]$Global_intensity<- 0
household[1616874:1620098,]$Voltage<- 0
household[1616874:1620098,]$Sub_metering_1<- 0
household[1616874:1620098,]$Sub_metering_2<- 0
household[1616874:1620098,]$Sub_metering_3<- 0

household[1712790:1714815,]$Global_active_power<- 0
household[1712790:1714815,]$Global_reactive_power<- 0
household[1712790:1714815,]$Global_intensity<- 0
household[1712790:1714815,]$Voltage<- 0
household[1712790:1714815,]$Sub_metering_1<- 0
household[1712790:1714815,]$Sub_metering_2<- 0
household[1712790:1714815,]$Sub_metering_3<- 0

household[1929820:1990189,]$Global_active_power<- 0
household[1929820:1990189,]$Global_reactive_power<- 0
household[1929820:1990189,]$Global_intensity<- 0
household[1929820:1990189,]$Voltage<- 0
household[1929820:1990189,]$Sub_metering_1<- 0
household[1929820:1990189,]$Sub_metering_2<- 0
household[1929820:1990189,]$Sub_metering_3<- 0

sum(is.na(household))


####Replacing NAs with mean####
#https://stackoverflow.com/questions/22916525/replace-na-with-previous-and-next-rows-mean-in-r
ind <- which(is.na(household$Global_active_power))
household$Global_active_power[ind] <- sapply(ind, function(i) with(household, c(Global_active_power[i+106])))
household


household$Global_active_power[ind] <- sapply(ind, function(i) with(household, c(Global_active_power[i+106])))

#### first two terms####
n1 = 0
n2 = 1
count = 1

#### Set the Bucle####
print(n2)
for (i in vector) {
  nth = n1 + n2
  
} { 
  nth = n1 + n2
  return (nth)
  n1 = n2
  n2 = nth
  count = count + 1
}


 sum(is.na(household$Global_active_power))


household[41833,]$Global_active_power

NA2<- which(is.na(household$Global_active_power))
NA2

####SEASON SUBSETS#
household %>% group_by(SeasonWNames) %>% summarise(mean(Global_active_power))
NA2<- which(is.na(household$Global_active_power))
NA2














####Substitution of NAs by mean####

####Month###
household$Mes<-month(household$Date, label= TRUE, abbr = FALSE)
household$Mes
unique(household$Mes)
sum(is.na(household$Mes))
which(is.na(household$Mes))

####Day###
household$Dia<-day(household$Date)
household$Dia
unique(household$Dia)
sum(is.na(household$Dia))

####Creating a new column with the Weekday####
household$DayOfWeek<- wday(household$Date, label = TRUE, abbr = FALSE)
household$DayOfWeek
unique(household$DayOfWeek)
sum(is.na(household$DayOfWeek))

####Creating a new column with the Season####

####Substitution of NAs by mean####
household$Global_active_power[is.na(household$Global_active_power)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Global_reactive_power[is.na(household$Global_reactive_power)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Global_intensity[is.na(household$Global_intensity)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Voltage[is.na(household$Voltage)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Sub_metering_1[is.na(household$Sub_metering_1)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Sub_metering_2[is.na(household$Sub_metering_2)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Sub_metering_3[is.na(household$Sub_metering_3)]<-mean(DatasetName$ColumnName,na.rm = TRUE)


####Substitution of NAs by 0####
household$Global_active_power[is.na(household$Global_active_power)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Global_reactive_power[is.na(household$Global_reactive_power)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Global_intensity[is.na(household$Global_intensity)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Voltage[is.na(household$Voltage)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Sub_metering_1[is.na(household$Sub_metering_1)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Sub_metering_2[is.na(household$Sub_metering_2)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
household$Sub_metering_3[is.na(household$Sub_metering_3)]<-mean(DatasetName$ColumnName,na.rm = TRUE)



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







####Show only hours####
#household$DateTime2<- format(as.POSIXct(household$DateTime,format='%d/%m/%Y %H:%M:%S'),format='%d/%m/%Y %H')
#household$DateTime2
#View(household)

####DO NOT USE####
####Aggregate per hour####
#household2<- household
#household2$Date<- NULL
#household2$Time<- NULL


#str(household2)
#household2<-aggregate(.~DateTime2,household2, FUN = sum)

#ACTIVEPOWER <- aggregate(household$Global_active_power ~ DateTime2, data=household, FUN=sum)
#ACTIVEPOWER

#REACTIVEPOWER <- aggregate(household$Global_reactive_power ~ DateTime2, data=household, FUN=sum)
#REACTIVEPOWER

#SUBMETERING1 <- aggregate(household$Sub_metering_1 ~ DateTime2, data=household, FUN=mean)
#SUBMETERING1

#SUBMETERING2 <- aggregate(household$Sub_metering_2 ~ DateTime2, data=household, FUN=mean)
#SUBMETERING2

#SUBMETERING3 <- aggregate(household$Sub_metering_3 ~ DateTime2, data=household, FUN=mean)
#SUBMETERING3

#VOLT <- aggregate(household$Voltage ~ DateTime2, data=household, FUN=mean)
#VOLT

#INTENS <- aggregate(household$Global_intensity ~ DateTime2, data=household, FUN=mean)
#INTENS

#household2$Global_active_power<- NULL
#household2$Global_reactive_power<- NULL
#household2$Global_intensity<- NULL
#household2$Voltage<- NULL
#household2$Sub_metering_1<- NULL
#household2$Sub_metering_2<- NULL
#household2$Sub_metering_3<- NULL
#household2$Active <- ACTIVEPOWER[,2]
#household2$Reactive <- REACTIVEPOWER[,2]
#household2$Sub1 <- SUBMETERING1[,2]
#household2$Sub2 <- SUBMETERING2[,2]
#household2$Sub3 <- SUBMETERING3[,2]
#household2$Volt <- VOLT[,2]
#household2$Intens <- INTENS[,2]
#household2$Global_active_power<- NULL
#household2$Global_reactive_power<- NULL
#household2$Global_intensity<- NULL
#household2$Voltage<- NULL
#household2$Sub_metering_1<- NULL
#household2$Sub_metering_2<- NULL
#household2$Sub_metering_3<- NULL























####CREATING SUBSETS####
### 1. If it is Day of Week and Season###


####WORK DAYS VS HOLIDAYS#### #Based on NAs#
####Filtering Dates#### #https://blog.exploratory.io/filter-with-date-function-ce8e84be680#
WeekendsOut<-which(household$DiaSemana == 1 | 
                     household$xDiaSemana == 2) 


household<-household[-WeekendsOut,]



household$Season[household$Season==3] <- "Summer"
household$Season[household$Season==4] <- "Fall"