library(dataRetrieval)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(xts)
library(dygraphs)
library(lubridate)

#BLUE
BLUEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/BLUEdata.csv")

BLUEdata$dateTime <- ymd_hms(BLUEdata$dateTime)

ggplot(BLUEdata, aes(dateTime)) +
  geom_line(aes(y = Nitrate_mgl, color = "red")) +
  geom_line(aes(y = Flow_Inst, color = "blue"))


BLUEstorm <- BLUEdata %>%
  filter(dateTime > "2015-08-17 02:45:00" & dateTime < "2015-08-24 10:30:00")

BLUEplot <- ggplot(BLUEstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() + 
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2015 Storm Event at Blue Springs") 
print(BLUEplot)


#CAL
#Largest discharge event
CALdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CALdata.csv")

CALdata$dateTime <- ymd_hms(CALdata$dateTime)

CALstorm <- CALdata %>%
  filter(dateTime > "2017-09-09 08:30:00" & dateTime < "2017-09-17 01:45:00")

CALplot <- ggplot(CALstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2017 Storm Event at Caloosahatchee River")
print(CALplot)

#2016-01-27 04:30:00 2016-02-08 03:15:00

#FAN
#Largest discharge event with enough data points
FANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FANdata.csv")

FANdata$dateTime <- ymd_hms(FANdata$dateTime)

FANstorm <- FANdata %>%
  filter(dateTime > "2017-12-09 07:15:00" & dateTime < "2017-12-13 15:45:00")

FANplot <- ggplot(FANstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2017 Storm Event at Fanning Springs")
print(FANplot)


#HOL
#Not usable = Nitrate readings are too discrete
HOLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOLdata.csv")

HOLdata$dateTime <- ymd_hms(HOLdata$dateTime)

dygraph(
  cbind(
    Flow = xts(HOLdata$Flow_Inst, order.by = HOLdata$dateTime), 
    Nitrate = xts(HOLdata$Nitrate_mgl, order.by = HOLdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

HOLstorm <- HOLdata %>%
  filter(dateTime > "2016-12-04 03:00:00" & dateTime < "2016-12-09 22:15:00")

HOLplot <- ggplot(HOLstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(HOLplot)


#MAD
#Largest discharge legible
MADdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MADdata.csv")

MADdata$dateTime <- ymd_hms(MADdata$dateTime)

MADstorm <- MADdata %>%
  filter(dateTime > "2015-11-05 16:30:00" & dateTime < "2015-11-16 24:00:00")

MADplot <- ggplot(MADstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2015 Storm Event at Madison Blue Spring") 
print(MADplot)


#MAN
#Largest discharge event with least chaotic data
MANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MANdata.csv")

MANdata$dateTime <- ymd_hms(MANdata$dateTime)

MANstorm <- MANdata %>%
  filter(dateTime > "2016-02-04 02:00:00" & dateTime < "2016-02-09 06:45:00")

MANplot <- ggplot(MANstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2016 Storm Event at Manatee Spring")
print(MANplot)


#WAC
#Largest discharge event
WACdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/WACdata.csv")

WACdata$dateTime <- ymd_hms(WACdata$dateTime)

WACstorm <- WACdata %>%
  filter(dateTime > "2018-12-02 24:30:00" & dateTime < "2018-12-09 04:30:00")

WACplot <- ggplot(WACstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2018 Storm Event at Wacissa River")
print(WACplot)
