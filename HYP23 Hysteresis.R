library(dataRetrieval)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(EcoHydRology)
library(xts)
library(dygraphs)
library(lubridate)
library(viridis)


BLUEdata


CALdata <- read.csv("/ENV322_group_project/CALdata.csv")

FANdata <- read.csv("/ENV322_group_project/FANdata.csv")



HOLdata <- read.csv("/ENV322_group_project/HOLdata.csv")



MADdata <- read.csv("/ENV322_group_project/MADdata.csv")

MANdata <- read.csv("/ENV322_group_project/MANdata.csv")


WACdata <- read.csv("/ENV322_group_project/WACdata.csv")

#BLUE
BLUEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/BLUEdata.csv")

BLUEdata$dateTime <- ymd_hms(BLUEdata$dateTime)

dygraph(
  cbind(
    Flow = xts(BLUEdata$Flow_Inst, order.by = BLUEdata$dateTime), 
    Nitrate = xts(BLUEdata$Nitrate_mgl, order.by = BLUEdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

BLUEstorm <- BLUEdata %>%
  filter(dateTime > "2015-08-28 01:00:00" & dateTime < "2015-11-05  04:30:00")

BLUEplot <- ggplot(BLUEstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2015 Storm Event at Blue Springs")
print(BLUEplot)


#CAL
CALdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CALdata1.csv")

CALdata$dateTime <- ymd_hms(CALdata$dateTime)

dygraph(
  cbind(
    Flow = xts(CALdata$Flow_Inst, order.by = CALdata$dateTime), 
    Nitrate = xts(CALdata$Nitrate_mgl, order.by = CALdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

CALstorm <- CALdata %>%
  filter(dateTime > "2016-01-27 04:30:00" & dateTime < "2016-02-08 03:15:00")

CALplot <- ggplot(CALstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2016 Storm Event at Caloosahatchee River")
print(CALplot)


#FAN
FANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FANdata1.csv")

FANdata$dateTime <- ymd_hms(FANdata$dateTime)

dygraph(
  cbind(
    Flow = xts(FANdata$Flow_Inst, order.by = FANdata$dateTime), 
    Nitrate = xts(FANdata$Nitrate_mgl, order.by = FANdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

FANstorm <- FANdata %>%
  filter(dateTime > "2017-12-09 07:15:00" & dateTime < "2017-12-12 01:45:00")

FANplot <- ggplot(FANstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2017 Storm Event at Fanning Springs")
print(FANplot)


#HOL
HOLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOLdata1.csv")

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
  filter(dateTime > "" & dateTime < "")

HOLplot <- ggplot(HOLstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(HOLplot)


#MAD
MADdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MADdata1.csv")

MADdata$dateTime <- ymd_hms(MADdata$dateTime)

dygraph(
  cbind(
    Flow = xts(MADdata$Flow_Inst, order.by = MADdata$dateTime), 
    Nitrate = xts(MADdata$Nitrate_mgl, order.by = MADdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

MADstorm <- MADdata %>%
  filter(dateTime > "2015-05-30 04:00:00" & dateTime < "2016-06-05 12:15:00")

MADplot <- ggplot(MADstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2015 Storm Event at Madison Blue Spring") +
  scale_fill_viridis_c(option = "magma")
print(MADplot)


#MAN
MANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MANdata.csv")

MANdata$dateTime <- ymd_hms(MANdata$dateTime)

dygraph(
  cbind(
    Flow = xts(MANdata$Flow_Inst, order.by = MANdata$dateTime), 
    Nitrate = xts(MANdata$Nitrate_mgl, order.by = MANdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

MANstorm <- MANdata %>%
  filter(dateTime > "2016-01-22 07:15:00" & dateTime < "2016-01-29 05:15:00")

MANplot <- ggplot(MANstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(MANplot)


#WAC
WACdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/WACdata1.csv")

WACdata$dateTime <- ymd_hms(WACdata$dateTime)

dygraph(
  cbind(
    Flow = xts(WACdata$Flow_Inst, order.by = WACdata$dateTime), 
    Nitrate = xts(WACdata$Nitrate_mgl, order.by = WACdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

WACstorm <- WACdata %>%
  filter(dateTime > "" & dateTime < "")

WACplot <- ggplot(WACstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(WACplot)