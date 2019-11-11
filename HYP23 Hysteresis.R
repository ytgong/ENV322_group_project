library(dataRetrieval)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(EcoHydRology)
library(xts)
library(dygraphs)
library(lubridate)


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


#BOW
BOWdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/BOWdata.csv")

BOWdata$dateTime <- ymd_hms(BOWdata$dateTime)

BOWdata <- BOWdata %>% drop_na()

dygraph(
  cbind(
    Flow = xts(BOWdata$Flow_Inst, order.by = BOWdata$dateTime), 
    Nitrate = xts(BOWdata$Nitrate_mgl, order.by = BOWdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

BOWstorm <- BOWdata %>%
  filter(dateTime > "2018-03-19 06:00:00" & dateTime < "2018-03-21 03:00:00")

BOWplot <- ggplot(BOWstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2018 Storm Event at Rainbow River Near Dunnellon")
print(BOWplot)


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


#CHAS
CHASdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CHASdata1.csv")

CHASdata$dateTime <- ymd_hms(CHASdata$dateTime)

dygraph(
  cbind(
    Flow = xts(CHASdata$Flow_Inst, order.by = CHASdata$dateTime), 
    Nitrate = xts(CHASdata$Nitrate_mgl, order.by = CHASdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

CHASstorm <- CHASdata %>%
  filter(dateTime > "" & dateTime < "")

CHASplot <- ggplot(CHASstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(CHASplot)


#CRANE
CRANEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CRANEdata1.csv")

CRANEdata$dateTime <- ymd_hms(CRANEdata$dateTime)

dygraph(
  cbind(
    Flow = xts(CRANEdata$Flow_Inst, order.by = CRANEdata$dateTime), 
    Nitrate = xts(CRANEdata$Nitrate_mgl, order.by = CRANEdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

CRANEstorm <- CRANEdata %>%
  filter(dateTime > "2015-01-12 03:30:00" & dateTime < "2015-01-25 05:00:00")

CRANEplot <- ggplot(CRANEstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2015 Storm Event at Crane Creek")
print(CRANEplot)


#DRAIN
DRAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/DRAINdata1.csv")

DRAINdata$dateTime <- ymd_hms(DRAINdata$dateTime)

dygraph(
  cbind(
    Flow = xts(DRAINdata$Flow_Inst, order.by = DRAINdata$dateTime), 
    Nitrate = xts(DRAINdata$Nitrate_mgl, order.by = DRAINdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

DRAINstorm <- DRAINdata %>%
  filter(dateTime > "2016-03-24 01:30:00" & dateTime < "2016-03-29 04:30:00")

DRAINplot <- ggplot(DRAINstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2016 Storm Event at Drainage Canal")
print(DRAINplot)


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


#FELL
FELLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FELLdata1.csv")

FELLdata$dateTime <- ymd_hms(FELLdata$dateTime)

dygraph(
  cbind(
    Flow = xts(FELLdata$Flow_Inst, order.by = FELLdata$dateTime), 
    Nitrate = xts(FELLdata$Nitrate_mgl, order.by = FELLdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

FELLstorm <- FELLdata %>%
  filter(dateTime > "2016-08-07 06:30:00" & dateTime < "2016-08-23 04:30:00")

FELLplot <- ggplot(FELLstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2016 Storm Event at Fellsmere Canal")
print(FELLplot)


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


#HOM
HOMdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOMdata1.csv")

HOMdata$dateTime <- ymd_hms(HOMdata$dateTime)

dygraph(
  cbind(
    Flow = xts(HOMdata$Flow_Inst, order.by = HOMdata$dateTime), 
    Nitrate = xts(HOMdata$Nitrate_mgl, order.by = HOMdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

HOMstorm <- HOMdata %>%
  filter(dateTime > "2017-10-29 06:00:00" & dateTime < "2017-11-06 06:15:00")

HOMplot <- ggplot(HOMstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(HOMplot)


#HUN
HUNdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HUNdata1.csv")

HUNdata$dateTime <- ymd_hms(HUNdata$dateTime)

dygraph(
  cbind(
    Flow = xts(HUNdata$Flow_Inst, order.by = HUNdata$dateTime), 
    Nitrate = xts(HUNdata$Nitrate_mgl, order.by = HUNdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

HUNstorm <- HUNdata %>%
  filter(dateTime > "" & dateTime < "")

HUNplot <- ggplot(HUNstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(HUNplot)


#ICHE
ICHEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/ICHEdata1.csv")

ICHEdata$dateTime <- ymd_hms(ICHEdata$dateTime)

dygraph(
  cbind(
    Flow = xts(ICHEdata$Flow_Inst, order.by = ICHEdata$dateTime), 
    Nitrate = xts(ICHEdata$Nitrate_mgl, order.by = ICHEdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

ICHEstorm <- ICHEdata %>%
  filter(dateTime > "" & dateTime < "")

ICHEplot <- ggplot(ICHEstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(ICHEplot)


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
  ggtitle("2015 Storm Event at Madison Blue Spring")
print(MADplot)


#MAIN
MAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MAINdata1.csv")

MAINdata$dateTime <- ymd_hms(MAINdata$dateTime)

dygraph(
  cbind(
    Flow = xts(MAINdata$Flow_Inst, order.by = MAINdata$dateTime), 
    Nitrate = xts(MAINdata$Nitrate_mgl, order.by = MAINdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

MAINstorm <- MAINdata %>%
  filter(dateTime > "2014-10-20 11:30:00" & dateTime < "2014-11-04 01:15:00")

MAINplot <- ggplot(MAINstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("2014 Storm Event at Main Canal")
print(MAINplot)


#MAN
MANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MANdata1.csv")

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


#RAIN
RAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/RAINdata1.csv")

RAINdata$dateTime <- ymd_hms(RAINdata$dateTime)

dygraph(
  cbind(
    Flow = xts(RAINdata$Flow_Inst, order.by = RAINdata$dateTime), 
    Nitrate = xts(RAINdata$Nitrate_mgl, order.by = RAINdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

RAINstorm <- RAINdata %>%
  filter(dateTime > "" & dateTime < "")

RAINplot <- ggplot(RAINstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(RAINplot)


#SANTA
SANTAdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/SANTAdata1.csv")

SANTAdata$dateTime <- ymd_hms(SANTAdata$dateTime)

dygraph(
  cbind(
    Flow = xts(SANTAdata$Flow_Inst, order.by = SANTAdata$dateTime), 
    Nitrate = xts(SANTAdata$Nitrate_mgl, order.by = SANTAdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

SANTAstorm <- SANTAdata %>%
  filter(dateTime > "" & dateTime < "")

SANTAplot <- ggplot(SANTAstorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() +
  labs(x = "Instantaneous Discharge (m/s)", y = "Nitrate Concentration (mg/l)", color = "Date") +
  ggtitle("")
print(SANTAplot)