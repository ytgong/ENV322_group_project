library(dataRetrieval)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(EcoHydRology)
library(xts)
library(dygraphs)
library(lubridate)

BLUEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/BLUEdata.csv")
BOWdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/BOWdata.csv")
CALdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CALdata.csv")
CHASdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CHASdata.csv")
CRANEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CRANEdata.csv")
DRAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/DRAINdata.csv")
FANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FANdata.csv")
FELLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FELLdata.csv")
HOLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOLdata.csv")
HOMdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOMData.csv")
HUNdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HUNdata.csv")
ICHEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/ICHEdata.csv")
MADdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MADdata.csv")
MAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MAINdata.csv")
MANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MANdata.csv")
RAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/RAINdata.csv")
SANTAdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/SANTAdata.csv")
THREEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/THREEdata.csv")
TURKdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/TURKdata.csv")
TURNdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/TURNdata.csv")
WACdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/WACdata.csv")


#BLUEdata
BLUEdata$dateTime <- ymd_hms(BLUEdata$dateTime) 

BLUEdata <- BLUEdata %>% filter(dateTime > "2015-05-11 12:00:00" & dateTime < "2015-12-30 11:15:00")

dygraph(
  cbind(
    Flow = xts(BLUEdata$Flow_Inst, order.by = BLUEdata$dateTime), 
    Nitrate = xts(BLUEdata$Nitrate_mgl, order.by = BLUEdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

write.csv(BLUEdata, file = "/ENV322_group_project/FL_CSV/hyp1/BLUEdata1.csv")

#Bow
BOWdata$dateTime <- ymd_hms(BOWdata$dateTime)

BOWdata <- BOWdata %>% filter(dateTime > "2018-01-05 03:30:00" & dateTime < "2018-08-31 12:15:00")

dygraph(
  cbind(
    Flow = xts(BOWdata$Flow_Inst, order.by = BOWdata$dateTime), 
    Nitrate = xts(BOWdata$Nitrate_mgl, order.by = BOWdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#CALdata
CALdata$dateTime <- ymd_hms(CALdata$dateTime)

CALdata <- CALdata %>% filter(dateTime > "2015-10-31")

dygraph(
  cbind(
    Flow = xts(CALdata$Flow_Inst, order.by = CALdata$dateTime), 
    Nitrate = xts(CALdata$Nitrate_mgl, order.by = CALdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#CHASdata
CHASdata$dateTime <- ymd_hms(CHASdata$dateTime)

CHASdata <- CHASdata %>% filter(dateTime > "2018-03-13")

dygraph(
  cbind(
    Flow = xts(CHASdata$Flow_Inst, order.by = CHASdata$dateTime), 
    Nitrate = xts(CHASdata$Nitrate_mgl, order.by = CHASdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#CRANEdata
CRANEdata$dateTime <- ymd_hms(CRANEdata$dateTime)

CRANEdata <- CRANEdata %>% filter(dateTime < "2015-04-15")

dygraph(
  cbind(
    Flow = xts(CRANEdata$Flow_Inst, order.by = CRANEdata$dateTime), 
    Nitrate = xts(CRANEdata$Nitrate_mgl, order.by = CRANEdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#DRAINdata
DRAINdata$dateTime <- ymd_hms(DRAINdata$dateTime)

DRAINdata <- DRAINdata %>% filter(dateTime > "2015-11-17")

dygraph(
  cbind(
    Flow = xts(DRAINdata$Flow_Inst, order.by = DRAINdata$dateTime), 
    Nitrate = xts(DRAINdata$Nitrate_mgl, order.by = DRAINdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#FANdata
FANdata$dateTime <- ymd_hms(FANdata$dateTime)

FANdata <- FANdata %>% filter(dateTime > "2017-10-27 09:00:00" & dateTime < "2018-05-10 09:45:00")

dygraph(
  cbind(
    Flow = xts(FANdata$Flow_Inst, order.by = FANdata$dateTime), 
    Nitrate = xts(FANdata$Nitrate_mgl, order.by = FANdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#FELLdata
FELLdata$dateTime <- ymd_hms(FELLdata$dateTime)

FELLdata <- FELLdata %>% filter(dateTime > "2016-07-06")

dygraph(
  cbind(
    Flow = xts(FELLdata$Flow_Inst, order.by = FELLdata$dateTime), 
    Nitrate = xts(FELLdata$Nitrate_mgl, order.by = FELLdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#HOLdata
HOLdata$dateTime <- ymd_hms(HOLdata$dateTime)

HOLdata <- HOLdata %>% filter(dateTime > "2016-06-24 12:00:00" & dateTime <"2018-12-16 03:15:00")

dygraph(
  cbind(
    Flow = xts(HOLdata$Flow_Inst, order.by = HOLdata$dateTime), 
    Nitrate = xts(HOLdata$Nitrate_mgl, order.by = HOLdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#HOMdata
HOMdata$dateTime <- ymd_hms(HOMdata$dateTime)

HOMdata <- HOMdata %>% filter(dateTime > "2017-10-12 12:15:00")

dygraph(
  cbind(
    Flow = xts(HOMdata$Flow_Inst, order.by = HOMdata$dateTime), 
    Nitrate = xts(HOMdata$Nitrate_mgl, order.by = HOMdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#HUNdata
HUNdata$dateTime <- ymd_hms(HUNdata$dateTime)

HUNdata <- HUNdata %>% filter(dateTime < "2018-08-16 03:30:00")

dygraph(
  cbind(
    Flow = xts(HUNdata$Flow_Inst, order.by = HUNdata$dateTime), 
    Nitrate = xts(HUNdata$Nitrate_mgl, order.by = HUNdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#ICHEdata
ICHEdata$dateTime <- ymd_hms(ICHEdata$dateTime)

ICHEdata <- ICHEdata %>% filter(dateTime > "2017-09-28 09:15:00" & dateTime < "2018-03-14 10:15:00")

dygraph(
  cbind(
    Flow = xts(ICHEdata$Flow_Inst, order.by = ICHEdata$dateTime), 
    Nitrate = xts(ICHEdata$Nitrate_mgl, order.by = ICHEdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#MADdata
MADdata$dateTime <- ymd_hms(MADdata$dateTime)

MADdata <- MADdata %>% filter(dateTime > "2015-05-11 12:00:00" & dateTime < "2015-12-30 12:15:00")

dygraph(
  cbind(
    Flow = xts(MADdata$Flow_Inst, order.by = MADdata$dateTime), 
    Nitrate = xts(MADdata$Nitrate_mgl, order.by = MADdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#MAINdata
MAINdata$dateTime <- ymd_hms(MAINdata$dateTime)

MAINdata <- MAINdata %>% filter(dateTime > "2014-10-10 12:00:00" & dateTime < "2015-06-03 11:45:00")

dygraph(
  cbind(
    Flow = xts(MAINdata$Flow_Inst, order.by = MAINdata$dateTime), 
    Nitrate = xts(MAINdata$Nitrate_mgl, order.by = MAINdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#RAINdata
RAINdata$dateTime <- ymd_hms(RAINdata$dateTime)

RAINdata <- RAINdata %>% filter(dateTime > "2016-06-16 04:00:00")

dygraph(
  cbind(
    Flow = xts(RAINdata$Flow_Inst, order.by = RAINdata$dateTime), 
    Nitrate = xts(RAINdata$Nitrate_mgl, order.by = RAINdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#SANTAdata
SANTAdata$dateTime <- ymd_hms(SANTAdata$dateTime)

SANTAdata <- SANTAdata %>% filter(dateTime > "2013-07-24 12:00:00" & dateTime < "2014-07-24 11:45:00")

dygraph(
  cbind(
    Flow = xts(SANTAdata$Flow_Inst, order.by = SANTAdata$dateTime), 
    Nitrate = xts(SANTAdata$Nitrate_mgl, order.by = SANTAdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#THREEdata
THREEdata$dateTime <- ymd_hms(THREEdata$dateTime)

THREEdata <- THREEdata %>% filter(dateTime > "2016-01-14 01:15:00" & dateTime < "2017-07-25 12:30:00")

dygraph(
  cbind(
    Flow = xts(THREEdata$Flow_Inst, order.by = THREEdata$dateTime), 
    Nitrate = xts(THREEdata$Nitrate_mgl, order.by = THREEdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#TURKdata
TURKdata$dateTime <- ymd_hms(TURKdata$dateTime)

TURKdata <- TURKdata %>% filter(dateTime > "2014-12-02 12:00:00" & dateTime < "2015-07-02 11:45:00")

dygraph(
  cbind(
    Flow = xts(TURKdata$Flow_Inst, order.by = TURKdata$dateTime), 
    Nitrate = xts(TURKdata$Nitrate_mgl, order.by = TURKdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#TURNdata
TURNdata$dateTime <- ymd_hms(TURNdata$dateTime)

TURNdata <- TURNdata %>% filter(dateTime > "2015-12-21 10:00:00" & dateTime < "2016-01-24 11:45:00")

dygraph(
  cbind(
    Flow = xts(TURNdata$Flow_Inst, order.by = TURNdata$dateTime), 
    Nitrate = xts(TURNdata$Nitrate_mgl, order.by = TURNdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


#WACdata
WACdata$dateTime <- ymd_hms(WACdata$dateTime)

WACdata <- WACdata %>% filter(dateTime > "2017-12-04 02:45:00" & dateTime < "2018-12-15 05:15:00")

dygraph(
  cbind(
    Flow = xts(WACdata$Flow_Inst, order.by = WACdata$dateTime), 
    Nitrate = xts(WACdata$Nitrate_mgl, order.by = WACdata$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()
