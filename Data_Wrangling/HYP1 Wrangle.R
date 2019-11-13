library(tidyverse)
library(lubridate)

library(dygraphs)
library(xts)

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

write.csv(BLUEdata, file = "/ENV322_group_project/FL_CSV/hyp1/BLUEdata1.csv")

#Bow
BOWdata$dateTime <- ymd_hms(BOWdata$dateTime)

BOWdata <- BOWdata %>% filter(dateTime > "2018-01-05 03:30:00" & dateTime < "2018-08-31 12:15:00")



write.csv(BOWdata, file = "/ENV322_group_project/FL_CSV/hyp1/BOWdata.csv")

#CALdata
CALdata$dateTime <- ymd_hms(CALdata$dateTime)

CALdata <- CALdata %>% filter(dateTime > "2015-10-31")



write.csv(CALdata, file = "/ENV322_group_project/FL_CSV/hyp1/CALdata1.csv")

#CHASdata
CHASdata$dateTime <- ymd_hms(CHASdata$dateTime)

CHASdata <- CHASdata %>% filter(dateTime > "2018-03-13")



write.csv(CHASdata, file = "/ENV322_group_project/FL_CSV/hyp1/CHASdata1.csv")

#CRANEdata
CRANEdata$dateTime <- ymd_hms(CRANEdata$dateTime)

CRANEdata <- CRANEdata %>% filter(dateTime < "2015-04-15")



write.csv(CRANEdata, file = "/ENV322_group_project/FL_CSV/hyp1/CRANEdata1.csv")

#DRAINdata
DRAINdata$dateTime <- ymd_hms(DRAINdata$dateTime)

DRAINdata <- DRAINdata %>% filter(dateTime > "2015-11-17")



write.csv(DRAINdata, file = "/ENV322_group_project/FL_CSV/hyp1/DRAINdata1.csv")

#FANdata
FANdata$dateTime <- ymd_hms(FANdata$dateTime)

FANdata <- FANdata %>% filter(dateTime > "2017-10-27 09:00:00" & dateTime < "2018-05-10 09:45:00")



write.csv(FANdata, file = "/ENV322_group_project/FL_CSV/hyp1/FANdata1.csv")

#FELLdata
FELLdata$dateTime <- ymd_hms(FELLdata$dateTime)

FELLdata <- FELLdata %>% filter(dateTime > "2016-07-06")


write.csv(FELLdata, file = "/ENV322_group_project/FL_CSV/hyp1/FELLdata.csv")

#HOLdata
HOLdata$dateTime <- ymd_hms(HOLdata$dateTime)

HOLdata <- HOLdata %>% filter(dateTime > "2016-06-24 12:00:00" & dateTime <"2018-12-16 03:15:00")



write.csv(HOLdata, file = "/ENV322_group_project/FL_CSV/hyp1/HOLdata1.csv")

#HOMdata
HOMdata$dateTime <- ymd_hms(HOMdata$dateTime)

HOMdata <- HOMdata %>% filter(dateTime > "2017-10-12 12:15:00")



write.csv(HOMdata, file = "/ENV322_group_project/FL_CSV/hyp1/HOMdata1.csv")

#HUNdata
HUNdata$dateTime <- ymd_hms(HUNdata$dateTime)

HUNdata <- HUNdata %>% filter(dateTime < "2018-08-16 03:30:00")



write.csv(HUNdata, file = "/ENV322_group_project/FL_CSV/hyp1/HUNdata1.csv")

#ICHEdata
ICHEdata$dateTime <- ymd_hms(ICHEdata$dateTime)

ICHEdata <- ICHEdata %>% filter(dateTime > "2017-09-28 09:15:00" & dateTime < "2018-03-14 10:15:00")



write.csv(ICHEdata, file = "/ENV322_group_project/FL_CSV/hyp1/ICHEdata1.csv")

#MADdata
MADdata$dateTime <- ymd_hms(MADdata$dateTime)

MADdata <- MADdata %>% filter(dateTime > "2015-05-11 12:00:00" & dateTime < "2015-12-30 12:15:00")



write.csv(MADdata, file = "/ENV322_group_project/FL_CSV/hyp1/MADdata1.csv")

#MAINdata
MAINdata$dateTime <- ymd_hms(MAINdata$dateTime)

MAINdata <- MAINdata %>% filter(dateTime > "2014-10-10 12:00:00" & dateTime < "2015-06-03 11:45:00")



write.csv(MAINdata, file = "/ENV322_group_project/FL_CSV/hyp1/MAINdata1.csv")

#MANdata
MANdata$dateTime <- ymd_hms(MANdata$dateTime)

MANdata <- MANdata %>% filter(dateTime > "2016-01-06 03:15:00")

dygraph(
  cbind(
    Flow = xts(MANdata$Flow_Inst, order.by = MANdata$dateTime),
    Nitrate = xts(MANdata$Nitrate_mgl, order.by = MANdata$dateTime)
  )
) %>%
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()


write.csv(MANdata, file = "/ENV322_group_project/FL_CSV/hyp1/MANdata1.csv")

#RAINdata
RAINdata$dateTime <- ymd_hms(RAINdata$dateTime)

RAINdata <- RAINdata %>% filter(dateTime > "2016-06-16 04:00:00")



write.csv(RAINdata, file = "/ENV322_group_project/FL_CSV/hyp1/RAINdata1.csv")

#SANTAdata
SANTAdata$dateTime <- ymd_hms(SANTAdata$dateTime)

SANTAdata <- SANTAdata %>% filter(dateTime > "2013-07-24 12:00:00" & dateTime < "2014-07-24 11:45:00")



write.csv(SANTAdata, file = "/ENV322_group_project/FL_CSV/hyp1/SANTAdata1.csv")

#THREEdata
THREEdata$dateTime <- ymd_hms(THREEdata$dateTime)

THREEdata <- THREEdata %>% filter(dateTime > "2016-01-14 01:15:00" & dateTime < "2017-07-25 12:30:00")



write.csv(THREEdata, file = "/ENV322_group_project/FL_CSV/hyp1/THREEdata1.csv")

#TURKdata
TURKdata$dateTime <- ymd_hms(TURKdata$dateTime)

TURKdata <- TURKdata %>% filter(dateTime > "2014-12-02 12:00:00" & dateTime < "2015-07-02 11:45:00")



write.csv(TURKdata, file = "/ENV322_group_project/FL_CSV/hyp1/TURKdata1.csv")

#TURNdata
TURNdata$dateTime <- ymd_hms(TURNdata$dateTime)

TURNdata <- TURNdata %>% filter(dateTime > "2015-12-21 10:00:00" & dateTime < "2016-01-24 11:45:00")



write.csv(TURNdata, file = "/ENV322_group_project/FL_CSV/hyp1/TURNdata1.csv")

#WACdata
WACdata$dateTime <- ymd_hms(WACdata$dateTime)

WACdata <- WACdata %>% filter(dateTime > "2017-12-04 02:45:00" & dateTime < "2018-12-15 05:15:00")


write.csv(WACdata, file = "/ENV322_group_project/FL_CSV/hyp1/WACdata1.csv")
