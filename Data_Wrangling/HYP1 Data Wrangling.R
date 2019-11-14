library(dataRetrieval)
library(tidyverse)


discharge_sites<-whatNWISsites(stateCd="FL", parameterCd="00060", hasDataTypeCd="iv")
conductance_sites<-whatNWISsites(stateCd= "FL", parameterCd="00095", hasDataTypeCd="iv")
nitrate_sites<-whatNWISsites(stateCd= "FL", parameterCd="99133", hasDataTypeCd="iv")
DO_sites<-whatNWISsites(stateCd="FL", parameterCd="00300", hasDataTypeCd="iv")
pH_sites<-whatNWISsites(stateCd="FL", parameterCd="00400", hasDataTypeCd="iv")

Hyp1_Sites <- discharge_sites%>%
  filter(site_no %in% nitrate_sites$site_no)

Hyp23_Sites <- discharge_sites%>%
  filter(site_no %in% DO_sites$site_no)%>%
  filter(site_no %in% conductance_sites$site_no)%>%
  filter(site_no %in% pH_sites$site_no) %>%
  filter(site_no %in% nitrate_sites$site_no)




#Jackson Blue Spring
JACKNO3 <- readNWISuv( 
  site = "02358795", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

JACKdischarge <- readNWISuv( 
  site = "02358795", 
  parameterCd = "00060",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns()
#We need to drop this site - its nitrate measurement and discharge measurement dates do not line up


#WACISSA RIVER
WACNO3 <- readNWISuv( 
  site = "02326526", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

WACdischarge <- readNWISuv( 
  site = "02326526", 
  parameterCd = "00060",
  startDate = "2016-02-17", 
  endDate = "2019-04-11"
) %>% 
  renameNWISColumns

WACdata <- full_join(WACdischarge, WACNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A") #to make sure all of the data points we're looking at are officially approved


#MADISON BLUE SPRING
MADNO3 <- readNWISuv( 
  site = "02319302", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

MADdischarge <- readNWISuv( 
  site = "02319302", 
  parameterCd = "00060",
  startDate = "2014-07-09", 
  endDate = "2016-11-01"
) %>% 
  renameNWISColumns

MADdata <- full_join(MADdischarge, MADNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#BLUE SPRINGS
BLUENO3 <- readNWISuv( 
  site = "02319950", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

BLUEdischarge <- readNWISuv( 
  site = "02319950", 
  parameterCd = "00060",
  startDate = "2015-08-11", 
  endDate = "2018-10-30"
) %>% 
  renameNWISColumns

BLUEdata <- full_join(MADNO3, MADdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#MANATEE SPRINGS
MANNO3 <- readNWISuv( 
  site = "02323566", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

MANdischarge <- readNWISuv( 
  site = "02323566", 
  parameterCd = "00060",
  startDate = "2014-07-09", 
  endDate = "2016-11-01"
) %>% 
  renameNWISColumns

MANdata <- full_join(MANNO3, MANdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#FANNING SPRING
FANNO3 <- readNWISuv( 
  site = "02323502", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

FANdischarge <- readNWISuv( 
  site = "02323502", 
  parameterCd = "00060",
  startDate = "2014-07-03", 
  endDate = "2018-11-15"
) %>% 
  renameNWISColumns

FANdata <- full_join(FANNO3, FANdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#SANTA FE RIVER
SANTANO3 <- readNWISuv( 
  site = "02322800", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

SANTAdischarge <- readNWISuv( 
  site = "02322800", 
  parameterCd = "00060",
  startDate = "2012-09-27", 
  endDate = "2016-10-01"
) %>% 
  renameNWISColumns

SANTAdata <- full_join(SANTAdischarge, SANTANO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#ICHETUCKNEE RIVER
ICHENO3 <- readNWISuv( 
  site = "02322700", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

ICHEdischarge <- readNWISuv( 
  site = "02322700", 
  parameterCd = "00060",
  startDate = "2016-11-15", 
  endDate = "2019-08-19"
) %>% 
  renameNWISColumns

ICHEdata <- full_join(ICHEdischarge, ICHENO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#BLUE HOLE SPRING
#Abbreviated as HOL and not HOLE bc of the inevitable....HOLEdischarge....
HOLNO3 <- readNWISuv( 
  site = "02322688", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

HOLdischarge <- readNWISuv( 
  site = "02322688", 
  parameterCd = "00060",
  startDate = "2014-07-02", 
  endDate = "2019-08-21"
) %>% 
  renameNWISColumns

HOLdata <- full_join(HOLdischarge, HOLNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#HUNTER SPRING RUN
HUNNO3 <- readNWISuv( 
  site = "02310743", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

HUNdischarge <- readNWISuv( 
  site = "02310743", 
  parameterCd = "00060",
  startDate = "2017-01-26", 
  endDate = "2018-10-16"
) %>% 
  renameNWISColumns

HUNdata <- full_join(HUNdischarge, HUNNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#HOMOSASSA SPRINGS
HOMNO3 <- readNWISuv( 
  site = "02310678", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

HOMdischarge <- readNWISuv( 
  site = "02310678", 
  parameterCd = "00060",
  startDate = "2015-09-28", 
  endDate = "2018-10-02"
) %>% 
  renameNWISColumns

HOMdata <- full_join(HOMdischarge, HOMNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#CHASSAHOWITZAKA RIVER
CHASNO3 <- readNWISuv( 
  site = "02310650", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

CHASdischarge <- readNWISuv( 
  site = "02310650", 
  parameterCd = "00060",
  startDate = "2015-09-28", 
  endDate = "2019-06-07"
) %>% 
  renameNWISColumns

CHASdata <- full_join(CHASdischarge, CHASNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#RAINBOW RIVER
RAINNO3 <- readNWISuv( 
  site = "02313100", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

RAINdischarge <- readNWISuv( 
  site = "02313100", 
  parameterCd = "00060",
  startDate = "2015-10-05", 
  endDate = "2019-06-20"
) %>% 
  renameNWISColumns

RAINdata <- full_join(RAINdischarge, RAINNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#RAINBOW RIVER NEAR DUNNELLON
#Should we even do this one? Though it might be interesting to see the differences/similarities in the same river
BOWNO3 <- readNWISuv( 
  site = "02313098", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

BOWdischarge <- readNWISuv( 
  site = "02313098", 
  parameterCd = "00060",
  startDate = "2015-10-08", 
  endDate = "2019-06-20"
) %>% 
  renameNWISColumns

BOWdata <- full_join(BOWdischarge, BOWNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#CALOOSAHATCHEE RIVER
CALNO3 <- readNWISuv( 
  site = "02292900", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

CALdischarge <- readNWISuv( 
  site = "02292900", 
  parameterCd = "00060",
  startDate = "2014-12-23", 
  endDate = "2018-10-11"
) %>% 
  renameNWISColumns

CALdata <- full_join(CALdischarge, CALNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#THREE MILE CANAL
THREENO3 <- readNWISuv( 
  site = "02289035", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

THREEdischarge <- readNWISuv( 
  site = "02289035", 
  parameterCd = "00060",
  startDate = "2013-10-01", 
  endDate = "2019-04-24"
) %>% 
  renameNWISColumns

THREEdata <- full_join(THREEdischarge, THREENO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#TURNBULL CREEK
TURNNO3 <- readNWISuv( 
  site = "02248350", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

TURNdischarge <- readNWISuv( 
  site = "02248350", 
  parameterCd = "00060",
  startDate = "2015-11-25", 
  endDate = "2016-09-16"
) %>% 
  renameNWISColumns

TURNdata <- full_join(TURNdischarge, TURNNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#DRAINAGE CANAL AT PLAZ PKWY
DRAINNO3 <- readNWISuv( 
  site = "02248600", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

DRAINdischarge <- readNWISuv( 
  site = "02248600", 
  parameterCd = "00060",
  startDate = "2014-07-31", 
  endDate = "2016-10-01"
) %>% 
  renameNWISColumns

DRAINdata <- full_join(DRAINdischarge, DRAINNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#CRANE CREEK
CRANENO3 <- readNWISuv( 
  site = "02249500", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

CRANEdischarge  <- readNWISuv( 
  site = "02249500", 
  parameterCd = "00060",
  startDate = "2014-12-18", 
  endDate = "2016-09-13"
) %>% 
  renameNWISColumns

CRANEdata <- full_join(CRANEdischarge, CRANENO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#TURKEY CREEK
TURKNO3 <- readNWISuv( 
  site = "02250030", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

TURKdischarge <- readNWISuv( 
  site = "02250030", 
  parameterCd = "00060",
  startDate = "2014-05-30", 
  endDate = "2016-08-10"
) %>% 
  renameNWISColumns

TURKdata <- full_join(TURKdischarge, TURKNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#FELLSMERE CANAL
FELLNO3 <- readNWISuv( 
  site = "02251767", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

FELLdischarge <- readNWISuv( 
  site = "02251767", 
  parameterCd = "00060",
  startDate = "2015-05-27", 
  endDate = "2016-10-19"
) %>% 
  renameNWISColumns

FELLdata <-full_join(FELLdischarge, FELLNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")


#MAIN CANAL AT VERO BEACH
MAINNO3 <- readNWISuv( 
  site = "02253000", 
  parameterCd = "99133",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns %>%
  rename(Nitrate_mgl = 4)

MAINdischarge <- readNWISuv( 
  site = "02253000", 
  parameterCd = "00060",
  startDate = "2014-09-16", 
  endDate = "2016-10-01"
) %>% 
  renameNWISColumns

MAINdata <- full_join(MAINdischarge, MAINNO3, by = "dateTime") %>%
  drop_na() %>%
  filter(X_99133_Inst_cd == "A" & Flow_Inst_cd == "A")

#BLUEdata
BLUEdata$dateTime <- ymd_hms(BLUEdata$dateTime) 

BLUEdata <- BLUEdata %>% filter(dateTime > "2015-05-11 12:00:00" & dateTime < "2015-12-30 11:15:00")

write.csv(BLUEdata, file = "/ENV322_group_project/FL_CSV/hyp1/BLUEdata.csv")

#Bow
BOWdata$dateTime <- ymd_hms(BOWdata$dateTime)

BOWdata <- BOWdata %>% filter(dateTime > "2018-01-05 03:30:00" & dateTime < "2018-08-31 12:15:00")



write.csv(BOWdata, file = "/ENV322_group_project/FL_CSV/hyp1/BOWdata.csv")

#CALdata
CALdata$dateTime <- ymd_hms(CALdata$dateTime)

CALdata <- CALdata %>% filter(dateTime > "2015-10-31")



write.csv(CALdata, file = "/ENV322_group_project/FL_CSV/hyp1/CALdata.csv")

#CHASdata
CHASdata$dateTime <- ymd_hms(CHASdata$dateTime)

CHASdata <- CHASdata %>% filter(dateTime > "2018-03-13")



write.csv(CHASdata, file = "/ENV322_group_project/FL_CSV/hyp1/CHASdata.csv")

#CRANEdata
CRANEdata$dateTime <- ymd_hms(CRANEdata$dateTime)

CRANEdata <- CRANEdata %>% filter(dateTime < "2015-04-15")



write.csv(CRANEdata, file = "/ENV322_group_project/FL_CSV/hyp1/CRANEdata.csv")

#DRAINdata
DRAINdata$dateTime <- ymd_hms(DRAINdata$dateTime)

DRAINdata <- DRAINdata %>% filter(dateTime > "2015-11-17")



write.csv(DRAINdata, file = "/ENV322_group_project/FL_CSV/hyp1/DRAINdata.csv")

#FANdata
FANdata$dateTime <- ymd_hms(FANdata$dateTime)

FANdata <- FANdata %>% filter(dateTime > "2017-10-27 09:00:00" & dateTime < "2018-05-10 09:45:00")



write.csv(FANdata, file = "/ENV322_group_project/FL_CSV/hyp1/FANdata.csv")

#FELLdata
FELLdata$dateTime <- ymd_hms(FELLdata$dateTime)

FELLdata <- FELLdata %>% filter(dateTime > "2016-07-06")


write.csv(FELLdata, file = "/ENV322_group_project/FL_CSV/hyp1/FELLdata.csv")

#HOLdata
HOLdata$dateTime <- ymd_hms(HOLdata$dateTime)

HOLdata <- HOLdata %>% filter(dateTime > "2016-06-24 12:00:00" & dateTime <"2018-12-16 03:15:00")



write.csv(HOLdata, file = "/ENV322_group_project/FL_CSV/hyp1/HOLdata.csv")

#HOMdata
HOMdata$dateTime <- ymd_hms(HOMdata$dateTime)

HOMdata <- HOMdata %>% filter(dateTime > "2017-10-12 12:15:00")



write.csv(HOMdata, file = "/ENV322_group_project/FL_CSV/hyp1/HOMdata.csv")

#HUNdata
HUNdata$dateTime <- ymd_hms(HUNdata$dateTime)

HUNdata <- HUNdata %>% filter(dateTime < "2018-08-16 03:30:00")



write.csv(HUNdata, file = "/ENV322_group_project/FL_CSV/hyp1/HUNdata.csv")

#ICHEdata
ICHEdata$dateTime <- ymd_hms(ICHEdata$dateTime)

ICHEdata <- ICHEdata %>% filter(dateTime > "2017-09-28 09:15:00" & dateTime < "2018-03-14 10:15:00")



write.csv(ICHEdata, file = "/ENV322_group_project/FL_CSV/hyp1/ICHEdata.csv")

#MADdata
MADdata$dateTime <- ymd_hms(MADdata$dateTime)

MADdata <- MADdata %>% filter(dateTime > "2015-05-11 12:00:00" & dateTime < "2015-12-30 12:15:00")



write.csv(MADdata, file = "/ENV322_group_project/FL_CSV/hyp1/MADdata.csv")

#MAINdata
MAINdata$dateTime <- ymd_hms(MAINdata$dateTime)

MAINdata <- MAINdata %>% filter(dateTime > "2014-10-10 12:00:00" & dateTime < "2015-06-03 11:45:00")



write.csv(MAINdata, file = "/ENV322_group_project/FL_CSV/hyp1/MAINdata.csv")

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


write.csv(MANdata, file = "/ENV322_group_project/FL_CSV/hyp1/MANdata.csv")

#RAINdata
RAINdata$dateTime <- ymd_hms(RAINdata$dateTime)

RAINdata <- RAINdata %>% filter(dateTime > "2016-06-16 04:00:00")



write.csv(RAINdata, file = "/ENV322_group_project/FL_CSV/hyp1/RAINdata.csv")

#SANTAdata
SANTAdata$dateTime <- ymd_hms(SANTAdata$dateTime)

SANTAdata <- SANTAdata %>% filter(dateTime > "2013-07-24 12:00:00" & dateTime < "2014-07-24 11:45:00")



write.csv(SANTAdata, file = "/ENV322_group_project/FL_CSV/hyp1/SANTAdata.csv")

#THREEdata
THREEdata$dateTime <- ymd_hms(THREEdata$dateTime)

THREEdata <- THREEdata %>% filter(dateTime > "2016-01-14 01:15:00" & dateTime < "2017-07-25 12:30:00")



write.csv(THREEdata, file = "/ENV322_group_project/FL_CSV/hyp1/THREEdata.csv")

#TURKdata
TURKdata$dateTime <- ymd_hms(TURKdata$dateTime)

TURKdata <- TURKdata %>% filter(dateTime > "2014-12-02 12:00:00" & dateTime < "2015-07-02 11:45:00")



write.csv(TURKdata, file = "/ENV322_group_project/FL_CSV/hyp1/TURKdata.csv")

#TURNdata
TURNdata$dateTime <- ymd_hms(TURNdata$dateTime)

TURNdata <- TURNdata %>% filter(dateTime > "2015-12-21 10:00:00" & dateTime < "2016-01-24 11:45:00")



write.csv(TURNdata, file = "/ENV322_group_project/FL_CSV/hyp1/TURNdata.csv")

#WACdata
WACdata$dateTime <- ymd_hms(WACdata$dateTime)

WACdata <- WACdata %>% filter(dateTime > "2017-12-04 02:45:00" & dateTime < "2018-12-15 05:15:00")


write.csv(WACdata, file = "/ENV322_group_project/FL_CSV/hyp1/WACdata.csv")


#CSVs
write.csv(BLUEdata, file = "/ENV322_group_project/BLUEdata.csv")
write.csv(BOWdata, file = "/ENV322_group_project/BOWdata.csv")
write.csv(CALdata, file = "/ENV322_group_project/CALdata.csv")
write.csv(CHASdata, file = "/ENV322_group_project/CHASdata.csv")
write.csv(CRANEdata, file = "/ENV322_group_project/CRANEdata.csv")
write.csv(DRAINdata, file = "/ENV322_group_project/DRAINdata.csv")
write.csv(FANdata, file = "/ENV322_group_project/FANdata.csv")
write.csv(FELLdata, file = "/ENV322_group_project/FELLdata.csv")
write.csv(HOLdata, file = "/ENV322_group_project/HOLdata.csv")
write.csv(HOMdata, file = "/ENV322_group_project/HOMdata.csv")
write.csv(HUNdata, file = "/ENV322_group_project/HUNdata.csv")
write.csv(ICHEdata, file = "/ENV322_group_project/ICHEdata.csv")
write.csv(MADdata, file = "/ENV322_group_project/MADdata.csv")
write.csv(MAINdata, file = "/ENV322_group_project/MAINdata.csv")
write.csv(MANdata, file = "/ENV322_group_project/MANdata.csv")
write.csv(RAINdata, file = "/ENV322_group_project/RAINdata.csv")
write.csv(SANTAdata, file = "/ENV322_group_project/SANTAdata.csv")
write.csv(THREEdata, file = "/ENV322_group_project/THREEdata.csv")
write.csv(TURKdata, file = "/ENV322_group_project/TURKdata.csv")
write.csv(TURNdata, file = "/ENV322_group_project/TURNdata.csv")
write.csv(WACdata, file = "/ENV322_group_project/WACdata.csv")

