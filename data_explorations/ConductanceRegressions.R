library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
library(dataRetrieval)

discharge_sites<-whatNWISsites(stateCd="FL", parameterCd="00060", hasDataTypeCd="iv")
conductance_sites<-whatNWISsites(stateCd= "FL", parameterCd="00095", hasDataTypeCd="iv")
nitrate_sites<-whatNWISsites(stateCd="FL", parameterCd="99133", hasDataTypeCd="iv")
hyp2_sites<-discharge_sites%>%
  filter(site_no %in% conductance_sites$site_no) %>%
  filter(site_no %in% nitrate_sites$site_no)
hyp2_sites <- hyp2_sites %>%
  select(site_no, station_nm, dec_lat_va, dec_long_va) %>%
  slice(2:13)


write.csv(hyp2_sites, "/ENV322_group_project/Processed_Dataframes/CondNitrateSites.csv")

#BLUE Conductance ~ Discharge
BLUESC <- readNWISuv( 
  site = "02319950", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

BLUEdischarge <- readNWISuv( 
  site = "02319950", 
  parameterCd = "00060",
  startDate = "2015-06-16", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

BLUEdata <- full_join(BLUESC, BLUEdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

BLUESCdata <- BLUEdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(BLUESCdata, "/ENV322_group_project/Processed_Dataframes/BLUESCdata.csv")

BLUESC.lm <- lm(data = BLUEdata, SpecCond_Inst ~ Flow_Inst)
summary(BLUESC.lm)
#Significant! (P<2e-16, Rsq = 0.2976)

#CAL Conductance ~ Discharge
CALSC <- readNWISuv( 
  site = "02292900", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

CALdischarge <- readNWISuv( 
  site = "02292900", 
  parameterCd = "00060",
  startDate = "2014-05-01", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

CALdata <- full_join(CALSC, CALdischarge, by = "dateTime") %>%
  filter(UPSTREAM_SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A") %>%
  select(dateTime, Flow_Inst, UPSTREAM_SpecCond_Inst) %>%
  drop_na()

CALSC.lm <- lm(data = CALdata, UPSTREAM_SpecCond_Inst ~ Flow_Inst)
summary(CALSC.lm)
#Significant! (P<2e-16, Rsq = 0.2671)

write.csv(CALdata, "/ENV322_group_project/Processed_Dataframes/CALSCdata.csv")

#CHAS Conductance ~ Discharge
CHASSC <- readNWISuv( 
  site = "02310650", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

CHASdischarge <- readNWISuv( 
  site = "02310650", 
  parameterCd = "00060",
  startDate = "2015-01-01", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

CHASdata <- full_join(CHASSC, CHASdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

CHASSC.lm <- lm(data = CHASdata, SpecCond_Inst ~ Flow_Inst)
summary(CHASSC.lm)
#Significant! (P<2e-16, Rsq = 0.2585)

CHASSCdata <- CHASdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(CHASSCdata, "/ENV322_group_project/Processed_Dataframes/CHASSCdata.csv")

#FAN
FANSC <- readNWISuv( 
  site = "02323502", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

FANdischarge <- readNWISuv( 
  site = "02323502", 
  parameterCd = "00060",
  startDate = "2014-07-03", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

FANdata <- full_join(FANSC, FANdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

FANSC.lm <- lm(data = FANdata, SpecCond_Inst ~ Flow_Inst)
summary(FANSC.lm)
# significant! (P <2e-16, Rsq = 0.0608)

FANSCdata <- FANdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(FANSCdata, "/ENV322_group_project/Processed_Dataframes/FANSCdata.csv")


#HOM
HOMSC <- readNWISuv( 
  site = "02310678", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

HOMdischarge <- readNWISuv( 
  site = "02310678", 
  parameterCd = "00060",
  startDate = "2015-01-01", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

HOMdata <- full_join(HOMSC, HOMdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

HOMSC.lm <- lm(data = HOMdata, SpecCond_Inst ~ Flow_Inst)
summary(HOMSC.lm)
#Significant! (P<2e-16, Rsq = 0.2095)

HOMSCdata <- HOMdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(HOMSCdata, "/ENV322_group_project/Processed_Dataframes/HOMSCdata.csv")

#HUN
HUNSC <- readNWISuv( 
  site = "02310743", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

HUNdischarge <- readNWISuv( 
  site = "02310743", 
  parameterCd = "00060",
  startDate = "2017-02-01", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

HUNdata <- full_join(HUNSC, HUNdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

HUNSC.lm <- lm(data = HUNdata, SpecCond_Inst ~ Flow_Inst)
summary(HUNSC.lm)
#Significant! (P<2e-16, Rsq = 0.09142)

HUNSCdata <- HUNdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(HUNSCdata, "/ENV322_group_project/Processed_Dataframes/HUNSCdata.csv")

#ICHE
ICHESC <- readNWISuv( 
  site = "02322700", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

ICHEdischarge <- readNWISuv( 
  site = "02322700", 
  parameterCd = "00060",
  startDate = "2017-01-18", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

ICHEdata <- full_join(ICHESC, ICHEdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

ICHESC.lm <- lm(data = ICHEdata, SpecCond_Inst ~ Flow_Inst)
summary(ICHESC.lm)
#Significant! (P = 3.52e-05, Rsq = 0.0004319)

ICHESCdata <- ICHEdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(ICHESCdata, "/ENV322_group_project/Processed_Dataframes/ICHESCdata.csv")

#MAD
MADSC <- readNWISuv( 
  site = "02319302", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

MADdischarge <- readNWISuv( 
  site = "02319302", 
  parameterCd = "00060",
  startDate = "2014-07-09", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

MADdata <- full_join(MADSC, MADdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

MADSC.lm <- lm(data = MADdata, SpecCond_Inst ~ Flow_Inst)
summary(MADSC.lm)
# significant! (P <2e-16, Rsq = 0.282)

MADSCdata <- MADdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(MADSCdata, "/ENV322_group_project/Processed_Dataframes/MADSCdata.csv")

#MAN
MANSC <- readNWISuv( 
  site = "02323566", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

MANdischarge <- readNWISuv( 
  site = "02323566", 
  parameterCd = "00060",
  startDate = "2014-07-02", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

MANdata <- full_join(MANSC, MANdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

MANSC.lm <- lm(data = MANdata, SpecCond_Inst ~ Flow_Inst)
summary(MANSC.lm)
# significant! (P <2e-16, Rsq = 0.03221)

MANSCdata <- MANdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(MANSCdata, "/ENV322_group_project/Processed_Dataframes/MANSCdata.csv")

MAN23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/MANhyp23.csv")

#SANTA
SANTASC <- readNWISuv( 
  site = "02322800", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

SANTAdischarge <- readNWISuv( 
  site = "02322800", 
  parameterCd = "00060",
  startDate = "2015-09-02", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

SANTAdata <- full_join(SANTASC, SANTAdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

SANTASC.lm <- lm(data = SANTAdata, SpecCond_Inst ~ Flow_Inst)
summary(SANTASC.lm)
#Significant! (P<2e-16, Rsq = 0.552)

SANTASCdata <- SANTAdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(SANTASCdata, "/ENV322_group_project/Processed_Dataframes/SANTASCdata.csv")

#WAC
WACSC <- readNWISuv( 
  site = "02326526", 
  parameterCd = "00095",
  startDate = "", 
  endDate = ""
) %>% 
  renameNWISColumns

WACdischarge <- readNWISuv( 
  site = "02326526", 
  parameterCd = "00060",
  startDate = "2016-01-28", 
  endDate = "2019-11-16"
) %>% 
  renameNWISColumns

WACdata <- full_join(WACSC, WACdischarge, by = "dateTime") %>%
  drop_na() %>%
  filter(SpecCond_Inst_cd == "A" & Flow_Inst_cd == "A")

WACSC.lm <- lm(data = WACdata, SpecCond_Inst ~ Flow_Inst)
summary(WACSC.lm)
# significant! (P <2e-16, Rsq = 0.1744)

WACSCdata <- WACdata %>%
  select(dateTime, SpecCond_Inst, Flow_Inst)
write.csv(WACSCdata, "/ENV322_group_project/Processed_Dataframes/WACSCdata.csv")
