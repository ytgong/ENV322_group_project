library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
library(grid)
library(gridExtra)

#BLUE
BLUESCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/BLUESCdata.csv")

BLUESCdv <- BLUESCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

BLUEdischargedv <- BLUESCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

BLUESC <- full_join(BLUESCdv, BLUEdischargedv, by = "dateTime")

BLUESCplot <- ggplot(BLUESC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Blue Springs SpecCond")

print(BLUESCplot) #POS


#CAL
CALSCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/CALSCdata.csv")

CALSCdv <- CALSCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(UPSTREAM_SpecCond_Inst))

CALdischargedv <- CALSCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

CALSC <- full_join(CALSCdv, CALdischargedv, by = "dateTime")

CALSCplot <- ggplot(CALSC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Caloosahatchee River SpecCond")

print(CALSCplot) #NEG


#CHAS
CHASSCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/CHASSCdata.csv")

CHASSCdv <- CHASSCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

CHASdischargedv <- CHASSCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

CHASSC <- full_join(CHASSCdv, CHASdischargedv, by = "dateTime")

CHASSCplot <- ggplot(CHASSC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Chassahowitzka River SpecCond")

print(CHASSCplot) #NEG


#FAN
FANSCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/FANSCdata.csv")

FANSCdv <- FANSCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

FANdischargedv <- FANSCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

FANSC <- full_join(FANSCdv, FANdischargedv, by = "dateTime")

FANSCplot <- ggplot(FANSC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Fanning Springs SpecCond")

print(FANSCplot) #NEG


#HOM
HOMSCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/HOMSCdata.csv")

HOMSCdv <- HOMSCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

HOMdischargedv <- HOMSCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

HOMSC <- full_join(HOMSCdv, HOMdischargedv, by = "dateTime")

HOMSCplot <- ggplot(HOMSC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Homosassa Springs SpecCond")

print(HOMSCplot) #NEG


#HUN
HUNSCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/HUNSCdata.csv")

HUNSCdv <- HUNSCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

HUNdischargedv <- HUNSCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

HUNSC <- full_join(HUNSCdv, HUNdischargedv, by = "dateTime")

HUNSCplot <- ggplot(HUNSC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Hunter Spring Run SpecCond")

print(HUNSCplot) #NEG


#ICHE
ICHESCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/ICHESCdata.csv")

ICHESCdv <- ICHESCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

ICHEdischargedv <- ICHESCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

ICHESC <- full_join(ICHESCdv, ICHEdischargedv, by = "dateTime")

ICHESCplot <- ggplot(ICHESC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Ichetucknee River SpecCond")

print(ICHESCplot) #NEAR 0


#MAD
MADSCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/MADSCdata.csv")

MADSCdv <- MADSCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

MADdischargedv <- MADSCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

MADSC <- full_join(MADSCdv, MADdischargedv, by = "dateTime")

MADSCplot <- ggplot(MADSC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Madison Blue Spring SpecCond")

print(MADSCplot) #POS


#MAN
MANSCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/MANSCdata.csv")

MANSCdv <- MANSCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

MANdischargedv <- MANSCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

MANSC <- full_join(MANSCdv, MANdischargedv, by = "dateTime")

MANSCplot <- ggplot(MANSC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Manatee Spring SpecCond")

print(MANSCplot) #POS


#SANTA
SANTASCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/SANTASCdata.csv")

SANTASCdv <- SANTASCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

SANTAdischargedv <- SANTASCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

SANTASC <- full_join(SANTASCdv, SANTAdischargedv, by = "dateTime")

SANTASCplot <- ggplot(SANTASC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Santa River SpecCond")

print(SANTASCplot) #NEG


#WAC
WACSCdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Conductance_Sites/WACSCdata.csv")

WACSCdv <- WACSCdata %>%
  group_by(dateTime) %>%
  summarise(SpecCond_DV = mean(SpecCond_Inst))

WACdischargedv <- WACSCdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

WACSC <- full_join(WACSCdv, WACdischargedv, by = "dateTime")

WACSCplot <- ggplot(WACSC, aes(x = Flow_DV, y = SpecCond_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = "") +
  ggtitle("Wacissa River SpecCond")

print(WACSCplot) #NEG



#Combined Plots

PositivePlots <- plot_grid(
  BLUESCplot, MANSCplot, MADSCplot
)

y.grob <- textGrob("Daily Mean Conductance (S/m)", 
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("Daily Mean Discharge (m/s)", 
                   gp=gpar(fontface="bold", fontsize=15))

PositivePlots <- grid.arrange(arrangeGrob(PositivePlots, left = y.grob, bottom = x.grob))
print(PositivePlots)


NegativePlots <- plot_grid(
  SANTASCplot, CALSCplot, CHASSCplot, FANSCplot, HOMSCplot,
  WACSCplot, HUNSCplot, ncol = 3
)

NegativePlots <- grid.arrange(arrangeGrob(NegativePlots, left = y.grob, bottom = x.grob))

print(NegativePlots)
