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
  ggtitle("Blue Springs")

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
  ggtitle("Caloosahatchee River")

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
  ggtitle("Chassahowitzka River")

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
  ggtitle("Fanning Springs")

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
  ggtitle("Homosassa Springs")

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
  ggtitle("Hunter Spring Run")

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
  ggtitle("Ichetucknee River")

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
  ggtitle("Madison Blue Spring")

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
  ggtitle("Manatee Spring")

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
  ggtitle("Santa Fe River")

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
  ggtitle("Wacissa River")

print(WACSCplot) #NEG



#Combined Plots



y.grob <- textGrob("Daily Mean Conductance (S/m)", 
                   gp=gpar(fontface="bold", fontsize=12), rot=90)

x.grob <- textGrob("Daily Mean Discharge (ft3/s)", 
                   gp=gpar(fontface="bold", fontsize=12))

positive.grob <- textGrob("Systems with Positive Specific Conductance Slopes",
                          gp=gpar(fontface="bold", fontsize=15))
negative.grob <- textGrob("Systems with Negative Specific Conductance Slopes",
                          gp=gpar(fontface="bold", fontsize=15))

PositivePlots <- plot_grid(
  BLUESCplot, MANSCplot, MADSCplot
)
PositivePlots <- grid.arrange(arrangeGrob(PositivePlots, left = y.grob, bottom = x.grob, top = positive.grob))



NegativePlots <- plot_grid(
  SANTASCplot, CALSCplot, CHASSCplot, FANSCplot, HOMSCplot,
  WACSCplot, HUNSCplot, ncol = 3
)

NegativePlots <- grid.arrange(arrangeGrob(NegativePlots, left = y.grob, bottom = x.grob, top = negative.grob))

