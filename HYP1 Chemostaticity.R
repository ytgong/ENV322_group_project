library(tidyverse)
library(ggplot2)
library(cowplot)

#Blue
BLUEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/BLUEdata.csv")

BLUENitrateDV <- BLUEdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

BLUEFlowDV <- BLUEdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

BLUEChemo <- full_join(BLUENitrateDV, BLUEFlowDV, by = "dateTime")

BLUEChemoplot <- ggplot(BLUEChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Blue Springs Near Dell, FL")
  
print(BLUEChemoplot)

BLUEChemo.lm <- lm(data = BLUEChemo, Flow_DV ~ Nitrate_DV)
summary(BLUEChemo.lm)

#BOW
BOWdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/BOWdata.csv")

BOWNitrateDV <- BOWdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

BOWFlowDV <- BOWdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

BOWChemo <- full_join(BOWNitrateDV, BOWFlowDV, by = "dateTime")

BOWChemoplot <- ggplot(BOWChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Rainbow River Near Dunnellon, FL")

print(BOWChemoplot)

BOWChemo.lm <- lm(data = BOWChemo, Flow_DV ~ Nitrate_DV)
summary(BOWChemo.lm)


#CAL
CALdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CALdata1.csv")

CALNitrateDV <- CALdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

CALFlowDV <- CALdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

CALChemo <- full_join(CALNitrateDV, CALFlowDV, by = "dateTime")

CALChemoplot <- ggplot(CALChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Caloosahatchee River at S-79 Near Olga, FL")

print(CALChemoplot)

CALChemo.lm <- lm(data = CALChemo, Flow_DV ~ Nitrate_DV)
summary(CALChemo.lm)


#CHAS
CHASdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CHASdata1.csv")

CHASNitrateDV <- CHASdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

CHASFlowDV <- CHASdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

CHASChemo <- full_join(CHASNitrateDV, CHASFlowDV, by = "dateTime")

CHASChemoplot <- ggplot(CHASChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Chassahowitzka River Near Homosassa, FL")

print(CHASChemoplot)

CHASChemo.lm <- lm(data = CHASChemo, Flow_DV ~ Nitrate_DV)
summary(CHASChemo.lm)


#CRANE
CRANEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CRANEdata1.csv")

CRANENitrateDV <- CRANEdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

CRANEFlowDV <- CRANEdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

CRANEChemo <- full_join(CRANENitrateDV, CRANEFlowDV, by = "dateTime")

CRANEChemoplot <- ggplot(CRANEChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Crane Creek at Melbourne, FL")

print(CRANEChemoplot)

CRANEChemo.lm <- lm(data = CRANEChemo, Flow_DV ~ Nitrate_DV)
summary(CRANEChemo.lm)


#DRAIN
DRAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/DRAINdata1.csv")

DRAINNitrateDV <- DRAINdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

DRAINFlowDV <- DRAINdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

DRAINChemo <- full_join(DRAINNitrateDV, DRAINFlowDV, by = "dateTime")

DRAINChemoplot <- ggplot(DRAINChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Drainage Canal at Plaza Pkwy at Cocoa, FL")
  
print(DRAINChemoplot)

DRAINChemo.lm <- lm(data = DRAINChemo, Flow_DV ~ Nitrate_DV)
summary(DRAINChemo.lm)


#FAN
FANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FANdata1.csv")

FANNitrateDV <- FANdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

FANFlowDV <- FANdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

FANChemo <- full_join(FANNitrateDV, FANFlowDV, by = "dateTime")

FANChemoplot <- ggplot(FANChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Fanning Springs Near Wilcox, FL")

print(FANChemoplot)

FANChemo.lm <- lm(data = FANChemo, Flow_DV ~ Nitrate_DV)
summary(FANChemo.lm)


#FELL
FELLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FELLdata1.csv")

FELLNitrateDV <- FELLdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

FELLFlowDV <- FELLdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

FELLChemo <- full_join(FELLNitrateDV, FELLFlowDV, by = "dateTime")

FELLChemoplot <- ggplot(FELLChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Fellsmere Canal Near Micco, FL")

print(FELLChemoplot)

FELLChemo.lm <- lm(data = FELLChemo, Flow_DV ~ Nitrate_DV)
summary(FELLChemo.lm)


#HOL
HOLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOLdata1.csv")

HOLNitrateDV <- HOLdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

HOLFlowDV <- HOLdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

HOLChemo <- full_join(HOLNitrateDV, HOLFlowDV, by = "dateTime")

HOLChemoplot <- ggplot(HOLChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Blue Hole Spring Near Hildreth, FL")

print(HOLChemoplot)

HOLChemo.lm <- lm(data = HOLChemo, Flow_DV ~ Nitrate_DV)
summary(HOLChemo.lm)


#HOM
HOMdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOMdata1.csv")

HOMNitrateDV <- HOMdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

HOMFlowDV <- HOMdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

HOMChemo <- full_join(HOMNitrateDV, HOMFlowDV, by = "dateTime")

HOMChemoplot <- ggplot(HOMChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Homosassa Springs at Homosassa Springs, FL")

print(HOMChemoplot)

HOMChemo.lm <- lm(data = HOMChemo, Flow_DV ~ Nitrate_DV)
summary(HOMChemo.lm)


#HUN
HUNdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HUNdata1.csv")

HUNNitrateDV <- HUNdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

HUNFlowDV <- HUNdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

HUNChemo <- full_join(HUNNitrateDV, HUNFlowDV, by = "dateTime")

HUNChemoplot <- ggplot(HUNChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Hunter Spring Run at Beach L at Crystal River, FL")

print(HUNChemoplot)

HUNChemo.lm <- lm(data = HUNChemo, Flow_DV ~ Nitrate_DV)
summary(HUNChemo.lm)


#ICHE
ICHEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/ICHEdata1.csv")

ICHENitrateDV <- ICHEdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

ICHEFlowDV <- ICHEdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

ICHEChemo <- full_join(ICHENitrateDV, ICHEFlowDV, by = "dateTime")

ICHEChemoplot <- ggplot(ICHEChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Ichetucknee River at Hwy27 Near Hildreth, FL")

print(ICHEChemoplot)

ICHEChemo.lm <- lm(data = ICHEChemo, Flow_DV ~ Nitrate_DV)
summary(ICHEChemo.lm)


#MAD
MADdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MADdata1.csv")

MADNitrateDV <- MADdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

MADFlowDV <- MADdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

MADChemo <- full_join(MADNitrateDV, MADFlowDV, by = "dateTime")

MADChemoplot <- ggplot(MADChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Madison Blue Spring near Blue Springs, FL")

print(MADChemoplot)

MADChemo.lm <- lm(data = MADChemo, Flow_DV ~ Nitrate_DV)
summary(MADChemo.lm)


#MAIN
MAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MAINdata1.csv")

MAINNitrateDV <- MAINdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

MAINFlowDV <- MAINdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

MAINChemo <- full_join(MAINNitrateDV, MAINFlowDV, by = "dateTime")

MAINChemoplot <- ggplot(MAINChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Main Canal at Vero Beach, FL")

print(MAINChemoplot)

MAINChemo.lm <- lm(data = MAINChemo, Flow_DV ~ Nitrate_DV)
summary(MAINChemo.lm)


#MAN
MANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MANdata1.csv")

MANNitrateDV <- MANdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

MANFlowDV <- MANdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

MANChemo <- full_join(MANNitrateDV, MANFlowDV, by = "dateTime")

MANChemoplot <- ggplot(MANChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Manatee Spring Near Chiefland, FL")

print(MANChemoplot)

MANChemo.lm <- lm(data = MANChemo, Flow_DV ~ Nitrate_DV)
summary(MANChemo.lm)

#RAIN
RAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/RAINdata1.csv")

RAINNitrateDV <- RAINdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

RAINFlowDV <- RAINdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

RAINChemo <- full_join(RAINNitrateDV, RAINFlowDV, by = "dateTime")

RAINChemoplot <- ggplot(RAINChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Rainbow River at Dunnellon, FL")

print(RAINChemoplot)

RAINChemo.lm <- lm(data = RAINChemo, Flow_DV ~ Nitrate_DV)
summary(RAINChemo.lm)


#SANTA
SANTAdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/SANTAdata1.csv")

SANTANitrateDV <- SANTAdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

SANTAFlowDV <- SANTAdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

SANTAChemo <- full_join(SANTANitrateDV, SANTAFlowDV, by = "dateTime")

SANTAChemoplot <- ggplot(SANTAChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Santa Fe River Near Hildreth, FL")

print(SANTAChemoplot)

SANTAChemo.lm <- lm(data = SANTAChemo, Flow_DV ~ Nitrate_DV)
summary(SANTAChemo.lm)


#THREE
THREEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/THREEdata.csv")

THREENitrateDV <- THREEdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

THREEFlowDV <- THREEdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

THREEChemo <- full_join(THREENitrateDV, THREEFlowDV, by = "dateTime")

THREEChemoplot <- ggplot(THREEChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Three Mile Canal Near Clewiston, FL")

print(THREEChemoplot)

THREEChemo.lm <- lm(data = THREEChemo, Flow_DV ~ Nitrate_DV)
summary(THREEChemo.lm)


#TURK
TURKdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/TURKdata1.csv")

TURKNitrateDV <- TURKdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

TURKFlowDV <- TURKdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

TURKChemo <- full_join(TURKNitrateDV, TURKFlowDV, by = "dateTime")

TURKChemoplot <- ggplot(TURKChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Turkey Creek at Palm Bay, FL")

print(TURKChemoplot)

TURKChemo.lm <- lm(data = TURKChemo, Flow_DV ~ Nitrate_DV)
summary(TURKChemo.lm)


#TURN
TURNdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/TURNdata1.csv")

TURNNitrateDV <- TURNdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

TURNFlowDV <- TURNdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

TURNChemo <- full_join(TURNNitrateDV, TURNFlowDV, by = "dateTime")

TURNChemoplot <- ggplot(TURNChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Turnbull Creek Near Oak Hill, FL")

print(TURNChemoplot)

TURNChemo.lm <- lm(data = TURNChemo, Flow_DV ~ Nitrate_DV)
summary(TURNChemo.lm)


#WAC
WACdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/WACdata1.csv")

WACNitrateDV <- WACdata %>%
  group_by(dateTime) %>%
  summarise(Nitrate_DV = mean(Nitrate_mgl))

WACFlowDV <- WACdata %>%
  group_by(dateTime) %>%
  summarise(Flow_DV = mean(Flow_Inst))

WACChemo <- full_join(WACNitrateDV, WACFlowDV, by = "dateTime")

WACChemoplot <- ggplot(WACChemo, aes(x = Flow_DV, y = Nitrate_DV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Daily Mean Discharge (m/s)", y = "Daily Mean Nitrate (mg/l)") +
  ggtitle("Chemostaticity of Wacissa River Near Wacissa, FL")

print(WACChemoplot)

WACChemo.lm <- lm(data = WACChemo, Flow_DV ~ Nitrate_DV)
summary(WACChemo.lm)
