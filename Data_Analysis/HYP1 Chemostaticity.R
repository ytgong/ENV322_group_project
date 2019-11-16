library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)

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
  labs(x = "", y = "") +
  ggtitle("Blue Springs C-Q")
  
print(BLUEChemoplot)

BLUEChemo.lm <- lm(data = BLUEChemo, Nitrate_DV ~ Flow_DV)
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
  labs(x = "", y = "") +
  ggtitle("Rainbow River Near Dunnellon C-Q")

print(BOWChemoplot)

BOWChemo.lm <- lm(data = BOWChemo, Nitrate_DV ~ Flow_DV)
summary(BOWChemo.lm)


#CAL
CALdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CALdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Caloosahatchee River C-Q")

print(CALChemoplot)

CALChemo.lm <- lm(data = CALChemo, Nitrate_DV ~ Flow_DV)
summary(CALChemo.lm)


#CHAS
CHASdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CHASdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Chassahowitzka River C-Q")

print(CHASChemoplot)

CHASChemo.lm <- lm(data = CHASChemo, Nitrate_DV ~ Flow_DV)
summary(CHASChemo.lm)


#CRANE
CRANEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/CRANEdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Crane Creek C-Q")

print(CRANEChemoplot)

CRANEChemo.lm <- lm(data = CRANEChemo, Nitrate_DV ~ Flow_DV)
summary(CRANEChemo.lm)


#DRAIN
DRAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/DRAINdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Drainage Canal C-Q")
  
print(DRAINChemoplot)

DRAINChemo.lm <- lm(data = DRAINChemo, Nitrate_DV ~ Flow_DV)
summary(DRAINChemo.lm)


#FAN
FANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FANdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Fanning Springs C-Q")

print(FANChemoplot)

FANChemo.lm <- lm(data = FANChemo, Nitrate_DV ~ Flow_DV)
summary(FANChemo.lm)


#FELL
FELLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/FELLdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Fellsmere Canal C-Q")

print(FELLChemoplot)

FELLChemo.lm <- lm(data = FELLChemo, Nitrate_DV ~ Flow_DV)
summary(FELLChemo.lm)


#HOL
HOLdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOLdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Blue Hole Spring C-Q")

print(HOLChemoplot)

HOLChemo.lm <- lm(data = HOLChemo, Nitrate_DV ~ Flow_DV)
summary(HOLChemo.lm)


#HOM
HOMdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HOMdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Homosassa Springs C-Q")

print(HOMChemoplot)

HOMChemo.lm <- lm(data = HOMChemo, Nitrate_DV ~ Flow_DV)
summary(HOMChemo.lm)


#HUN
HUNdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/HUNdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Hunter Spring Run C-Q")

print(HUNChemoplot)

HUNChemo.lm <- lm(data = HUNChemo, Nitrate_DV ~ Flow_DV)
summary(HUNChemo.lm)


#ICHE
ICHEdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/ICHEdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Ichetucknee River C-Q")

print(ICHEChemoplot)

ICHEChemo.lm <- lm(data = ICHEChemo, Nitrate_DV ~ Flow_DV)
summary(ICHEChemo.lm)


#MAD
MADdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MADdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Madison Blue Spring C-Q")

print(MADChemoplot)

MADChemo.lm <- lm(data = MADChemo, Nitrate_DV ~ Flow_DV)
summary(MADChemo.lm)


#MAIN
MAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MAINdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Main Canal C-Q")

print(MAINChemoplot)

MAINChemo.lm <- lm(data = MAINChemo, Nitrate_DV ~ Flow_DV)
summary(MAINChemo.lm)


#MAN
MANdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/MANdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Manatee Spring C-Q")

print(MANChemoplot)

MANChemo.lm <- lm(data = MANChemo, Nitrate_DV ~ Flow_DV)
summary(MANChemo.lm)

#RAIN
RAINdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/RAINdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Rainbow River at Dunnellon C-Q")

print(RAINChemoplot)

RAINChemo.lm <- lm(data = RAINChemo, Nitrate_DV ~ Flow_DV)
summary(RAINChemo.lm)


#SANTA
SANTAdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/SANTAdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Santa Fe River C-Q")

print(SANTAChemoplot)

SANTAChemo.lm <- lm(data = SANTAChemo, Nitrate_DV ~ Flow_DV)
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
  labs(x = "", y = "") +
  ggtitle("Three Mile Canal C-Q")

print(THREEChemoplot)

THREEChemo.lm <- lm(data = THREEChemo, Nitrate_DV ~ Flow_DV)
summary(THREEChemo.lm)


#TURK
TURKdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/TURKdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Turkey Creek C-Q")

print(TURKChemoplot)

TURKChemo.lm <- lm(data = TURKChemo, Nitrate_DV ~ Flow_DV)
summary(TURKChemo.lm)


#TURN
TURNdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/TURNdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Turnbull Creek C-Q")

print(TURNChemoplot)

TURNChemo.lm <- lm(data = TURNChemo, Nitrate_DV ~ Flow_DV)
summary(TURNChemo.lm)


#WAC
WACdata <- read.csv("/ENV322_group_project/FL_CSV/hyp1/WACdata.csv")

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
  labs(x = "", y = "") +
  ggtitle("Wacissa River C-Q") 

print(WACChemoplot)

WACChemo.lm <- lm(data = WACChemo, Nitrate_DV ~ Flow_DV)
summary(WACChemo.lm)



ChemostaticPlot <- plot_grid(
  CHASChemoplot, FANChemoplot, RAINChemoplot,  
   TURNChemoplot, WACChemoplot,
  ncol = 3
)
y.grob <- textGrob("Daily Mean Nitrate (mg/l)", 
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("Daily Mean Discharge (m/s)", 
                   gp=gpar(fontface="bold", fontsize=15))
ChemostaticPlot <- grid.arrange(arrangeGrob(ChemostaticPlot, left = y.grob, bottom = x.grob))

print(ChemostaticPlot)

FlushingPlot <- plot_grid(
  MAINChemoplot, FELLChemoplot, CRANEChemoplot, MADChemoplot, 
  THREEChemoplot, CALChemoplot, HUNChemoplot,
  ncol = 3
)
FlushingPlot <- grid.arrange(arrangeGrob(FlushingPlot, left = y.grob, bottom = x.grob))

print(FlushingPlot)

DilutingPlot <- plot_grid(
  ICHEChemoplot, SANTAChemoplot, BLUEChemoplot, HOLChemoplot, DRAINChemoplot, 
  BOWChemoplot,  MANChemoplot, HOMChemoplot, TURKChemoplot,
  ncol = 3
)

DilutingPlot <- grid.arrange(arrangeGrob(DilutingPlot, left = y.grob, bottom = x.grob))
print(DilutingPlot)
