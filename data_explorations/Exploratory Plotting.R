library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
library(grid)
library(gridExtra)



#Spring
BLUEdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Nitrate_Sites/BLUEdata.csv")
BLUEdata$dateTime <- as.Date(BLUEdata$dateTime)

BLUEplot <- ggplot(BLUEdata, aes(x= dateTime, y = Flow_Inst)) +
  geom_line(color = "blue") +
  labs(x = "", y = expression("Instantaneous Flow (ft"^3*"/s)"))
print(BLUEplot)

BLUENO3plot <- ggplot(BLUEdata, aes(x = dateTime, y = Nitrate_mgl)) +
  geom_line() +
  labs(x = "Date", y = "Instantaneous Nitrate (mg/l)")
print(BLUENO3plot)

BLUECombined <- plot_grid(BLUEplot, BLUENO3plot, ncol = 1)

BLUE.grob <- textGrob("Blue Springs Discharge/Nitrate Over Time",
                     gp=gpar(fontface="bold", fontsize=15))

BLUECombined <- grid.arrange(arrangeGrob(BLUECombined, top = BLUE.grob))


#River
CALdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Nitrate_Sites/CALdata.csv")
CALdata$dateTime <- as.Date(CALdata$dateTime)

CALplot <- ggplot(CALdata, aes(x= dateTime, y = Flow_Inst)) +
  geom_line(color = "blue") +
  labs(x = "", y = expression("Instantaneous Flow (ft"^3*"/s)"))
print(CALplot)

CALNO3plot <- ggplot(CALdata, aes(x = dateTime, y = Nitrate_mgl)) +
  geom_line() +
  labs(x = "Date", y = "Instantaneous Nitrate (mg/l)")
print(CALNO3plot)

CALCombined <- plot_grid(CALplot, CALNO3plot, ncol = 1)

CAL.grob <- textGrob("Caloosahatchee River Discharge/Nitrate Over Time",
                      gp=gpar(fontface="bold", fontsize=15))

CALCombined <- grid.arrange(arrangeGrob(CALCombined, top = CAL.grob))


#Canal
MAINdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Nitrate_Sites/MAINdata.csv")
MAINdata$dateTime <- as.Date(MAINdata$dateTime)

MAINplot <- ggplot(MAINdata, aes(x= dateTime, y = Flow_Inst)) +
  geom_line(color = "blue") +
  labs(x = "", y = expression("Instantaneous Flow (ft"^3*"/s)"))
print(MAINplot)

MAINNO3plot <- ggplot(MAINdata, aes(x = dateTime, y = Nitrate_mgl)) +
  geom_line() +
  labs(x = "Date", y = "Instantaneous Nitrate (mg/l)")
print(MAINNO3plot)

MAINCombined <- plot_grid(MAINplot, MAINNO3plot, ncol = 1)

MAIN.grob <- textGrob("Main Canal Discharge/Nitrate Over Time",
                     gp=gpar(fontface="bold", fontsize=15))

MAINCombined <- grid.arrange(arrangeGrob(MAINCombined, top = MAIN.grob))


#Creek
TURKdata <- read.csv("/ENV322_group_project/Processed_Dataframes/Nitrate_Sites/TURKdata.csv")
TURKdata$dateTime <- as.Date(TURKdata$dateTime)

TURKplot <- ggplot(TURKdata, aes(x= dateTime, y = Flow_Inst)) +
  geom_line(color = "blue") +
  labs(x = "", y = expression("Instantaneous Flow (ft"^3*"/s)"))
print(TURKplot)

TURKNO3plot <- ggplot(TURKdata, aes(x = dateTime, y = Nitrate_mgl)) +
  geom_line() +
  labs(x = "Date", y = "Instantaneous Nitrate (mg/l)")
print(TURKNO3plot)

TURKCombined <- plot_grid(TURKplot, TURKNO3plot, ncol = 1)

TURK.grob <- textGrob("Turkey Creek Discharge/Nitrate Over Time",
                     gp=gpar(fontface="bold", fontsize=15))

TURKCombined <- grid.arrange(arrangeGrob(TURKCombined, top = TURK.grob))
