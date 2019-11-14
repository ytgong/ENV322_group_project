library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)

#CAL
CAL23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/CALhyp23.csv")

CALSC <- CAL23data %>%
  select(dateTime, Flow_Inst, UPSTREAM_SpecCond_Inst, FLOATING_SpecCond_Inst) %>%
  drop_na()
CALDO <- CAL23data %>%
  select(dateTime, Flow_Inst, UPSTREAM_DO_Inst, FLOATING_DO_Inst) %>%
  drop_na()
CALpH <- CAL23data %>%
  select(dateTime, Flow_Inst, UPSTREAM_pH_Inst, FLOATING_pH_Inst) %>%
  drop_na()

CAL23upstreamSC.lm <- lm(data = CALSC, UPSTREAM_SpecCond_Inst ~ Flow_Inst)
summary(CAL23upstreamSC.lm)
#^ significant! (P = 0.000467, Rsq = 0.00475)

CAL23floatingSC.lm <- lm(data = CALSC, FLOATING_SpecCond_Inst ~ Flow_Inst)
summary(CAL23floatingSC.lm)
# ^ not significant! (P = 0.346, Rsq = -4.723e-05)

CAL23upstreamDO.lm <- lm(data = CALDO, UPSTREAM_DO_Inst ~ Flow_Inst)
summary(CAL23upstreamDO.lm)
# significant! (P < 2e-16, Rsq = 0.0442)

CAL23floatingDO.lm <- lm(data = CALDO, FLOATING_DO_Inst ~ Flow_Inst)
summary(CAL23floatingDO.lm)
# significant! (P <2e-16, Rsq = 0.04685)

CAL23upstreampH.lm <- lm(data = CALpH, UPSTREAM_pH_Inst ~ Flow_Inst)
summary(CAL23upstreampH.lm)
# significant! (P < 2e-16, Rsq = 0.03162)

CAL23floatingpH.lm <- lm(data = CALpH, FLOATING_pH_Inst ~ Flow_Inst)
summary(CAL23floatingpH.lm)
# significant! (P < 2e-16, Rsq = 0.03295)


#DELL
DELL23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/DELLhyp23.csv")

DELL23SC.lm <- lm(data = DELL23data, SpecCond_Inst ~ Flow_Inst)
summary(DELL23SC.lm)
#^ significant! (P <2e-16, Rsq = 0.2036)

DELL23DO.lm <- lm(data = DELL23data, DO_Inst ~ Flow_Inst)
summary(DELL23DO.lm)
# significant! (P < 2e-16, Rsq = 0.2168)

DELL23pH.lm <- lm(data = DELL23data, pH_Inst ~ Flow_Inst)
summary(DELL23pH.lm)
# significant! (P < 2e-16, Rsq = 0.1806)


#FAN
FAN23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/FANhyp23.csv")

FAN23SC.lm <- lm(data = FAN23data, SpecCond_Inst ~ Flow_Inst)
summary(FAN23SC.lm)
# significant! (P <2e-16, Rsq = 0.2556)

FAN23DO.lm <- lm(data = FAN23data, DO_Inst ~ Flow_Inst)
summary(FAN23DO.lm)
#significant! (P <2e-16, Rsq = 0.1169)

FAN23pH.lm <- lm(data = FAN23data, pH_Inst ~ Flow_Inst)
summary(FAN23pH.lm)
#significant! (P <2e-16, Rsq = 0.1048)


#HILD
HILD23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/HILDhyp23.csv")

HILD23SC.lm <- lm(data = HILD23data, SpecCond_Inst ~ Flow_Inst)
summary(HILD23SC.lm)
# significant! (P <2e-16, Rsq = 0.05794)

HILD23DO.lm <- lm(data = HILD23data, DO_Inst ~ Flow_Inst)
summary(HILD23DO.lm)
#significant! (P <2e-16, Rsq = 0.1307)

HILD23pH.lm <- lm(data = HILD23data, pH_Inst ~ Flow_Inst)
summary(HILD23pH.lm)
#significant! (P <2e-16, Rsq = 0.235)


#JACK
#error in lm.fit(x, y, offset = offset, singular.ok = singular.ok ,...)
JACK23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/JACKhyp23.csv")

JACK23SC.lm <- lm(data = JACK23data, SpecCond_Inst ~ Flow_Inst)
summary(JACK23SC.lm)
# significant! (P <2e-16, Rsq = 0.1125)

JACK23DO.lm <- lm(data = JACK23data, DO_Inst ~ Flow_Inst)
summary(JACK23DO.lm)
#significant! (P <2e-16, Rsq = 0.009476)

JACK23pH.lm <- lm(data = JACK23data, pH_Inst ~ Flow_Inst)
summary(JACK23pH.lm)
#significant! (P <2e-16, Rsq = 0.04177)


#MAD
MAD23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/MADhyp23.csv")

MAD23SC.lm <- lm(data = MAD23data, SpecCond_Inst ~ Flow_Inst)
summary(MAD23SC.lm)
# significant! (P <2e-16, Rsq = 0.3478)

MAD23DO.lm <- lm(data = MAD23data, DO_Inst ~ Flow_Inst)
summary(MAD23DO.lm)
#significant! (P <2e-16, Rsq = 0.2019)

MAD23pH.lm <- lm(data = MAD23data, pH_Inst ~ Flow_Inst)
summary(MAD23pH.lm)
#significant! (P <2e-16, Rsq = 0.2023)


#MAN
MAN23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/MANhyp23.csv")

MAN23SC.lm <- lm(data = MAN23data, SpecCond_Inst ~ Flow_Inst)
summary(MAN23SC.lm)
# significant! (P = 0.000127, Rsq = 7.606e-05)

MAN23DO.lm <- lm(data = MAN23data, DO_Inst ~ Flow_Inst)
summary(MAN23DO.lm)
#significant! (P <2e-16, Rsq = 0.109)

MAN23pH.lm <- lm(data = MAN23data, pH_Inst ~ Flow_Inst)
summary(MAN23pH.lm)
#significant! (P <2e-16, Rsq = 0.002716)


#WAC
WAC23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/WAChyp23.csv")

WAC23SC.lm <- lm(data = WAC23data, SpecCond_Inst ~ Flow_Inst)
summary(WAC23SC.lm)
# significant! (P <2e-16, Rsq = 0.179)

WAC23DO.lm <- lm(data = WAC23data, DO_Inst ~ Flow_Inst)
summary(WAC23DO.lm)
#significant! (P <2e-16, Rsq = 0.004983)

WAC23pH.lm <- lm(data = WAC23data, pH_Inst ~ Flow_Inst)
summary(WAC23pH.lm)
#significant! (P <2e-16, Rsq = 0.002116)
