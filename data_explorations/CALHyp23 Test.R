library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)

CAL23data <- read.csv("/ENV322_group_project/FL_CSV/hyp23/CALhyp23.csv")
#2019 only!

#pH
CALpH <- CAL23data %>%
  select(dateTime, Flow_Inst, UPSTREAM_pH_Inst, FLOATING_pH_Inst) %>%
  drop_na() 

CALpHflow <- ggplot(CALpH, aes(dateTime)) +
  geom_point(aes(y = Flow_Inst))
print(CALpHflow)

CALpHupstream <- ggplot(CALpH, aes(x = dateTime)) +
  geom_point(aes(y = UPSTREAM_pH_Inst))
print(CALpHupstream)

CALpHfloating <- ggplot(CALpH, aes(x = dateTime)) +
  geom_point(aes(y = FLOATING_pH_Inst))
print(CALpHfloating)

plot_grid(CALpHfloating, CALpHflow, CALpHupstream, nrow = 3)  

#DO
CALDO <- CAL23data %>%
  select(dateTime, Flow_Inst, UPSTREAM_DO_Inst, FLOATING_DO_Inst) %>%
  drop_na()

CALDOflow <- ggplot(CALDO, aes(dateTime)) +
  geom_point(aes(y = Flow_Inst))
print(CALDOflow)

CALDOupstream <- ggplot(CALDO, aes(dateTime)) +
  geom_point(aes(y = UPSTREAM_DO_Inst))
print(CALDOupstream)

CALDOfloating <- ggplot(CALDO, aes(dateTime)) +
  geom_point(aes(y = FLOATING_DO_Inst))
print(CALDOfloating)

plot_grid(CALDOfloating, CALDOflow, CALDOupstream, nrow = 3)

#Conductance
CALSC <- CAL23data %>%
  select(dateTime, Flow_Inst, UPSTREAM_SpecCond_Inst, FLOATING_SpecCond_Inst) %>%
  drop_na()

CALSCflow <- ggplot(CALSC, aes(dateTime)) +
  geom_point(aes(y = Flow_Inst))
print(CALSCflow)

CALSCupstream <- ggplot(CALSC, aes(dateTime)) +
  geom_point(aes(y = UPSTREAM_SpecCond_Inst))
print(CALSCupstream)

CALSCfloating <- ggplot(CALSC, aes(dateTime)) +
  geom_point(aes(y = FLOATING_SpecCond_Inst))
print(CALSCfloating)

plot_grid(CALSCfloating, CALSCflow, CALSCupstream, nrow = 3)


#FLOATING
plot_grid(CALpHfloating, CALDOfloating, CALSCfloating, nrow = 3)

#UPSTREAM
plot_grid(CALpHupstream, CALDOupstream, CALSCupstream, nrow = 3)


#Regression?
CAL23data1 <- CAL23data %>%
  drop_na()

CAL23upstream.lm <- lm(data = CAL23data1, Flow_Inst ~ UPSTREAM_pH_Inst + UPSTREAM_DO_Inst +
                         UPSTREAM_SpecCond_Inst)
summary(CAL23upstream.lm)
#DO fails the null

CAL23floating.lm <- lm(data = CAL23data1, Flow_Inst ~ FLOATING_pH_Inst + FLOATING_DO_Inst +
                         FLOATING_SpecCond_Inst)
summary(CAL23floating.lm)
#DO fails the null again


#Some plots + regression lines
ggplot(CALDO, aes(x = Flow_Inst, y = UPSTREAM_DO_Inst)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10()

ggplot(CALDO, aes(x = Flow_Inst, y = FLOATING_DO_Inst)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10()
#not much difference between upstream and floating

ggplot(CALpH, aes(x = Flow_Inst, y = UPSTREAM_pH_Inst)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10()

ggplot(CALpH, aes(x = Flow_Inst, y = FLOATING_pH_Inst)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10()
#not much difference between upstream and floating

ggplot(CALSC, aes(x = Flow_Inst, y = UPSTREAM_SpecCond_Inst)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() 
#pos regression

ggplot(CALSC, aes(x = Flow_Inst, y = FLOATING_SpecCond_Inst)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10()
#slighting neg regression!
