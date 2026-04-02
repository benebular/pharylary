## Modeling for Laryperc and Pharyperc Behavioral
# blang@ucsd.edu

## Modeling for PharyLary
## author: ben lang, blang@ucsd.edu

# library(lmerTest)
library(plyr)
library(dplyr) # this checks for normality
# library(ggpubr) # this plots normality
library(magrittr)
# library(effects)
library(ggplot2)
library(gghalves)
# library(ggsignif)
library(tidyr)
# library(scales)
# library(reshape2)
library(lme4)
library(lmerTest)
# library(mgcv)
library(emmeans)
#library(forcats)
# library(psycho)
# library(janitor)
#library(data.table)
# library(psyphy)
# library("cowplot")
# library("forcats")
# library("optimx")
# library("rlang")
# library("splines")
library("stringr")
library("tidyverse")
library("devtools")
library(grid)
library(gridExtra)
library(scales)

#paths
# orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/2/laryperc_events_behav_merged_allsubs.csv')
orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/2/laryperc_events_behav_merged_allsubs.csv')

orig_data = read.csv(orig_data_path)
df <- orig_data

df <- df %>%
  filter(TrialType != "control")

df <- df %>%
  mutate(Condition = relevel(factor(Condition), ref = "t"))

df <- df %>%
  mutate(CarrierType = relevel(factor(CarrierType), ref = "noncreaky"))

mod_RT <- lmer(
  formula = reaction_time_log_z ~
    Condition + CarrierType + Condition*CarrierType +
    (1|trial_number),
  data = df
)
summary(mod_RT)

emms_RT <- emmeans(mod_RT, ~ Condition*CarrierType)
# pairs(emms_RT)

sorted_res_RT <- pairs(emms_RT) %>%
  as.data.frame() %>%
  arrange(p.value)
print(sorted_res_RT)

mod_ACC <- lmer(
  formula = accuracy_num ~
    Condition + CarrierType + Condition*CarrierType +
    (1|trial_number) + (1|subject),
  data = df
)
summary(mod_ACC)

emms_ACC <- emmeans(mod_ACC, ~ Condition*CarrierType)
# pairs(emms_ACC)

sorted_res_ACC <- pairs(emms_ACC) %>%
  as.data.frame() %>%
  arrange(p.value)
print(sorted_res_ACC)