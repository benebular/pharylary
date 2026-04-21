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


# 1. Extract RT Estimates (on the log-z scale)
plot_data_rt <- emmeans(mod_RT, ~ Condition * CarrierType) %>%
  as.data.frame()

# 2. Extract Accuracy Estimates 
# NOTE: We use 'type = "response"' to convert log-odds back into 0-1 probability (percentages)
plot_data_acc <- emmeans(mod_ACC, ~ Condition * CarrierType, type = "response") %>%
  as.data.frame()

ggplot(plot_data_rt, aes(x = Condition, y = emmean, fill = CarrierType)) +
  # Create the bars, 'stat = "identity"' tells it to use the values in the dataframe
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  # Add the error bars
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Model-Estimated Reaction Time",
       y = "Reaction Time (log-z score)",
       x = "Condition") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggplot(plot_data_acc, aes(x = Condition, y = emmean, fill = CarrierType)) +
  # Create the bars
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  # Add the error bars using your specific column names
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  # Format as percentages
  scale_y_continuous(labels = scales::percent) +
  # Zoom in on the top 20% so you can actually see the differences
  # coord_cartesian(ylim = c(0.8, 1.0)) + 
  labs(title = "Model-Estimated Accuracy",
       y = "Predicted Accuracy (%)",
       x = "Condition") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")

# Assuming your data frame is named 'df'
ggplot(df, aes(x = reaction_time_raw)) +
  geom_histogram(binwidth = 0.5, # Adjust binwidth based on your data scale
                 fill = "steelblue", 
                 color = "white") +
  labs(title = "Distribution of Reaction Times",
       x = "Reaction Time (Raw)",
       y = "Frequency") +
  theme_minimal()