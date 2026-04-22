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
library(marginaleffects)
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
orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/2/pharyperc_events_behav_merged_allsubs.csv')
# orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/2/pharyperc_events_behav_merged_allsubs.csv')

orig_data = read.csv(orig_data_path)
df <- orig_data

# Standardize pairs by sorting the two words alphabetically within each row
# so target="cat", distractor="dog" and target="dog", distractor="cat" both become "cat-dog"

w <- t(apply(df[c("target_word", "distractor_word")], 1, sort))

df$pair <- paste(w[, 1], w[, 2], sep = "-")

### outlier removal
rt <- df$reaction_time_raw

mu  <- mean(rt, na.rm = TRUE)
sd_ <- sd(rt, na.rm = TRUE)

df_no_outliers <- df %>%
  dplyr::filter(
    !is.na(reaction_time_raw),
    abs(reaction_time_raw - mu) <= 2 * sd_
  )

### data inspection

ggplot(df_no_outliers, aes(x = reaction_time_raw)) +
  geom_density(fill = "gray70", color = "black", alpha = 0.6) +
  labs(
    title = "Density of raw reaction times (±2 SD trimmed)",
    x = "Reaction time (raw units)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(df_no_outliers, aes(x = reaction_time_log)) +
  geom_density(fill = "gray70", color = "black", alpha = 0.6) +
  labs(
    title = "Density of raw reaction times (±2 SD trimmed)",
    x = "Reaction time (raw units)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(df_no_outliers, aes(x = reaction_time_log_z)) +
  geom_density(fill = "gray70", color = "black", alpha = 0.6) +
  labs(
    title = "Density of raw reaction times (±2 SD trimmed)",
    x = "Reaction time (raw units)",
    y = "Density"
  ) +
  theme_minimal()


### models
df_no_outliers <- df_no_outliers %>%
  filter(TrialType != "control")

df_no_outliers$accuracy_num <- as.integer(df_no_outliers$accuracy_num)
df_no_outliers$pair <- as.factor(df_no_outliers$pair)
df_no_outliers$Condition <- as.factor(df_no_outliers$Condition)
df_no_outliers$CarrierType <- as.factor(df_no_outliers$CarrierType)

vars_needed <- c("accuracy_num","reaction_time_log_z", "TargetSegment", "CarrierType", "Pair")
ok <- complete.cases(df_no_outliers[, vars_needed]) &
  is.finite(df_no_outliers$reaction_time_log_z)

df_prepared <- df_no_outliers[ok, ]

# simple lm
# mod_RT_lm <- lm(
#   formula = reaction_time_log_z ~
#     Condition + CarrierType + Condition*CarrierType,
#   data = df_prepared
# )
# summary(mod_RT_lm)
# 
# emms_RT_lm <- emmeans(mod_RT_lm, ~ Condition*CarrierType)
# 
# sorted_res_RT_lm <- pairs(emms_RT_lm) %>%
#   as.data.frame() %>%
#   arrange(p.value)
# print(sorted_res_RT_lm)

# mixed effects
mod_RT <- lmer(
  formula = reaction_time_log_z ~
    TargetSegment + CarrierType + TargetSegment*CarrierType +
    (1|Pair),
  data = df_prepared
)
summary(mod_RT)

# emms_RT <- emmeans(mod_RT, ~ Condition*CarrierType)
# pairs(emms_RT)

emms_RT <- emmeans(
  mod_RT, ~ TargetSegment * CarrierType,
  lmerTest.limit = 21744,
  pbkrtest.limit = 21744
)
# pairs(emms_RT)

sorted_res_RT <- pairs(emms_RT) %>%
  as.data.frame() %>%
  arrange(p.value)
print(sorted_res_RT)



# accuracy logistic 
mod_ACC <- glmer(
  accuracy_num ~ Condition * CarrierType + (1 | pair),
  data = df_prepared,
  family = binomial(link = "logit")
)

# mod_ACC <- lmer(
#   formula = accuracy_num ~
#     Condition + CarrierType + Condition*CarrierType +
#     (1|pair),
#   data = df
# )
summary(mod_ACC)

emms_ACC <- emmeans(
  mod_ACC, ~ Condition*CarrierType
)
# pairs(emms_ACC)

sorted_res_ACC <- pairs(emms_ACC) %>%
  as.data.frame() %>%
  arrange(p.value)
print(sorted_res_ACC)


# 1. Extract RT Estimates (on the log-z scale)
plot_data_rt <- emms_RT %>%
  as.data.frame()

# 2. Extract Accuracy Estimates 
# NOTE: We use 'type = "response"' to convert log-odds back into 0-1 probability (percentages)
plot_data_acc <- emms_ACC %>%
  as.data.frame()

ggplot(plot_data_rt, aes(x = Condition, y = emmean, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Model-Estimated Reaction Time",
       y = "Model-Estimates (Reaction Time (log-z score))",
       x = "Condition") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


ggplot(plot_data_acc, aes(x = Condition, y = emmean, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Model-Estimated Accuracy",
       y = "Model-Estimates (Accuracy (%))",
       x = "Condition") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# raw

df_sum <- df_prepared %>%
  group_by(TargetSegment, CarrierType) %>%
  summarise(
    mean_rt = mean(reaction_time_log_z, na.rm = TRUE),
    sd_rt   = sd(reaction_time_log_z, na.rm = TRUE),
    n       = sum(!is.na(reaction_time_log_z)),
    SE      = sd_rt / sqrt(n),
    .groups = "drop"
  )

ggplot(df_sum, aes(x = TargetSegment, y = mean_rt, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_rt - SE, ymax = mean_rt + SE),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Reaction Time (raw mean ± SE)",
       y = "Reaction Time (log-z score)",
       x = "Condition") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


acc_sum <- df_prepared %>%
  group_by(Condition, CarrierType) %>%
  summarise(
    p = mean(accuracy_num, na.rm = TRUE),
    n = sum(!is.na(accuracy_num)),
    SE = sqrt(p * (1 - p) / n),
    .groups = "drop"
  )

ggplot(acc_sum, aes(x = Condition, y = p, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = p - SE, ymax = p + SE),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Accuracy (raw proportion ± SE)",
       y = "Accuracy (%)",
       x = "Condition") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")