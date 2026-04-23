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
# orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/2/laryperc_events_behav_merged_allsubs.csv')
orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/2/laryperc_events_behav_merged_allsubs.csv')

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

vars_needed <- c("accuracy_num","reaction_time_log_z", "Condition", "CarrierType", "pair")
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
    Condition*CarrierType +
    (1|pair),
  data = df_prepared
)
summary(mod_RT)

# emms_RT <- emmeans(mod_RT, ~ Condition*CarrierType)
# pairs(emms_RT)

emms_RT <- emmeans(
  mod_RT, ~ Condition * CarrierType
)
# pairs(emms_RT)

sorted_res_RT <- pairs(emms_RT) %>%
  as.data.frame() %>%
  arrange(p.value)
print(sorted_res_RT)



# accuracy logistic 
mod_ACC <- glmer(
  accuracy_num ~ Condition * CarrierType + (1 | pair) + (1|subject),
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

# Define a function to recode consistently for both data frames
recode_data <- function(d) {
  d %>%
    mutate(
      # 1. Rename and reorder Conditions
      Condition = factor(Condition, 
                         levels = c("t", "gs", "none"),
                         labels = c("[t]", "[ʔ]", "No /t/")),
      # 2. Rename CarrierType (Yes/No for Creaky?)
      CarrierType = factor(CarrierType, 
                           levels = c("creaky", "noncreaky"),
                           labels = c("Yes", "No"))
    )
}

df_sum_plot <- recode_data(df_sum)
acc_sum_plot <- recode_data(acc_sum)

library(wesanderson)
pal1 <- wes_palette("Zissou1", 2)
pal2 <- wes_palette("Darjeeling1", 2)
pal3 <- wes_palette("FantasticFox1", 2)
pal4 <- wes_palette("FrenchDispatch", 2)
pal5 <- wes_palette("AsteroidCity1", 2)

ggplot(df_sum_plot, aes(x = Condition, y = mean_rt, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_rt - SE, ymax = mean_rt + SE),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Reaction Time (raw mean ± SE)",
       y = "Reaction Time (log, z-scored)",
       x = "Condition",
       fill = "Creaky?") +
  theme_minimal() +
  # Use manual scale here:
  scale_fill_manual(values = pal4)

ggplot(acc_sum_plot, aes(x = Condition, y = p, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = p - SE, ymax = p + SE),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Accuracy (raw proportion ± SE)",
       y = "Accuracy (%)",
       x = "Condition",
       fill = "Creaky?") +
  theme_minimal() +
  # Use manual scale here:
  scale_fill_manual(values = pal4)


# Function to clean up the contrast labels and fix column names
clean_contrasts <- function(d) {
  d %>%
    # 1. RENAME CI columns safely (handles both lower.CL and asymp.LCL)
    rename(low = any_of(c("lower.CL", "asymp.LCL")),
           high = any_of(c("upper.CL", "asymp.UCL"))) %>%
    # 2. Update labels
    mutate(
      contrast = str_replace_all(contrast, "gs", "[ʔ]"),
      contrast = str_replace_all(contrast, "t", "[t]"),
      contrast = str_replace_all(contrast, "none", "No /t/"),
      # Logic for Significance coloring
      sig = ifelse(p.value < 0.05, "Significant", "Not Significant"),
      sig = factor(sig, levels = c("Not Significant", "Significant"))
    )
}

# Process the data
rt_plot_data <- pairs(emms_RT, infer = TRUE) %>% as.data.frame() %>% clean_contrasts()
acc_plot_data <- pairs(emms_ACC, infer = TRUE, type = "response") %>% as.data.frame() %>% clean_contrasts()

# Map pal2 colors
plot_colors <- c("Not Significant" = pal2[1], "Significant" = pal2[2])

ggplot(rt_plot_data, aes(x = estimate, y = reorder(contrast, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Now using the standardized 'low' and 'high' names
  geom_errorbarh(aes(xmin = low, xmax = high, color = sig), height = 0.2) +
  geom_point(aes(color = sig), size = 2) +
  scale_color_manual(values = plot_colors) +
  labs(title = "RT Pairwise Contrasts",
       x = "Difference in Log-Z Reaction Time",
       y = NULL,
       color = "Significance") +
  theme_minimal()

ggplot(acc_plot_data, aes(x = odds.ratio, y = reorder(contrast, odds.ratio))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  # Now using the standardized 'low' and 'high' names
  geom_errorbarh(aes(xmin = low, xmax = high, color = sig), height = 0.2) +
  geom_point(aes(color = sig), size = 2) +
  scale_x_log10() +
  scale_color_manual(values = plot_colors) +
  labs(title = "Accuracy Pairwise Contrasts",
       x = "Odds Ratio (Log Scale)",
       y = NULL,
       color = "Significance") +
  theme_minimal()

## test

# cond_levels <- c("t", "gs", "none")
# 
# plot_data_rt$Condition <- factor(plot_data_rt$Condition, levels = cond_levels)
# df_sum$Condition       <- factor(df_sum$Condition, levels = cond_levels)
# 
# pairs_df <- pairs(emms_RT) %>% as.data.frame()
# 
# library(dplyr)
# library(stringr)
# 
# pairs_df <- pairs(emms_RT) %>% as.data.frame()
# 
# sig_df <- pairs_df %>%
#   # keep only contrasts that are within a CarrierType (creaky vs creaky, noncreaky vs noncreaky)
#   filter(str_detect(contrast, "creaky") | str_detect(contrast, "noncreaky")) %>%
#   mutate(
#     left  = str_trim(str_split_fixed(contrast, "-", 2)[, 1]),
#     right = str_trim(str_split_fixed(contrast, "-", 2)[, 2]),
#     
#     cond1 = word(left,  1),
#     car1  = word(left,  2),
#     cond2 = word(right, 1),
#     car2  = word(right, 2)
#   ) %>%
#   filter(car1 == car2) %>%              # within same CarrierType
#   mutate(
#     CarrierType = car1,
#     
#     # enforce x-axis ordering (so brackets draw correctly left->right)
#     group1 = factor(cond2, levels = cond_levels),
#     group2 = factor(cond1, levels = cond_levels),
#     
#     # asterisk label only when p < .05; otherwise drop it
#     p.label = ifelse(p.value < 0.05, "*", NA_character_)
#   ) %>%
#   filter(!is.na(p.label)) %>%
#   select(CarrierType, group1, group2, p.value, p.label)
# 
# # base heights per panel (CarrierType)
# base_y_model <- plot_data_rt %>%
#   group_by(CarrierType) %>%
#   summarise(y0 = max(emmean + SE, na.rm = TRUE), .groups = "drop")
# 
# sig_df_model <- sig_df %>%
#   left_join(base_y_model, by = "CarrierType") %>%
#   group_by(CarrierType) %>%
#   arrange(p.value) %>%
#   mutate(y.position = y0 + row_number() * 0.08) %>%   # spacing; adjust if needed
#   ungroup()
# 
# base_y_raw <- df_sum %>%
#   group_by(CarrierType) %>%
#   summarise(y0 = max(mean_rt + SE, na.rm = TRUE), .groups = "drop")
# 
# sig_df_raw <- sig_df %>%
#   left_join(base_y_raw, by = "CarrierType") %>%
#   group_by(CarrierType) %>%
#   arrange(p.value) %>%
#   mutate(y.position = y0 + row_number() * 0.08) %>%   # spacing; adjust if needed
#   ungroup()
# 
# library(ggpubr)
# 
# p_model <- ggplot(plot_data_rt, aes(x = Condition, y = emmean, fill = CarrierType)) +
#   geom_col(alpha = 0.8) +
#   geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
#                 width = 0.2, color = "black") +
#   facet_wrap(~ CarrierType) +
#   ggpubr::stat_pvalue_manual(
#     sig_df_model,
#     label = "p.label",
#     xmin = "group1", xmax = "group2",
#     y.position = "y.position",
#     tip.length = 0.01
#   ) +
#   labs(title = "Model-Estimated Reaction Time",
#        y = "Reaction Time (log-z score)",
#        x = "Condition") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set1") +
#   theme(legend.position = "none")
# 
# p_model
# 
# p_raw <- ggplot(df_sum, aes(x = Condition, y = mean_rt, fill = CarrierType)) +
#   geom_col(alpha = 0.8) +
#   geom_errorbar(aes(ymin = mean_rt - SE, ymax = mean_rt + SE),
#                 width = 0.2, color = "black") +
#   facet_wrap(~ CarrierType) +
#   ggpubr::stat_pvalue_manual(
#     sig_df_raw,
#     label = "p.label",
#     xmin = "group1", xmax = "group2",
#     y.position = "y.position",
#     tip.length = 0.01
#   ) +
#   labs(title = "Reaction Time (raw mean ± SE)",
#        y = "Reaction Time (log-z score)",
#        x = "Condition") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set1") +
#   theme(legend.position = "none")
# 
# p_raw