## Modeling for Laryperc and Pharyperc Behavioral (Arabic speakers)
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
orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/2/pharyperc_events_behav_merged_allsubs.csv')

orig_data = read.csv(orig_data_path)
df <- orig_data

# df <- df %>%
#   dplyr::filter(subject != "nbl_063")

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
    x = "Reaction time (log units)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(df_no_outliers, aes(x = reaction_time_log_z)) +
  geom_density(fill = "gray70", color = "black", alpha = 0.6) +
  labs(
    title = "Density of raw reaction times (±2 SD trimmed)",
    x = "Reaction time (log z-scored units)",
    y = "Density"
  ) +
  theme_minimal()


### models
# Arabic dataset: preserve only TrialType == "test" (instead of dropping "control")
df_no_outliers <- df_no_outliers %>%
  filter(TrialType == "test")

df_no_outliers$TargetSegment <- stringr::str_replace_all(df_no_outliers$TargetSegment, "\u0294", "gs-phon")
df_no_outliers$TargetSegment[df_no_outliers$TargetSegment == "gs"] <- "gs-allo"
df_no_outliers$accuracy_num <- as.integer(df_no_outliers$accuracy_num)
df_no_outliers$pair <- as.factor(df_no_outliers$pair)

# Arabic dataset: Condition -> TargetSegment
df_no_outliers$TargetSegment <- as.factor(df_no_outliers$TargetSegment)

df_no_outliers$CarrierType <- as.factor(df_no_outliers$CarrierType)

vars_needed <- c("accuracy_num","reaction_time_log_z", "TargetSegment", "CarrierType", "pair")
ok <- complete.cases(df_no_outliers[, vars_needed]) &
  is.finite(df_no_outliers$reaction_time_log_z)

df_prepared <- df_no_outliers[ok, ]

# simple lm
# mod_RT_lm <- lm(
#   formula = reaction_time_log_z ~
#     TargetSegment + CarrierType + TargetSegment*CarrierType,
#   data = df_prepared
# )
# summary(mod_RT_lm)
#
# emms_RT_lm <- emmeans(mod_RT_lm, ~ TargetSegment*CarrierType)
#
# sorted_res_RT_lm <- pairs(emms_RT_lm) %>%
#   as.data.frame() %>%
#   arrange(p.value)
# print(sorted_res_RT_lm)

# mixed effects
mod_RT <- lmer(
  formula = reaction_time_log_z ~
    TargetSegment*CarrierType +
    (1|pair),
  data = df_prepared
)
summary(mod_RT)

# emms_RT <- emmeans(mod_RT, ~ TargetSegment*CarrierType)
# pairs(emms_RT)

emms_RT <- emmeans(
  mod_RT, ~ TargetSegment * CarrierType
)
# pairs(emms_RT)

sorted_res_RT <- pairs(emms_RT) %>%
  as.data.frame() %>%
  arrange(p.value)
print(sorted_res_RT)



# accuracy logistic 
mod_ACC <- glmer(
  accuracy_num ~ TargetSegment * CarrierType + (1 | pair) + (1|subject),
  data = df_prepared,
  family = binomial(link = "logit")
)

# mod_ACC <- lmer(
#   formula = accuracy_num ~
#     TargetSegment + CarrierType + TargetSegment*CarrierType +
#     (1|pair),
#   data = df
# )
summary(mod_ACC)

emms_ACC <- emmeans(
  mod_ACC, ~ TargetSegment*CarrierType
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

ggplot(plot_data_rt, aes(x = TargetSegment, y = emmean, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Model-Estimated Reaction Time",
       y = "Model-Estimates (Reaction Time (log-z score))",
       x = "TargetSegment") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


ggplot(plot_data_acc, aes(x = TargetSegment, y = emmean, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Model-Estimated Accuracy",
       y = "Model-Estimates (Accuracy (%))",
       x = "TargetSegment") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# palettes
library(wesanderson)
pal1 <- wes_palette("Zissou1", 2)
pal2 <- wes_palette("Darjeeling1", 2)
pal3 <- wes_palette("FantasticFox1", 2)
pal4 <- wes_palette("FrenchDispatch", 2)
pal5 <- wes_palette("AsteroidCity1", 2)

# forest plots

library(dplyr)
library(stringr)
library(ggplot2)

# Function to standardize CI columns and clean up phonetic labels
clean_contrasts <- function(d) {
  d %>%
    # 1. Standardize Confidence Interval column names
    rename(low = any_of(c("lower.CL", "asymp.LCL")),
           high = any_of(c("upper.CL", "asymp.UCL"))) %>%
    # 2. Update phonetic labels
    mutate(
      # Replace 'gs' with glottal stop symbol
      contrast = str_replace_all(contrast, "gs", "[ʔ]"),
      
      # Replace 't' ONLY when it is a standalone word (avoids breaking 'noncreaky')
      contrast = str_replace_all(contrast, "\\bt\\b", "[t]"),
      
      # Replace 'none' with the specific 'No /t/' label
      contrast = str_replace_all(contrast, "none", "No /t/"),
      
      # Logic for Significance coloring based on p-value
      sig = ifelse(p.value < 0.05, "Significant", "Not Significant"),
      sig = factor(sig, levels = c("Not Significant", "Significant"))
    )
}

# 3. Process the model outputs
# For RT (Linear scale)
rt_plot_data <- pairs(emms_RT, infer = TRUE) %>% 
  as.data.frame() %>% 
  clean_contrasts()

# For Accuracy (Odds Ratio scale)
acc_plot_data <- pairs(emms_ACC, infer = TRUE, type = "response") %>% 
  as.data.frame() %>% 
  clean_contrasts()

# 4. Define colors using your pal2
# pal2[1] = Not Significant (Gray/Base), pal2[2] = Significant (Highlight)
# plot_colors <- c("Not Significant" = pal2[1], "Significant" = pal2[2])
# plot_colors <- c("Not Significant" = pal3[2], "Significant" = pal3[1])
# plot_colors <- c("Not Significant" = pal4[1], "Significant" = pal4[2])
# plot_colors <- c("Not Significant" = pal5[2], "Significant" = pal5[1])
plot_colors <- c("Not Significant" = "#d01c8b", "Significant" = "#4dac26")


# 2. IMPORTANT: Update factor levels so 'Significant' comes first in the legend
rt_plot_data$sig <- factor(rt_plot_data$sig, levels = c("Significant", "Not Significant"))
acc_plot_data$sig <- factor(acc_plot_data$sig, levels = c("Significant", "Not Significant"))

# 3. Update Forest RT
forest_RT <- ggplot(rt_plot_data, aes(x = estimate, y = reorder(contrast, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = low, xmax = high, color = sig), height = 0.2) +
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_color_manual(values = plot_colors) +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 17)) +
  labs(title = "Pairwise Contrasts: Reaction Time",
       subtitle = "Estimates (Log-Z) with 95% Confidence Intervals",
       x = "Estimated Difference (Log-Z)",
       y = NULL,
       color = "p < 0.05",
       shape = "p < 0.05") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12))

# 4. Update Forest Accuracy
forest_acc <- ggplot(acc_plot_data, aes(x = odds.ratio, y = reorder(contrast, odds.ratio))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = low, xmax = high, color = sig), height = 0.2) +
  geom_point(aes(color = sig, shape = sig), size = 3) +
  scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 5)) +
  scale_color_manual(values = plot_colors) +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 17)) +
  labs(title = "Pairwise Contrasts: Accuracy",
       subtitle = "Odds Ratios with 95% Confidence Intervals",
       x = "Odds Ratio (Log Scale)",
       y = NULL,
       color = "p < 0.05",
       shape = "p < 0.05") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12))

# raw

# 1. Calculate the summaries from df_prepared
df_sum <- df_prepared %>%
  group_by(TargetSegment, CarrierType) %>%
  summarise(
    mean_rt = mean(reaction_time_log_z, na.rm = TRUE),
    sd_rt   = sd(reaction_time_log_z, na.rm = TRUE),
    n       = sum(!is.na(reaction_time_log_z)),
    SE      = sd_rt / sqrt(n),
    .groups = "drop"
  )

acc_sum <- df_prepared %>%
  group_by(TargetSegment, CarrierType) %>%
  summarise(
    p = mean(accuracy_num, na.rm = TRUE),
    n = sum(!is.na(accuracy_num)),
    SE = sqrt(p * (1 - p) / n),
    .groups = "drop"
  )

# Define a function to recode consistently for both data frames
recode_data <- function(d) {
  d %>%
    mutate(
      # 1. Rename and reorder Conditions
      TargetSegment = factor(TargetSegment, 
                             levels = c("t", "k", "gs-phon", "gs-allo", "q"),
                             labels = c("[t]", "[k]", "/ʔ/", "[ʔ]", "[q]")),
      # 2. Rename CarrierType (Yes/No for Creaky?)
      CarrierType = factor(CarrierType, 
                           levels = c("creaky", "non-creaky"),
                           labels = c("Yes", "No"))
    )
}

df_sum_plot <- recode_data(df_sum)
acc_sum_plot <- recode_data(acc_sum)

raw_RT_plot <- ggplot(df_sum_plot, aes(x = TargetSegment, y = mean_rt, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_rt - SE, ymax = mean_rt + SE),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Reaction Time",
       y = "Reaction Time (log, z-scored)",
       x = "TargetSegment",
       fill = "Creaky?") +
  theme_minimal() +
  # Use manual scale here:
  scale_fill_manual(values = pal5) +
  theme(legend.position = "bottom")  +
  theme(
    # Axis Titles (labels like "TargetSegment", "Reaction Time")
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    
    # Axis Text (tick labels like "[t]", "[ʔ]", "0.5", "1.0")
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12))

raw_acc_plot <- ggplot(acc_sum_plot, aes(x = TargetSegment, y = p, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = p - SE, ymax = p + SE),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Accuracy",
       y = "Accuracy (%)",
       x = "TargetSegment",
       fill = "Creaky?") +
  theme_minimal() +
  # Use manual scale here:
  scale_fill_manual(values = pal5) +
  theme(legend.position = "bottom")  +
  theme(
    # Axis Titles (labels like "TargetSegment", "Reaction Time")
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    
    # Axis Text (tick labels like "[t]", "[ʔ]", "0.5", "1.0")
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12))

library(patchwork)

# --- RT Pair ---
(raw_RT_plot + forest_RT) + 
  plot_annotation(
    tag_levels = 'a' # Automatically assigns 'a' to the first plot and 'b' to the second
  ) +
  plot_layout(widths = c(1, 1.2)) & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    # Styling the 'a' and 'b' tags
    plot.tag = element_text(size = 18, face = "bold"), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 11)
  )

# --- Accuracy Pair ---
(raw_acc_plot + forest_acc) + 
  plot_annotation(
    tag_levels = 'a'
  ) +
  plot_layout(widths = c(1, 1.2)) & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    plot.tag = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 11)
  )