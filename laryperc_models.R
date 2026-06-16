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
library(knitr)
library(kableExtra)
# library(lmtest)
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
orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/2/laryperc_events_behav_merged_allsubs.csv')
# orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/2/laryperc_events_behav_merged_allsubs.csv')

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
    x = "Reaction time (log units)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(df_no_outliers, aes(x = reaction_time_log_z)) +
  geom_density(fill = "gray70", color = "black", alpha = 0.6) +
  labs(
    title = "Density of raw reaction times (±2 SD trimmed)",
    x = "Reaction time (log, z-scored units)",
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
  data = df_prepared,
  REML = FALSE
)
summary(mod_RT)

# log likelihood and AIC tests
mod_RT_no_int <- lmer(
  formula = reaction_time_log_z ~
    Condition + CarrierType +
    (1|pair),
  data = df_prepared,
  REML = FALSE
)
summary(mod_RT)

mod_RT_condition_only <- lmer(
  formula = reaction_time_log_z ~
    Condition +
    (1|pair),
  data = df_prepared,
  REML = FALSE
)

mod_RT_carrier_only <- lmer(
  formula = reaction_time_log_z ~
    CarrierType +
    (1|pair),
  data = df_prepared,
  REML = FALSE
)

full_model_rt <- anova(mod_RT, mod_RT_no_int)
condition_only_model_rt <- anova(mod_RT_no_int, mod_RT_condition_only)
carrier_only_model_rt <- anova(mod_RT_no_int, mod_RT_carrier_only)

# --- Table 1: Model summary ---------------------------------------------------
extract_model_table <- function(models, model_names) {
  rows <- mapply(function(m, name) {
    s       <- summary(m)
    fe      <- fixef(m)
    n_fixed <- length(fe)
    vc      <- as.data.frame(VarCorr(m))
    n_rand  <- nrow(vc)
    loglik  <- round(logLik(m)[1], 2)
    aic     <- round(AIC(m), 2)
    nobs    <- nobs(m)
    
    data.frame(
      Model      = name,
      Fixed      = paste(names(fe), collapse = ", "),
      N_Fixed    = n_fixed,
      N_Random   = n_rand,
      LogLik     = loglik,
      AIC        = aic,
      N_Obs      = nobs,
      stringsAsFactors = FALSE
    )
  }, models, model_names, SIMPLIFY = FALSE)
  
  do.call(rbind, rows)
}

model_list  <- list(mod_RT, mod_RT_no_int, mod_RT_condition_only, mod_RT_carrier_only)
model_names <- c("Full ($Segment \\times Carrier Type$)", "Additive ($Segment + Carrier Type$)",
                 "Segment only ($Segment$)", "Carrier Type only ($Carrier Type$)")

tbl1 <- extract_model_table(model_list, model_names)

tbl1_out <- tbl1[, c("Model"), drop = FALSE]

colnames(tbl1_out) <- c("Model")

tbl1_tex <- kable(tbl1_out,
                  format   = "latex",
                  booktabs = TRUE,
                  escape   = FALSE,
                  caption  = "Summary of linear mixed-effects models. All models were fit using maximum likelihood (\\texttt{REML = FALSE}) to allow likelihood-ratio comparison of fixed effects.",
                  label    = "laryperc_models") %>%
  kable_styling(latex_options = c("hold_position"))

# --- Table 2: LRT results -----------------------------------------------------
extract_lrt_row <- function(lrt_obj, comparison_label, effect_label) {
  r      <- as.data.frame(lrt_obj)
  chi_sq <- round(r$Chisq[2], 3)
  df_val <- r$Df[2]                    # was r$`Chi Df`[2]
  p_val  <- r$`Pr(>Chisq)`[2]
  
  p_fmt <- ifelse(p_val < .001, "$< .001$",
                  paste0("$", format(round(p_val, 3), nsmall = 3), "$"))
  
  data.frame(
    Comparison = comparison_label,
    Effect     = effect_label,
    Chi2       = chi_sq,
    Df         = df_val,
    p          = p_fmt,
    stringsAsFactors = FALSE
  )
}

tbl2 <- rbind(
  extract_lrt_row(full_model_rt, "$M_{full}$ vs. $M_{additive}$",      "Interaction ($A \\times B$)"),
  extract_lrt_row(condition_only_model_rt,   "$M_{additive}$ vs. $M_{Segment\\ only}$",  "Main effect of $Carrier Type$"),
  extract_lrt_row(carrier_only_model_rt,   "$M_{additive}$ vs. $M_{Carrier Type\\ only}$",  "Main effect of $Segment$")
)

colnames(tbl2) <- c("Comparison", "Effect tested", "$\\chi^2$", "$df$", "$p$")

tbl2_tex <- kable(tbl2,
                  format   = "latex",
                  booktabs = TRUE,
                  escape   = FALSE,
                  caption  = "Reaction Time: Likelihood-ratio tests for fixed effects in reaction time models. The interaction was tested against the full model; main effects were tested against the additive model.",
                  label    = "laryperc_lrt_rt") %>%
  kable_styling(latex_options = c("hold_position")) # %>%
  # footnote(general       = "$p$-values based on $\\chi^2$ approximation.",
  #          escape        = FALSE)

# --- Save out -----------------------------------------------------------------
# save_kable(tbl1_tex, file = "/Volumes/cassandra/alldata/dissertation/2/tables/model_comp_RT.tex")
# save_kable(tbl2_tex, file = "/Volumes/cassandra/alldata/dissertation/2/tables/model_comp_outputs_RT.tex")

save_kable(tbl1_tex, file = "/Volumes/circe/alldata/dissertation/2/tables/laryperc_model_comp.tex")
save_kable(tbl2_tex, file = "/Volumes/circe/alldata/dissertation/2/tables/model_comp_outputs_RT.tex")

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
summary(mod_ACC)

mod_ACC_no_int <- glmer(
  accuracy_num ~ Condition + CarrierType + (1 | pair) + (1|subject),
  data = df_prepared,
  family = binomial(link = "logit")
)

mod_ACC_condition_only <- glmer(
  accuracy_num ~ Condition + (1 | pair) + (1|subject),
  data = df_prepared,
  family = binomial(link = "logit")
)

mod_ACC_carrier_only <- glmer(
  accuracy_num ~ CarrierType + (1 | pair) + (1|subject),
  data = df_prepared,
  family = binomial(link = "logit")
)

full_model_acc <- anova(mod_ACC, mod_ACC_no_int)
condition_only_model_acc <- anova(mod_ACC_no_int, mod_ACC_condition_only)
carrier_only_model_acc <- anova(mod_ACC_no_int, mod_ACC_carrier_only)

# --- Table 2: LRT results -----------------------------------------------------
extract_lrt_row <- function(lrt_obj, comparison_label, effect_label) {
  r      <- as.data.frame(lrt_obj)
  chi_sq <- round(r$Chisq[2], 3)
  df_val <- r$Df[2]                    # was r$`Chi Df`[2]
  p_val  <- r$`Pr(>Chisq)`[2]
  
  p_fmt <- ifelse(p_val < .001, "$< .001$",
                  paste0("$", format(round(p_val, 3), nsmall = 3), "$"))
  
  data.frame(
    Comparison = comparison_label,
    Effect     = effect_label,
    Chi2       = chi_sq,
    Df         = df_val,
    p          = p_fmt,
    stringsAsFactors = FALSE
  )
}

tbl2 <- rbind(
  extract_lrt_row(full_model_acc, "$M_{full}$ vs. $M_{additive}$",      "Interaction ($A \\times B$)"),
  extract_lrt_row(condition_only_model_acc,   "$M_{additive}$ vs. $M_{Segment\\ only}$",  "Main effect of $Carrier Type$"),
  extract_lrt_row(carrier_only_model_acc,   "$M_{additive}$ vs. $M_{Carrier Type\\ only}$",  "Main effect of $Segment$")
)

colnames(tbl2) <- c("Comparison", "Effect tested", "$\\chi^2$", "$df$", "$p$")

tbl2_tex <- kable(tbl2,
                  format   = "latex",
                  booktabs = TRUE,
                  escape   = FALSE,
                  caption  = "Accuracy: Likelihood-ratio tests for fixed effects. The interaction was tested against the full model; main effects were tested against the additive model.",
                  label    = "laryperc_lrt_acc") %>%
  kable_styling(latex_options = c("hold_position")) # %>%
# footnote(general       = "$p$-values based on $\\chi^2$ approximation.",
#          escape        = FALSE)

# --- Save out -----------------------------------------------------------------
# save_kable(tbl2_tex, file = "/Volumes/cassandra/alldata/dissertation/2/tables/model_comp_outputs_RT.tex")
save_kable(tbl2_tex, file = "/Volumes/circe/alldata/dissertation/2/tables/model_comp_outputs_acc.tex")


emms_ACC <- emmeans(
  mod_ACC, ~ Condition*CarrierType
)
# pairs(emms_ACC)

sorted_res_ACC <- pairs(emms_ACC) %>%
  as.data.frame() %>%
  arrange(p.value)
print(sorted_res_ACC)


# --- Build the newcommand reference table ------------------------------------
lrt_csv <- rbind(
  extract_lrt_row(full_model,           "full_vs_additive",    "Interaction"),
  extract_lrt_row(condition_only_model, "additive_vs_segment", "Segment"),
  extract_lrt_row(carrier_only_model,   "additive_vs_carrier", "CarrierType")
) %>%
  mutate(
    # strip LaTeX math formatting for the command names
    cmd_chi = paste0("lrt", gsub("_", "", Comparison), "Chi"),
    cmd_df  = paste0("lrt", gsub("_", "", Comparison), "Df"),
    cmd_p   = paste0("lrt", gsub("_", "", Comparison), "P")
  )

write.csv(lrt_csv, 
          "/Volumes/circe/alldata/dissertation/2/tables/lrt_RT_values.csv",
          row.names = FALSE)

# write.csv(lrt_csv, 
#           "/Volumes/cassandra/alldata/dissertation/2/tables/lrt_RT_values.csv",
#           row.names = FALSE)


# --- RT Model Tables ---

# Pairwise comparisons for RT
pairs_RT_table <- sorted_res_RT %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  rename(
    "Contrast" = contrast,
    "Estimate" = estimate,
    "SE" = SE,
    "df" = df,
    "z-ratio" = z.ratio,
    "p-value" = p.value
  ) %>%
  mutate(`p-value` = ifelse(`p-value` < 0.001, "$<$0.001", as.character(`p-value`))) %>%
  kable(format = "latex",
        booktabs = TRUE,
        caption = "Pairwise Comparisons for Reaction Time",
        label = "tab:laryperc_pairs_rt",
        linesep = "\\addlinespace",
        escape = FALSE) %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(0, bold = TRUE)

save_kable(pairs_RT_table, file = "/Volumes/circe/alldata/dissertation/2/tables/laryperc_pairs_rt.tex")
# save_kable(pairs_RT_table, file = "/Volumes/cassandra/alldata/dissertation/2/tables/laryperc_pairs_rt.tex")

# --- Accuracy Model Tables ---

# Pairwise comparisons for accuracy
pairs_ACC_table <- sorted_res_ACC %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  rename(
    "Contrast" = contrast,
    "Estimate" = estimate,
    "SE" = SE,
    "df" = df,
    "z-ratio" = z.ratio,
    "p-value" = p.value
  ) %>%
  mutate(`p-value` = ifelse(`p-value` < 0.001, "$<$0.001", as.character(`p-value`))) %>%
  kable(format = "latex",
        booktabs = TRUE,
        caption = "Pairwise Comparisons for Accuracy",
        label = "tab:laryperc_pairs_acc",
        linesep = "\\addlinespace",
        escape = FALSE) %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(0, bold = TRUE)

save_kable(pairs_ACC_table, file = "/Volumes/circe/alldata/dissertation/2/tables/laryperc_pairs_acc.tex")
# save_kable(pairs_ACC_table, file = "/Volumes/cassandra/alldata/dissertation/2/tables/laryperc_pairs_acc.tex")


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
  group_by(Condition, CarrierType) %>%
  summarise(
    mean_rt = mean(reaction_time_log_z, na.rm = TRUE),
    sd_rt   = sd(reaction_time_log_z, na.rm = TRUE),
    n       = sum(!is.na(reaction_time_log_z)),
    SE      = sd_rt / sqrt(n),
    .groups = "drop"
  )

acc_sum <- df_prepared %>%
  group_by(Condition, CarrierType) %>%
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

raw_RT_plot <- ggplot(df_sum_plot, aes(x = Condition, y = mean_rt, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_rt - SE, ymax = mean_rt + SE),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(title = "Reaction Time",
       y = "Reaction Time (log, z-scored)",
       x = "Condition",
       fill = "Creaky?") +
  theme_minimal() +
  # Use manual scale here:
  scale_fill_manual(values = pal5) +
  theme(legend.position = "bottom")  +
  theme(
    # Axis Titles (labels like "Condition", "Reaction Time")
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    
    # Axis Text (tick labels like "[t]", "[ʔ]", "0.5", "1.0")
    axis.text.x = element_text(family = "Doulos SIL", size = 12),
    axis.text.y = element_text(size = 12))

raw_acc_plot <- ggplot(acc_sum_plot, aes(x = Condition, y = p, fill = CarrierType)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = p - SE, ymax = p + SE),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Accuracy",
       y = "Accuracy (%)",
       x = "Condition",
       fill = "Creaky?") +
  theme_minimal() +
  # Use manual scale here:
  scale_fill_manual(values = pal5) +
  theme(legend.position = "bottom")  +
  theme(
    # Axis Titles (labels like "Condition", "Reaction Time")
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    
    # Axis Text (tick labels like "[t]", "[ʔ]", "0.5", "1.0")
    axis.text.x = element_text(family = "Doulos SIL", size = 12),
    axis.text.y = element_text(size = 12))

library(patchwork)

# --- RT Pair ---
rt_combined <- ((raw_RT_plot + forest_RT) + 
                  plot_annotation(tag_levels = 'a') +
                  plot_layout(widths = c(1, 1.2))) & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    plot.tag = element_text(size = 18, face = "bold"), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 11)
  )

ggsave("/Volumes/circe/alldata/dissertation/2/figs/laryperc_rt_combined.pdf", plot = rt_combined,
       width = 14.6, height = 8.5, units = "in", device = cairo_pdf)

# ggsave("/Volumes/cassandra/alldata/dissertation/2/figs/laryperc_rt_combined.pdf", plot = rt_combined, 
#        width = 14.6, height = 8.5, units = "in", device = cairo_pdf)


# --- Accuracy Pair ---
acc_combined <- ((raw_acc_plot + forest_acc) + 
                   plot_annotation(tag_levels = 'a') +
                   plot_layout(widths = c(1, 1.2))) & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    plot.tag = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 11)
  )

ggsave("/Volumes/circe/alldata/dissertation/2/figs/laryperc_acc_combined.pdf", plot = acc_combined,
       width = 14.6, height = 8.5, units = "in",  device = cairo_pdf)

# ggsave("/Volumes/cassandra/alldata/dissertation/2/figs/laryperc_acc_combined.pdf", plot = acc_combined, 
#        width = 14.6, height = 8.5, units = "in",  device = cairo_pdf)

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