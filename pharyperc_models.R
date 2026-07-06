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
# mod_RT <- lmer(
#   formula = reaction_time_log_z ~
#     TargetSegment*CarrierType +
#     (1|pair),
#   data = df_prepared
# )
# summary(mod_RT)

mod_RT <- lmer(
  formula = reaction_time_log_z ~
    TargetSegment*CarrierType +
    (1|pair),
  data = df_prepared,
  REML = FALSE
)
summary(mod_RT)

# log likelihood and AIC tests
mod_RT_no_int <- lmer(
  formula = reaction_time_log_z ~
    TargetSegment + CarrierType +
    (1|pair),
  data = df_prepared,
  REML = FALSE
)
summary(mod_RT)

mod_RT_condition_only <- lmer(
  formula = reaction_time_log_z ~
    TargetSegment +
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
                  label    = "pharyperc_models") %>%
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
                  label    = "pharyperc_lrt_rt") %>%
  kable_styling(latex_options = c("hold_position")) # %>%
# footnote(general       = "$p$-values based on $\\chi^2$ approximation.",
#          escape        = FALSE)

# --- Save out -----------------------------------------------------------------
save_kable(tbl1_tex, file = "/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_model_comp.tex")
save_kable(tbl2_tex, file = "/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_model_comp_outputs_RT.tex")

# save_kable(tbl1_tex, file = "/Volumes/circe/alldata/dissertation/3/tables/pharyperc_model_comp.tex")
# save_kable(tbl2_tex, file = "/Volumes/circe/alldata/dissertation/3/tables/pharyperc_model_comp_outputs_RT.tex")

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
summary(mod_ACC)

mod_ACC_no_int <- glmer(
  accuracy_num ~ TargetSegment + CarrierType + (1 | pair) + (1|subject),
  data = df_prepared,
  family = binomial(link = "logit")
)

mod_ACC_condition_only <- glmer(
  accuracy_num ~ TargetSegment + (1 | pair) + (1|subject),
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
                  label    = "pharyperc_lrt_acc") %>%
  kable_styling(latex_options = c("hold_position")) # %>%
# footnote(general       = "$p$-values based on $\\chi^2$ approximation.",
#          escape        = FALSE)

# --- Save out -----------------------------------------------------------------
save_kable(tbl2_tex, file = "/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_model_comp_outputs_acc.tex")
# save_kable(tbl2_tex, file = "/Volumes/circe/alldata/dissertation/3/tables/pharyperc_model_comp_outputs_acc.tex")


emms_ACC <- emmeans(
  mod_ACC, ~ TargetSegment*CarrierType
)
# pairs(emms_ACC)

sorted_res_ACC <- pairs(emms_ACC) %>%
  as.data.frame() %>%
  arrange(p.value)
print(sorted_res_ACC)


# --- RT comparisons -----------------------------------------------------------
lrt_csv_rt <- rbind(
  extract_lrt_row(full_model_rt,           "full_vs_additive",    "Interaction"),
  extract_lrt_row(condition_only_model_rt, "additive_vs_segment", "Segment"),
  extract_lrt_row(carrier_only_model_rt,   "additive_vs_carrier", "CarrierType")
) %>%
  mutate(
    outcome = "RT",
    cmd_chi = paste0("lrtRT", gsub("_", "", Comparison), "Chi"),
    cmd_df  = paste0("lrtRT", gsub("_", "", Comparison), "Df"),
    cmd_p   = paste0("lrtRT", gsub("_", "", Comparison), "P")
  )

# --- Accuracy comparisons -----------------------------------------------------
lrt_csv_acc <- rbind(
  extract_lrt_row(full_model_acc,           "full_vs_additive",    "Interaction"),
  extract_lrt_row(condition_only_model_acc, "additive_vs_segment", "Segment"),
  extract_lrt_row(carrier_only_model_acc,   "additive_vs_carrier", "CarrierType")
) %>%
  mutate(
    outcome = "Accuracy",
    cmd_chi = paste0("lrtAcc", gsub("_", "", Comparison), "Chi"),
    cmd_df  = paste0("lrtAcc", gsub("_", "", Comparison), "Df"),
    cmd_p   = paste0("lrtAcc", gsub("_", "", Comparison), "P")
  )

# --- Combine and save ---------------------------------------------------------
lrt_csv <- rbind(lrt_csv_rt, lrt_csv_acc)

write.csv(lrt_csv,
          "/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_lrt_values.csv",
          row.names = FALSE)

# write.csv(lrt_csv,
#           "/Volumes/circe/alldata/dissertation/3/tables/pharyperc_lrt_values.csv",
#           row.names = FALSE)

generate_newcommands <- function(csv_path, tex_path) {
  d     <- read.csv(csv_path)
  lines <- c("% Auto-generated LRT values for RT model -- do not edit manually")
  
  for (i in seq_len(nrow(d))) {
    lines <- c(lines,
               sprintf("\\newcommand{\\%s}{%s}",  d$cmd_chi[i], d$Chi2[i]),
               sprintf("\\newcommand{\\%s}{%s}",  d$cmd_df[i],  d$Df[i]),
               sprintf("\\newcommand{\\%s}{%s}",  d$cmd_p[i],   d$p[i])
    )
  }
  
  writeLines(lines, tex_path)
  message("Written: ", tex_path)
}

generate_newcommands(
  csv_path = "/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_lrt_values.csv",
  tex_path = "/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_lrt_commands.tex"
)

# generate_newcommands(
#   csv_path = "/Volumes/circe/alldata/dissertation/3/tables/pharyperc_lrt_values.csv",
#   tex_path = "/Volumes/circe/alldata/dissertation/3/tables/pharyperc_lrt_commands.tex"
# )

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
        label = "tab:pharyperc_pairs_rt",
        linesep = "\\addlinespace",
        escape = FALSE) %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(0, bold = TRUE)

# save_kable(pairs_RT_table, file = "/Volumes/circe/alldata/dissertation/3/tables/pharyperc_pairs_rt.tex")
save_kable(pairs_RT_table, file = "/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_pairs_rt.tex")

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
        label = "tab:pharyperc_pairs_acc",
        linesep = "\\addlinespace",
        escape = FALSE) %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(0, bold = TRUE)

# save_kable(pairs_ACC_table, file = "/Volumes/circe/alldata/dissertation/3/tables/pharyperc_pairs_acc.tex")
save_kable(pairs_ACC_table, file = "/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_pairs_acc.tex")



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
      contrast = str_replace_all(contrast, "\\s*/\\s*", " - "), # makes the symbol between contrasts a hyphen
      contrast = str_replace_all(contrast, "gs-allo", "Allophonic [ʔ]"),
      contrast = str_replace_all(contrast, "gs-phon", "Phonemic [ʔ]"),
      contrast = str_replace_all(contrast, "\\bt\\b", "[t]"),
      contrast = str_replace_all(contrast, "\\bk\\b", "[k]"),
      contrast = str_replace_all(contrast, "\\bq\\b", "[q]"),
      
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
  geom_errorbarh(aes(xmin = low, xmax = high, color = sig), height = 0.3) +
  geom_point(aes(color = sig, shape = sig), size = 5) +
  scale_color_manual(values = plot_colors) +
  scale_shape_manual(values = c("Significant" = 17, "Not Significant" = 18)) +
  labs(title = "Pairwise Contrasts: Reaction Time",
       subtitle = "Estimates (Log-Z) with 95% Confidence Intervals",
       x = "Estimated Difference (Log-Z)",
       y = NULL,
       color = "p < 0.05",
       shape = "p < 0.05") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16))

ggsave("/Volumes/cassandra/alldata/dissertation/3/figs/pharyperc_rt_forest.pdf", plot = forest_RT,
       width = 14.6, height = 16.5, units = "in", device = cairo_pdf)

# ggsave("/Volumes/circe/alldata/dissertation/3/figs/pharyperc_rt_forest.pdf", plot = forest_RT,
#        width = 14.6, height = 8.5, units = "in", device = cairo_pdf)


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

ggsave("/Volumes/cassandra/alldata/dissertation/3/figs/pharyperc_acc_forest.pdf", plot = forest_acc,
       width = 14.6, height = 8.5, units = "in", device = cairo_pdf)

# ggsave("/Volumes/circe/alldata/dissertation/3/figs/pharyperc_acc_forest.pdf", plot = forest_acc,
#        width = 14.6, height = 8.5, units = "in", device = cairo_pdf)

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
                             labels = c("[t]", "[k]", "Phonemic [ʔ]", "Allophonic [ʔ]", "[q]")),
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
  # theme(
  #   # Axis Titles (labels like "TargetSegment", "Reaction Time")
  #   axis.title.x = element_text(size = 14),
  #   axis.title.y = element_text(size = 14),
  #   
  #   # Axis Text (tick labels like "[t]", "[ʔ]", "0.5", "1.0")
  #   axis.text.x = element_text(size = 12),
  #   axis.text.y = element_text(size = 12)
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(family = "Doulos SIL", size = 20),
    axis.text.y  = element_text(size = 20),
    legend.text  = element_text(size = 18),
    legend.title = element_text(size = 18),
    plot.title   = element_text(size = 22, face = "bold", hjust = 0.5)
    )

ggsave("/Volumes/cassandra/alldata/dissertation/3/figs/pharyperc_rt_bar.pdf", plot = raw_RT_plot,
       width = 14.6, height = 8.5, units = "in", device = cairo_pdf)

# ggsave("/Volumes/circe/alldata/dissertation/3/figs/pharyperc_rt_bar.pdf", plot = raw_RT_plot,
#        width = 14.6, height = 8.5, units = "in", device = cairo_pdf)

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
  # theme(
  #   # Axis Titles (labels like "TargetSegment", "Reaction Time")
  #   axis.title.x = element_text(size = 14),
  #   axis.title.y = element_text(size = 14),
  #   
  #   # Axis Text (tick labels like "[t]", "[ʔ]", "0.5", "1.0")
  #   axis.text.x = element_text(size = 12),
  #   axis.text.y = element_text(size = 12))
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(family = "Doulos SIL", size = 20),
    axis.text.y  = element_text(size = 20),
    legend.text  = element_text(size = 18),
    legend.title = element_text(size = 18),
    plot.title   = element_text(size = 22, face = "bold", hjust = 0.5)
  )

ggsave("/Volumes/cassandra/alldata/dissertation/3/figs/pharyperc_acc_bar.pdf", plot = raw_acc_plot,
       width = 14.6, height = 8.5, units = "in", device = cairo_pdf)

# ggsave("/Volumes/circe/alldata/dissertation/3/figs/pharyperc_acc_bar.pdf", plot = raw_acc_plot,
#        width = 14.6, height = 8.5, units = "in", device = cairo_pdf)

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

# ggsave("/Volumes/circe/alldata/dissertation/3/figs/pharyperc_rt_combined.pdf", plot = rt_combined,
#        width = 14.6, height = 8.5, units = "in", device = cairo_pdf)

ggsave("/Volumes/cassandra/alldata/dissertation/3/figs/pharyperc_rt_combined.pdf", plot = rt_combined,
       width = 14.6, height = 8.5, units = "in", device = cairo_pdf)


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

# ggsave("/Volumes/circe/alldata/dissertation/3/figs/pharyperc_acc_combined.pdf", plot = acc_combined,
#        width = 14.6, height = 8.5, units = "in",  device = cairo_pdf)

ggsave("/Volumes/cassandra/alldata/dissertation/3/figs/pharyperc_acc_combined.pdf", plot = acc_combined,
       width = 14.6, height = 8.5, units = "in",  device = cairo_pdf)