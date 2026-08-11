## Pharyperc Acoustic Analysis
# blang@ucsd.edu

library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gghalves)
library(marginaleffects)
library(tidyr)
library(lme4)
library(lmerTest)
library(knitr)
library(kableExtra)
library(emmeans)
library("stringr")
library("tidyverse")
library("devtools")
library(grid)
library(gridExtra)
library(scales)


#paths
# orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/3/raw_data/pharyperc/stims/grids/vs/pharyperc_acoustic_output.csv')
orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/3/raw_data/pharyperc/stims/grids/vs/pharyperc_acoustic_output.csv')

orig_data = read.csv(orig_data_path)
df <- orig_data

# merge metadata onto df

# metadata_path <- sprintf('/Volumes/cassandra/alldata/dissertation/3/arabic_perception_stimuli - trials_reduced.csv')
metadata_path <- sprintf('/Volumes/cassandra/alldata/dissertation/3/arabic_perception_stimuli - trials_reduced.csv')

metadata = read.csv(metadata_path)
df_meta <- metadata

# Extract the numeric prefix from Filename (text before the first underscore)
df <- df %>%
  mutate(Trial = as.numeric(str_extract(Filename, "^[0-9]+(?=_)")))

# Keep only the first 126 ROWS of df_meta (by position, not by value)
df_meta_unique <- df_meta %>%
  slice(1:126)

# Confirm Trial is now unique in df_meta_unique -- this should return TRUE
n_distinct(df_meta_unique$Trial) == nrow(df_meta_unique)

# If the above is FALSE, find out which Trial values are repeated
df_meta_unique %>%
  count(Trial) %>%
  filter(n > 1)

# Join metadata into df based on Trial
df_matched <- df %>%
  left_join(df_meta_unique, by = "Trial")

# fix a quick error in the voicesauce output. this has been corrrected in the textgrids.
# run voicesauce again and then comment this out eventually
df_matched <- df_matched %>%
  mutate(Label = if_else(Trial == 66 & Label == "ʔ", "q", Label))

## calculate mean values for each dataframe
features <- c(
  "strF0",
  "H1H2c","H1c","HNR05",
  "CPP","soe","Energy",
  "sF1","sF2","sF3"
)

df_matched <- df_matched %>%
  group_by(Trial, Label) %>%
  mutate(
    across(all_of(features), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean")
  ) %>%
  ungroup()

# log transform for all dfs
cols_to_log <- c("soe_mean", "Energy_mean", "CPP_mean")
feats <- intersect(cols_to_log, names(df_matched))

df_matched <- df_matched %>%
  # group_by(participant) %>%
  mutate(
    across(
      all_of(feats),
      ~ {
        v <- suppressWarnings(as.numeric(.x))     # coerce if needed
        out <- ifelse(is.finite(v) & v > 0, log(v), NA_real_)  # log only positive, finite
        out
      },
      .names = "{.col}_log"
    )
  )

## z-scoring any existing normal or logged to normal
cols_to_z <- c("strF0_mean","H1H2c_mean","CPP_mean_log","Energy_mean_log","soe_mean_log")
thresh <- 3

df_matched <- df_matched %>%
  # group_by(participant) %>%
  mutate(
    across(all_of(cols_to_z), ~{
      m <- mean(.x, na.rm = TRUE)
      s <- sd(.x,   na.rm = TRUE)
      if (is.finite(s) && s > 0) (.x - m) / s else NA_real_
    }, .names = "{.col}_z")
  ) %>%
  ungroup() %>%
  mutate(
    across(
      ends_with("z"),
      ~ if_else(is.finite(.x) & abs(.x) > thresh, "outlier", "OK"),
      .names = "{.col}_outlier"
    )
  )

# --- 1) Diagnose quickly -----------------------------------------------------
c(
  H1c_nonfinite    = sum(!is.finite(df_matched$H1c_mean)),
  Energy_NA        = sum(is.na(df_matched$Energy_mean_log_z)),
  Energy_nonfinite = sum(!is.finite(df_matched$Energy_mean_log_z)),
  Energy_le0       = sum(df_matched$Energy_mean_log_z <= 0, na.rm = TRUE)
)

# --- 2) Clean + prepare model frame ------------------------------------------
H1c_mod_dat <- df_matched %>%
  filter(
    is.finite(H1c_mean),
    is.finite(Energy_mean_log_z),
    !is.na(strF0_mean_z_outlier))
# %>%
# filter(formant_outlier != "outlier" | is.na(formant_outlier))

# --- 3) Fit model ------------------------------------------------------------
mod <- lm(H1c_mean ~ Energy_mean_log + strF0_mean, data = H1c_mod_dat)

sm <- summary(mod)
H1res_estimate <- coef(sm)["Energy_mean_log", "Estimate"]

# --- 4) Compute H1res safely in-place on subset_time -------------------------
# use the same logEnergy definition as above, ensure finite computation only
df_matched <- df_matched %>%
  mutate(
    H1res_mean = if_else(
      is.finite(H1c_mean) & is.finite(Energy_mean_log),
      H1c_mean - H1res_estimate * Energy_mean_log,
      NA_real_
    )
  )

# optional: check non-finite values
sum(!is.finite(df_matched$H1res_mean))

## z-scoring any existing normal or logged to normal
cols_to_z <- c("H1res_mean")
thresh <- 3

df_matched <- df_matched %>%
  # group_by(participant) %>%
  mutate(
    across(all_of(cols_to_z), ~{
      m <- mean(.x, na.rm = TRUE)
      s <- sd(.x,   na.rm = TRUE)
      if (is.finite(s) && s > 0) (.x - m) / s else NA_real_
    }, .names = "{.col}_z")
  ) %>%
  ungroup() %>%
  mutate(
    across(
      ends_with("z"),
      ~ if_else(is.finite(.x) & abs(.x) > thresh, "outlier", "OK"),
      .names = "{.col}_outlier"
    )
  )

## subset based on vowels adjacent and based on consonants themselves
# subset_glot = subset(df_matched, Label == 'ʔ' & Condition != 'control')

## grab preceding and following vowels of only the target consonants and place them in dataframes
df_matched_filter <- df_matched %>%
  mutate(`TargetSegment` = if_else(`TargetSegment` == "gs", "ʔ", `TargetSegment`))

# Define vowel labels, including length-marked versions
vowels <- c("ɑ", "æ", "a", "i", "ɪ", "u", "ʊ")
vowels_with_length <- c(vowels, paste0(vowels, "ː"))

# Step 1: identify contiguous blocks of identical Label within each Trial
df_blocks <- df_matched_filter %>%
  group_by(Trial) %>%
  mutate(
    block_id = cumsum(Label != lag(Label, default = first(Label)) | row_number() == 1)
  ) %>%
  ungroup()

# Step 2: summarize each block (one row per block) so we can find neighbors easily
block_summary <- df_blocks %>%
  group_by(Trial, block_id) %>%
  summarise(
    Label = first(Label),
    Target_Segment = first(`TargetSegment`),
    n_rows = n(),
    .groups = "drop"
  ) %>%
  group_by(Trial) %>%
  mutate(
    is_target_block = Label == Target_Segment,
    prev_block_id = lag(block_id),
    next_block_id = lead(block_id),
    prev_label = lag(Label),
    next_label = lead(Label)
  ) %>%
  ungroup()

# Step 2b: within each Trial, identify all ʔ blocks and number their occurrence
ipa_target_char <- "ʔ"  # the character we need to disambiguate

block_summary <- block_summary %>%
  group_by(Trial) %>%
  mutate(
    is_ipa_target_char = Label == ipa_target_char,
    ipa_occurrence = if_else(is_ipa_target_char, cumsum(is_ipa_target_char), NA_integer_)
  ) %>%
  ungroup()

# Step 2c: redefine is_target_block so that for the ʔ character specifically,
# only the SECOND occurrence counts as the true target block.
# All other Target Segment characters keep the original logic.
block_summary <- block_summary %>%
  group_by(Trial) %>%
  mutate(
    is_ipa_target_char = Label == ipa_target_char,
    ipa_occurrence = if_else(is_ipa_target_char, cumsum(is_ipa_target_char), NA_integer_),
    max_ipa_occurrence = max(ipa_occurrence, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    is_target_block = case_when(
      Label == ipa_target_char & max_ipa_occurrence >= 2 ~ (ipa_occurrence == 2),
      Label == ipa_target_char & max_ipa_occurrence == 1 & `Target_Segment` == ipa_target_char ~ TRUE,
      TRUE ~ Label == `Target_Segment`
    )
  )

# Get the block_ids that are the TRUE ʔ target blocks (2nd occurrence, per the disambiguation logic)
glot_target_block_ids <- block_summary %>%
  filter(Label == "ʔ", is_target_block) %>%
  select(Trial, block_id)

# Pull the full ms-level rows for those blocks, then restrict to non-control
subset_glot <- df_blocks %>%
  semi_join(glot_target_block_ids, by = c("Trial", "block_id")) %>%
  filter(Condition != "control")

# Step 3: find the block_ids of vowel blocks immediately preceding a target block
preceding_block_ids <- block_summary %>%
  filter(is_target_block, prev_label %in% vowels_with_length) %>%
  select(Trial, block_id = prev_block_id)

# Step 4: find the block_ids of vowel blocks immediately following a target block
following_block_ids <- block_summary %>%
  filter(is_target_block, next_label %in% vowels_with_length) %>%
  select(Trial, block_id = next_block_id)

# Step 5: pull the full set of original ms-level rows for those blocks
preceding_vowels <- df_blocks %>%
  semi_join(preceding_block_ids, by = c("Trial", "block_id"))

following_vowels <- df_blocks %>%
  semi_join(following_block_ids, by = c("Trial", "block_id"))

# print some stats to doule check
# preceding should only have medial and final
# following should only have initial and medial
# both should have control, phoneme, and allophone

preceding_vowels_unique <- preceding_vowels %>%
  distinct(Trial, .keep_all = TRUE)

preceding_vowels_unique %>%
  summarise(
    n_target_segment = n_distinct(`TargetSegment`),
    n_trials = n_distinct(Trial),
    n_position = n_distinct(Position),
    n_condition = n_distinct(Condition)
  )

preceding_vowels_unique %>% count(`TargetSegment`)
preceding_vowels_unique %>% count(Position)
preceding_vowels_unique %>% count(Condition)

following_vowels_unique <- following_vowels %>%
  distinct(Trial, .keep_all = TRUE)

following_vowels_unique %>%
  summarise(
    n_target_segment = n_distinct(`TargetSegment`),
    n_trials = n_distinct(Trial),
    n_position = n_distinct(Position),
    n_condition = n_distinct(Condition)
  )

following_vowels_unique %>% count(`TargetSegment`)
following_vowels_unique %>% count(Position)
following_vowels_unique %>% count(Condition)

# 126 (one for each of all the test trials)
combined_unique <- bind_rows(preceding_vowels_unique, following_vowels_unique) %>%
  distinct(Trial, .keep_all = TRUE)

combined_unique %>%
  summarise(
    n_trials = n_distinct(Trial),
    n_target_segment = n_distinct(`TargetSegment`)
  )

combined_unique %>% count(`TargetSegment`)


# 50 (24 creaky, 24 breathy, 2 bonus)
subset_glot_unique <- subset_glot %>%
  distinct(Trial, .keep_all = TRUE)

subset_glot_unique %>%
  summarise(
    n_trials = n_distinct(Trial),
    n_target_segment = n_distinct(`TargetSegment`)
  )

subset_glot_unique %>% count(`TargetSegment`)

# separate by positions
# pre_vow_medial <- subset(preceding_vowels, Position = 'Medial')
# pre_vow_final <- subset(preceding_vowels, Position = 'Medial')
# 
# fol_vow_initial <- subset(preceding_vowels, Position = 'Medial')
# fol_vow_medial <- subset(preceding_vowels, Position = 'Medial')
# 
# glot_initial <- subset(subset_glot, Position = 'Initial')
# glot_medial <- subset(subset_glot, Position = 'Medial')
# glot_final <- subset(subset_glot, Position = 'Final')


### flagging formant outliers
### Calculate Mahalanobis distance for formants

# vmahalanobis = function (dat) {
#   if (nrow(dat) < 25) {
#     dat$zF1F2 = NA
#     return(dat)
#   }
#   means = c(mean(dat$sF1_mean, na.rm=T), mean(dat$sF2_mean, na.rm=T))
#   cov = cov(cbind(dat$sF1_mean, dat$sF2_mean))
#   
#   dat$zF1F2 = mahalanobis(cbind(dat$sF1_mean, dat$sF2_mean),
#                           center=means, cov=cov)
#   dat
# }
# 
# # Distance larger than 6 is considered as outlier    #MG: smaller numbers = more outliers. The paper I linked to uses 4.
# distance_cutoff = 6
# 
# # Perform Mahalanobis on dataset
# preceding_vowels =  preceding_vowels %>%                 #MG: this was cut from a dataset called "tot_fin"
#   group_by(Trial) %>%
#   do(vmahalanobis(.)) %>%
#   ungroup() %>%
#   mutate(formant_outlier = NA)
# 
# # Visualize the formants with flagged values
# preceding_vowels %>%
#   filter(is.na(formant_outlier)) %>%
#   ggplot(aes(x = sF2_mean, y = sF1_mean, color = zF1F2 > distance_cutoff)) +       #MG: sF2 and sF1 = Snack values from VS
#   geom_point(size = 0.6) +
#   facet_wrap(.~interval)+
#   scale_y_reverse(limits = c(2000,0),position = "right") +
#   scale_x_reverse(limits = c(3500,0),position = "top")+
#   theme_bw()
# 
# # Tag flagged values
# for (i in 1:nrow(preceding_vowels)) {
#   if (!is.na(preceding_vowels$zF1F2[i])) {
#     if (preceding_vowels$zF1F2[i] > distance_cutoff){
#       preceding_vowels$formant_outlier[i] = "outlier"
#     }
#   }
#   
# }

# Visualize the vowel formants
preceding_vowels %>%
  # filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2_mean, y = sF1_mean, label = Trial)) +
  # geom_point(size = 0.6) +
  geom_text(size = 2.5)+
  facet_wrap(.~Label)+
  #geom_density_2d() +
  #  scale_color_manual(values=c('#a6611a','#dfc27d','#018571'))+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

# Visualize the vowel formants
following_vowels %>%
  # filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2_mean, y = sF1_mean, label = Trial)) +
  # geom_point(size = 0.6) +
  geom_text(size = 2.5)+
  facet_wrap(.~Label)+
  #geom_density_2d() +
  #  scale_color_manual(values=c('#a6611a','#dfc27d','#018571'))+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()


### models

subset_glot <- subset_glot %>%
  mutate(TargetSegment = if_else(TargetSegment == "ʔ" & Condition == "allophone", "gs", TargetSegment))
preceding_vowels <- preceding_vowels %>%
  mutate(`TargetSegment` = if_else(`TargetSegment` == "ʔ" & Condition == "allophone", "gs", `TargetSegment`))
following_vowels <- following_vowels %>%
  mutate(`TargetSegment` = if_else(`TargetSegment` == "ʔ" & Condition == "allophone", "gs", `TargetSegment`))

subset_glot$CarrierType <- as.factor(subset_glot$CarrierType)
subset_glot$TargetSegment <- as.factor(subset_glot$TargetSegment)
subset_glot$IPA <- as.factor(subset_glot$IPA)

preceding_vowels$CarrierType <- as.factor(preceding_vowels$CarrierType)
preceding_vowels$TargetSegment <- as.factor(preceding_vowels$TargetSegment)
preceding_vowels$IPA <- as.factor(preceding_vowels$IPA)

following_vowels$CarrierType <- as.factor(following_vowels$CarrierType)
following_vowels$TargetSegment <- as.factor(following_vowels$TargetSegment)
following_vowels$IPA <- as.factor(following_vowels$IPA)

# Outer list: your three dataframes
df_list <- list(
  subset_glot     = subset_glot,
  preceding_vowels = preceding_vowels,
  following_vowels = following_vowels
)

# Feature to model for each of the groups above
features <- c("strF0_mean_z", 
              "H1H2c_mean_z", 
              "H1res_mean_z", 
              "CPP_mean_log_z", 
              "soe_mean_log_z", 
              "sF1_mean", 
              "sF2_mean")

# Nested results container: results_list[[df_name]][[feature_name]]
results_list <- list()

# Features to model, with their outlier column if one exists (NA means no outlier filter)
feature_outlier_map <- c(
  "strF0_mean_z" = "strF0_mean_z_outlier",
  "H1H2c_mean_z" = "H1H2c_mean_z_outlier", 
  "H1res_mean_z" = "H1res_mean_z_outlier", 
  "CPP_mean_log_z"   = "CPP_mean_log_z_outlier",
  "soe_mean_log_z"   = "soe_mean_log_z_outlier",
  "sF1_mean"   = NA,
  "sF2_mean"   = NA
)

for (df_name in names(df_list)) {
  
  cat("=== Processing dataframe:", df_name, "===\n")
  df_current <- df_list[[df_name]]
  
  results_list[[df_name]] <- list()
  
  for (feature in names(feature_outlier_map)) {
    
    cat("  -- Feature:", feature, "\n")
    
    outlier_col <- feature_outlier_map[[feature]]
    
    ### --- Filter to complete/finite cases, plus outlier filter if applicable ---
    vars_needed <- c(feature, "CarrierType", "TargetSegment", "IPA")
    ok <- complete.cases(df_current[, vars_needed]) &
      is.finite(df_current[[feature]])
    
    df_prepared <- df_current[ok, ]
    
    if (!is.na(outlier_col)) {
      df_prepared <- df_prepared %>%
        filter(.data[[outlier_col]] == "OK")
    }
    
    df_prepared <- df_prepared %>%
      distinct(Trial, .keep_all = TRUE)
    
    ### --- Guard: skip if any predictor has fewer than 2 levels ---
    predictors <- c("CarrierType", "TargetSegment")
    n_levels <- sapply(predictors, function(p) n_distinct(df_prepared[[p]]))
    
    if (any(n_levels < 2)) {
      cat("  -- Skipping: insufficient levels in",
          paste(predictors[n_levels < 2], collapse = ", "), "\n")
      results_list[[df_name]][[feature]] <- list(skipped = TRUE, reason = n_levels)
      next
    }
    
    ### --- Model formulae ---
    formula_full         <- as.formula(paste0(feature, " ~ CarrierType * TargetSegment + (1 | IPA)"))
    formula_no_int       <- as.formula(paste0(feature, " ~ CarrierType + TargetSegment + (1 | IPA)"))
    formula_carrier_only <- as.formula(paste0(feature, " ~ CarrierType + (1 | IPA)"))
    formula_segment_only <- as.formula(paste0(feature, " ~ TargetSegment + (1 | IPA)"))
    
    mod_full         <- lmer(formula_full,         data = df_prepared, REML = FALSE)
    mod_no_int       <- lmer(formula_no_int,       data = df_prepared, REML = FALSE)
    mod_carrier_only <- lmer(formula_carrier_only, data = df_prepared, REML = FALSE)
    mod_segment_only <- lmer(formula_segment_only, data = df_prepared, REML = FALSE)
    
    comp_full_vs_no_int    <- anova(mod_full,   mod_no_int)       # does interaction help?
    comp_no_int_vs_carrier <- anova(mod_no_int, mod_carrier_only) # does TargetSegment add anything?
    comp_no_int_vs_segment <- anova(mod_no_int, mod_segment_only) # does CarrierType add anything?
    
    emm <- emmeans(mod_full, ~ CarrierType * TargetSegment)
    emm_pairs <- pairs(emm) %>% as.data.frame() %>% arrange(p.value)
    
    results_list[[df_name]][[feature]] <- list(
      df_prepared            = df_prepared,
      mod_full               = mod_full,
      mod_no_int             = mod_no_int,
      mod_carrier_only       = mod_carrier_only,
      mod_segment_only       = mod_segment_only,
      comp_full_vs_no_int    = comp_full_vs_no_int,
      comp_no_int_vs_carrier = comp_no_int_vs_carrier,
      comp_no_int_vs_segment = comp_no_int_vs_segment,
      emm_pairs              = emm_pairs
    )
  }
}

# # preceding_vowels$strF0_mean_z <- as.integer(preceding_vowels$strF0_mean_z)
# preceding_vowels <- preceding_vowels %>%
#   mutate(`TargetSegment` = if_else(`TargetSegment` == "ʔ" & Condition == "allophone", "gs", `TargetSegment`))
# 
# preceding_vowels$CarrierType <- as.factor(preceding_vowels$CarrierType)
# preceding_vowels$TargetSegment <- as.factor(preceding_vowels$TargetSegment)
# 
# vars_needed <- c("strF0_mean_z","CarrierType", "Trial", "TargetSegment")
# ok <- complete.cases(preceding_vowels[, vars_needed]) &
#   is.finite(preceding_vowels$strF0_mean_z)
# 
# df_prepared <- preceding_vowels[ok, ] %>%
#   distinct(Trial, .keep_all = TRUE)
# 
# ### add a calculation for overall accuracy and overall reaction time here
# 
# # mixed effects
# mod_f0 <- lmer(
#   formula = strF0_mean_z ~
#     CarrierType*TargetSegment +
#     (1 | CarrierType + TargetSegment),
#   data = df_prepared,
#   REML = FALSE
# )
# summary(mod_f0)
# 
# # log likelihood and AIC tests
# mod_f0_no_int <- lmer(
#   formula = strF0_mean_z ~
#     CarrierType + TargetSegment +
#     (1 | CarrierType + TargetSegment),
#   data = df_prepared,
#   REML = FALSE
# )
# summary(mod_f0_no_int)
# 
# mod_f0_carrier_only <- lmer(
#   formula = strF0_mean_z ~
#     CarrierType +
#     (1 | CarrierType),
#   data = df_prepared,
#   REML = FALSE
# )
# summary(mod_f0_carrier_only)
# 
# mod_f0_segment_only <- lmer(
#   formula = strF0_mean_z ~
#     TargetSegment +
#     (1 | TargetSegment),
#   data = df_prepared,
#   REML = FALSE
# )
# summary(mod_f0_segment_only)
# 
# full_model_rt <- anova(mod_f0, mod_f0_no_int)
# condition_only_model_rt <- anova(mod_f0_no_int, mod_f0_carrier_only)
# carrier_only_model_rt <- anova(mod_f0_no_int, mod_f0_segment_only)
# 
# emms_f0 <- emmeans(
#   mod_f0, ~ CarrierType * TargetSegment
# )
# # pairs(emms_RT)
# 
# sorted_res_f0 <- pairs(emms_f0) %>%
#   as.data.frame() %>%
#   arrange(p.value)
# print(sorted_res_f0)

# ============================================================
# DICTIONARIES for clean table labels
# ============================================================

feature_display <- c(
  "strF0_mean_z"   = "f0",
  "H1H2c_mean_z"   = "H1-H2*",
  "H1res_mean_z"   = "Residual H1*",
  "CPP_mean_log_z" = "CPP",
  "soe_mean_log_z" = "Strength of Excitation",
  "sF1_mean"       = "F1",
  "sF2_mean"       = "F2"
)

segment_display <- c(
  "gs"             = "Allophonic \\ipa{[ʔ]}",
  "k"              = "[k]",
  "q"              = "[q]",
  "t"              = "[t]",
  "ʔ"   = "Phonemic \\ipa{[ʔ]}"
)

df_display <- c(
  "subset_glot"     = "Phonemic and Allophonic \\ipa{[ʔ]}",
  "preceding_vowels" = "Vowels Preceding Target Segment",
  "following_vowels" = "Vowels Following Target Segment"
)

# Column order for preceding and following vowels (segment columns only)
vowel_col_order <- c(
  "Feature",
  "[k]",
  "[t]",
  "[q]",
  "Phonemic \\ipa{[ʔ]}",
  "Allophonic \\ipa{[ʔ]}"
)

# Column order for subset_glot
glot_col_order <- c(
  "Feature",
  "Phonemic \\ipa{[ʔ]}",
  "Allophonic \\ipa{[ʔ]}"
)

# ============================================================
# HELPER: format p-values
# ============================================================
fmt_p <- function(x) {
  ifelse(x < 0.001, "$<$0.001", as.character(round(x, 3)))
}

# Model summary table — predictor structure only
model_comp_table <- data.frame(
  Model = c(
    "Full ($CarrierType \\times TargetSegment$)",
    "Additive ($CarrierType + TargetSegment$)"
  )
)

model_comp_out <- model_comp_table %>%
  kable(format   = "latex",
        booktabs = TRUE,
        escape   = FALSE,
        col.names = "Model",
        caption  = "Summary of linear mixed-effects models fit to each acoustic feature. 
                    All models were fit using maximum likelihood (\\texttt{REML = FALSE}) 
                    to allow likelihood-ratio comparison of fixed effects. 
                    All models included a random intercept for (1 $|$ UniquePairIndex).",
        label    = "tab:acoustic_models",
        linesep  = "\\addlinespace") %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(0, bold = TRUE)

save_kable(model_comp_out, file = "tables/acoustic_model_comp.tex")

# ============================================================
# 2. DESCRIPTIVE STATS TABLES (one per dataframe)
#    Mean (SD) per acoustic feature x TargetSegment — wide format
# ============================================================

features <- names(feature_outlier_map)

desc_inputs <- list(
  preceding_vowels = preceding_vowels,
  following_vowels = following_vowels,
  subset_glot      = subset_glot
)

for (df_name in names(desc_inputs)) {
  
  df_current <- desc_inputs[[df_name]]
  
  desc_rows <- lapply(features, function(feature) {
    
    stats <- df_current %>%
      filter(is.finite(.data[[feature]])) %>%
      group_by(TargetSegment) %>%
      summarise(
        mean_val = round(mean(.data[[feature]], na.rm = TRUE), 3),
        sd_val   = round(sd(.data[[feature]],   na.rm = TRUE), 3),
        .groups  = "drop"
      ) %>%
      mutate(mean_sd = paste0(mean_val, " (", sd_val, ")")) %>%
      select(TargetSegment, mean_sd)
    
    wide <- stats %>%
      tidyr::pivot_wider(names_from  = TargetSegment,
                         values_from = mean_sd)
    
    bind_cols(data.frame(Feature = feature_display[[feature]]), wide)
    
  }) %>% bind_rows()
  
  colnames(desc_rows)[1] <- "Feature"
  
  # Rename segment columns using segment_display dictionary
  colnames(desc_rows) <- ifelse(
    colnames(desc_rows) %in% names(segment_display),
    segment_display[colnames(desc_rows)],
    colnames(desc_rows)
  )
  
  # Pick the right column order for this dataframe
  col_order <- if (df_name == "subset_glot") glot_col_order else vowel_col_order
  
  # Reorder columns, keeping only those that exist in this table
  desc_rows <- desc_rows %>%
    select(any_of(col_order))
  
  desc_table <- desc_rows %>%
    kable(format   = "latex",
          booktabs = TRUE,
          escape   = FALSE,
          caption  = paste(df_display[[df_name]], "- Mean (SD) by Target Segment"),
          label    = paste0("tab:desc_", df_name),
          linesep  = "\\addlinespace") %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    row_spec(0, bold = TRUE)
  
  save_kable(desc_table,
             file = paste0("tables/desc_", df_name, ".tex"))
  
  cat("Saved: tables/desc_", df_name, ".tex\n", sep = "")
}

# ============================================================
# 3. MODEL COMPARISON TABLES (one per dataframe x feature)
# ============================================================

for (df_name in names(results_list)) {
  for (feature in names(results_list[[df_name]])) {
    
    res <- results_list[[df_name]][[feature]]
    
    # Skip if this combination was skipped during modeling
    if (!is.null(res$skipped)) next
    
    # Pull the three comparisons into a single tidy table
    comp_table <- bind_rows(
      as.data.frame(res$comp_full_vs_no_int)    %>% mutate(Comparison = "Full vs. No Interaction"),
      as.data.frame(res$comp_no_int_vs_carrier) %>% mutate(Comparison = "Additive vs. CarrierType Only"),
      as.data.frame(res$comp_no_int_vs_segment) %>% mutate(Comparison = "Additive vs. TargetSegment Only")
    ) %>%
      select(Comparison, npar, AIC, BIC, logLik, Chisq, Df, `Pr(>Chisq)`) %>%
      mutate(`Pr(>Chisq)` = fmt_p(`Pr(>Chisq)`))
    
    comp_out <- comp_table %>%
      kable(format    = "latex",
            booktabs  = TRUE,
            escape    = FALSE,
            caption   = paste0("Model comparisons for ", feature_display[[feature]], " - ", df_display[[df_name]]),
            label     = paste0("tab:comp_", df_name, "_", feature),
            linesep   = "\\addlinespace") %>%
      kable_styling(latex_options = c("hold_position", "scale_down")) %>%
      row_spec(0, bold = TRUE)
    
    save_kable(comp_out,
               file = paste0("tables/comp_", df_name, "_", feature, ".tex"))
    
    cat("Saved: tables/comp_", df_name, "_", feature, ".tex\n", sep = "")
  }
}

# ============================================================
# 4. EMMEANS PAIRWISE TABLES (one per dataframe x feature)
# ============================================================

for (df_name in names(results_list)) {
  for (feature in names(results_list[[df_name]])) {
    
    res <- results_list[[df_name]][[feature]]
    
    if (!is.null(res$skipped)) next
    
    emm_table <- res$emm_pairs %>%
      mutate(
        p.value  = fmt_p(p.value),
        contrast = stringr::str_replace_all(
          contrast,
          c(
            "\\bgs\\b"              = "Allophonic [\\\\textipa{P}]",
            "\\bPhonemic \\[ʔ\\]\\b"   = "Phonemic [\\\\textipa{P}]",
            "\\bAllophonic \\[ʔ\\]\\b" = "Allophonic [\\\\textipa{P}]"
          )
        )
      ) %>%
      select(contrast, estimate, SE, df, t.ratio, p.value)
    
    emm_out <- emm_table %>%
      kable(format    = "latex",
            booktabs  = TRUE,
            escape    = FALSE,
            caption   = paste0( df_display[[df_name]], " - ", "Pairwise Comparisons (emmeans)", " - ", feature_display[[feature]]),
            label     = paste0("tab:emm_", df_name, "_", feature),
            linesep   = "\\addlinespace") %>%
      kable_styling(latex_options = c("hold_position", "scale_down")) %>%
      row_spec(0, bold = TRUE)
    
    save_kable(emm_out,
               file = paste0("tables/emm_", df_name, "_", feature, ".tex"))
    
    cat("Saved: tables/emm_", df_name, "_", feature, ".tex\n", sep = "")
  }
}

# ============================================================
# 5. CROSS-DATAFRAME SUMMARY TABLES (one per feature)
# ============================================================

for (feature in features) {
  
  cat("Building cross-df table for:", feature, "\n")
  
  all_rows <- lapply(names(desc_inputs), function(df_name) {
    
    df_current <- desc_inputs[[df_name]]
    
    stats <- df_current %>%
      filter(is.finite(.data[[feature]])) %>%
      group_by(TargetSegment) %>%
      summarise(
        mean_val = round(mean(.data[[feature]], na.rm = TRUE), 3),
        sd_val   = round(sd(.data[[feature]],   na.rm = TRUE), 3),
        .groups  = "drop"
      ) %>%
      mutate(
        mean_sd       = paste0(mean_val, " (", sd_val, ")"),
        TargetSegment = segment_display[as.character(TargetSegment)]
      ) %>%
      select(TargetSegment, mean_sd) %>%
      tidyr::pivot_wider(
        names_from  = TargetSegment,
        values_from = mean_sd
      ) %>%
      mutate(Context = df_display[[df_name]]) %>%
      select(Context, everything())
    
  }) %>% bind_rows()
  
  # Reorder columns: Context first, then segments in display order
  col_order <- c("Context", vowel_col_order[vowel_col_order != "Feature"])
  all_rows <- all_rows %>%
    select(any_of(col_order))
  
  # Row that corresponds to subset_glot for the separator
  glot_row <- which(names(desc_inputs) == "subset_glot")
  
  cross_table <- all_rows %>%
    kable(
      format    = "latex",
      booktabs  = TRUE,
      escape    = FALSE,
      caption   = paste0("Normalized Mean (SD) for ", feature_display[[feature]], " across segment groups"),
      label     = paste0("tab:cross_", feature),
      linesep   = "",
      row.names = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(glot_row, hline_after = TRUE)
  
  save_kable(cross_table,
             file = paste0("tables/cross_", feature, ".tex"))
  
  cat("Saved: tables/cross_", feature, ".tex\n", sep = "")
}

library(ggplot2)
library(gghalves)

library(showtext)
font_add_google("Charis SIL", "CharisSIL")
showtext_auto()

dir.create("figures", showWarnings = FALSE)

df_list <- list(
  preceding_vowels = preceding_vowels,
  following_vowels = following_vowels,
  subset_glot = subset_glot
)

# Define colors for Label levels — extend as needed
label_colors <- c(
  "ɑ" = "#1b9e77", "æ" = "#d95f02", "a" = "#7570b3",
  "i" = "#e7298a", "ɪ" = "#66a61e", "u" = "#e6ab02", "ʊ" = "#a6761d",
  "ɑː" = "#1b9e77", "æː" = "#d95f02", "aː" = "#7570b3",
  "iː" = "#e7298a", "ɪː" = "#66a61e", "uː" = "#e6ab02", "ʊː" = "#a6761d"
)

segment_colors <- c(
  "Phonemic [ʔ]"   = "#1b9e77",
  "Allophonic [ʔ]" = "#d95f02",
  "gs"             = "#7570b3",
  "k"              = "#e7298a",
  "q"              = "#66a61e",
  "t"              = "#e6ab02"
)

# y axis labels for each feature
feature_labels <- c(
  "strF0_mean_z"   = "Normalized F0",
  "H1H2c_mean_z"   = "Normalized H1-H2c",
  "H1res_mean_z"   = "Normalized H1 Residual",
  "CPP_mean_log_z" = "Normalized CPP",
  "soe_mean_log_z" = "Normalized SoE",
  "sF1_mean"       = "Normalized F1",
  "sF2_mean"       = "Normalized F2"
)

feature_titles <- c(
  "strF0_mean_z"   = "f0",
  "H1H2c_mean_z"   = "H1-H2c",
  "H1res_mean_z"   = "Residual H1",
  "CPP_mean_log_z" = "Cepstral Peak Prominence",
  "soe_mean_log_z" = "Strength of Excitation",
  "sF1_mean"       = "F1",
  "sF2_mean"       = "F2"
)

for (df_name in names(df_list)) {
  
  cat("=== Plotting dataframe:", df_name, "===\n")
  df_current <- df_list[[df_name]]
  
  for (feature in names(feature_outlier_map)) {
    
    cat("  -- Feature:", feature, "\n")
    
    outlier_col <- feature_outlier_map[[feature]]
    
    ### --- Same filtering as model loop but NO distinct() collapse ---
    vars_needed <- c(feature, "CarrierType", "TargetSegment", "IPA", "Label")
    ok <- complete.cases(df_current[, vars_needed]) &
      is.finite(df_current[[feature]])
    
    df_plot <- df_current[ok, ]
    
    if (!is.na(outlier_col)) {
      df_plot <- df_plot %>%
        filter(.data[[outlier_col]] == "OK")
    }
    
    df_plot <- df_plot %>%
      mutate(TargetSegment = case_when(
        TargetSegment == "ʔ" & Condition == "phoneme"   ~ "Phonemic [ʔ]",
        TargetSegment == "gs" & Condition == "allophone" ~ "Allophonic [ʔ]",
        TRUE ~ TargetSegment
      ))
    
    # Get Label order from the data
    label_order <- sort(unique(df_plot$Label))
    
    df_plot <- df_plot %>%
      mutate(Label = factor(Label, levels = label_order))
    
    y_label <- feature_labels[[feature]]
    
    df_plot <- df_plot %>%
      mutate(SegmentCarrier = interaction(TargetSegment, CarrierType, sep = " "))
    
    segment_order <- c("t", "k", "q", "Phonemic [ʔ]", "Allophonic [ʔ]")
    
    df_plot <- df_plot %>%
      mutate(TargetSegment = factor(TargetSegment, levels = segment_order))
    
    p <- ggplot(
      df_plot,
      aes(x = TargetSegment, y = .data[[feature]], 
          fill = TargetSegment, alpha = CarrierType)
    ) +
      geom_boxplot(
        width = 0.5, outlier.shape = NA,
        position = position_dodge(width = 0.6),
        show.legend = FALSE
      ) +
      geom_point(
        data = data.frame(TargetSegment = unique(df_plot$TargetSegment)),
        aes(x = TargetSegment, y = NA_real_, fill = TargetSegment),
        shape = 22, size = 8,
        position = position_identity(),
        inherit.aes = FALSE, show.legend = TRUE, alpha = 0
      ) +
      scale_alpha_manual(
        values = c("creaky" = 0.8, "non-creaky" = 0.3),
        name   = "Carrier Type",
        labels = c("creaky" = "Yes", "non-creaky" = "No")
      ) +
      scale_fill_manual(
        values = segment_colors,
        name   = "Target Segment",
        labels = c(
          "k"              = "[k]",
          "q"              = "[q]",
          "t"              = "[t]",
          "Phonemic [ʔ]"   = "Phonemic [ʔ]",
          "Allophonic [ʔ]" = "Allophonic [ʔ]"
        )
      ) +
      coord_flip(clip = "off") +
      labs(x = NULL, y = y_label, title = feature_titles[[feature]]) +
      guides(
        fill = guide_legend(
          reverse = TRUE,
          override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
        ),
        alpha = guide_legend(
          title = "Creaky?",
          override.aes = list(
            fill   = "grey50",
            colour = "black",
            size   = 8,
            shape  = 22,
            alpha  = c(0.8, 0.3)
          )
        )
      ) +
      theme_minimal(base_size = 18, base_family = "CharisSIL") +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position    = "right",
        legend.direction   = "vertical",
        legend.background  = element_rect(fill = "white", color = "white"),
        legend.key.size    = unit(1.2, "cm"),
        legend.title       = element_text(size = 18),
        legend.text        = element_text(size = 16),
        axis.text.y        = element_blank(),
        axis.ticks.y       = element_blank(),
        axis.text.x        = element_text(size = 16),
        axis.title.x       = element_text(size = 20, margin = margin(t = 10)),
        plot.margin        = margin(20, 20, 20, 20),
        plot.title         = element_text(hjust = 0.5, size = 30)
      )
    
    ggsave(
      filename = paste0("figures/", df_name, "_", feature, ".pdf"),
      plot     = p,
      width    = 14,
      height   = 8
    )
    
    cat("  Saved: figures/", df_name, "_", feature, ".pdf\n", sep = "")
  }
}