# Data Cleaning for PharyLary
# Author: ben lang, blang@ucsd.edu
# ================================

# ---------------------------------------------------------------------------
# LIBRARY LOADING
# ---------------------------------------------------------------------------

# Core libraries
library(plyr)
library(dplyr)
library(magrittr)
library(purrr)
library(tidyr)
library(lme4)
library(lmerTest)
library(stringr)
library(tidyverse)

# ---------------------------------------------------------------------------
# DATA LOADING AND PATHS
# ---------------------------------------------------------------------------

# Define data paths - check which volume is available
sync_paths <- c('/Volumes/circe/',
                '/Volumes/cassandra/')

base_path <- sync_paths[dir.exists(sync_paths)][1]

# Path to preprocessed data (matches from VS analysis)
data_path <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/preproc_matchesformeans.csv')
data <- read.csv(data_path)

n_distinct(data$participant)
table(data$participant)

# ---------------------------------------------------------------------------
# DATA CLEANING: INTERVAL SELECTION AND FILTERING
# ---------------------------------------------------------------------------
# Goal: Extract intervals for laryngeal and pharyngeal segments
# Target sounds: ħ, ʕ, h, ʔ (glottal/pharyngeal)
# Also include sonorants w, j for comparative analysis

## Subset for laryngeal/pharyngeal segments
subset_int <- data %>%
  filter(interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')

## Subset for sonorants (for comparison plots)
sonorant_subset <- data %>%
  filter(interval == 'w' | interval == 'j')

## Combine both subsets
subset_int <- bind_rows(subset_int, sonorant_subset)

# ---------------------------------------------------------------------------
# DATA CLEANING: FILTER FOR PHONETIC TIER ONLY
# ---------------------------------------------------------------------------
# Note: We filter to "phonetic" tier to avoid duplicate glottis tier rows
# (The code checks glottis tier first for inspection purposes)

subset_int <- subset_int %>%
  filter(tier == "phonetic")

# ---------------------------------------------------------------------------
# DATA CLEANING: REMOVE MISMATCHED INTERVAL/SEGMENT PAIRS
# ---------------------------------------------------------------------------
# This catches rows where the 'interval' code doesn't match the 'Segment' label
# We flag and report mismatches but filter them out for analysis

## 1. Capture mismatched rows
mismatched_data <- subset_int %>%
  filter(interval != Segment)

## 2. Keep only matching rows
subset_int <- subset_int %>%
  filter(interval == Segment)

## 3. Report summary of mismatches
cat("--- Mismatch Filter Summary ---\n")

if (nrow(mismatched_data) > 0) {
  # A. Unique interval units removed per participant
  interval_removal_summary <- mismatched_data %>%
    distinct(participant, phrase, interval) %>%
    group_by(participant, interval) %>%
    summarise(unique_intervals_removed = n(), .groups = "drop")
  
  cat("\n--- Unique Interval Units REMOVED per Participant ---\n")
  print(interval_removal_summary)
  
  # B. Unique intervals remaining
  interval_remaining_summary <- subset_int %>%
    distinct(participant, phrase, interval) %>%
    group_by(participant, interval) %>%
    summarise(unique_intervals_remaining = n(), .groups = "drop")
  
  cat("\n--- Unique Interval Units REMAINING in subset_int ---\n")
  cat("(Count of unique participant-phrase trials after filtering)\n")
  print(interval_remaining_summary)
  
  # C. Detailed mismatch breakdown
  mismatch_summary <- mismatched_data %>%
    distinct(participant, phrase, interval, Segment) %>%
    arrange(participant, phrase)
  
  cat("\n--- Detailed Breakdown: Mismatched Trial Units ---\n")
  print(mismatch_summary)
} else {
  cat("No mismatches found.\n")
}

# ---------------------------------------------------------------------------
# DATA CLEANING: REMOVE COMMENTS CONTAINING "IGNORE"
# ---------------------------------------------------------------------------
# These are trials marked to be excluded from analysis

removed_rows <- subset(subset_int, grepl("ignore", comments, ignore.case = TRUE))
subset_int <- subset(subset_int, !grepl("ignore", comments, ignore.case = TRUE))

removed_summary <- unique(removed_rows[, c("participant", "interval", "phrase", "comments")])
cat("Total rows removed:", nrow(removed_rows), "\n")
cat("Unique entries removed:\n")
print(removed_summary)

# ---------------------------------------------------------------------------
# COMMENT ANALYSIS: VOWEL CONTEXT FLAGS
# ---------------------------------------------------------------------------
# Examine remaining comments for potential issues
# Flag comments containing "vowel" - these may indicate missing/added vowels

comment_summary <- unique(subset_int[, c("comments", "participant", "phrase")])
comment_summary$has_vowel <- grepl("vowel", comment_summary$comments, ignore.case = TRUE)

## Global summary
total_vowel <- sum(comment_summary$has_vowel)
total_no_vowel <- sum(!comment_summary$has_vowel)
total_rows <- nrow(comment_summary)
total_percent <- (total_vowel / total_rows) * 100

cat("--- Global Summary ---\n")
cat("Contains 'vowel':", total_vowel, "\n")
cat("Does not contain 'vowel':", total_no_vowel, "\n")
cat("Percentage containing 'vowel':", round(total_percent, 2), "%\n\n")

## Participant-level summary
participant_stats <- comment_summary %>%
  group_by(participant) %>%
  summarise(
    Vowel_Comments = sum(has_vowel),
    Total_Comments = n(),
    Percentage = round((sum(has_vowel) / n()) * 100, 2),
    .groups = "drop"
  )

cat("--- Participant Breakdown ---\n")
print(participant_stats)

# ---------------------------------------------------------------------------
# FEATURE MEANS CALCULATION
# ---------------------------------------------------------------------------
# Calculate mean values across multiple acoustic features
# List can be edited as needed

features <- c(
  "strF0",      # Fundamental frequency (F0)
  "H1H2c",      # Harmonicity-to-noise ratio, spectral slope
  "H1c",        # Harmonicity-to-noise ratio, absolute value
  "HNR05",      # Harmonic-to-noise ratio at 0.5 ms
  "CPP",        # Cepstral peak prominence (voice quality)
  "soe",        # Spectral occupancy index
  "Energy",     # Signal energy
  "sF1",        # Formant 1 (snack values)
  "sF2",        # Formant 2 (snack values)
  "sF3"         # Formant 3 (snack values)
)

subset_mean <- subset_int %>%
  group_by(participant, phrase, interval) %>%
  mutate(
    across(all_of(features), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean")
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------
# FORMANT OUTLIER DETECTION (Mahalanobis Distance)
# ---------------------------------------------------------------------------
# Use Mahalanobis distance to identify formant outliers
# Larger distances indicate more unusual formant combinations
# Cutoff = 4 (smaller values = more outliers; paper uses 4)

## Mahalanobis distance function
vmahalanobis <- function(dat) {
  # Skip if too few observations (< 25)
  if (nrow(dat) < 25) {
    dat$zF1F2 <- NA
    return(dat)
  }
  
  # Calculate mean vector and covariance for F1/F2
  means <- c(mean(dat$sF1_mean, na.rm = TRUE), mean(dat$sF2_mean, na.rm = TRUE))
  cov_matrix <- cov(cbind(dat$sF1_mean, dat$sF2_mean))
  
  # Calculate Mahalanobis distance
  dat$zF1F2 <- mahalanobis(cbind(dat$sF1_mean, dat$sF2_mean),
                           center = means, cov = cov_matrix)
  dat
}

## Apply Mahalanobis distance function
subset_mean <- subset_mean %>%
  group_by(participant, interval) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

## Flag outliers (distance > 4)
for (i in 1:nrow(subset_mean)) {
  if (!is.na(subset_mean$zF1F2[i])) {
    if (subset_mean$zF1F2[i] > 4) {
      subset_mean$formant_outlier[i] <- "outlier"
    }
  }
}

# ---------------------------------------------------------------------------
# FORMANT NORMALIZATION (Delta-F Method)
# ---------------------------------------------------------------------------
# Normalize formants using the Delta-F method (Johnson 2020)
# Divides each formant by a participant-specific average scaled by its F0 frequency

subset_mean <- subset_mean %>%
  group_by(participant) %>%
  rowwise() %>% 
  mutate(
    # Compute scaling factor: weighted mean of normalized formants
    DF = mean(c(sF1_mean/0.5, sF2_mean/1.5, sF3_mean/2.5)),
    # Normalize each formant by DF
    F1n_mean = sF1_mean/DF,
    F2n_mean = sF2_mean/DF,
    F3n_mean = sF3_mean/DF
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------
# DISTRIBUTION CHECKS
# ---------------------------------------------------------------------------
# Plot histograms for key acoustic features to check distribution
features_to_plot <- c(
  "H1c_mean", "H1H2c_mean", "HNR05_mean", 
  "strF0_mean", "CPP_mean", "Energy_mean", "soe_mean"
)

plots <- map(features_to_plot, function(feat) {
  ggplot(subset_mean %>% filter(is.finite(.data[[feat]])),
         aes(x = .data[[feat]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, colour = "black", fill = "white") +
    geom_density(alpha = 0.2, fill = "#FF6666") +
    labs(
      title = paste("Histogram of", feat),
      x = feat,
      y = "Density"
    ) +
    theme_minimal()
})

names(plots) <- features_to_plot
walk(plots, print)

# ---------------------------------------------------------------------------
# LOG TRANSFORMATION
# ---------------------------------------------------------------------------
# Log transform positively-skewed features (soe, Energy, CPP)
# Only apply to positive, finite values

cols_to_log <- c("soe_mean", "Energy_mean", "CPP_mean")
feats <- intersect(cols_to_log, names(subset_mean))

subset_mean <- subset_mean %>%
  group_by(participant) %>%
  mutate(
    across(
      all_of(feats),
      ~ {
        v <- suppressWarnings(as.numeric(.x))
        # Log only positive, finite values
        out <- ifelse(is.finite(v) & v > 0, log(v), NA_real_)
        out
      },
      .names = "{.col}_log"
    )
  )

# Plot log-transformed distributions
plot_features_log <- c("CPP_mean_log", "Energy_mean_log", "soe_mean_log")
plots_log <- map(plot_features_log, function(feat) {
  ggplot(subset_mean %>% filter(is.finite(.data[[feat]])),
         aes(x = .data[[feat]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, colour = "black", fill = "white") +
    geom_density(alpha = 0.2, fill = "#FF6666") +
    labs(
      title = paste("Histogram of", feat),
      x = feat,
      y = "Density"
    ) +
    theme_minimal()
})

names(plots_log) <- plot_features_log
walk(plots_log, print)

# ---------------------------------------------------------------------------
# Z-SCORING
# ---------------------------------------------------------------------------
# Z-score normalizes features per participant
# Flag outliers beyond ±3 standard deviations

cols_to_z <- c("strF0_mean", "H1H2c_mean", "CPP_mean_log", 
               "Energy_mean_log", "soe_mean_log")
thresh <- 3

subset_mean <- subset_mean %>%
  group_by(participant) %>%
  mutate(
    across(all_of(cols_to_z), ~{
      m <- mean(.x, na.rm = TRUE)
      s <- sd(.x, na.rm = TRUE)
      # Z-score with safety check for sd = 0 or NA
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

# ---------------------------------------------------------------------------
# DATA QUALITY DIAGNOSIS
# ---------------------------------------------------------------------------
cat("--- Non-finite Value Diagnosis ---\n")
cat("H1c_mean non-finite:", sum(!is.finite(subset_mean$H1c_mean)), "\n")
cat("Energy_mean_log NA:", sum(is.na(subset_mean$Energy_mean_log_z)), "\n")
cat("Energy_mean_log non-finite:", sum(!is.finite(subset_mean$Energy_mean_log_z)), "\n")
cat("Energy_mean_log <= 0:", sum(subset_mean$Energy_mean_log_z <= 0, na.rm = TRUE), "\n")

# ---------------------------------------------------------------------------
# MODEL FITTING: H1res CALCULATION
# ---------------------------------------------------------------------------
# Calculate residualized H1c (H1res) to control for Energy and strF0 effects
# Model: H1c ~ Energy + strF0, with random effects for Energy by participant

## Prepare model frame (exclude non-finite and outlier values)
H1c_mod_dat <- subset_mean %>%
  filter(
    is.finite(H1c_mean),
    is.finite(Energy_mean_log),
    !is.na(strF0_mean_z_outlier),
    formant_outlier != "outlier" | is.na(formant_outlier)
  )

## Fit mixed-effects model
mod <- lmer(H1c_mean ~ Energy_mean_log + strF0_mean + 
              (Energy_mean_log | participant), 
            data = H1c_mod_dat)

## Extract coefficient for Energy_mean_log
sm <- summary(mod)
H1res_estimate <- coef(sm)["Energy_mean_log", "Estimate"]

cat("\nH1res regression coefficient (Energy_mean_log):", H1res_estimate, "\n")

## Calculate H1res per observation
subset_mean <- subset_mean %>%
  mutate(
    H1res_mean = if_else(
      is.finite(H1c_mean) & is.finite(Energy_mean_log),
      H1c_mean - H1res_estimate * Energy_mean_log,
      NA_real_
    )
  )

# Check non-finite values
cat("\nNon-finite H1res_mean:", sum(!is.finite(subset_int$H1res_mean)), "\n")

# Z-score H1res (same procedure as other features)
cols_to_z_h1res <- c("H1res_mean")
thresh <- 3

subset_mean <- subset_mean %>%
  group_by(participant) %>%
  mutate(
    across(all_of(cols_to_z_h1res), ~{
      m <- mean(.x, na.rm = TRUE)
      s <- sd(.x, na.rm = TRUE)
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

# ---------------------------------------------------------------------------
# VOWEL CONTEXT EXTRACTION
# ---------------------------------------------------------------------------
# Find vowel segments preceding and following target consonants
# Vowels are identified by IPA vowel pattern regex

## 1. Filter for phonetic tier only
data_vowel <- data %>% 
  filter(tier == "phonetic")

## 2. Identify unique tokens and their frequencies
unique_tokens <- data_vowel %>%
  pull(interval) %>%
  unique() %>%
  sort()

cat("\n--- Unique Tokens in the Interval Column ---\n")
print(unique_tokens)

token_counts <- data_vowel %>% count(interval, sort = TRUE)
cat("\n--- Token Frequency (Top 20) ---\n")
print(head(token_counts, 20))

## 3. Create occurrence IDs to handle repeated intervals in phrases
# This prevents row inflation when joining back to main data
data_vowel <- data_vowel %>%
  group_by(participant, phrase) %>%
  mutate(interval_id = data.table::rleid(interval)) %>%
  group_by(participant, phrase, interval) %>%
  mutate(occurrence = as.integer(as.factor(interval_id))) %>%
  ungroup() %>%
  select(-interval_id)

## 4. Define target phonemes for context extraction
targets <- c('ħ', 'ʕ', 'h', 'ʔ', 'w', 'j', 't', 'd', 's', 'tˤ', 'dˤ', 'sˤ')

## 5. Collapse time-series to sequence (one label per phrase)
phrase_sequences <- data_vowel %>%
  group_by(participant, phrase) %>%
  summarise(
    sequence = list(rle(as.character(interval))$values), 
    .groups = "drop"
  )

## 6. Find neighboring segments for each target
get_neighbors_indexed <- function(seq, targets, direction = "preceding") {
  indices <- which(seq %in% targets)
  if (length(indices) == 0) return(NULL)
  
  target_tracker <- list()
  
  results <- lapply(indices, function(i) {
    target_val <- seq[i]
    # Track occurrence number of this specific sound
    if (is.null(target_tracker[[target_val]])) {
      target_tracker[[target_val]] <<- 1
    } else {
      target_tracker[[target_val]] <<- target_tracker[[target_val]] + 1
    }
    
    if (direction == "preceding") {
      context_val <- if (i > 1) seq[i - 1] else NA
    } else {
      context_val <- if (i < length(seq)) seq[i + 1] else NA
    }
    data.frame(interval = target_val, context = context_val, occurrence = target_tracker[[target_val]])
  })
  bind_rows(results)
}

## Extract preceding contexts
df_preceding_check <- phrase_sequences %>%
  rowwise() %>%
  do({
    neighbors <- get_neighbors_indexed(.$sequence, targets, "preceding")
    if (is.null(neighbors)) data.frame() else cbind(data.frame(participant=.$participant, phrase=.$phrase), neighbors)
  }) %>% rename(preceding_interval = context)

## Extract following contexts
df_following_check <- phrase_sequences %>%
  rowwise() %>%
  do({
    neighbors <- get_neighbors_indexed(.$sequence, targets, "following")
    if (is.null(neighbors)) data.frame() else cbind(data.frame(participant=.$participant, phrase=.$phrase), neighbors)
  }) %>% rename(following_interval = context)

## 7. Vowel pattern matching
# Pattern includes: all IPA vowels, length marks (ː, :), and whitespace
vowel_pattern <- "^[aeiouyæɑɔəɛɪʊʌɤɯɜɒ\\sː:]+$"

## Filter for vowel contexts
filter_vowel_contexts <- function(df, context_col) {
  col_name <- sym(context_col)
  df %>%
    filter(!is.na(!!col_name)) %>%
    # Clean tabs and control characters
    mutate(!!col_name := str_remove_all(!!col_name, "[\t\n\r]")) %>%
    # Keep only valid vowel patterns
    filter(str_detect(str_trim(!!col_name), vowel_pattern))
}

df_preceding_vowels <- filter_vowel_contexts(df_preceding_check, "preceding_interval")
df_following_vowels <- filter_vowel_contexts(df_following_check, "following_interval")

## 8. Join vowel contexts back to main data
# Join on participant, phrase, interval, AND occurrence for 1-to-1 matching
data_vowel <- data_vowel %>%
  left_join(df_preceding_vowels, by = c("participant", "phrase", "interval", "occurrence")) %>%
  left_join(df_following_vowels, by = c("participant", "phrase", "interval", "occurrence"))

## 9. Final verification
cat("\n--- Final Integration Summary ---\n")
cat("Total rows in data_vowel:", nrow(data_vowel), "\n")
cat("Rows with valid preceding vowel context:", sum(!is.na(data_vowel$preceding_interval)), "\n")
cat("Rows with valid following vowel context:", sum(!is.na(data_vowel$following_interval)), "\n")

# Verify specific vowel tokens are present
kept_tokens <- unique(c(data_vowel$preceding_interval, data_vowel$following_interval))
targets_to_check <- c("ɜː", "a:", "ɒː")

cat("\nTarget Vowel Verification (Should be Kept):\n")
for (t in targets_to_check) {
  status <- if (t %in% kept_tokens) "SUCCESS: Kept" else "MISSING"
  cat(t, "->", status, "\n")
}

# ---------------------------------------------------------------------------
# FRICTIVE DATA EXCACTION
# ---------------------------------------------------------------------------
# Extract data for fricative analysis

subset_fric_data <- data %>%
  filter(interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 's' |
           interval == 'sˤ' | interval == 'ð' | interval == 'ʁ' | interval == 'ðˤ')

# ---------------------------------------------------------------------------
# FRICTIVE FEATURE MEANS CALCULATION
# ---------------------------------------------------------------------------
subset_mean_fric <- subset_fric_data %>%
  group_by(participant, phrase, interval) %>%
  mutate(duration_mean = mean(duration, na.rm = TRUE)) %>%
  mutate(cog_mean = mean(cog, na.rm = TRUE)) %>%
  mutate(peak_mean = mean(peak, na.rm = TRUE)) %>%
  mutate(peakamp_mean = mean(peakamp, na.rm = TRUE)) %>%
  mutate(midbandpeak_mean = mean(midbandpeak, na.rm = TRUE)) %>%
  mutate(minbandmin_mean = mean(minbandmin, na.rm = TRUE)) %>%
  mutate(spectralvar_mean = mean(spectral.var, na.rm = TRUE)) %>%
  mutate(skew_mean = mean(skew, na.rm = TRUE)) %>%
  mutate(kurtosis_mean = mean(kurtosis, na.rm = TRUE)) %>%
  mutate(degsibilance_mean = mean(degsibilance, na.rm = TRUE)) %>%
  ungroup()

# ---------------------------------------------------------------------------
# OUTPUT
# ---------------------------------------------------------------------------

# Write results to CSV
output_path_cassandra <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_subset_mean_truncated.csv')
write.csv(subset_mean, output_path_cassandra, row.names = FALSE)

cat("\n--- Pipeline Complete ---\n")
cat("Output written to:", output_path_cassandra, "\n")