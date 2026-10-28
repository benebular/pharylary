# Data Cleaning for PharyLary
# Author: ben lang, blang@ucsd.edu
# ================================
#
# Layout
#   1. Libraries
#   2. Load the data
#   3. Clean the data ONCE   <- every relabelling and exclusion happens here; all later sections use this `data`
#   4. Select the target segments
#   5. Consonant targets: means, formant outliers, Delta-F, logs, z-scores, H1res   -> subset_mean
#   6. Vowel context: the vowels before / after each target, same measures          -> vowel_context_df
#   7. Fricatives
#   8. Output

# ---------------------------------------------------------------------------
# 1. LIBRARY LOADING
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
# 2. DATA LOADING AND PATHS
# ---------------------------------------------------------------------------

# Define data paths - check which volume is available
sync_paths <- c('/Volumes/circe',
                '/Volumes/cassandra')

base_path <- sync_paths[dir.exists(sync_paths)][1]

# Path to preprocessed data (matches from VS analysis)
data_path <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/preproc_matchesformeans_v2.csv')
data <- as_tibble(read.csv(data_path))   # a tibble, so print(x, n = ...) works everywhere below
data$interval_raw <- data$interval       # the label exactly as it came in; kept for reference

# ---------------------------------------------------------------------------
# 3. CLEAN THE DATA ONCE
# ---------------------------------------------------------------------------
# Everything that relabels or removes rows happens in this section, in this order:
#   1 whitespace -> 2 manual label swaps -> 3 collapse length marks -> 4 excluded trials -> 5 "ignore" comments
# Every later section works from the `data` that comes out of here.

## 0. Target segments (used by the checks below and by the sections after this one)
consonant_targets <- c('ħ', 'ʕ', 'h', 'ʔ', 'w', 'j')                  # the consonant analysis (subset_int / subset_mean)
targets <- c(consonant_targets, 't', 'd', 's', 'tˤ', 'dˤ', 'sˤ')       # + the extra targets used for the vowel context

## 1. Stray whitespace in the label columns -------------------------------------------------------------
# A label like " h" (leading space) is a different string from "h", so it would be treated as a
# different segment. Leading/trailing spaces, tabs and non-breaking spaces are removed from `interval`
# and `Segment`. (The TextGrid itself is unchanged; this only cleans the data read into R.)
ws_labels <- data %>%
  filter(interval != str_trim(interval) | Segment != str_trim(Segment)) %>%
  distinct(participant, phrase, interval, Segment) %>%
  mutate(interval = paste0("[", interval, "]"), Segment = paste0("[", Segment, "]"))

data <- data %>% mutate(across(c(interval, Segment), str_trim))

cat("\n--- Labels cleaned ---\n")
cat("Trials with leading/trailing whitespace in interval or Segment:", nrow(ws_labels), "\n")
if (nrow(ws_labels) > 0) print(head(ws_labels, 30))

## 2. Label swaps: fix weird characters and mismarked labels -------------------------------------------
# One row per swap: every `interval` label equal to `from` becomes `to`, but only where the other columns
# match. Leave participant, phrase or tier as NA to mean "any".
#   participant : e.g. "SD0030"                 NA = every participant
#   phrase      : e.g. 72                        NA = every phrase
#   tier        : "phonetic", "word", ...        NA = every tier   (use "phonetic" for segment labels)
# Rows are applied top to bottom, so a later row sees the result of the earlier ones.
# Labels are compared after Unicode normalisation (NFC) and trimming, so a decomposed é matches é.
# Fill the table below, or point label_swap_key_path at a CSV with the same columns (save it as UTF-8;
# empty cells count as NA).
label_swap_key_path <- NULL      # e.g. file.path(base_path, 'alldata/dissertation/vs/label_swap_key.csv')
label_swap_key <- tribble(
  ~participant, ~phrase, ~tier,      ~from, ~to,
  # NA,           NA,      "phonetic", "ʔ̞",   "ʔ",     # a weird variant of a symbol, wherever it occurs
  # "SD0035",     72,      "phonetic", " h",   "h",     # one participant's mismarked label in one phrase
)

if (!is.null(label_swap_key_path)) {
  label_swap_key <- read.csv(label_swap_key_path, fileEncoding = "UTF-8", na.strings = c("", "NA"),
                             stringsAsFactors = FALSE)
}
label_swap_key <- label_swap_key %>%
  transmute(participant = as.character(participant),
            phrase = as.integer(phrase),
            tier = as.character(tier),
            from = stringi::stri_trans_nfc(str_trim(as.character(from))),
            to   = stringi::stri_trans_nfc(str_trim(as.character(to))))
if (any(is.na(label_swap_key$from) | is.na(label_swap_key$to))) {
  stop("every row of label_swap_key needs both a from and a to label")
}

interval_nfc <- stringi::stri_trans_nfc(data$interval)       # computed once, compared against `from`
swap_log <- vector("list", nrow(label_swap_key))
for (i in seq_len(nrow(label_swap_key))) {
  k <- label_swap_key[i, ]
  hit <- interval_nfc == k$from &
    (is.na(k$participant) | data$participant == k$participant) &
    (is.na(k$phrase)      | data$phrase == k$phrase) &
    (is.na(k$tier)        | data$tier == k$tier)
  hit[is.na(hit)] <- FALSE
  data$interval[hit] <- k$to
  interval_nfc[hit] <- k$to
  swap_log[[i]] <- tibble(key_row = i, participant = k$participant, phrase = k$phrase, tier = k$tier,
                          from = k$from, to = k$to,
                          rows_changed = sum(hit), tokens_changed = n_distinct(data$token_id[hit]))
}

cat("\n--- Label swaps ---\n")
if (length(swap_log) == 0) {
  cat("No swaps listed.\n")
} else {
  swap_report <- bind_rows(swap_log)
  print(swap_report, n = Inf)
  if (any(swap_report$rows_changed == 0)) {
    cat("Swap rows that matched NOTHING (check from / participant / phrase / tier):",
        paste(swap_report$key_row[swap_report$rows_changed == 0], collapse = ", "), "\n")
  }
}

## 3. Collapse length marks and dental diacritics in the phonetic tier ---------------------------------
# So that hː, hˑ -> h    t̪ː -> t    sˤˑ -> sˤ    iː -> i ... and every later filter that compares labels
# (interval %in% targets, interval == Segment, the vowel context ...) catches all the variants.
# Only the PHONETIC tier is changed (word labels keep their marks). interval_raw still has the original label.
# To collapse more marks, add them inside the brackets (a combining mark must be written as \\uXXXX,
# e.g. \\u032A is the dental diacritic).
strip_marks <- "[ːˑ:\\u032A]"

is_phonetic <- !is.na(data$tier) & data$tier == "phonetic"
labels_u <- unique(data$interval[is_phonetic])
collapsed_u <- stringi::stri_trans_nfc(str_remove_all(labels_u, strip_marks))
collapsed_u[is.na(collapsed_u) | collapsed_u == ""] <- labels_u[is.na(collapsed_u) | collapsed_u == ""]  # never leave a label empty

before_collapse <- data$interval
data$interval[is_phonetic] <- collapsed_u[match(data$interval[is_phonetic], labels_u)]

cat("\n--- Label variants collapsed (phonetic tier) ---\n")
changed_labels <- tibble(before = before_collapse, after = data$interval) %>%
  filter(before != after) %>%
  count(before, after, sort = TRUE)
cat("Distinct labels changed:", nrow(changed_labels), "| rows affected:", sum(changed_labels$n), "\n")
print(head(changed_labels, 40))

# Labels that START like a target but are still not that target after collapsing: look here for other
# variants that should be merged too (add their mark to strip_marks, or use a swap row in step 2).
leftover_labels <- data %>%
  filter(tier == "phonetic") %>%
  count(interval_raw, interval) %>%
  filter(!interval %in% targets,
         str_detect(interval, paste0("^(", paste(targets, collapse = "|"), ")")))
cat("\nLabels that start with a target but are not one (check whether they should be merged):\n")
print(head(arrange(leftover_labels, desc(n)), 40))

## 4. Excluded trials: TextGrid labels need fixing -----------------------------------------------------
# Each row removes ONE participant's trial of ONE phrase (all tiers, all frames, every repetition of it).
# Add rows as you go through the report; `reason` is only a note for you.
# (Source: phonetic_intervals_by_phrase_segment_participant from the Python script.)

exclude_trials <- tribble(
  ~participant, ~phrase, ~reason,
  # "SD0030",     72,      "interval m, Segment ʔ",
  # "Y0401",      122,     "interval b, Segment h",
  # "SD0009",     65,      "interval m, Segment ʔ",
  # "SD0030",     90,      "interval m, Segment ʔ",
  # "SD0030",     91,      "interval m, Segment ʕ",
  # "SD0030",     114,     "interval m, Segment h",
  # "SD0030",     130,     "interval m, Segment t",
  "SD0013",     130,     "interval ???, Segment t",
  "SD0018",     74,      "weird doubled character aːʔ",
  
)

# Broader exclusions, if you ever need them (leave empty otherwise):
exclude_participants <- character(0)   # e.g. c("SD0009")  -> ALL trials of that participant
exclude_phrases      <- numeric(0)     # e.g. c(130)       -> that phrase for EVERY participant

n_rows_before <- nrow(data)
trials_present <- distinct(data, participant, phrase)

# rows in exclude_trials that match nothing in the data (typo in the participant or phrase?)
exclude_unmatched <- exclude_trials %>% anti_join(trials_present, by = c("participant", "phrase"))

data <- data %>%
  anti_join(exclude_trials, by = c("participant", "phrase")) %>%
  filter(!participant %in% exclude_participants, !phrase %in% exclude_phrases)

cat("\n--- Excluded trials ---\n")
cat("Rows removed:", n_rows_before - nrow(data), "of", n_rows_before, "\n")
cat("Trials listed:", nrow(exclude_trials), "| found in the data:", nrow(exclude_trials) - nrow(exclude_unmatched), "\n")
if (nrow(exclude_unmatched) > 0) {
  cat("Listed but NOT found in the data (check participant / phrase):\n")
  print(exclude_unmatched)
}

## 5. Trials marked "ignore" in the comments ----------------------------------------------------------
# The comment belongs to the whole trial, so the whole trial is dropped here (it used to be dropped only from
# subset_int; now the vowel context and the fricatives work without it too).
ignore_rows <- grepl("ignore", data$comments, ignore.case = TRUE)
ignored_trials <- data %>% filter(ignore_rows) %>% distinct(participant, phrase, comments)
data <- data %>% filter(!ignore_rows)

cat("\n--- Trials marked 'ignore' ---\n")
cat("Rows removed:", sum(ignore_rows), "| trials:", nrow(ignored_trials), "\n")
print(ignored_trials, n = 50)

## 6. Check the cleaned data ---------------------------------------------------------------------------
n_distinct(data$participant)
table(data$participant)

cat("\n--- Phonetic-tier labels after cleaning (tokens per label) ---\n")
print(data %>% filter(tier == "phonetic") %>% distinct(token_id, interval) %>% count(interval, sort = TRUE), n = Inf)

# ---------------------------------------------------------------------------
# 4. SELECT THE TARGET SEGMENTS
# ---------------------------------------------------------------------------
# Phonetic tier only: the glottis tier repeats some labels, so using it too would duplicate rows.
# A token is a target only where its interval label equals the stimulus Segment for that phrase;
# mismatches are reported below and left out.

target_rows <- data %>%
  filter(tier == "phonetic", interval %in% targets)

mismatched_data <- target_rows %>% filter(interval != Segment)
subset_vowel_targets <- target_rows %>% filter(interval == Segment)             # all targets (vowel context)
subset_int <- subset_vowel_targets %>% filter(interval %in% consonant_targets)  # consonant analysis

## Report summary of mismatches
cat("\n--- Mismatch Filter Summary ---\n")

if (nrow(mismatched_data) > 0) {
  # A. Unique interval units removed per participant
  interval_removal_summary <- mismatched_data %>%
    distinct(participant, phrase, interval) %>%
    group_by(participant, interval) %>%
    summarise(unique_intervals_removed = n(), .groups = "drop")
  
  cat("\n--- Unique Interval Units REMOVED per Participant ---\n")
  print(interval_removal_summary)
  
  # B. Unique intervals remaining
  interval_remaining_summary <- subset_vowel_targets %>%
    distinct(participant, phrase, interval) %>%
    group_by(participant, interval) %>%
    summarise(unique_intervals_remaining = n(), .groups = "drop")
  
  cat("\n--- Unique Interval Units REMAINING after filtering ---\n")
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
# 5. CONSONANT TARGETS
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

### flagging formant outliers
### Calculate Mahalanobis distance for formants
# (This one function is used for the consonant targets here and again for the vowels in section 6.)
# Groups under 25 rows get NA. Rows with missing F1/F2 get NA instead of making the group fail, and a group
# whose F1/F2 covariance cannot be inverted (fewer than 3 distinct F1/F2 points, or all on one line) is
# skipped with a message. Every other group gets the same numbers as the plain calculation.

vmahalanobis = function (dat) {
  dat$zF1F2 <- NA_real_
  ok <- is.finite(dat$sF1_mean) & is.finite(dat$sF2_mean)
  if (sum(ok) < 25) return(dat)
  m <- cbind(dat$sF1_mean[ok], dat$sF2_mean[ok])
  S <- cov(m)
  if (qr(S)$rank < 2 || rcond(S) < 1e-12) {
    message("Mahalanobis skipped (singular covariance): ", dat$participant[1], " / ", dat$interval[1],
            " (", nrow(unique(m)), " distinct F1/F2 points)")
    return(dat)
  }
  dat$zF1F2[ok] <- mahalanobis(m, center = colMeans(m), cov = S)
  dat
}

# Distance larger than 6 is considered as outlier    #MG: smaller numbers = more outliers. The paper I linked to uses 4.
distance_cutoff = 6

# Perform Mahalanobis on dataset
subset_mean =  subset_mean %>%                 #MG: this was cut from a dataset called "tot_fin"
  group_by(participant,interval) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
subset_mean %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2_mean, y = sF1_mean, color = zF1F2 > distance_cutoff)) +       #MG: sF2 and sF1 = Snack values from VS
  geom_point(size = 0.6) +
  facet_wrap(.~interval)+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

# Tag flagged values
subset_mean <- subset_mean %>%
  mutate(formant_outlier = if_else(!is.na(zF1F2) & zF1F2 > distance_cutoff, "outlier", NA_character_))

# Visualize the vowel formant after exclusion
subset_mean %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2_mean, y = sF1_mean)) +
  geom_point(size = 0.6) +
  #geom_text()+
  facet_wrap(.~interval)+
  #geom_density_2d() +
  #  scale_color_manual(values=c('#a6611a','#dfc27d','#018571'))+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

# ---------------------------------------------------------------------------
# FORMANT NORMALIZATION (Delta-F Method)
# ---------------------------------------------------------------------------
# Normalize formants using the Delta-F method (Johnson 2020)
# Divides each formant by a participant-specific average scaled by its F0 frequency
# (vectorised: the same numbers as the per-row mean() it replaces, without rowwise())

subset_mean <- subset_mean %>%
  mutate(
    # Compute scaling factor: weighted mean of normalized formants
    DF = (sF1_mean/0.5 + sF2_mean/1.5 + sF3_mean/2.5) / 3,
    # Normalize each formant by DF
    F1n_mean = sF1_mean/DF,
    F2n_mean = sF2_mean/DF,
    F3n_mean = sF3_mean/DF
  )

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
cat("\nNon-finite H1res_mean:", sum(!is.finite(subset_mean$H1res_mean)), "\n")

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
# 6. VOWEL CONTEXT: THE VOWELS BEFORE AND AFTER EACH TARGET SEGMENT
# ---------------------------------------------------------------------------
# Builds vowel_context_df: one row per frame of each vowel interval next to a target token, with the same
# measures as subset_mean (means, Mahalanobis, Delta-F, logs, z-scores, H1res). All of them are computed from
# the vowel rows themselves; nothing is taken from the consonant measurements (not even the H1res coefficient).
# vowel_position says whether the vowel PRECEDES or FOLLOWS the target token in the target_* columns.
# (This replaces the older sequence-based "vowel context extraction", which only labelled the target rows.)

## 0. Settings ---------------------------------------------------------------
# TRUE : the vowel must be the IMMEDIATELY neighbouring phonetic interval; a pause (empty interval)
#        between target and vowel means that vowel is not used.
# FALSE: use the nearest labelled vowel even across a pause.
require_adjacent <- TRUE

# IPA-based labels: the expected vowel next to each target is read from the stimulus transcription (the IPA
# column) and becomes the vowel's label, whatever it was labelled in the TextGrid.
# Priority: manual swap key below > IPA > the label in the TextGrid.  FALSE turns the IPA option off.
use_ipa_vowel_key <- TRUE

vowel_chars <- "aeiouyæɑɔəɛɪʊʌɤɯɜɒ"                             # vowel symbols (vowel test and IPA lookup)
vowel_pattern <- paste0("^[", vowel_chars, "\\sː:]+$")           # a label made only of vowel symbols
vowel_strip <- "[ːˑ:\\u035C\\u0361]"                             # removed from vowel labels: length marks and tie bars
# (iː -> i, a͡ʊ -> aʊ)

# Swap key: relabel vowel tokens in particular phrases so they fall into the same bins.
# One row per swap: in phrase `phrase`, every vowel labelled `from` is relabelled `to`.
# Labels may be written with or without length marks / tie bars. Applied to preceding and following vowels alike.
# Fill the table below, or point vowel_swap_key_path at a CSV with the columns phrase, from, to.
vowel_swap_key_path <- NULL      # e.g. file.path(base_path, 'alldata/dissertation/vs/vowel_swap_key.csv')
vowel_swap_key <- tribble(
  ~phrase, ~from, ~to,
  # 35,      "ɜ",   "a",
  # 112,     "ɛ",   "e",
)

if (!is.null(vowel_swap_key_path)) {
  vowel_swap_key <- read.csv(vowel_swap_key_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
}
vowel_swap_key <- vowel_swap_key %>%
  transmute(phrase = as.integer(phrase),
            from = str_remove_all(stringi::stri_trans_nfc(str_trim(as.character(from))), vowel_strip),
            to   = str_remove_all(stringi::stri_trans_nfc(str_trim(as.character(to))), vowel_strip))
dup_keys <- vowel_swap_key %>% count(phrase, from) %>% filter(n > 1)
if (nrow(dup_keys) > 0) {
  print(dup_keys)
  stop("vowel_swap_key has more than one row for the same phrase + from label")
}

## 1. Target tokens: one row per token (an interval in one file) ------------------------------------
# subset_vowel_targets (section 4) already holds the full target set, cleaned the same way as subset_int.
target_tokens <- subset_vowel_targets %>%
  distinct(participant, phrase, stem, token_id, interval, interval_idx)

cat("\n--- Target tokens (full target set) ---\n")
print(count(target_tokens, interval, sort = TRUE), n = Inf)
cat("Tokens in subset_int that are missing from the full set (should be 0):",
    length(setdiff(unique(subset_int$token_id), target_tokens$token_id)), "\n")

## 2. Order of the labelled intervals in each file's phonetic tier -------------------------------------
# interval_idx is the interval's position in its tier, so a jump of more than 1 means an empty
# interval (pause) sits in between.
phon_seq <- data %>%
  filter(tier == "phonetic") %>%
  distinct(stem, interval_idx, token_id, interval) %>%
  arrange(stem, interval_idx) %>%
  group_by(stem) %>%
  mutate(prev_token_id = lag(token_id), prev_label = lag(interval), prev_idx = lag(interval_idx),
         next_token_id = lead(token_id), next_label = lead(interval), next_idx = lead(interval_idx)) %>%
  # which occurrence of its label each token is within its file (1st h, 2nd h, ...) and how many there are
  group_by(stem, interval) %>%
  mutate(label_occ = row_number(), n_same = n()) %>%
  ungroup()

## 2b. Expected neighbouring vowels from the IPA transcription -----------------------------------------
# For every phrase: find each occurrence of the target Segment in the IPA string and read the vowel
# directly before it and directly after it (skipping consonant modifiers such as ː ˤ after the consonant).
# A vowel unit = a vowel symbol, optionally a tie-bar diphthong (a͡ɪ), plus length marks / diacritics.
# (The \\u escapes are written with a double backslash on purpose, so the regex engine reads them.)
ipa_V <- paste0("[", vowel_chars, "]")
ipa_marks <- "[ːˑ:\\u0300-\\u0360\\u0362-\\u036F]*"
ipa_vowel_unit <- paste0(ipa_V, "(?:\\u0361", ipa_V, ")?", ipa_marks)
ipa_cons_mods <- "[ːˑ:ˤʰʷʲˠ\\u0300-\\u0360\\u0362-\\u036F]*"

ipa_table <- data %>%
  distinct(phrase, IPA, Segment) %>%
  mutate(IPA = stringi::stri_trans_nfc(as.character(IPA)),
         Segment = stringi::stri_trans_nfc(as.character(Segment)))
if (anyDuplicated(ipa_table$phrase) > 0) stop("more than one IPA / Segment per phrase in data")

# one row per phrase x occurrence of the target in IPA x position, with the vowel found there (NA = no vowel)
ipa_key_long <- ipa_table %>%
  filter(!is.na(IPA), !is.na(Segment), IPA != "", Segment != "") %>%
  mutate(loc = stringi::stri_locate_all_fixed(IPA, Segment),
         start = map(loc, ~ .x[, "start"]),
         end = map(loc, ~ .x[, "end"])) %>%
  select(-loc) %>%
  unnest(c(start, end)) %>%
  filter(!is.na(start)) %>%
  # s, t, d are different symbols from sˤ, tˤ, dˤ: unless the target itself ends in ˤ, skip the places where
  # it is followed by ˤ (otherwise the s in sˤ would be counted as an s)
  filter(str_detect(Segment, "ˤ$") | str_sub(IPA, end + 1, end + 1) != "ˤ") %>%
  group_by(phrase) %>%
  mutate(occurrence = row_number()) %>%
  ungroup() %>%
  mutate(preceding = str_match(str_sub(IPA, 1, start - 1), paste0("(", ipa_vowel_unit, ")$"))[, 2],
         following = str_match(str_sub(IPA, end + 1), paste0("^", ipa_cons_mods, "(", ipa_vowel_unit, ")"))[, 2]) %>%
  pivot_longer(c(preceding, following), names_to = "vowel_position", values_to = "ipa_label") %>%
  mutate(ipa_label = str_remove_all(ipa_label, vowel_strip))

# per phrase x position: how many occurrences in IPA, and the vowel if ALL occurrences agree
ipa_key_consensus <- ipa_key_long %>%
  group_by(phrase, vowel_position) %>%
  summarise(n_ipa_occ = n_distinct(occurrence),
            ipa_label_consensus = if (n_distinct(ipa_label) == 1) first(ipa_label) else NA_character_,
            .groups = "drop")

## 3. Neighbours of each target token (long format: one row per target x position) ----------------------
nb_all <- target_tokens %>%
  left_join(phon_seq %>% select(token_id, prev_token_id, prev_label, prev_idx,
                                next_token_id, next_label, next_idx, label_occ, n_same),
            by = "token_id")

nb_long <- bind_rows(
  nb_all %>% transmute(phrase, target_token_id = token_id, target_interval = interval,
                       target_interval_idx = interval_idx,
                       target_label_occ = label_occ, target_n_same = n_same,
                       vowel_position = "preceding",
                       vowel_token_id = prev_token_id, vowel_label = prev_label, vowel_idx = prev_idx),
  nb_all %>% transmute(phrase, target_token_id = token_id, target_interval = interval,
                       target_interval_idx = interval_idx,
                       target_label_occ = label_occ, target_n_same = n_same,
                       vowel_position = "following",
                       vowel_token_id = next_token_id, vowel_label = next_label, vowel_idx = next_idx)
) %>%
  mutate(
    vowel_label_clean = str_trim(str_remove_all(vowel_label, "[\t\n\r]")),
    is_vowel = !is.na(vowel_label_clean) & str_detect(vowel_label_clean, vowel_pattern),
    adjacent = !is.na(vowel_idx) & abs(vowel_idx - target_interval_idx) == 1,
    # label with length marks / tie bars removed
    vowel_label_collapsed = str_remove_all(stringi::stri_trans_nfc(vowel_label_clean), vowel_strip)
  ) %>%
  # the manual swap key (matched on phrase + the label above)
  left_join(vowel_swap_key, by = c("phrase", "vowel_label_collapsed" = "from")) %>%
  rename(manual_to = to) %>%
  # expected vowel from IPA: the k-th occurrence of the target in IPA when the TextGrid and IPA have the
  # same number of occurrences; otherwise only if every IPA occurrence gives the same vowel
  left_join(ipa_key_long %>% select(phrase, occurrence, vowel_position, ipa_label_k = ipa_label),
            by = c("phrase", "target_label_occ" = "occurrence", "vowel_position")) %>%
  left_join(ipa_key_consensus, by = c("phrase", "vowel_position")) %>%
  mutate(
    ipa_status = case_when(
      is.na(n_ipa_occ) ~ "target not found in IPA",
      target_n_same == n_ipa_occ & !is.na(ipa_label_k) ~ "k-th occurrence",
      target_n_same == n_ipa_occ ~ "no vowel there in IPA",
      !is.na(ipa_label_consensus) ~ "counts differ, IPA occurrences agree",
      TRUE ~ "ambiguous (counts differ, IPA occurrences disagree)"),
    ipa_label = case_when(
      ipa_status == "k-th occurrence" ~ ipa_label_k,
      ipa_status == "counts differ, IPA occurrences agree" ~ ipa_label_consensus,
      TRUE ~ NA_character_),
    vowel_label_source = case_when(
      !is.na(manual_to) ~ "manual swap key",
      use_ipa_vowel_key & !is.na(ipa_label) ~ "IPA",
      TRUE ~ "TextGrid label"),
    vowel_label_final = str_remove_all(
      case_when(!is.na(manual_to) ~ manual_to,
                use_ipa_vowel_key & !is.na(ipa_label) ~ ipa_label,
                TRUE ~ vowel_label_collapsed),
      vowel_strip)                                      # no tie bars / length marks in the final label
  ) %>%
  select(-manual_to, -ipa_label_k, -ipa_label_consensus, -n_ipa_occ)

## 4. Keep the vowels ---------------------------------------------------------------------------------
vowel_links <- nb_long %>%
  filter(is_vowel, nchar(vowel_label_final) > 0, adjacent | !require_adjacent)

cat("\n--- Vowel context: neighbour check ---\n")
cat("Target tokens:", nrow(target_tokens), "\n")
cat("Neighbours that exist (label present):", sum(!is.na(nb_long$vowel_label_clean)), "\n")
cat("Neighbours that are vowels:", sum(nb_long$is_vowel), "\n")
cat("Vowels dropped because a pause sits between them and the target:",
    if (require_adjacent) sum(nb_long$is_vowel & !nb_long$adjacent) else 0, "\n")
cat("Kept vowel links:\n")
print(table(vowel_links$vowel_position))
cat("Vowel tokens used (a vowel between two targets is used twice, once per role):",
    n_distinct(vowel_links$vowel_token_id), "unique,", nrow(vowel_links), "links\n")

# Label standardisation: what was swapped or taken from IPA
cat("\n--- Vowel label standardisation (kept links) ---\n")
cat("Relabelled by the manual swap key:",
    sum(vowel_links$vowel_label_source == "manual swap key" & vowel_links$vowel_label_final != vowel_links$vowel_label_collapsed), "\n")
cat("Relabelled by the IPA transcription:",
    sum(vowel_links$vowel_label_source == "IPA" & vowel_links$vowel_label_final != vowel_links$vowel_label_collapsed), "\n")
cat("\nHow the IPA lookup went for the kept links:\n")
print(vowel_links %>% count(ipa_status, sort = TRUE), n = Inf)
cat("\nLabel changes (TextGrid label -> final label):\n")
print(vowel_links %>%
        filter(vowel_label_final != vowel_label_collapsed) %>%
        count(vowel_label_clean, vowel_label_final, vowel_label_source, sort = TRUE), n = 40)
unused_key <- vowel_swap_key %>%
  anti_join(nb_long %>% filter(is_vowel) %>% distinct(phrase, vowel_label_collapsed),
            by = c("phrase", "from" = "vowel_label_collapsed"))
if (nrow(unused_key) > 0) {
  cat("Swap-key rows that matched NO neighbouring vowel (check the phrase number and label):\n")
  print(unused_key, n = Inf)
}

# Files to double-check the IPA option against:
#  1) the full comparison, one row per phrase x position x TextGrid label x IPA label
ipa_check <- vowel_links %>%
  left_join(ipa_table %>% select(phrase, IPA, Segment), by = "phrase") %>%
  count(phrase, IPA, Segment, vowel_position, vowel_label_collapsed, ipa_label, ipa_status,
        vowel_label_source, vowel_label_final, name = "n_pairings") %>%
  arrange(ipa_status, phrase)
write.csv(ipa_check, file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_vowel_ipa_check.csv'),
          row.names = FALSE)
#  2) the same relabelling written as a swap key (phrase, from, to) that vowel_swap_key_path can read.
#     conflict = TRUE where the same TextGrid label would need two different 'to' values in one phrase
#     (a key cannot express that; resolve those by hand).
ipa_swap_key <- vowel_links %>%
  filter(vowel_label_source == "IPA", vowel_label_final != vowel_label_collapsed) %>%
  count(phrase, from = vowel_label_collapsed, to = vowel_label_final, name = "n_pairings") %>%
  group_by(phrase, from) %>%
  mutate(conflict = n_distinct(to) > 1) %>%
  ungroup() %>%
  arrange(phrase, from)
write.csv(ipa_swap_key, file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_vowel_swap_key_from_ipa.csv'),
          row.names = FALSE)
cat("IPA-derived relabellings:", nrow(ipa_swap_key), "phrase x label rows, of which",
    sum(ipa_swap_key$conflict), "conflicting. Written to pharylary_vowel_ipa_check.csv and pharylary_vowel_swap_key_from_ipa.csv\n")

# Neighbouring labels rejected as 'not a vowel': look here for vowel symbols missing from vowel_chars
# (diphthongs with a tie bar, other IPA vowels, diacritics ...)
cat("\nMost common neighbour labels REJECTED as non-vowels (check for missing vowel symbols):\n")
print(head(nb_long %>%
             filter(!is.na(vowel_label_clean), !is_vowel) %>%
             count(vowel_label_clean, sort = TRUE), 30))

## 5. The vowel rows for each (target interval, position) ---------------------------------------------
# One block of rows per vowel interval x the target it precedes/follows. context_id names that
# pairing. Everything below is measured across each block's own rows.
vowel_frames <- data %>%
  filter(token_id %in% vowel_links$vowel_token_id)

# interval = the STANDARDISED vowel label (length marks / tie bars removed, swap key and IPA applied); every
# grouping below uses it. interval_original keeps the label exactly as it came in (before any cleaning).
vowel_context_df <- vowel_links %>%
  select(token_id = vowel_token_id, vowel_position, vowel_label_final, vowel_label_source,
         ipa_label, ipa_status, target_token_id, target_interval, target_interval_idx) %>%
  inner_join(vowel_frames, by = "token_id", relationship = "many-to-many") %>%
  mutate(context_id = paste(target_token_id, vowel_position, sep = "|"),
         interval_original = interval_raw,
         interval = vowel_label_final) %>%
  select(-vowel_label_final) %>%
  relocate(interval_original, vowel_label_source, ipa_label, ipa_status, vowel_position, target_interval,
           target_interval_idx, target_token_id, context_id, .after = interval)

## 6. Same processing as the consonant targets (mirrors section 5) -----------------------------------------
## 6a. Feature means, across the rows of each vowel interval (context_id)
vowel_context_df <- vowel_context_df %>%
  group_by(context_id) %>%
  mutate(across(all_of(features), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean")) %>%
  ungroup()

## 6b. Formant outliers (Mahalanobis, same function and cutoff), grouped by participant and vowel label.
# Each vowel interval repeats ONE mean on every frame, so a participant with fewer than 3 intervals of a
# vowel has too few distinct F1/F2 points: those groups are skipped (zF1F2 = NA) and reported.
vowel_context_df <- vowel_context_df %>%
  group_by(participant, interval) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = if_else(!is.na(zF1F2) & zF1F2 > distance_cutoff, "outlier", NA_character_))

## 6c. Delta-F normalisation
vowel_context_df <- vowel_context_df %>%
  mutate(
    DF = (sF1_mean/0.5 + sF2_mean/1.5 + sF3_mean/2.5) / 3,
    F1n_mean = sF1_mean/DF,
    F2n_mean = sF2_mean/DF,
    F3n_mean = sF3_mean/DF
  )

## 6d. Log transformation (same columns as the consonants: `feats`)
vowel_context_df <- vowel_context_df %>%
  mutate(
    across(
      all_of(feats),
      ~ {
        v <- suppressWarnings(as.numeric(.x))
        ifelse(is.finite(v) & v > 0, log(v), NA_real_)
      },
      .names = "{.col}_log"
    )
  )

## 6e. Z-scoring per participant (across that participant's vowel rows) and outlier flags
vowel_context_df <- vowel_context_df %>%
  group_by(participant) %>%
  mutate(
    across(all_of(cols_to_z), ~{
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

## 6f. H1res: the same model as for the consonant targets, but FITTED ON THE VOWEL ROWS
H1c_mod_dat_vowel <- vowel_context_df %>%
  filter(
    is.finite(H1c_mean),
    is.finite(Energy_mean_log),
    !is.na(strF0_mean_z_outlier),
    formant_outlier != "outlier" | is.na(formant_outlier)
  )

mod_vowel <- lmer(H1c_mean ~ Energy_mean_log + strF0_mean +
                    (Energy_mean_log | participant),
                  data = H1c_mod_dat_vowel)

H1res_estimate_vowel <- coef(summary(mod_vowel))["Energy_mean_log", "Estimate"]
cat("\nVowel H1res regression coefficient (Energy_mean_log):", H1res_estimate_vowel, "\n")

vowel_context_df <- vowel_context_df %>%
  mutate(
    H1res_mean = if_else(
      is.finite(H1c_mean) & is.finite(Energy_mean_log),
      H1c_mean - H1res_estimate_vowel * Energy_mean_log,
      NA_real_
    )
  ) %>%
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

## 7. Summary: how many vowels were preserved, and what the Mahalanobis step had to work with --------------
cat("\n--- Vowel context dataframe ---\n")
cat("Rows (frames):", nrow(vowel_context_df), "\n")
cat("Vowel intervals x target pairings (context_id):", n_distinct(vowel_context_df$context_id), "\n")
cat("Rows by position:\n")
print(table(vowel_context_df$vowel_position))
# A vowel between two targets forms two pairings with identical rows. Its means are the same in both,
# but it counts twice in the per-participant z-scores, the Mahalanobis fit and the H1res model.
shared <- vowel_context_df %>% distinct(token_id, context_id) %>% count(token_id) %>% filter(n > 1)
cat("Vowel tokens that appear in more than one pairing:", nrow(shared), "\n")

# unique vowel tokens per vowel label
vowel_token_counts <- vowel_context_df %>%
  group_by(interval) %>%
  summarise(unique_vowel_tokens = n_distinct(token_id),   # a vowel interval in one file
            pairings = n_distinct(context_id),             # vowel interval x target it precedes/follows
            participants = n_distinct(participant),
            frames = n(), .groups = "drop") %>%
  arrange(desc(unique_vowel_tokens))
cat("\n--- Unique vowel tokens preserved, by vowel label ---\n")
print(vowel_token_counts, n = Inf)
cat("Total unique vowel tokens:", n_distinct(vowel_context_df$token_id),
    "| total pairings:", n_distinct(vowel_context_df$context_id), "\n")

# Mahalanobis groups (participant x label), vowels vs the consonant targets.
# distinct_f1f2 = number of different (F1, F2) points in the group. Every frame of an interval carries the
# same mean, so this is the number of intervals that actually inform the covariance (at least 3, and not on
# one line, for the distance to be computable).
mahal_groups <- bind_rows(
  vowels  = select(vowel_context_df, participant, interval, token_id, sF1_mean, sF2_mean, zF1F2),
  targets = select(subset_mean,      participant, interval, token_id, sF1_mean, sF2_mean, zF1F2),
  .id = "set") %>%
  group_by(set, participant, interval) %>%
  summarise(tokens = n_distinct(token_id),
            rows = n(),
            distinct_f1f2 = { ok <- is.finite(sF1_mean) & is.finite(sF2_mean)
            nrow(unique(cbind(sF1_mean[ok], sF2_mean[ok]))) },
            computed = any(!is.na(zF1F2)),
            .groups = "drop")

cat("\n--- Mahalanobis groups (participant x label): vowels vs consonant targets ---\n")
print(mahal_groups %>%
        group_by(set) %>%
        summarise(groups = n(),
                  n_computed = sum(computed),
                  n_skipped = sum(!computed),
                  median_tokens = median(tokens),
                  median_distinct_f1f2 = median(distinct_f1f2),
                  min_distinct_f1f2 = min(distinct_f1f2), .groups = "drop"))

vowel_groups <- mahal_groups %>% filter(set == "vowels") %>% select(-set)
cat("\nVowel groups WITHOUT a Mahalanobis value (fewest distinct F1/F2 points first):\n")
print(vowel_groups %>% filter(!computed) %>% arrange(distinct_f1f2, rows), n = 40)
cat("Share of vowel rows without zF1F2:", round(mean(is.na(vowel_context_df$zF1F2)), 3), "\n")

write.csv(vowel_groups,
          file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_vowel_group_counts.csv'),
          row.names = FALSE)

# Write results to CSV
vowel_output_path <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_vowel_context_subset_mean.csv')
write.csv(vowel_context_df, vowel_output_path, row.names = FALSE)
cat("Vowel context output written to:", vowel_output_path, "\n")

# ---------------------------------------------------------------------------
# 7. FRICATIVES
# ---------------------------------------------------------------------------
# FRICATIVE DATA EXTRACTION
# ---------------------------------------------------------------------------
# Extract data for fricative analysis (phonetic tier only, like the targets above)

subset_fric_data <- data %>%
  filter(tier == "phonetic",
         interval %in% c('ħ', 'ʕ', 'h', 's', 'sˤ', 'ð', 'ʁ', 'ðˤ'))

# ---------------------------------------------------------------------------
# FRICATIVE FEATURE MEANS CALCULATION
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
# 8. OUTPUT
# ---------------------------------------------------------------------------

# Write results to CSV
output_path <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_subset_mean_truncated.csv')
write.csv(subset_mean, output_path, row.names = FALSE)

cat("\n--- Pipeline Complete ---\n")
cat("Output written to:", output_path, "\n")