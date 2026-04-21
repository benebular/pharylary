##### Data Cleaning for PharyLary
#### author: ben lang, blang@ucsd.edu

# library(lmerTest)
library(plyr)
library(dplyr)
# library(ggpubr)
library(magrittr)
# library(effects)
library(ggplot2)
# library(ggsignif)
library(purrr)
library(tidyr)
# library(scales)
# library(reshape2)
library(lme4)
library(lmerTest)
# library(mgcv)
# library(emmeans)
#library(forcats)
# library(psycho)
# library(janitor)
# library(data.table)
# library(psyphy)
# library("cowplot")
# library("forcats")
# library("optimx")
# library("rlang")
# library("scales")
# library("splines")
library("stringr")
library("tidyverse")
# library("devtools")
# library(grid)
# library(gridExtra)

### data for the intervals as extracted from overlap with tier 3 because there's no unique labels in tier 1
# data_path <- sprintf('/Volumes/circe/alldata/dissertation/vs/output_preproc/preproc_matchesformeans.csv')
data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/preproc_matchesformeans.csv')
data = read.csv(data_path)

### Interval and means section for abstracts
# order of operations
# average token

# count numbers of intervals
# flag places where the interval is not the target sound and remove
# remove any intervals where the comment contains ignore
# print out all the other comments to see if any need to be removed
# calculate some percentages of the comments that contain the word vowel since it usuall means a missing vowel or an added vowel
# optional filtering of all comments out, only keeping the none and spaces -- results in too few trials at the moment for clean processing

# vowels
# average token
# mahalanobis
# delta F
# stop here for formants

# then check distribution for CPP, H1, soe, f0, HNR05, H1H2c, energy
# if log normal, log transform
# then z score all, including the log ones
# all by speaker

# res H1
# remove f0 and remove f1 and f2 outliers
# calculate using means and. logged values
# z-score and flag outliers

### subsetting laryngeal and pharyngeal segments
subset_int = subset(data, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')

# temporarily show any weird rows and then filter for only the phonetic tier
# show the glottis rows (just to inspect)
subset_int %>%
  filter(tier == "glottis") %>%
  View()   # or print(), head(), etc.

# keep only phonetic rows
subset_int <- subset_int %>%
  filter(tier == "phonetic")

# subset = subset(data, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')
# subset = subset(data, interval == 'ħ' | interval == 'ʕ')
# subset = subset(subset, Contrast_.IPA. == 'h - ħ' | Contrast_.IPA. == 'ʔ - ʕ')

### susbetting for plots below based on sonorants
sonorant_subset = subset(data, interval == 'w' | interval == 'j')
# 
# sonorant_subset <- sonorant_subset %>%
#   mutate(facet_contrast = "Sonorant /j w/")
# 
# subset <- subset %>%
#   mutate(facet_contrast = ifelse(grepl("ħ", interval), "/ħ/", "/ʕ/"))
# 
subset_int <- rbind(subset_int, sonorant_subset)


## check how many intervals there are per participant
interval_check <- subset_int %>%
  group_by(participant, phrase) %>%
  summarise(intervals_present = paste(unique(interval), collapse = ", "), .groups = "drop")

print(interval_check)

## report on and remove any rows where the interval is not the target
# 1. Capture the mismatched rows
mismatched_data <- subset_int %>%
  filter(interval != Segment)

# 2. Apply the filter to keep only matches
subset_int <- subset_int %>%
  filter(interval == Segment)

# --- READOUT ---
cat("--- Mismatch Filter Summary ---\n")

if(nrow(mismatched_data) > 0) {
  
  # A. Unique Intervals REMOVED per Participant
  interval_removal_summary <- mismatched_data %>%
    distinct(participant, phrase, interval) %>% 
    group_by(participant, interval) %>%
    summarise(unique_intervals_removed = n(), .groups = "drop")
  
  cat("\n--- Unique Interval Units REMOVED per Participant ---\n")
  print(interval_removal_summary)
  
} else {
  cat("No mismatches found.\n")
}

# B. Unique Intervals REMAINING per Participant (In the filtered subset_int)
interval_remaining_summary <- subset_int %>%
  distinct(participant, phrase, interval) %>%
  group_by(participant, interval) %>%
  summarise(unique_intervals_remaining = n(), .groups = "drop")

cat("\n--- Unique Interval Units REMAINING in subset_int ---\n")
cat("(Count of unique participant-phrase trials left after filtering)\n")
print(interval_remaining_summary)

# C. Detailed mismatch list (if any)
if(nrow(mismatched_data) > 0) {
  mismatch_summary <- mismatched_data %>%
    distinct(participant, phrase, interval, Segment) %>%
    arrange(participant, phrase)
  
  cat("\n--- Detailed Breakdown: Mismatched Trial Units ---\n")
  print(mismatch_summary)
}

## remove the rows that contain the comment ignore
removed_rows <- subset(subset_int, grepl("ignore", comments, ignore.case = TRUE))
subset_int <- subset(subset_int, !grepl("ignore", comments, ignore.case = TRUE))
removed_summary <- unique(removed_rows[, c("participant", "interval", "phrase", "comments")])
cat("Total rows removed:", nrow(removed_rows), "\n")
cat("Unique entries removed:\n")
print(removed_summary)

## remaining comments to evaluate what else needs to go
comment_summary = unique(subset_int[, c("comments", "participant", "phrase")])
print(comment_summary)

## flag any comments with the word vowel in it
# 1. Create a logical flag for the presence of "vowel"
comment_summary$has_vowel <- grepl("vowel", comment_summary$comments, ignore.case = TRUE)

# --- TOTAL SUMMARY ---
total_vowel <- sum(comment_summary$has_vowel)
total_no_vowel <- sum(!comment_summary$has_vowel)
total_rows <- nrow(comment_summary)
total_percent <- (total_vowel / total_rows) * 100

cat("--- Global Summary ---\n")
cat("Contains 'vowel':", total_vowel, "\n")
cat("Does not contain 'vowel':", total_no_vowel, "\n")
cat("Percentage containing 'vowel':", round(total_percent, 2), "%\n\n")

# --- PARTICIPANT SUMMARY ---
# Calculate counts and percentages per participant
participant_stats <- aggregate(has_vowel ~ participant, data = comment_summary, 
                               FUN = function(x) {
                                 c(Count = sum(x), 
                                   Total = length(x), 
                                   Percent = round((sum(x) / length(x)) * 100, 2))
                               })

# Flatten the aggregate result into a clean data frame
participant_stats <- data.frame(participant_stats$participant, participant_stats$has_vowel)
colnames(participant_stats) <- c("Participant", "Vowel_Comments", "Total_Comments", "Percentage")

cat("--- Participant Breakdown ---\n")
print(participant_stats)

## only keep target segments

# # 1. Capture removed rows (Meaningful comments)
# removed_data <- subset_int %>%
#   filter(!grepl("^\\s*$|^none$", comments, ignore.case = TRUE))
# 
# # 2. Apply filter to main data (Keep only meaningless/none)
# subset_int <- subset_int %>%
#   filter(grepl("^\\s*$|^none$", comments, ignore.case = TRUE))
# 
# # 3. Generate initial breakdown (Total rows per unique entry)
# removal_breakdown <- removed_data %>%
#   group_by(participant, phrase, interval) %>%
#   summarise(rows_removed = n(), .groups = "drop")
# 
# cat("--- Detailed Removal Breakdown ---\n")
# print(removal_breakdown)
# 
# # --- NEW SUMMARIES ---
# 
# # 4. Summary: Unique Trials Removed per Participant
# # (Using distinct to count a participant+phrase as one entry)
# participant_summary <- removed_data %>%
#   distinct(participant, phrase) %>%
#   group_by(participant) %>%
#   summarise(unique_trials_removed = n(), .groups = "drop")
# 
# cat("\n--- Summary: Unique Trials Removed per Participant ---\n")
# print(participant_summary)
# 
# # 5. Summary: Interval Removals and Percentages
# # Count unique trials (participant+phrase) for removed vs remaining
# interval_removed_counts <- removed_data %>%
#   distinct(participant, phrase, interval) %>%
#   group_by(interval) %>%
#   summarise(n_removed = n())
# 
# interval_remaining_counts <- subset_int %>%
#   distinct(participant, phrase, interval) %>%
#   group_by(interval) %>%
#   summarise(n_remaining = n())
# 
# # Join, handle zeros, and calculate percentage of that interval removed
# interval_summary <- full_join(interval_removed_counts, interval_remaining_counts, by = "interval") %>%
#   mutate(
#     n_removed = coalesce(n_removed, 0),
#     n_remaining = coalesce(n_remaining, 0),
#     total_for_interval = n_removed + n_remaining,
#     percent_removed = round((n_removed / total_for_interval) * 100, 2)
#   ) %>%
#   select(interval, n_removed, total_for_interval, percent_removed)
# 
# cat("\n--- Summary: Percentage of Interval Trials Removed ---\n")
# print(interval_summary)

# set the features you want means for (edit this list as needed)
features <- c(
  "strF0",
  "H1H2c","H1c","HNR05",
  "CPP","soe","Energy",
  "sF1","sF2","sF3"
)

subset_mean <- subset_int %>%
  group_by(participant, phrase, interval) %>%
  mutate(
    across(all_of(features), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean")
  ) %>%
  ungroup()

### flagging formant outliers
### Calculate Mahalanobis distance for formants

vmahalanobis = function (dat) {
  if (nrow(dat) < 25) {
    dat$zF1F2 = NA
    return(dat)
  }
  means = c(mean(dat$sF1_mean, na.rm=T), mean(dat$sF2_mean, na.rm=T))
  cov = cov(cbind(dat$sF1_mean, dat$sF2_mean))
  
  dat$zF1F2 = mahalanobis(cbind(dat$sF1_mean, dat$sF2_mean),
                          center=means, cov=cov)
  dat
}

# Distance larger than 6 is considered as outlier    #MG: smaller numbers = more outliers. The paper I linked to uses 4.
distance_cutoff = 4

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
for (i in 1:nrow(subset_mean)) {
  if (!is.na(subset_mean$zF1F2[i])) {
    if (subset_mean$zF1F2[i] > distance_cutoff){
      subset_mean$formant_outlier[i] = "outlier"
    }
  }
  
}

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

#### Formant normalization 

### Delta-F method for normalizing VT length (Johnson 2020)
subset_mean = subset_mean %>%
  group_by(participant) %>%
  rowwise() %>% 
  mutate(DF = mean(c(sF1_mean/0.5, sF2_mean/1.5, sF3_mean/2.5)),
         F1n_mean = sF1_mean/DF,
         F2n_mean = sF2_mean/DF,
         F3n_mean = sF3_mean/DF)

# list of features to plot
features <- c("H1c_mean","H1H2c_mean","HNR05_mean","strF0_mean","CPP_mean","Energy_mean", "soe_mean")

# create a named list of ggplot objects
plots <- map(features, function(feat) {
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

# optionally name the list for easier reference
names(plots) <- features

# or display all in sequence
walk(plots, print)

## log transforming
cols_to_log <- c("soe_mean", "Energy_mean", "CPP_mean")
feats <- intersect(cols_to_log, names(subset_mean))

subset_mean <- subset_mean %>%
  group_by(participant) %>%
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

# list of features to plot post transform
features <- c("CPP_mean_log","Energy_mean_log", "soe_mean_log")

# create a named list of ggplot objects
plots <- map(features, function(feat) {
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

# optionally name the list for easier reference
names(plots) <- features

# or display all in sequence
walk(plots, print)

# plot log-transformed measures
# ggplot(subset_mean %>% filter(is.finite(log_Energy)),
#        aes(x = log_Energy)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  bins = 30, colour = "black", fill = "white") +
#   geom_density(alpha = 0.2, fill = "#FF6666") +
#   labs(title = "Histogram of logEnergy", x = "log_Energy", y = "Density") +
#   theme_minimal()
# 
# ggplot(subset_mean %>% filter(is.finite(log_soe)),
#        aes(x = log_soe)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  bins = 30, colour = "black", fill = "white") +
#   geom_density(alpha = 0.2, fill = "#FF6666") +
#   labs(title = "Histogram of log SoE", x = "log_soe", y = "Density") +
#   theme_minimal()


## z-scoring any existing normal or logged to normal
cols_to_z <- c("strF0_mean","H1H2c_mean","CPP_mean_log","Energy_mean_log","soe_mean_log")
thresh <- 3

subset_mean <- subset_mean %>%
  group_by(participant) %>%
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
  H1c_nonfinite    = sum(!is.finite(subset_mean$H1c_mean)),
  Energy_NA        = sum(is.na(subset_mean$Energy_mean_log_z)),
  Energy_nonfinite = sum(!is.finite(subset_mean$Energy_mean_log_z)),
  Energy_le0       = sum(subset_mean$Energy_mean_log_z <= 0, na.rm = TRUE)
)

# --- 2) Clean + prepare model frame ------------------------------------------
H1c_mod_dat <- subset_mean %>%
  filter(
    is.finite(H1c_mean),
    is.finite(Energy_mean_log_z),
    !is.na(strF0_mean_z_outlier),
  ) %>%
  filter(formant_outlier != "outlier" | is.na(formant_outlier))

# --- 3) Fit model ------------------------------------------------------------
mod <- lmer(H1c_mean ~ Energy_mean_log + strF0_mean + (Energy_mean_log | participant), data = H1c_mod_dat)

sm <- summary(mod)
H1res_estimate <- coef(sm)["Energy_mean_log", "Estimate"]

# --- 4) Compute H1res safely in-place on subset_time -------------------------
# use the same logEnergy definition as above, ensure finite computation only
subset_mean <- subset_mean %>%
  mutate(
    H1res_mean = if_else(
      is.finite(H1c_mean) & is.finite(Energy_mean_log),
      H1c_mean - H1res_estimate * Energy_mean_log,
      NA_real_
    )
  )

# optional: check non-finite values
sum(!is.finite(subset_int$H1res_mean))

## z-scoring any existing normal or logged to normal
cols_to_z <- c("H1res_mean")
thresh <- 3

subset_mean <- subset_mean %>%
  group_by(participant) %>%
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

## calculate mean values for all intervals in each word for each participant

# subset_mean <- subset_int %>% group_by(participant,phrase,interval) %>% mutate(strF0_mean = mean(strF0, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(strF0z_mean = mean(strF0z, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(H1H2c_mean = mean(H1H2c, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(CPP_mean = mean(CPP, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(soe_mean = mean(soe, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(sF1_mean = mean(sF1, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(sF2_mean = mean(sF2, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(sF3_mean = mean(sF3, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(F1n_mean = mean(F1n, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(F2n_mean = mean(F2n, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(F3n_mean = mean(F3n, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(HNR05_mean = mean(HNR05, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(H1c_mean = mean(H1c, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(H1res_mean = mean(H1res, na.rm = TRUE))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(H1resz_mean = mean(H1resz, na.rm = TRUE))


# merged_df <- data %>%
#   left_join(subset_mean, by = c("Filename","participant", "interval", "tier","phrase"))

# write unfiltered subset_mean
# write.csv(subset_mean, "/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_subset_mean.csv", row.names=FALSE)
# write.csv(subset_mean, "/Volumes/circe/alldata/dissertation/vs/output_preproc/pharylary_subset_mean.csv", row.names=FALSE)
write.csv(subset_mean, "/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_subset_mean_truncated.csv", row.names=FALSE)
# write.csv(subset_mean, "/Users/bcl/Desktop/subset_mean.csv", row.names=FALSE)


### finding the vowel means adjacent to the intervals and preprocessing them

# temporarily show any weird rows and then filter for only the phonetic tier
# show the glottis rows (just to inspect)
data %>%
  filter(tier == "glottis") %>%
  View()   # or print(), head(), etc.

# keep only phonetic rows
data_filtered <- data %>%
  filter(tier == "phonetic")

# 1. Get unique intervals and sort them alphabetically
unique_tokens <- data_filtered %>%
  pull(interval) %>%
  unique() %>%
  sort()

# 2. Print the list
cat("--- Unique Tokens in the Interval Column ---\n")
print(unique_tokens)

# 3. Optional: See how common each token is (Frequency Table)
# This helps you know if a token is a typo or a common category
token_counts <- data_filtered %>%
  count(interval, sort = TRUE)

cat("\n--- Token Frequency (Top 20) ---\n")
print(head(token_counts, 20))

library(dplyr)

# 1. Define target intervals
targets <- c('ħ', 'ʕ', 'h', 'ʔ', 'w', 'j')

# 2. Collapse the time-series into a sequence of unique labels per trial
# We use 'rle' (Run Length Encoding) logic to get the sequence of labels 
# without the thousands of repeated time-series rows.
phrase_sequences <- data_filtered %>%
  group_by(participant, phrase) %>%
  summarise(
    # This keeps the order of labels but reduces repetitions to 1
    sequence = list(rle(as.character(interval))$values), 
    .groups = "drop"
  )

# 3. Create a helper function to find neighbors in a list
get_neighbors <- function(seq, targets, direction = "preceding") {
  indices <- which(seq %in% targets)
  
  if (length(indices) == 0) return(NULL)
  
  results <- lapply(indices, function(i) {
    target_val <- seq[i]
    if (direction == "preceding") {
      context_val <- if (i > 1) seq[i - 1] else NA
    } else {
      context_val <- if (i < length(seq)) seq[i + 1] else NA
    }
    data.frame(target = target_val, context = context_val)
  })
  
  bind_rows(results)
}

# 4. Extract context into separate DataFrames
df_preceding_check <- phrase_sequences %>%
  rowwise() %>%
  do({
    neighbors <- get_neighbors(.$sequence, targets, "preceding")
    if (is.null(neighbors)) data.frame() else cbind(data.frame(participant=.$participant, phrase=.$phrase), neighbors)
  }) %>%
  rename(preceding_interval = context)

df_following_check <- phrase_sequences %>%
  rowwise() %>%
  do({
    neighbors <- get_neighbors(.$sequence, targets, "following")
    if (is.null(neighbors)) data.frame() else cbind(data.frame(participant=.$participant, phrase=.$phrase), neighbors)
  }) %>%
  rename(following_interval = context)

# --- VERIFICATION READOUT ---

cat("--- Summary: Contextual Consistency Check ---\n")

# Check for phrases where different participants have different preceding contexts
inconsistent_pre <- df_preceding_check %>%
  group_by(phrase, target) %>%
  summarise(unique_preceding = paste(unique(na.omit(preceding_interval)), collapse=", "),
            n_variants = n_distinct(preceding_interval, na.rm = TRUE), .groups = "drop") %>%
  filter(n_variants > 1)

if(nrow(inconsistent_pre) > 0) {
  cat("\n[!] WARNING: These phrases have varying PRECEDING intervals across participants:\n")
  print(inconsistent_pre)
} else {
  cat("\n[+] SUCCESS: Preceding contexts are consistent (where they exist).\n")
}

# Identify rows where the target is the absolute start or end (NA contexts)
start_of_phrase <- df_preceding_check %>% filter(is.na(preceding_interval))
end_of_phrase <- df_following_check %>% filter(is.na(following_interval))

cat("\n--- Edge Cases Found ---\n")
cat("Phrases where target is the FIRST segment (no preceding):", n_distinct(start_of_phrase$phrase), "\n")
cat("Phrases where target is the LAST segment (no following):", n_distinct(end_of_phrase$phrase), "\n")

# Display the unique context list for your inspection
cat("\n--- Example: Unique Preceding Contexts per Phrase ---\n")
print(df_preceding_check %>% distinct(phrase, target, preceding_interval) %>% head(15))

### subsetting laryngeal and pharyngeal segments
subset_vowels = subset(data, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')





### cleaning for fricatives

#### data for fricatives
# orig_fric_data_path <- sprintf('/Volumes/circe/alldata/dissertation/vs/output_preproc/subset_fricative_data.csv')
# orig_fric_data <- read.csv(orig_fric_data_path)

#### fricative plotting and analaysis
## calculate mean values for all intervals in each word for each participant

subset_fric_data = subset(data, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 's' |
                            interval == 'sˤ' | interval == 'ð' | interval == 'ʁ' | interval == 'ðˤ'
)

subset_mean_fric <- subset_fric_data %>% group_by(participant,phrase,interval) %>% mutate(duration_mean = mean(duration, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(cog_mean = mean(cog, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(peak_mean = mean(peak, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(peakamp_mean = mean(peakamp, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(midbandpeak_mean = mean(midbandpeak, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(minbandmin_mean = mean(minbandmin, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(spectralvar_mean = mean(spectral.var, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(skew_mean = mean(skew, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(kurtosis_mean = mean(kurtosis, na.rm = TRUE))
subset_mean_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% mutate(degsibilance_mean = mean(degsibilance, na.rm = TRUE))

# write unfiltered subset_mean
# write.csv(subset_mean_fric, "/Volumes/circe/alldata/dissertation/vs/output_preproc/pharylary_fricative_subset_mean.csv", row.names=FALSE)
write.csv(subset_mean_fric, "/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_fricative_subset_mean.csv", row.names=FALSE)

## VOT