## Background Survey Processing for PharyLary
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

# 
ms_colors <- c(
  "ħ"    = "#1b9e77", # orange
  "h" = "#d95f02", # blue
  "ʔ" = "#7570b3",  # green
  "ʕ" = "#e7298a"  # red
)

#paths
# orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/1A/survey/bkgrd_survey_November 10, 2025_07.59.csv')
orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/1A/survey/bkgrd_survey_November 10, 2025_07.59.csv')

survey_data = read.csv(orig_data_path)

## do some cleaning

header_rows <- survey_data[1:2, ]

data_rows <- survey_data[3:nrow(survey_data), ]

## remove participants not in ASAL39 poster

target_values <- c("Y0395","Y0396","Y0388","Y0401","Y0409",
                   "SD0001","SD0003","SD0005","SD0007","SD0008",
                   "SD0009","SD0011","SD0012","SD0013","SD0014",
                   "SD0015","SD0016","SD0017","SD0018","SD0024",
                   "SD0025","SD0026","SD0027","SD0028","SD0029",
                   "SD0030","SD0031","SD0033","SD0034","SD0036","SD0037")

# 1. Define your column groups
cols_to_z <- c("Q76_1", "Q75_4", "Q83_1", "Q83_2")
z_cols    <- paste0(cols_to_z, "z") # Creates: "Q76_1z", "Q75_4z", etc.

# 2. Perform the z-scoring (assuming filtered_and_sorted exists from previous steps)
filtered_and_sorted <- filtered_and_sorted %>%
  # group_by(Q2) %>%
  mutate(
    across(all_of(cols_to_z), ~ {
      val <- as.numeric(.x)
      m <- mean(val, na.rm = TRUE)
      s <- sd(val,   na.rm = TRUE)
      if (!is.na(s) && s > 0) (val - m) / s else NA_real_
    }, .names = "{.col}z")
  )

# 3. Create a comprehensive summary for ALL target columns
summary_stats <- filtered_and_sorted %>%
  summarise(
    # Count total rows once
    n_rows = n(),
    
    # Calculate Mean and Sum for all RAW columns
    across(all_of(cols_to_z), 
           list(mean = ~mean(as.numeric(.x), na.rm = TRUE), 
                sum  = ~sum(as.numeric(.x),  na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    
    # Calculate Mean for all Z-SCORE columns
    across(all_of(z_cols), 
           list(avg_z = ~mean(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )

# Optional: Pivot the results if you want them in a long list instead of one wide row
summary_long <- summary_stats %>%
  tidyr::pivot_longer(cols = everything(), 
                      names_to = "Metric", 
                      values_to = "Value")