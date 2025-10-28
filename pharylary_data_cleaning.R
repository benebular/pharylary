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
data_path <- sprintf('/Volumes/circe/alldata/dissertation/vs/output_preproc/preproc_matchesformeans.csv')
# data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/preproc_matchesformeans.csv')
data = read.csv(data_path)

### time-series data cleaning
subset_time = subset(data, tier == 'V-sequence')

subset_time = subset(subset_time, interval == 'ħ-V' | interval == 'h-V' | interval == 'ʔ-V' | interval == 'ʕ-V' |
                     interval == 'V-ħ-V' | interval == 'V-h-V' | interval == 'V-ʔ-V' | interval == 'V-ʕ-V' |
                     interval == 'V-ħ' | interval == 'V-h' | interval == 'V-ʔ' | interval == 'V-ʕ' |
                      interval == 'V-son' | interval == 'V-son-V' | interval == 'son-V'
                     )

# subset_time = subset_time %>%
#   group_by(participant) %>%
#   mutate(strF0z = (strF0 - mean(strF0, na.rm = T))/sd(strF0, na.rm = T)) %>%
#   ungroup()
# 
# subset_time = subset_time %>%
#   mutate(str_outlier = if_else(abs(strF0z) > 3, "outlier", "OK"))

cols_to_z <- c("strF0", "CPP", "soe", "H1c", "H1H2c", "sF3", "HNR05", "Energy")
thresh <- 3

subset_time <- subset_time %>%
  group_by(participant) %>%
  mutate(
    across(all_of(cols_to_z), ~{
      m <- mean(.x, na.rm = TRUE)
      s <- sd(.x,   na.rm = TRUE)
      if (is.finite(s) && s > 0) (.x - m) / s else NA_real_
    }, .names = "{.col}z")
  ) %>%
  ungroup() %>%
  mutate(
    across(
      ends_with("z"),
      ~ if_else(is.finite(.x) & abs(.x) > thresh, "outlier", "OK"),
      .names = "{.col}_outlier"
    )
  )

### flagging formant outliers
### Calculate Mahalanobis distance for formants

vmahalanobis = function (dat) {
  if (nrow(dat) < 25) {
    dat$zF1F2 = NA
    return(dat)
  }
  means = c(mean(dat$sF1, na.rm=T), mean(dat$sF2, na.rm=T))
  cov = cov(cbind(dat$sF1, dat$sF2))
  
  dat$zF1F2 = mahalanobis(cbind(dat$sF1, dat$sF2),
                          center=means, cov=cov)
  dat
}

# Distance larger than 6 is considered as outlier    #MG: smaller numbers = more outliers. The paper I linked to uses 4.
distance_cutoff = 6

# Perform Mahalanobis on dataset
subset_time =  subset_time %>%                 #MG: this was cut from a dataset called "tot_fin"
  group_by(interval) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
subset_time %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1, color = zF1F2 > distance_cutoff)) +       #MG: sF2 and sF1 = Snack values from VS
  geom_point(size = 0.6) +
  facet_wrap(.~interval)+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

# Tag flagged values
for (i in 1:nrow(subset_time)) {
  if (!is.na(subset_time$zF1F2[i])) {
    if (subset_time$zF1F2[i] > distance_cutoff){
      subset_time$formant_outlier[i] = "outlier"
    }
  }
  
}

# Visualize the vowel formant after exclusion
subset_time %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1)) +
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
subset_time = subset_time %>%
  rowwise() %>% 
  mutate(DF = mean(c(sF1/0.5, sF2/1.5, sF3/2.5)),
         F1n = sF1/DF,
         F2n = sF2/DF,
         F3n = sF3/DF)

# --- 1) Diagnose quickly -----------------------------------------------------
c(
  H1c_nonfinite    = sum(!is.finite(subset_time$H1c)),
  Energy_NA        = sum(is.na(subset_time$Energy)),
  Energy_nonfinite = sum(!is.finite(subset_time$Energy)),
  Energy_le0       = sum(subset_time$Energy <= 0, na.rm = TRUE)
)

# --- 2) Clean + prepare model frame ------------------------------------------
H1c_mod_dat <- subset_time %>%
  mutate(
    participant = as.factor(participant),
    logEnergy   = if_else(Energy > 0 & is.finite(Energy), log(Energy), NA_real_)
  ) %>%
  filter(
    is.finite(H1c),
    is.finite(logEnergy),
    !is.na(strF0z_outlier),
    !is.na(H1cz_outlier),
    Energy >= 0
  )

# --- 3) Fit model ------------------------------------------------------------
mod <- lmer(H1c ~ logEnergy + strF0z + (logEnergy || participant), data = H1c_mod_dat)

sm <- summary(mod)
H1res_estimate <- coef(sm)["logEnergy", "Estimate"]

# --- 4) Compute H1res safely in-place on subset_time -------------------------
# use the same logEnergy definition as above, ensure finite computation only
subset_time <- subset_time %>%
  mutate(
    logEnergy = if_else(Energy > 0 & is.finite(Energy), log(Energy), NA_real_)
  ) %>%
  mutate(
    H1res = if_else(
      is.finite(H1c) & is.finite(logEnergy),
      H1c - H1res_estimate * logEnergy,
      NA_real_
    )
  )

# optional: check non-finite values
sum(!is.finite(subset_time$H1res))

subset_time <- subset_time %>%
  group_by(participant) %>%
  mutate(
    H1resz = (H1res - mean(H1res, na.rm = TRUE)) / sd(H1res, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    H1resz_outlier = if_else(abs(H1resz) > 3, "outlier", "OK")
  )

## calculate mean values for all intervals in each word for each participant

subset_mean_time <- subset_time %>% group_by(participant,phrase,interval) %>% mutate(strF0_mean = mean(strF0, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(strF0z_mean = mean(strF0z, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(H1H2c_mean = mean(H1H2c, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(CPP_mean = mean(CPP, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(soe_mean = mean(soe, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(sF1_mean = mean(sF1, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(sF2_mean = mean(sF2, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(sF3_mean = mean(sF3, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(F1n_mean = mean(F1n, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(F2n_mean = mean(F2n, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(F3n_mean = mean(F3n, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(HNR05_mean = mean(HNR05, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(H1c_mean = mean(H1c, na.rm = TRUE))
subset_mean_time <- subset_mean_time %>% group_by(participant,phrase,interval) %>% mutate(H1res_mean = mean(H1res, na.rm = TRUE))

# Histogram of raw Energy values
ggplot(subset_mean_time, aes(x = Energy)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = "Histogram of Energy", x = "Energy", y = "Density")

# Histogram of log-transformed Energy values
ggplot(subset_mean_time, aes(x = log(Energy))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = "Histogram of log-transformed Energy", x = "Log(Energy)", y = "Density")


# write unfiltered subset_mean
write.csv(subset_mean_time, "/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_subset_mean_time.csv", row.names=FALSE)
# write.csv(subset_mean, "/Volumes/circe/alldata/dissertation/vs/output_preproc/pharylary_subset_mean_time.csv", row.names=FALSE)
# write.csv(subset_mean, "/Users/bcl/Desktop/subset_mean.csv", row.names=FALSE)







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

### remove f0 outliers

# subset_time = subset_time %>%
#   group_by(participant) %>%
#   mutate(strF0z = (strF0 - mean(strF0, na.rm = T))/sd(strF0, na.rm = T)) %>%
#   ungroup()
# 
# subset_time = subset_time %>%
#   mutate(str_outlier = if_else(abs(strF0z) > 3, "outlier", "OK"))

cols_to_z <- c("strF0", "CPP", "soe", "H1c", "H1H2c","Energy")
thresh <- 3

subset_int <- subset_int %>%
  group_by(participant) %>%
  mutate(
    across(all_of(cols_to_z), ~{
      m <- mean(.x, na.rm = TRUE)
      s <- sd(.x,   na.rm = TRUE)
      if (is.finite(s) && s > 0) (.x - m) / s else NA_real_
    }, .names = "{.col}z")
  ) %>%
  ungroup() %>%
  mutate(
    across(
      ends_with("z"),
      ~ if_else(is.finite(.x) & abs(.x) > thresh, "outlier", "OK"),
      .names = "{.col}_outlier"
    )
  )

### flagging formant outliers
### Calculate Mahalanobis distance for formants

vmahalanobis = function (dat) {
  if (nrow(dat) < 25) {
    dat$zF1F2 = NA
    return(dat)
  }
  means = c(mean(dat$sF1, na.rm=T), mean(dat$sF2, na.rm=T))
  cov = cov(cbind(dat$sF1, dat$sF2))
  
  dat$zF1F2 = mahalanobis(cbind(dat$sF1, dat$sF2),
                          center=means, cov=cov)
  dat
}

# Distance larger than 6 is considered as outlier    #MG: smaller numbers = more outliers. The paper I linked to uses 4.
distance_cutoff = 6

# Perform Mahalanobis on dataset
subset_int =  subset_int %>%                 #MG: this was cut from a dataset called "tot_fin"
  group_by(interval) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
subset_int %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1, color = zF1F2 > distance_cutoff)) +       #MG: sF2 and sF1 = Snack values from VS
  geom_point(size = 0.6) +
  facet_wrap(.~interval)+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

# Tag flagged values
for (i in 1:nrow(subset_int)) {
  if (!is.na(subset_int$zF1F2[i])) {
    if (subset_int$zF1F2[i] > distance_cutoff){
      subset_int$formant_outlier[i] = "outlier"
    }
  }
  
}

# Visualize the vowel formant after exclusion
subset_int %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1)) +
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
subset_int = subset_int %>%
  rowwise() %>% 
  mutate(DF = mean(c(sF1/0.5, sF2/1.5, sF3/2.5)),
         F1n = sF1/DF,
         F2n = sF2/DF,
         F3n = sF3/DF)

# --- 1) Diagnose quickly -----------------------------------------------------
c(
  H1c_nonfinite    = sum(!is.finite(subset_int$H1c)),
  Energy_NA        = sum(is.na(subset_int$Energy)),
  Energy_nonfinite = sum(!is.finite(subset_int$Energy)),
  Energy_le0       = sum(subset_int$Energy <= 0, na.rm = TRUE)
)

# --- 2) Clean + prepare model frame ------------------------------------------
H1c_mod_dat <- subset_int %>%
  mutate(
    participant = as.factor(participant),
    logEnergy   = if_else(Energy > 0 & is.finite(Energy), log(Energy), NA_real_)
  ) %>%
  filter(
    is.finite(H1c),
    is.finite(logEnergy),
    !is.na(strF0z_outlier),
    !is.na(H1cz_outlier),
    Energy >= 0
  )

# --- 3) Fit model ------------------------------------------------------------
mod <- lmer(H1c ~ logEnergy + strF0z + (logEnergy || participant), data = H1c_mod_dat)

sm <- summary(mod)
H1res_estimate <- coef(sm)["logEnergy", "Estimate"]

# --- 4) Compute H1res safely in-place on subset_time -------------------------
# use the same logEnergy definition as above, ensure finite computation only
subset_int <- subset_int %>%
  mutate(
    logEnergy = if_else(Energy > 0 & is.finite(Energy), log(Energy), NA_real_)
  ) %>%
  mutate(
    H1res = if_else(
      is.finite(H1c) & is.finite(logEnergy),
      H1c - H1res_estimate * logEnergy,
      NA_real_
    )
  )

# optional: check non-finite values
sum(!is.finite(subset_int$H1res))

subset_int <- subset_int %>%
  group_by(participant) %>%
  mutate(
    H1resz = (H1res - mean(H1res, na.rm = TRUE)) / sd(H1res, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    H1resz_outlier = if_else(abs(H1resz) > 3, "outlier", "OK")
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


# set the features you want means for (edit this list as needed)
features <- c(
  "strF0","H1H2c","CPP","soe",
  "strF0z","H1H2cz","CPPz","soez",
  "sF1","sF2","sF3","F1n","F2n","F3n",
  "H1c","H1res","H1resz"
)

subset_mean <- subset_int %>%
  group_by(participant, phrase, interval) %>%
  mutate(
    across(all_of(features), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean")
  ) %>%
  ungroup()

# Histogram of log-transformed Energy values
ggplot(subset_mean, aes(x = log(Energy))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = "Histogram of log-transformed Energy", x = "Log(Energy)", y = "Density")

# merged_df <- data %>%
#   left_join(subset_mean, by = c("Filename","participant", "interval", "tier","phrase"))

# write unfiltered subset_mean
# write.csv(subset_mean, "/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_subset_mean.csv", row.names=FALSE)
write.csv(subset_mean, "/Volumes/circe/alldata/dissertation/vs/output_preproc/pharylary_subset_mean.csv", row.names=FALSE)
# write.csv(subset_mean, "/Users/bcl/Desktop/subset_mean.csv", row.names=FALSE)



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