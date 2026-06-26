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

# set the features you want means for (edit this list as needed)
features <- c(
  "strF0",
  "H1H2c","H1c","HNR05",
  "CPP","soe","Energy",
  "sF1","sF2","sF3"
)

df_mean <- df_matched %>%
  group_by(Trial, Label) %>%
  mutate(
    across(all_of(features), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean")
  ) %>%
  ungroup()


## subset based on vowels adjacent and based on consonants themselves

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
