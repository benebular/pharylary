## Modeling for PharyLary
## author: ben lang, blang@ucsd.edu
## substantial code for splines drawn from Seyfarth & Garellek (2018)

# library(lmerTest)
library(plyr)
library(dplyr) # this checks for normality
# library(ggpubr) # this plots normality
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
# library("scales")
# library("splines")
library("stringr")
library("tidyverse")
# library("devtools")
library(grid)
library(gridExtra)

# ms_colors <- c(
#   "ħ-V"    = "#1b9e77", # orange
#   "h-V" = "#d95f02", # blue
#   "ʔ-V" = "#7570b3",  # green
#   "ʕ-V" = "#e7298a",  # red
#   "V-ħ-V"    = "#1b9e77", # orange
#   "V-h-V" = "#d95f02", # blue
#   "V-ʔ-V" = "#7570b3",  # green
#   "V-ʕ-V" = "#e7298a",  # red
#   "V-ħ"    = "#1b9e77", # orange
#   "V-h" = "#d95f02", # blue
#   "V-ʔ" = "#7570b3",  # green
#   "V-ʕ" = "#e7298a"  # red
# )
# 
# # ms_colors <- c(
# #   "ħ"    = "#1b9e77", # orange
# #   "h" = "#d95f02", # blue
# #   "ʔ" = "#7570b3",  # green
# #   "ʕ" = "#e7298a",  # red
# # )
# 
# ms_facets <- list(
#   scale_color_manual(name = NULL, values = ms_colors),
#   scale_fill_manual(name = NULL, values = ms_colors),
#   theme_light(),
#   theme(
#     aspect.ratio     = 1,
#     legend.position  = "bottom",
#     strip.background = element_blank(),
#     strip.text       = element_text(color = "black", hjust = 0, size = 11),
#     panel.border     = element_rect(color = "black", fill = NA)
#   )
# )

orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/preproc_output.csv')
# orig_data_path <- sprintf('/Users/bcl/Desktop/preproc_output.csv')
orig_data = read.csv(orig_data_path)

##### data for the intervals as extracted from overlap with tier 3 because there's no unique labels in tier 1
data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/preproc_matchesformeans.csv')
# data_path <- sprintf('/Users/bcl/Desktop/preproc_matchesformeans.csv')
data = read.csv(data_path)

###subsetting laryngeal and pharyngeal
subset = subset(data, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')
# subset = subset(data, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')
# subset = subset(data, interval == 'ħ' | interval == 'ʕ')
# subset = subset(subset, Contrast_.IPA. == 'h - ħ' | Contrast_.IPA. == 'ʔ - ʕ')

### susbetting for plots below based on sonorants
sonorant_subset = subset(orig_data, interval == 'w' | interval == 'j')
# 
# sonorant_subset <- sonorant_subset %>%
#   mutate(facet_contrast = "Sonorant /j w/")
# 
# subset <- subset %>%
#   mutate(facet_contrast = ifelse(grepl("ħ", interval), "/ħ/", "/ʕ/"))
# 
subset <- rbind(subset, sonorant_subset)

#calculate residual H1

# subset <- subset %>% group_by(participant, phrase, interval) %>% mutate(H1cz = H1c - mean(H1c, na.rm = TRUE))
# subset <- subset %>% group_by(participant, phrase, interval) %>% mutate(energyz = Energy - mean(Energy, na.rm = TRUE))
# mod_h1 <- lmer(H1c ~ energy + (energyz||participant), data = subset, REML = FALSE)
# 
# energy.factor = fixef(mod_h1)[2]
# 
# subset$H1c.resid = subset$H1cz - subset$energyz * energy.factor

## calculate mean values for all intervals in each word for each participant

subset_mean <- subset %>% group_by(participant,phrase,interval) %>% mutate(H1H2c_mean = mean(H1H2c, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(CPP_mean = mean(CPP, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(soe_mean = mean(soe, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(sF1_mean = mean(sF1, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(sF2_mean = mean(sF2, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(HNR05_mean = mean(HNR05, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(H1c_mean = mean(H1c, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(Energy_logged = log(Energy))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(energyz = Energy - mean(Energy, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(energyz_logged = log(energyz))
# subset_mean <- subset_mean %>% group_by(participant,phrase,interval) %>% mutate(H1c.resid_mean = mean(H1c.resid, na.rm = TRUE))

# write unfiltered subset_mean
write.csv(subset_mean, "/Volumes/cassandra/alldata/dissertation/vs/output_preproc/subset_mean.csv", row.names=FALSE)
# write.csv(subset_mean, "/Users/bcl/Desktop/subset_mean.csv", row.names=FALSE)


### remove the final position from subset and make variable that keeps it

subset_mean_pos_all = subset(subset_mean, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')
subset_mean = subset(subset_mean, Position == 'Initial' | Position == 'Medial')

### releveling and dummy coding

subset_mean$Position <-factor(subset_mean$Position, levels = c("Initial", "Medial")) 
# subset_mean$interval <-factor(subset_mean$interval, levels = c('ħ','ʕ','h','ʔ','j','w')) 
contrasts(subset_mean$Position) <- contr.treatment(2)
# contrasts(subset_mean$interval) <- contr.treatment(6)

## run models that don't need any outlier adjustments for f0 and formants

mod_CPP <- lmer(
  formula = CPP_mean ~
    interval*Position + (1|participant) + (1|phrase),
  data = subset_mean,
  REML = FALSE
)

mod_soe <- lmer(
  formula = soe_mean ~
    interval*Position + (1|participant) + (1|phrase),
  data = subset_mean,
  REML = FALSE
)

#### remove F1 outliers

# Calculate mean and standard deviation for each participant
subset_mean <- subset_mean %>%
  group_by(participant) %>%
  mutate(sF1_sd = sd(sF1, na.rm = TRUE)) %>%
  ungroup()

# Filter out rows where F1 is outside the range of 2.5 standard deviations from the mean
subset_mean_F1 <- subset_mean %>%
  filter(sF1 >= (sF1_mean - 3 * sF1_sd) & sF1 <= (sF1_mean + 3 * sF1_sd))

### relevel
subset_mean_F1$Position <-factor(subset_mean_F1$Position, levels = c("Initial", "Medial"))
contrasts(subset_mean_F1$Position) <- contr.treatment(2)

# run the model
mod_F1 <- lmer(
  formula = sF1_mean ~
    interval*Position + (1|participant) + (1|phrase),
  data = subset_mean_F1,
  REML = FALSE
)

### remove f0 outliers

# Calculate mean and standard deviation for each participant
subset_mean <- subset_mean %>%
  group_by(participant) %>%
  mutate(strF0_mean = mean(strF0, na.rm = TRUE), strF0_sd = sd(strF0, na.rm = TRUE)) %>%
  ungroup()

# Filter out rows where F1 is outside the range of 2.5 standard deviations from the mean
subset_mean_harmonics <- subset_mean %>%
  filter(strF0 >= (strF0_mean - 3 * strF0_sd) & strF0 <= (strF0_mean + 3 * strF0_sd))

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
subset_mean_harmonics =  subset_mean_harmonics %>%                 #MG: this was cut from a dataset called "tot_fin"
  group_by(interval) %>%
  do(vmahalanobis(.)) %>%
  ungroup() %>%
  mutate(formant_outlier = NA)

# Visualize the formants with flagged values
subset_mean_harmonics %>%
  filter(is.na(formant_outlier)) %>%
  ggplot(aes(x = sF2, y = sF1, color = zF1F2 > distance_cutoff)) +       #MG: sF2 and sF1 = Snack values from VS
  geom_point(size = 0.6) +
  facet_wrap(.~interval)+
  scale_y_reverse(limits = c(2000,0),position = "right") +
  scale_x_reverse(limits = c(3500,0),position = "top")+
  theme_bw()

# Remove flagged values
for (i in 1:nrow(subset_mean_harmonics)) {
  if (!is.na(subset_mean_harmonics$zF1F2[i])) {
    if (subset_mean_harmonics$zF1F2[i] > distance_cutoff){
      subset_mean_harmonics$formant_outlier[i] = "outlier"
    }
  }
  
}

# Visualize the vowel formant after exclusion
subset_mean_harmonics %>%
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

# Histogram of raw Energy values
ggplot(subset_mean_harmonics, aes(x = Energy)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = "Histogram of Energy", x = "Energy", y = "Density")

# Histogram of log-transformed Energy values
ggplot(subset_mean_harmonics, aes(x = log(Energy))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = "Histogram of log-transformed Energy", x = "Log(Energy)", y = "Density")

### relevel
# subset_mean_harmonics$Position <-factor(subset_mean_harmonics$Position, levels = c("Initial", "Medial")) 
# contrasts(subset_mean_harmonics$Position) <- contr.treatment(2)
subset_mean_harmonics_removed <- subset(subset_mean_harmonics, (!is.na(subset_mean_harmonics$Energy_logged) & (!is.infinite(subset_mean_harmonics$Energy_logged))))

### run harmonic models
mod_H1H2c <- lmer(
  formula = H1H2c_mean ~
    interval*Position + Energy_logged + strF0 + (1|participant) + (1|phrase),
  data = subset_mean_harmonics_removed,
  REML = FALSE
)

mod_H1c <- lmer(
  formula = H1c_mean ~ interval*Position + Energy_logged + strF0 + (1|participant) + (1|phrase),
  data = subset_mean_harmonics_removed,
  REML = FALSE
)

## grab residual H1c for plotting
energy.factor = fixef(mod_H1c)[2]

subset_mean_harmonics$H1c.resid = subset_mean_harmonics$H1c - subset_mean_harmonics$Energy * energy.factor


### just grab the first value in the intervals for each word for each participant since it's not the same within interval, word, participant
unique_data <- subset_mean %>% group_by(participant,phrase,interval) %>% summarize(
  H1H2c_mean_unique = first(H1H2c_mean),
  CPP_mean_unique = first(CPP_mean),
  soe_mean_unique = first(soe_mean),
  sF1_mean_unique = first(sF1_mean),
  sF2_mean_unique = first(sF2_mean),
  HNR05_mean_unique = first(HNR05_mean),
  # H1c.resid_mean_unique = first(H1c.resid_mean),
  #facet_contrast = first(facet_contrast),
  .groups = 'drop'  # This drops the grouping, so the data is no longer grouped after this operation
)


# combine sonorant label so they are collapsed
unique_data$interval <- str_replace(unique_data$interval, "j|w", "j/w")

rain_height <- .1

# Calculate stagger offsets based on the number of levels in 'interval'
num_levels <- length(levels(factor(unique_data$interval)))
stagger_offsets <- seq(-rain_height / 1.5, rain_height / 1.5, length.out = num_levels)


#### H1*-H2* ####
plot1 <- ggplot(unique_data, aes(x = "", y = H1H2c_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "H1*-H2* (dB)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
        )



##### CPP ####

plot2 <- ggplot(unique_data, aes(x = "", y = CPP_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
               # geom_signif(comparisons = list(c('h','ʔ','ħ','ʕ')), 
               # map_signif_level=TRUE) + 
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "CPP (dB)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  ) #+ facet_wrap(~Position, ncol = 2)

##### SoE ####

plot3 <- ggplot(unique_data, aes(x = "", y = soe_mean_unique*10, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "SoE (dB)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )


##### F1 ####

plot4 <- ggplot(unique_data, aes(x = "", y = sF1_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "F1 (Hz)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v"), 
  #                    labels = c("h - ħ", "ʔ - ʕ")), 
  #            nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

##### F2 ####

plot5 <- ggplot(unique_data, aes(x = "", y = sF2_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "F2 (Hz)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v"), 
  #                    labels = c("h - ħ", "ʔ - ʕ")), 
  #            nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", color = "white"))

##### HNR05 ####

plot6 <- ggplot(unique_data, aes(x = "", y = HNR05_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "HNR05 (dB)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v"), 
  #                    labels = c("h - ħ", "ʔ - ʕ")), 
  #            nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", color = "white"))

##### Residual H1* ####

plot7 <- ggplot(unique_data, aes(x = "", y = H1c.resid_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "SoE (dB)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )


grid.arrange(
  plot1, plot2, plot3, plot4,
  ncol = 2, nrow = 2,
  top = grid::textGrob("Acoustic Feature Means for Pharyngeal and Sonorant Consonants", gp=grid::gpar(fontsize=20))
)










#### FRICATIVES ####

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
# write.csv(subset_mean_fric, "/Volumes/circe/alldata/dissertation/vs/output_preproc/subset_mean_fric.csv", row.names=FALSE)
write.csv(subset_mean_fric, "/Volumes/cassandra/alldata/dissertation/vs/output_preproc/subset_mean_fric.csv", row.names=FALSE)

### just grab the first value in the intervals for each word for each participant since it's not the same within interval, word, participant
unique_data_fric <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% summarize(
  duration_mean_unique = first(duration_mean),
  cog_mean_unique = first(cog_mean),
  peak_mean_unique = first(peak_mean),
  peakamp_mean_unique = first(peakamp_mean),
  spectralvar_mean_unique = first(spectralvar_mean),
  skew_mean_unique = first(skew_mean),
  kurtosis_mean_unique = first(kurtosis_mean),
  #facet_contrast = first(facet_contrast),
  .groups = 'drop'  # This drops the grouping, so the data is no longer grouped after this operation
)

subset_unique_data_fric <- subset(unique_data_fric, interval == 'ħ' | interval == 'h' | interval == 's' | interval == 'sˤ' | 
                                    interval == 'ʕ' | interval == 'ʁ' | interval == 'ð' | interval == 'ðˤ') 

rain_height <- .1

# Calculate stagger offsets based on the number of levels in 'interval'
num_levels <- length(levels(factor(subset_unique_data_fric$interval)))
stagger_offsets <- seq(-rain_height / 1.5, rain_height / 1.5, length.out = num_levels)


#### COG ####
plot10 <- ggplot(subset_unique_data_fric, aes(x = "", y = cog_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "CoG (Hz)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

#### Duration ####
plot11 <- ggplot(subset_unique_data_fric, aes(x = "", y = duration_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Duration (ms)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

#### skew ####
plot12 <- ggplot(subset_unique_data_fric, aes(x = "", y = skew_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Skew",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

#### sdev ####
plot13 <- ggplot(subset_unique_data_fric, aes(x = "", y = spectralvar_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Standard Deviation",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

#### kurt ####
plot14 <- ggplot(subset_unique_data_fric, aes(x = "", y = kurt_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Kurtosis",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

#### peak ####
plot15 <- ggplot(subset_unique_data_fric, aes(x = "", y = peak_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Peak (Hz)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

#### peakamp ####
plot16 <- ggplot(subset_unique_data_fric, aes(x = "", y = peakamp_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Peak (Hz) Amplitude",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )


subset_mean_fric = subset(subset_mean_fric, interval == 'sˤ' | interval == 's')

### just grab the first value in the intervals for each word for each participant since it's not the same within interval, word, participant
unique_data_fric_s <- subset_mean_fric %>% group_by(participant,phrase,interval) %>% summarize(
  duration_mean_unique = first(duration_mean),
  cog_mean_unique = first(cog_mean),
  peak_mean_unique = first(peak_mean),
  peakamp_mean_unique = first(peakamp_mean),
  midbandpeak_mean_unique = first(midbandpeak_mean),
  minbandmin_mean_unique = first(minbandmin_mean),
  spectralvar_mean_unique = first(spectralvar_mean),
  skew_mean_unique = first(skew_mean),
  kurtosis_mean_unique = first(kurtosis_mean),
  degsibilance_mean_unique = first(degsibilance_mean),
  #facet_contrast = first(facet_contrast),
  .groups = 'drop'  # This drops the grouping, so the data is no longer grouped after this operation
)

subset_unique_data_fric <- subset(unique_data_fric_s, interval == 's' | interval == 'sˤ') 

#### degsibilance ####
plot17 <- ggplot(subset_unique_data_fric, aes(x = "", y = degsibilance_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "A(M) - Amin (dB) (Degree of Sibilance)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

#### midbandpeak ####
plot18 <- ggplot(subset_unique_data_fric, aes(x = "", y = midbandpeak_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Peak of Middle Band (Hz)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )

#### midbandpeak ####
plot19 <- ggplot(subset_unique_data_fric, aes(x = "", y = minbandmin_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               # position = position_nudge(x = -rain_height*2) +
               position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Minimum of Minimum Band (Hz)",
                     # breaks = seq(-30, 2, 30), 
                     # limits = c(-30, 30)) +
  ) +
  coord_flip() +
  # facet_wrap(~factor(facet_contrast, 
  #                    levels = c("vcl", "v","son"), 
  #                    labels = c("ħ", "ʕ","j/w")), 
  #            nrow = 1) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "white"),
        # strip.text = element_text(size = 12), # Adjust font size for facet labels
        axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
        axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
        legend.text = element_text(size = 14), # Adjust font size for legend text
        legend.title = element_text(size = 14) # Adjust font size for legend title
  )


grid.arrange(
  plot10, plot11, plot12, plot13,
  ncol = 2, nrow = 2,
  top = grid::textGrob("Acoustic Feature Means for Fricative Consonants", gp=grid::gpar(fontsize=20))
)

grid.arrange(
  plot15, plot16,
  ncol = 2, nrow = 1,
  top = grid::textGrob("Acoustic Feature Means for Fricative Consonants", gp=grid::gpar(fontsize=20))
)

grid.arrange(
  plot17, plot18, plot19,
  ncol = 2, nrow = 2,
  top = grid::textGrob("Acoustic Feature Means for Fricative Consonants", gp=grid::gpar(fontsize=20))
)


mod_cog <- lmer(
  formula = cog_mean ~
    interval + (1|participant) + (1|phrase),
  data = subset_mean_fric,
  REML = FALSE
)





###### Seyfarth & Garellek (2018) Analysis Type ##########


# orig_data_path <- sprintf('/Volumes/circe/vs/output_preproc/preproc_output.csv')
# # orig_data_path <- sprintf('/Users/bcl/Desktop/preproc_output.csv')
# orig_data = read.csv(orig_data_path)
# data_path <- sprintf('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv')
# data = read.csv(data_path)
# 
# predict_with_se <- function(model, newdata, ...) {
#   response <- terms(model)[[2]]
#   
#   predicted <- newdata
#   predicted[[response]] <- predict(model, newdata = newdata, ...)
#   
#   newdata_matrix <- model.matrix(terms(model), predicted)
#   predicted[["standard_error"]] <- sqrt(
#     diag(newdata_matrix %*% tcrossprod(vcov(model), newdata_matrix))
#   )
#   
#   predicted
# }
# 
# subset = subset(orig_data, tier == 'V-sequence')
# subset_subset = subset(subset, interval == 'ħ-V' | interval == 'h-V' | interval == 'ʔ-V' | interval == 'ʕ-V' |
#                          interval == 'V-ħ-V' | interval == 'V-h-V' | interval == 'V-ʔ-V' | interval == 'V-ʕ-V' |
#                          interval == 'V-ħ' | interval == 'V-h' | interval == 'V-ʔ' | interval == 'V-ʕ')
# 
# subset_subset$interval <- as.factor(subset_subset$interval)
# 
# initial_data <- subset_subset %>%
#   filter(Position.2 == "CV")
# 
# medial_data <- subset_subset %>%
#   filter(Position.2 == "VCV")
# 
# final_data <- subset_subset %>%
#   filter(Position.2 == "VC")
# 
# # contrasts(initial_data$interval) <- contr.sum(4)
# # contrasts(medial_data$interval) <- contr.sum(4)
# # contrasts(final_data$interval)   <- contr.sum(4)
# # 
# # subset_subset$interval <- as.factor(subset_subset$interval)
# # contrasts(subset_subset$interval) <- contr.sum(12)
# 
# initial_newdata <- crossing(
#   interval = names(ms_colors),
#   t_prop  = seq(0, 1, by = 0.01)
# )
# 
# medial_newdata <- crossing(
#   interval = names(ms_colors),
#   t_prop  = seq(0, 1, by = 0.01)
# )
# 
# medial_newdata <- crossing(
#   interval = names(ms_colors),
#   t_prop  = seq(0, 1, by = 0.01)
# )
# 
# 
# bind_rows(initial_data, medial_data, final_data) %>%
#   group_by(participant) %>%
#   mutate(
#     H1H2z    = H1H2c - mean(H1H2c, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(t_prop, H1H2z, color = interval)) +
#   geom_smooth(fill = NA) +
#   facet_wrap(~ Position.2, scales = "free_x") +
#   ms_facets +
#   labs(x = "Proportion of time", y = "H1*-H2* (centered within-speaker)")
# 
# 
# subset_subset %>%
#   # mutate(
#   #   H1H2z    = H1H2c - mean(H1H2c, na.rm = TRUE),
#   # ) %>%
#   ggplot(aes(t_prop, H1H2c, color = interval)) +
#   geom_smooth(method = 'gam', fill = NA) +
#   facet_wrap(~ factor(Position.2, c('CV','VCV','VC')), scales = "free_x", ncol=3) +
#   ms_facets +
#   labs(x = "Proportion of interval duration", y = "H1*-H2*")
# 
# subset_subset %>%
#   # mutate(
#   #   CPPz    = CPP - mean(CPP, na.rm = TRUE),
#   # ) %>%
#   ggplot(aes(t_prop, CPP, color = interval)) +
#   geom_smooth(method = 'gam', fill = NA) +
#   facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x", ncol=4) +
#   ms_facets +
#   labs(x = "Proportion of interval duration", y = "CPP")
# 
# subset_subset %>%
#   # mutate(
#   #   f0z    = strF0 - mean(strF0, na.rm = TRUE),
#   # ) %>%
#   ggplot(aes(t_prop, strF0, color = interval)) +
#   geom_smooth(method = 'gam', fill = NA) +
#   facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x", ncol=4) +
#   ms_facets +
#   labs(x = "Proportion of interval duration", y = "f0")
# 
# subset_subset %>%
#   # mutate(
#   #   f0z    = strF0 - mean(strF0, na.rm = TRUE),
#   # ) %>%
#   ggplot(aes(t_prop, energy_prop, color = interval)) +
#   geom_smooth(method = 'gam', fill = NA) +
#   facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x", ncol=4) +
#   ms_facets +
#   labs(x = "Proportion of interval duration", y = "RMS Energy (normalized)")
# 
# subset_subset %>%
#   # mutate(
#   #   SoEz    = soe - mean(soe, na.rm = TRUE),
#   # ) %>%
#   ggplot(aes(t_prop, soe, color = interval)) +
#   geom_smooth(method = 'gam', fill = NA) +
#   facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x", ncol=4) +
#   ms_facets +
#   labs(x = "Proportion of interval duration", y = "SoE")
# 
# 
# 
# 
# ### not working at the moment
# mod_H1H2c <- lmer(
#   formula = H1H2c ~
#     interval + t_ms + (1 | phrase),
#   data = subset_subset
# )
# 
# predictions <- bind_rows(
#   mutate(predict_with_se(mod_H1H2c, subset_subset, re.form = NA),
#          position = "Intervals"
#   )
# ) %>%
#   mutate(
#     interval  = fct_relevel(interval, names(ms_colors))
#   )
# 
# ggplot(
#   predictions,
#   aes(
#     x = t_norm, y = H1H2c,
#     ymin = H1H2c - standard_error, ymax = H1H2c + standard_error,
#     color = interval, fill = interval)) +
#   geom_smooth(stat = "identity", alpha = 0.25) +
#   facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x") +
#   ms_facets +
#   labs(x = "Proportion of vowel duration", y = "H1*-H2*") +
#   theme(strip.text = element_text(size = 10, margin = margin(0, 0, 5, 0)))
