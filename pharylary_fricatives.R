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

# orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/vs/output_preproc/pharylary_fricative_subset_mean.csv')
orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_fricative_subset_mean.csv')
# orig_data_path <- sprintf('/Users/bcl/Desktop/preproc_output.csv')
subset_mean_fric = read.csv(orig_data_path)

#### FRICATIVES ####

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