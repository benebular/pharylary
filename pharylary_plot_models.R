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

#paths
# orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/vs/output_preproc/pharylary_subset_mean.csv')
orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_subset_mean.csv')

subset_mean = read.csv(orig_data_path)

###Plotting means first!

### just grab the first value in the intervals for each word for each participant since it's not the same within interval, word, participant
unique_data <- subset_mean %>%
  group_by(participant, phrase, interval) %>%
  summarize(
    
    # raw values
    strF0_mean_unique = first(strF0_mean),
    H1H2c_mean_unique = first(H1H2c_mean),
    CPP_mean_log_unique   = first(CPP_mean_log),
    soe_mean_log_unique   = first(soe_mean_log),
    F1n_mean_unique   = first(F1n_mean),
    F2n_mean_unique   = first(F2n_mean),
    H1res_mean_unique = first(H1res_mean),
    
    # normalized values
    strF0_mean_z_unique = first(strF0_mean_z),
    H1H2c_mean_z_unique = first(H1H2c_mean_z),
    CPP_mean_log_z_unique   = first(CPP_mean_log_z),
    soe_mean_log_z_unique   = first(soe_mean_log_z),
    F1n_mean_unique   = first(F1n_mean),
    F2n_mean_unique   = first(F2n_mean),
    H1res_mean_z_unique = first(H1res_mean_z),
    
    # outlier flags
    strF0_mean_z_outlier_unique = first(strF0_mean_z_outlier),
    H1H2c_mean_z_outlier_unique = first(H1H2c_mean_z_outlier),
    H1res_mean_z_outlier_unique = first(H1res_mean_z_outlier),
    CPP_mean_log_z_outlier_unique   = first(CPP_mean_log_z_outlier),
    soe_mean_log_z_outlier_unique   = first(soe_mean_log_z_outlier),
    formant_outlier_unique   = first(formant_outlier),
    .groups = "drop"
  )



# combine sonorant label so they are collapsed
unique_data$interval <- str_replace(unique_data$interval, "j|w", "j/w")
# subset_mean$interval <- str_replace(subset_mean$interval, "j|w", "j/w")

# Desired order of intervals
# Desired order of intervals# Desired order of intervals
# interval_order <- c("j/w", "ħ", "ʕ", "h", "ʔ")
interval_order <- c("h", "j/w", "ʕ", "ħ", "ʔ")

plot8 <- ggplot(
  unique_data %>%
    filter(H1res_mean_z_outlier_unique == "OK") %>%
    mutate(interval = factor(interval, levels = interval_order)),
  aes(x = interval, y = H1res_mean_z_unique, fill = interval)
) +
  # half violin
  geom_half_violin(
    side = "r", alpha = 0.7, trim = TRUE, width = 1, show.legend = FALSE
  ) +
  # boxplot
  geom_boxplot(
    width = 0.3, alpha = 0.7, outlier.shape = NA,
    position = position_nudge(x = -0.2), show.legend = FALSE
  ) +
  # invisible points for solid legend squares
  geom_point(
    data = distinct(unique_data, interval) %>%
      mutate(interval = factor(interval, levels = interval_order)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized Residual H1") +
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "left",                    # <-- clean, external left legend
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 20),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20)
  )

plot9 <- ggplot(
  unique_data %>% filter(CPP_mean_log_z_outlier_unique=="OK") %>% mutate(interval = factor(interval, levels = interval_order)),
  aes(x = interval, y = CPP_mean_log_z_unique, fill = interval)
) +
  # half violin (no legend)
  geom_half_violin(
    side = "r", alpha = 0.7, trim = TRUE, width = 1, show.legend = FALSE
  ) +
  # boxplot (no legend)
  geom_boxplot(
    width = 0.3, alpha = 0.7, outlier.shape = NA,
    position = position_nudge(x = -0.2), show.legend = FALSE
  ) +
  # invisible points for solid legend squares
  geom_point(
    data = distinct(unique_data, interval) %>%
      mutate(interval = factor(interval, levels = interval_order)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized CPP") +
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "left",              # bottom-left inside
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
    legend.title = element_text(size = 18),  # larger title
    legend.text  = element_text(size = 16),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 20),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20)
  )

plot10 <- ggplot(
  unique_data %>% filter(soe_mean_log_z_outlier_unique=="OK") %>% mutate(interval = factor(interval, levels = interval_order)),
  aes(x = interval, y = soe_mean_log_z_unique, fill = interval)
) +
  # half violin (no legend)
  geom_half_violin(
    side = "r", alpha = 0.7, trim = TRUE, width = 1, show.legend = FALSE
  ) +
  # boxplot (no legend)
  geom_boxplot(
    width = 0.3, alpha = 0.7, outlier.shape = NA,
    position = position_nudge(x = -0.2), show.legend = FALSE
  ) +
  # invisible points for solid legend squares
  geom_point(
    data = distinct(unique_data, interval) %>%
      mutate(interval = factor(interval, levels = interval_order)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized SoE") +
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "left",              # bottom-left inside
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
    legend.title = element_text(size = 18),  # larger title
    legend.text  = element_text(size = 16),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 20),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20)
  )

plot11 <- ggplot(
  unique_data %>% filter(formant_outlier_unique != "outlier" | is.na(formant_outlier_unique)) %>% mutate(interval = factor(interval, levels = interval_order)),
  aes(x = interval, y = F1n_mean_unique, fill = interval)
) +
  # half violin (no legend)
  geom_half_violin(
    side = "r", alpha = 0.7, trim = TRUE, width = 1, show.legend = FALSE
  ) +
  # boxplot (no legend)
  geom_boxplot(
    width = 0.3, alpha = 0.7, outlier.shape = NA,
    position = position_nudge(x = -0.2), show.legend = FALSE
  ) +
  # invisible points for solid legend squares
  geom_point(
    data = distinct(unique_data, interval) %>%
      mutate(interval = factor(interval, levels = interval_order)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized F1") +
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "left",              # bottom-left inside
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
    legend.title = element_text(size = 18),  # larger title
    legend.text  = element_text(size = 16),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 20),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20)
  )

plot12 <- ggplot(
  unique_data %>% filter(strF0_mean_z_outlier_unique == "OK" | is.na(strF0_mean_z_outlier_unique)) %>% mutate(interval = factor(interval, levels = interval_order)),
  aes(x = interval, y = strF0_mean_z_unique, fill = interval)
) +
  # half violin (no legend)
  geom_half_violin(
    side = "r", alpha = 0.7, trim = TRUE, width = 1, show.legend = FALSE
  ) +
  # boxplot (no legend)
  geom_boxplot(
    width = 0.3, alpha = 0.7, outlier.shape = NA,
    position = position_nudge(x = -0.2), show.legend = FALSE
  ) +
  # invisible points for solid legend squares
  geom_point(
    data = distinct(unique_data, interval) %>%
      mutate(interval = factor(interval, levels = interval_order)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized f0") +
  scale_fill_brewer(palette = "Dark2", name = "Segment") +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "left",              # bottom-left inside
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
    legend.title = element_text(size = 18),  # larger title
    legend.text  = element_text(size = 16),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 20),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20)
  )


grid.arrange(
  plot8, plot9, plot10, plot11,
  ncol = 2, nrow = 2,
  top = grid::textGrob("Acoustic Feature Means for Laryngeal and Pharyngeal Consonants", gp=grid::gpar(fontsize=20))
)


### run some models!

### remove the final position from subset and make variable that keeps it

# subset_mean_pos_all = subset(subset_mean, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')
# subset_mean = subset(subset_mean, Position == 'Initial' | Position == 'Medial')

### releveling and dummy coding

subset_mean$Position <-factor(subset_mean$Position, levels = c("Initial", "Medial"))
contrasts(subset_mean$Position) <- contr.treatment(2)
subset_mean$interval <- str_replace(subset_mean$interval, "j|w", "j/w")

## run all models with appropriate outlier removal using the flags from z-scoring and outlier flagging

# use sonorant as base since they have stable voicing
subset_mean <- subset_mean %>%
  mutate(interval = relevel(factor(interval), ref = "j/w"))

mod_f0 <- lmer(
  formula = strF0_mean ~
    interval + (1|participant) + (1|phrase),
  data = subset_mean %>% filter(strF0_mean_z_outlier=="OK")
)
summary(mod_f0)

emms_f0 <- emmeans(mod_f0, ~ interval)
pairs(emms_f0)

mod_CPP <- lmer(
  formula = CPP_mean_log ~
    interval + (1|participant) + (1|phrase),
  data = subset_mean %>% filter(CPP_mean_log_z_outlier=="OK")
)
summary(mod_CPP)

emms_CPP <- emmeans(mod_CPP, ~ interval)
pairs(emms_CPP)

mod_soe <- lmer(
  formula = soe_mean_log ~
    interval + (1|participant) + (1|phrase),
  data = subset_mean %>% filter(soe_mean_log_z_outlier=="OK")
)
summary(mod_soe)

emms_soe <- emmeans(mod_soe, ~ interval)
pairs(emms_soe)

mod_F1 <- lmer(
  formula = F1n_mean ~
    interval + (1|participant) + (1|phrase),
  data = subset_mean %>% filter(formant_outlier != "outlier" | is.na(formant_outlier)) # filter out the outliers from mahalanobis
)
summary(mod_F1)

emms_F1 <- emmeans(mod_F1, ~ interval)
pairs(emms_F1)

### run harmonic model
mod_H1res <- lmer(
  formula = H1res_mean ~ interval  + (1|participant) + (1|phrase),
  data = subset_mean %>% filter(H1res_mean_z_outlier=="OK")
)
summary(mod_H1res)

emms_H1res <- emmeans(mod_H1res, ~ interval)
pairs(emms_H1res)  # all pairwise comparisons


#### rain clouds

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



#### CPP ####

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

#### SoE ####

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


#### F1 ####

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

#### F2 ####

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

#### HNR05 ####

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

#### Residual H1* ####

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



#### Time Series Plots

#paths
# orig_data_path <- sprintf('/Volumes/circe/alldata/dissertation/vs/output_preproc/pharylary_subset_mean_time.csv')
orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_subset_mean_time.csv')
# orig_data_path <- sprintf('/Users/bcl/Desktop/preproc_output.csv')

subset_mean_time = read.csv(orig_data_path)

ms_colors <- c(
  "ħ-V"    = "#1b9e77", # orange
  "h-V" = "#d95f02", # blue
  "ʔ-V" = "#7570b3",  # green
  "ʕ-V" = "#e7298a",  # red
  "V-ħ-V"    = "#1b9e77", # orange
  "V-h-V" = "#d95f02", # blue
  "V-ʔ-V" = "#7570b3",  # green
  "V-ʕ-V" = "#e7298a",  # red
  "V-ħ"    = "#1b9e77", # orange
  "V-h" = "#d95f02", # blue
  "V-ʔ" = "#7570b3",  # green
  "V-ʕ" = "#e7298a",  # red
  "V-son" = "#000000",
  "V-son-V" = "#000000",
  "son-V" = "#000000"
)

subset_mean_time_filtered <- subset_mean_time %>%
  filter(interval == 'ħ-V' | interval == 'h-V' | interval == 'ʔ-V' | interval == 'ʕ-V' |
           interval == 'V-ħ-V' | interval == 'V-h-V' | interval == 'V-ʔ-V' | interval == 'V-ʕ-V' |
           interval == 'V-ħ' | interval == 'V-h' | interval == 'V-ʔ' | interval == 'V-ʕ')

# # removing interval == 'ʔ-V'  temporarily
# subset_mean_time_filtered <- subset(subset_mean_time, interval == 'ħ-V' | interval == 'h-V' | interval == 'ʕ-V' |
#            interval == 'V-ħ-V' | interval == 'V-h-V' | interval == 'V-ʔ-V' | interval == 'V-ʕ-V' |
#            interval == 'V-ħ' | interval == 'V-h' | interval == 'V-ʔ' | interval == 'V-ʕ')

subset_mean_time_filtered  %>% 
  filter(CPPz_outlier=="OK") %>%
  ggplot(aes(t_prop, CPPz, color = interval)) +
  geom_smooth(method = "loess", alpha=0.25) +
  scale_color_manual(name = NULL, values = ms_colors) +
  theme_bw(base_size = 18) +
  scale_x_continuous(limits=c(0,1)) +
  theme(legend.position = "none") +
  # facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','son-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-son-V','V-h','V-ʔ','V-ħ','V-ʕ','V-son')), scales = "free_x") +
  facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x") +
  labs(x = "Proportion of sequence duration", y = "CPP (dB, z-scored)")

file.path("/Volumes/cassandra/alldata/dissertation/1A/figs/", "CPP_time.pdf") %>%
  ggsave(height = 6, width = 10, device = cairo_pdf, family="Arial Unicode MS")

subset_mean_time_filtered  %>%
  filter(H1resz_outlier=="OK") %>%
  ggplot(aes(t_prop, H1resz, color = interval)) +
  geom_smooth(method = "loess", alpha=0.25) +
  scale_color_manual(name = NULL, values = ms_colors)+
  theme_bw(base_size = 18)+
  scale_x_continuous(limits=c(0,1))+
  theme(legend.position = "none")+
  # facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','son-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-son-V','V-h','V-ʔ','V-ħ','V-ʕ','V-son')), scales = "free_x") +
  facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x") +
  labs(x = "Proportion of sequence duration", y = "Residual H1 (dB, z-scored)")

subset_mean_time_filtered  %>% 
  filter(strF0z_outlier=="OK") %>%
  ggplot(aes(t_prop, strF0z, color = interval)) +
  geom_smooth(method = "loess", alpha=0.25) +
  scale_color_manual(name = NULL, values = ms_colors)+
  theme_bw(base_size = 18)+
  scale_x_continuous(limits=c(0,1))+
  theme(legend.position = "none")+
  # facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','son-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-son-V','V-h','V-ʔ','V-ħ','V-ʕ','V-son')), scales = "free_x") +
  facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x") +
  labs(x = "Proportion of sequence duration", y = "f0 (normalized)")

subset_mean_time_filtered  %>% 
  filter(soez_outlier=="OK") %>%
  ggplot(aes(t_prop, soe, color = interval)) +
  geom_smooth(method = "loess", alpha=0.25) +
  scale_color_manual(name = NULL, values = ms_colors)+
  theme_bw(base_size = 18)+
  scale_x_continuous(limits=c(0,1))+
  theme(legend.position = "none")+
  # facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','son-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-son-V','V-h','V-ʔ','V-ħ','V-ʕ','V-son')), scales = "free_x") +
  facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x") +
  labs(x = "Proportion of sequence duration", y = "SoE")

subset_mean_time_filtered  %>% 
  #filter(str_outlier=="OK") %>%
  ggplot(aes(t_prop, H1H2c, color = interval)) +
  geom_smooth(method = "loess", alpha=0.25) +
  scale_color_manual(name = NULL, values = ms_colors)+
  theme_bw(base_size = 18)+
  scale_x_continuous(limits=c(0,1))+
  theme(legend.position = "none")+
  # facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','son-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-son-V','V-h','V-ʔ','V-ħ','V-ʕ','V-son')), scales = "free_x") +
  facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x") +
  labs(x = "Proportion of sequence duration", y = "H1H2c")

subset_mean_time_filtered %>% group_by(participant, interval, t_prop) %>%
  ggplot(aes(t_prop, CPPz, color = interval)) +
  geom_smooth(method = "loess", alpha=0.25) +
  scale_color_manual(name = NULL, values = ms_colors) +
  theme_bw(base_size = 18) +
  scale_x_continuous(limits=c(0,1)) +
  theme(legend.position = "none") +
  # facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','son-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-son-V','V-h','V-ʔ','V-ħ','V-ʕ','V-son')), scales = "free_x") +
  facet_wrap(~factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x") +
  labs(x = "Proportion of sequence duration", y = "CPP (dB)")





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
