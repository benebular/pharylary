## spline models from Seyfarth & Garellek (2018)
## author: ben lang, blang@ucsd.edu
## substantial code drawn from Seyfarth & Garellek (2018)

# library(lmerTest)
library(plyr)
library(dplyr) # this checks for normality
# library(ggpubr) # this plots normality
library(magrittr)
# library(effects)
library(ggplot2)
library(tidyr)
# library(scales)
# library(reshape2)
library(lme4)
# library(emmeans)
#library(forcats)
# library(psycho)
# library(janitor)
#library(data.table)
# library(psyphy)
# library("cowplot")
# library("forcats")
# library("lme4")
# library("optimx")
# library("rlang")
# library("scales")
library("splines")
library("stringr")
library("tidyverse")
library("devtools")
library(gridExtra)
library(grid)

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

# ms_colors <- c(
#   "ħ"    = "#1b9e77", # orange
#   "h" = "#d95f02", # blue
#   "ʔ" = "#7570b3",  # green
#   "ʕ" = "#e7298a",  # red
# )

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

# data_path <- sprintf('/Volumes/circe/vs/output_preproc/preproc_output.csv')
# data_path <- sprintf('/Users/bcl/Desktop/preproc_output.csv')
data_path <- sprintf('/Volumes/circe/vs/output_preproc/preproc_matchesformeans.csv')
data = read.csv(data_path)

subset = subset(data, interval == 'ħ' | interval == 'h' | interval == 'ʔ' | interval == 'ʕ')
# subset = subset(subset, Contrast_.IPA. == 'h - ħ' | Contrast_.IPA. == 'ʔ - ʕ')
subset <- subset %>%
  mutate(facet_contrast = ifelse(grepl("h|ħ", interval), "vcl", "v"))

subset_mean <- subset %>% group_by(phrase,interval) %>% mutate(H1H2c_mean = mean(H1H2c, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(phrase,interval) %>% mutate(CPP_mean = mean(CPP, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(phrase,interval) %>% mutate(soe_mean = mean(soe, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(phrase,interval) %>% mutate(sF1_mean = mean(sF1, na.rm = TRUE))
subset_mean <- subset_mean %>% group_by(phrase,interval) %>% mutate(sF2_mean = mean(sF2, na.rm = TRUE))

unique_data <- subset_mean %>% group_by(phrase,interval) %>% summarize(
  H1H2c_mean_unique = first(H1H2c_mean),
  CPP_mean_unique = first(CPP_mean),
  soe_mean_unique = first(soe_mean),
  sF1_mean_unique = first(sF1_mean),
  sF2_mean_unique = first(sF2_mean),
  facet_contrast = first(facet_contrast),
  .groups = 'drop'  # This drops the grouping, so the data is no longer grouped after this operation
)

write.csv(subset_mean, "/Volumes/circe/vs/output_preproc/subset_mean.csv", row.names=FALSE)

rain_height <- .1

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
               position = position_nudge(x = -rain_height*2)) +
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
  facet_wrap(~factor(facet_contrast, 
                     levels = c("vcl", "v"), 
                     labels = c("h - ħ", "ʔ - ʕ")), 
             nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", color = "white"))



##### CPP ####
rain_height <- .1

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
               position = position_nudge(x = -rain_height*2)) +
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
  facet_wrap(~factor(facet_contrast, 
                     levels = c("vcl", "v"), 
                     labels = c("h - ħ", "ʔ - ʕ")), 
             nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", color = "white"))

##### SoE ####
rain_height <- .1

plot3 <- ggplot(unique_data, aes(x = "", y = soe_mean_unique, fill = interval)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               position = position_nudge(x = -rain_height*2)) +
  # coord_flip() +
  # mean and SE point in the cloud
  # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
  #              position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "SoE (dB)",
                     # breaks = seq(0, 0.1, 1),
                     # limits = c(-0.1, 1.1)) +
                     ) +
  coord_flip() +
  facet_wrap(~factor(facet_contrast, 
                     levels = c("vcl", "v"), 
                     labels = c("h - ħ", "ʔ - ʕ")), 
             nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", color = "white"))

##### F1 ####
rain_height <- .1

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
               position = position_nudge(x = -rain_height*2)) +
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
  facet_wrap(~factor(facet_contrast, 
                     levels = c("vcl", "v"), 
                     labels = c("h - ħ", "ʔ - ʕ")), 
             nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", color = "white"))

##### F2 ####
rain_height <- .1

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
               position = position_nudge(x = -rain_height*2)) +
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
  facet_wrap(~factor(facet_contrast, 
                     levels = c("vcl", "v"), 
                     labels = c("h - ħ", "ʔ - ʕ")), 
             nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = "white", color = "white"))


grid.arrange(
  plot1, plot2, plot3, plot4,
  ncol = 2, nrow = 2,
  top = "Acoustic Feature Means for Laryngeal and Pharyngeal Consonants",
  gp=gpar(fontsize=20)
)





###### Seyfarth & Garellek (2018) Analysis Type ##########

predict_with_se <- function(model, newdata, ...) {
  response <- terms(model)[[2]]
  
  predicted <- newdata
  predicted[[response]] <- predict(model, newdata = newdata, ...)
  
  newdata_matrix <- model.matrix(terms(model), predicted)
  predicted[["standard_error"]] <- sqrt(
    diag(newdata_matrix %*% tcrossprod(vcov(model), newdata_matrix))
  )
  
  predicted
}

subset = subset(data, tier == 'V-sequence')
subset_subset = subset(subset, interval == 'ħ-V' | interval == 'h-V' | interval == 'ʔ-V' | interval == 'ʕ-V' |
                         interval == 'V-ħ-V' | interval == 'V-h-V' | interval == 'V-ʔ-V' | interval == 'V-ʕ-V' |
                         interval == 'V-ħ' | interval == 'V-h' | interval == 'V-ʔ' | interval == 'V-ʕ')

subset_subset$interval <- as.factor(subset_subset$interval)

initial_data <- subset_subset %>%
  filter(Position_2 == "CV")

medial_data <- subset_subset %>%
  filter(Position_2 == "VCV")

final_data <- subset_subset %>%
  filter(Position_2 == "VC")

# contrasts(initial_data$interval) <- contr.sum(4)
# contrasts(medial_data$interval) <- contr.sum(4)
# contrasts(final_data$interval)   <- contr.sum(4)
# 
# subset_subset$interval <- as.factor(subset_subset$interval)
# contrasts(subset_subset$interval) <- contr.sum(12)

initial_newdata <- crossing(
  interval = names(ms_colors),
  t_prop  = seq(0, 1, by = 0.01)
)

medial_newdata <- crossing(
  interval = names(ms_colors),
  t_prop  = seq(0, 1, by = 0.01)
)

medial_newdata <- crossing(
  interval = names(ms_colors),
  t_prop  = seq(0, 1, by = 0.01)
)


bind_rows(initial_data, medial_data, final_data) %>%
  group_by(participant) %>%
  mutate(
    H1H2z    = H1H2c - mean(H1H2c, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  ggplot(aes(t_prop, H1H2z, color = interval)) +
  geom_smooth(fill = NA) +
  facet_wrap(~ Position_2, scales = "free_x") +
  ms_facets +
  labs(x = "Proportion of time", y = "H1*-H2* (centered within-speaker)")


subset_subset %>%
  # mutate(
  #   H1H2z    = H1H2c - mean(H1H2c, na.rm = TRUE),
  # ) %>%
  ggplot(aes(t_prop, H1H2c, color = interval)) +
  geom_smooth(method = 'gam', fill = NA) +
  facet_wrap(~ factor(Position_2, c('CV','VCV','VC')), scales = "free_x", ncol=3) +
  ms_facets +
  labs(x = "Proportion of interval duration", y = "H1*-H2*")

subset_subset %>%
  # mutate(
  #   CPPz    = CPP - mean(CPP, na.rm = TRUE),
  # ) %>%
  ggplot(aes(t_prop, CPP, color = interval)) +
  geom_smooth(method = 'gam', fill = NA) +
  facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x", ncol=4) +
  ms_facets +
  labs(x = "Proportion of interval duration", y = "CPP")

subset_subset %>%
  # mutate(
  #   f0z    = strF0 - mean(strF0, na.rm = TRUE),
  # ) %>%
  ggplot(aes(t_prop, strF0, color = interval)) +
  geom_smooth(method = 'gam', fill = NA) +
  facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x", ncol=4) +
  ms_facets +
  labs(x = "Proportion of interval duration", y = "f0")

subset_subset %>%
  # mutate(
  #   f0z    = strF0 - mean(strF0, na.rm = TRUE),
  # ) %>%
  ggplot(aes(t_prop, energy_prop, color = interval)) +
  geom_smooth(method = 'gam', fill = NA) +
  facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x", ncol=4) +
  ms_facets +
  labs(x = "Proportion of interval duration", y = "RMS Energy (normalized)")

subset_subset %>%
  # mutate(
  #   SoEz    = soe - mean(soe, na.rm = TRUE),
  # ) %>%
  ggplot(aes(t_prop, soe, color = interval)) +
  geom_smooth(method = 'gam', fill = NA) +
  facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x", ncol=4) +
  ms_facets +
  labs(x = "Proportion of interval duration", y = "SoE")


mod_H1H2c <- lmer(
  formula = H1H2c ~
    interval + t_ms + (1 | phrase),
  data = subset_subset
)

predictions <- bind_rows(
  mutate(predict_with_se(mod_H1H2c, subset_subset_newdata, re.form = NA),
         position = "Intervals"
  )
) %>%
  mutate(
    interval  = fct_relevel(interval, names(ms_colors))
  )

ggplot(
  predictions,
  aes(
    x = t_norm, y = H1H2c,
    ymin = H1H2c - standard_error, ymax = H1H2c + standard_error,
    color = interval, fill = interval)) +
  geom_smooth(stat = "identity", alpha = 0.25) +
  facet_wrap(~ factor(interval, c('h-V','ʔ-V','ħ-V','ʕ-V','V-h-V','V-ʔ-V','V-ħ-V','V-ʕ-V','V-h','V-ʔ','V-ħ','V-ʕ')), scales = "free_x") +
  ms_facets +
  labs(x = "Proportion of vowel duration", y = "H1*-H2*") +
  theme(strip.text = element_text(size = 10, margin = margin(0, 0, 5, 0)))
