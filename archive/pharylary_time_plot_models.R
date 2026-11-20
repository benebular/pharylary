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