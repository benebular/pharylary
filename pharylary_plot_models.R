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
library("cowplot")
library("forcats")
library("lme4")
library("optimx")
library("rlang")
library("scales")
library("splines")
library("stringr")
library("tidyverse")

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
  "V-ʕ" = "#e7298a"  # red
)

ms_facets <- list(
  scale_color_manual(name = NULL, values = ms_colors),
  scale_fill_manual(name = NULL, values = ms_colors),
  theme_light(),
  theme(
    aspect.ratio     = 1,
    legend.position  = "bottom",
    strip.background = element_blank(),
    strip.text       = element_text(color = "black", hjust = 0, size = 11),
    panel.border     = element_rect(color = "black", fill = NA)
  )
)

data_path <- sprintf('/Volumes/circe/vs/output_preproc/preproc_output.csv')
data = read.csv(data_path)

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
contrasts(subset_subset$interval) <- contr.sum(12)

subset_subset_newdata <- crossing(
  interval = names(ms_colors),
  t_norm  = seq(0, 0.5, by = 0.01)
)



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
