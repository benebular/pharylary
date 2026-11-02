## Modeling for PharyLary
## author: ben lang, blang@ucsd.edu

library(lmerTest)
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
emm_options(pbkrtest.limit = 20000, lmerTest.limit = 20000)

# Define data paths - check which volume is available
sync_paths <- c('/Volumes/circe',
                '/Volumes/cassandra')

base_path <- sync_paths[dir.exists(sync_paths)][1]

#paths
orig_data_path <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_vowel_context_subset_mean.csv')
# orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_subset_mean.csv')

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
    sF1_mean_unique   = first(sF1_mean),
    sF2_mean_unique   = first(sF2_mean),
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
    
    # metadata
    Segment_unique = first(Segment),
    .groups = "drop"
  )



# combine sonorant label so they are collapsed
# unique_data$interval <- str_replace(unique_data$interval, "j|w", "j/w")
# subset_mean$interval <- str_replace(subset_mean$interval, "j|w", "j/w")

# Desired order of intervals
# Desired order of intervals# Desired order of intervals
# interval_order <- c("j/w", "ħ", "ʕ", "h", "ʔ")
interval_order <- c("h", "j/w", "ʕ", "ħ", "ʔ")
interval_order_H1res <- c("h", "j/w", "ʕ", "ħ", "ʔ")
interval_order_F1 <- c("ʕ","ħ","h","ʔ","j/w")
interval_order_soe <- c("j/w","ʕ","h","ʔ","ħ")
interval_order_CPP <- c("j/w","ʕ","ʔ","h","ħ")

interval_colors <- c(
  "ħ"   = "#e7298a",
  "ʕ"   = "#7570b3",
  "h"   = "#1b9e77",
  "ʔ"   = "#66a61e",
  "j/w" = "#d95f02"
)

# interval_colors <- c(
#   "ħ"   = "#1b9e77",
#   "ʕ"   = "#d95f02",
#   "h"   = "#7570b3",
#   "ʔ"   = "#e7298a",
#   "j/w" = "#66a61e"
# )

# H1res
plot8 <- ggplot(
  unique_data %>%
    filter(H1res_mean_z_outlier_unique == "OK") %>%
    mutate(interval = factor(interval, levels = interval_order_H1res)),
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
      mutate(interval = factor(interval, levels = interval_order_H1res)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized Residual H1*", title='Residual H1*') +
  scale_fill_manual(
    values = interval_colors, 
    name = "Segment"
  ) +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",              # bottom-left inside
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
    legend.title = element_text(size = 32),  # larger title
    legend.text  = element_text(size = 30),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_text(size = 34, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size=48)
  )

ggsave(
  filename = "resH1.tif",
  plot = plot8,
  width = 8, 
  height = 8, 
  dpi = 600,          # High resolution for printing
  compression = "lzw" # Keeps file size manageable without losing quality
)

# CPP
plot9 <- ggplot(
  unique_data %>% filter(CPP_mean_log_z_outlier_unique=="OK") %>% mutate(interval = factor(interval, levels = interval_order_CPP)),
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
      mutate(interval = factor(interval, levels = interval_order_CPP)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized CPP", title='CPP') +
  scale_fill_manual(
    values = interval_colors, 
    name = "Segment"
  ) +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",              # bottom-left inside
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
    legend.title = element_text(size = 32),  # larger title
    legend.text  = element_text(size = 30),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_text(size = 34, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size=48)
  )

ggsave(
  filename = "CPP.tif",
  plot = plot9,
  width = 8, 
  height = 8, 
  dpi = 600,          # High resolution for printing
  compression = "lzw" # Keeps file size manageable without losing quality
)

# soe
plot10 <- ggplot(
  unique_data %>% filter(soe_mean_log_z_outlier_unique=="OK") %>% mutate(interval = factor(interval, levels = interval_order_soe)),
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
      mutate(interval = factor(interval, levels = interval_order_soe)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized SoE", title='SoE') +
  scale_fill_manual(
    values = interval_colors, 
    name = "Segment"
  ) +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",              # bottom-left inside
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
    legend.title = element_text(size = 32),  # larger title
    legend.text  = element_text(size = 30),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_text(size = 34, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size=48)
  )

ggsave(
  filename = "soe.tif",
  plot = plot10,
  width = 8, 
  height = 8, 
  dpi = 600,          # High resolution for printing
  compression = "lzw" # Keeps file size manageable without losing quality
)

# F1
plot11 <- ggplot(
  unique_data %>% filter(formant_outlier_unique != "outlier" | is.na(formant_outlier_unique)) %>% mutate(interval = factor(interval, levels = interval_order_F1)),
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
      mutate(interval = factor(interval, levels = interval_order_F1)),
    aes(x = interval, y = NA, fill = interval),
    shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
  ) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Normalized F1", title='F1') +
  scale_fill_manual(
    values = interval_colors, 
    name = "Segment"
  ) +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
    )
  ) +
  theme_minimal(base_size = 18) +
  # theme(
  #   panel.grid.major.y = element_blank(),
  #   legend.position = "left",              # bottom-left inside
  #   legend.direction = "vertical",
  #   legend.background = element_rect(fill = "white", color = "white"),
  #   legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
  #   legend.title = element_text(size = 18),  # larger title
  #   legend.text  = element_text(size = 16),        # larger labels
  #   axis.text.y  = element_blank(),
  #   axis.ticks.y = element_blank(),
  #   axis.text.x  = element_text(size = 20),
  #   axis.title.x = element_text(size = 18, margin = margin(t = 10)),
  #   plot.margin  = margin(20, 20, 20, 20)
  # )
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",              # bottom-left inside
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key.size = unit(1.2, "cm"),            # makes legend squares larger
    legend.title = element_text(size = 32),  # larger title
    legend.text  = element_text(size = 30),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_text(size = 34, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size=48)
  )

ggsave(
  filename = "F1.tif",
  plot = plot11,
  width = 8, 
  height = 8, 
  dpi = 600,          # High resolution for printing
  compression = "lzw" # Keeps file size manageable without losing quality
)

# f0
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
  labs(x = NULL, y = "Normalized f0", title='Fundamental Frequency (f0)') +
  scale_fill_manual(
    values = interval_colors, 
    name = "Segment"
  ) +
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
    legend.title = element_text(size = 32),  # larger title
    legend.text  = element_text(size = 30),        # larger labels
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(size = 30),
    axis.title.x = element_text(size = 32, margin = margin(t = 10)),
    plot.margin  = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size=36)
  )


grid.arrange(
  plot8, plot9, plot10, plot11,
  ncol = 2, nrow = 2,
  top = grid::textGrob("Acoustic Feature Means for Laryngeal and Pharyngeal Consonants", gp=grid::gpar(fontsize=20))
)

featuregrid <- grid.arrange(
  plot11, plot10, plot8,
  ncol = 3, nrow = 1
  # top = grid::textGrob("Acoustic Feature Means for Laryngeal and Pharyngeal Consonants", gp=grid::gpar(fontsize=20))
)

# ggsave(
#   filename = "F1_soe_resH1_grid.tif",
#   plot = featuregrid,
#   width = 24, 
#   height = 8, 
#   dpi = 600,          # High resolution for printing
#   compression = "lzw" # Keeps file size manageable without losing quality
# )


### run some models!

### remove the final position from subset and make variable that keeps it

# subset_mean_pos_all = subset(subset_mean, interval == 'ħ' | interval == 'ʕ' | interval == 'h' | interval == 'ʔ')
# subset_mean = subset(subset_mean, Position == 'Initial' | Position == 'Medial')

### releveling and dummy coding

subset_mean$Position <-factor(subset_mean$Position, levels = c("Initial", "Medial"))
contrasts(subset_mean$Position) <- contr.treatment(2)
# subset_mean$interval <- str_replace(subset_mean$interval, "j|w", "j/w")

## run all models with appropriate outlier removal using the flags from z-scoring and outlier flagging

# use sonorant as base since they have stable voicing
subset_mean <- subset_mean %>%
  mutate(interval = relevel(factor(interval), ref = "a"))

carrier_ids <- c(
  "Y0409", "Y0395", "Y0388", "Y0401", "Y0396",
  "SD0001", "Y0410", "SD0002", "SD0003", "SD0004", "SD0005",
  "Y0415", "A0249", "Y0208",
  "SD0006", "SD0007", "SD0008", "SD0009", "SD0010",
  "SD0011", "SD0012", "SD0013", "SD0014", "SD0015"
)

subset_mean <- subset_mean %>%
  mutate(carrierphrase = factor(if_else(participant %in% carrier_ids, 1, 2)))

subset_mean <- subset_mean %>%
  mutate(unit_id = coalesce(context_id, paste(participant, phrase, interval, sep = "|")))

varies <- subset_mean %>%
  group_by(unit_id) %>%
  summarise(across(everything(), ~ n_distinct(.x) > 1), .groups = "drop") %>%
  summarise(across(-unit_id, any)) %>% unlist()

ms_cols <- names(varies)[varies]
subset_mean_collapsed <- subset_mean %>% select(-all_of(ms_cols)) %>% distinct()

# s_ized_targets <- c('s', 'sˤ')
# t_ized_targets <- c('t','tˤ')
# d_ized_targets <- c('d','dˤ')
# 
# subset_s_ized <- subset_mean_collapsed %>% filter(Segment %in% s_ized_targets)
# subset_t_ized <- subset_mean_collapsed %>% filter(Segment %in% t_ized_targets)
# subset_d_ized <- subset_mean_collapsed %>% filter(Segment %in% d_ized_targets) 

# run models and pairwise comparisons

# mod_f0 <- lmer(
#   formula = strF0_mean ~
#     Segment*vowel_position + (1|participant) + (1|phrase) + (1|carrierphrase),
#   data = subset_s_ized %>% filter(strF0_mean_z_outlier=="OK")
# )
# summary(mod_f0)
# 
# emms_f0 <- emmeans(mod_f0, ~ Segment*vowel_position, lmer.df = "satterthwaite")
# pairs_f0 <- pairs(emms_f0) %>% as.data.frame() %>% arrange(p.value)
# pairs_f0
# 
# mod_CPP <- lmer(
#   formula = CPP_mean_log ~
#     Segment*vowel_position + (1|participant) + (1|phrase) + (1|carrierphrase),
#   data = subset_s_ized %>% filter(CPP_mean_log_z_outlier=="OK")
# )
# summary(mod_CPP)
# 
# emms_CPP <- emmeans(mod_CPP, ~ Segment*vowel_position, lmer.df = "satterthwaite")
# pairs_CPP <- pairs(emms_CPP) %>% as.data.frame() %>% arrange(p.value)
# pairs_CPP
# 
# mod_soe <- lmer(
#   formula = soe_mean_log ~
#     Segment*vowel_position + (1|participant) + (1|phrase) + (1|carrierphrase),
#   data = subset_s_ized %>% filter(soe_mean_log_z_outlier=="OK")
# )
# summary(mod_soe)
# 
# emms_soe <- emmeans(mod_soe, ~ Segment*vowel_position, lmer.df = "satterthwaite")
# pairs_soe <- pairs(emms_soe) %>% as.data.frame() %>% arrange(p.value)
# pairs_soe
# 
# mod_F1 <- lmer(
#   formula = F1n_mean ~
#     Segment*vowel_position + (1|participant) + (1|phrase) + (1|carrierphrase),
#   data = subset_s_ized %>% filter(formant_outlier != "outlier" | is.na(formant_outlier)) # filter out the outliers from mahalanobis
# )
# summary(mod_F1)
# 
# emms_F1 <- emmeans(mod_F1, ~ Segment*vowel_position, lmer.df = "satterthwaite")
# pairs_F1 <- pairs(emms_F1) %>% as.data.frame() %>% arrange(p.value)
# pairs_F1
# 
# mod_F2 <- lmer(
#   formula = F2n_mean ~
#     Segment*vowel_position + (1|participant) + (1|phrase) + (1|carrierphrase),
#   data = subset_s_ized %>% filter(formant_outlier != "outlier" | is.na(formant_outlier)) # filter out the outliers from mahalanobis
# )
# summary(mod_F2)
# 
# emms_F2 <- emmeans(mod_F2, ~ Segment*vowel_position, lmer.df = "satterthwaite")
# pairs_F2 <- pairs(emms_F2) %>% as.data.frame() %>% arrange(p.value)
# pairs_F2
# 
# ### run harmonic model
# mod_H1res <- lmer(
#   formula = H1res_mean ~ Segment*vowel_position  + (1|participant) + (1|phrase) + (1|carrierphrase),
#   data = subset_s_ized %>% filter(H1res_mean_z_outlier=="OK")
# )
# summary(mod_H1res)
# 
# emms_H1res <- emmeans(mod_H1res, ~ Segment*vowel_position, lmer.df = "satterthwaite")
# pairs_H1res <- pairs(emms_H1res) %>% as.data.frame() %>% arrange(p.value)
# pairs_H1res


## ---- vowel categories: done once, before the split ---------------------------------------------------------
# a = ɑ, a, æ    i = i, ɪ    u = u, ʊ     (length marks are ignored). Any other interval, such as the diphthongs aɪ and aʊ, is removed.
vowel_key <- c("ɑ" = "a", "a" = "a", "æ" = "a", "i" = "i", "ɪ" = "i", "u" = "u", "ʊ" = "u")
model_vowels <- "a"                       # the vowel categories that go into the models (the models have no interval term, so use one)

vowel_category <- unname(vowel_key[str_remove_all(subset_mean_collapsed$interval, "[ːˑ:]")])    # NA = not in the key

cat("\n--- Intervals removed (not in the vowel key) ---\n")
print(subset_mean_collapsed %>% as_tibble() %>% filter(is.na(vowel_category)) %>% count(interval, sort = TRUE), n = Inf)

subset_mean_vowels <- subset_mean_collapsed %>%
  mutate(interval_detail = interval,          # the original label, kept for reference
         interval = vowel_category) %>%       # interval is now a / i / u
  filter(interval %in% model_vowels)          # only the vowel(s) in model_vowels
cat("\nTokens kept, by original label:\n")
print(subset_mean_vowels %>% as_tibble() %>% count(interval, interval_detail), n = Inf)

# the "-ized" target sets: one entry per set (add a line for another set, e.g. td_ized = c("t", "tˤ", "d", "dˤ"))
ized_targets <- list(
  s_ized = c("s", "sˤ"),
  t_ized = c("t", "tˤ"),
  d_ized = c("d", "dˤ")
)

# vowel position: the short name used in the data-set names, and the value it has in the vowel_position column
position_codes <- c(prec = "preceding", fol = "following")

# the six data sets: s_ized_prec, s_ized_fol, t_ized_prec, t_ized_fol, d_ized_prec, d_ized_fol
ized_data <- list()
for (set_name in names(ized_targets)) {
  for (pos in names(position_codes)) {
    ized_data[[paste0(set_name, "_", pos)]] <- subset_mean_vowels %>%
      filter(Segment %in% ized_targets[[set_name]], vowel_position == position_codes[[pos]])
  }
}

# what each data set holds: a Segment with only one level (or no rows) makes the model fail
data_check <- map_dfr(ized_data, function(d) {
  tibble(rows = nrow(d), segment_levels = n_distinct(d$Segment),
         tokens_per_segment = paste0(names(table(d$Segment)), " ", as.integer(table(d$Segment)), collapse = " | "),
         participants = n_distinct(d$participant), phrases = n_distinct(d$phrase))
}, .id = "set")
print(data_check, n = Inf)

# one row per model: the response and the flag column used to drop outliers
model_specs <- tribble(
  ~name,   ~response,      ~flag,
  "f0",    "strF0_mean",   "strF0_mean_z_outlier",
  "CPP",   "CPP_mean_log", "CPP_mean_log_z_outlier",
  "soe",   "soe_mean_log", "soe_mean_log_z_outlier",
  "F1",    "F1n_mean",     "formant_outlier",           # Mahalanobis flag
  "F2",    "F2n_mean",     "formant_outlier",
  "H1res", "H1res_mean",   "H1res_mean_z_outlier"
)

rhs <- "Segment + (1|participant) + (1|phrase) + (1|carrierphrase)"

lmer_results <- list()
for (set_name in names(ized_data)) {
  lmer_results[[set_name]] <- list()
  for (i in seq_len(nrow(model_specs))) {
    spec <- model_specs[i, ]
    # drop the flagged outliers (for the z-score flags this is the same as == "OK"; formant_outlier keeps its NAs)
    dat <- ized_data[[set_name]] %>% filter(is.na(.data[[spec$flag]]) | .data[[spec$flag]] != "outlier")
    lmer_results[[set_name]][[spec$name]] <- tryCatch({
      f    <- as.formula(paste(spec$response, "~", rhs))
      mod  <- eval(bquote(lmer(.(f), data = dat)))      # .(f) puts the formula itself in the call, so summary() prints it
      emms <- emmeans(mod, ~ Segment, lmer.df = "satterthwaite", data = dat)
      list(model = mod,
           summary = summary(mod, correlation = FALSE),   # no correlation block: printing it warns about non-ASCII Segment labels
           emmeans = emms,
           pairs = pairs(emms) %>% as.data.frame() %>% arrange(p.value),     # one comparison: the two Segments
           n_obs = nrow(dat), data = dat)
    }, error = function(e) list(error = conditionMessage(e), n_obs = nrow(dat), data = dat))     # data kept so a failure can be inspected
  }
}

## ---- looking at the results -------------------------------------------------------------------------
# lmer_results$s_ized_prec$CPP$summary             # model summary for one data set x one outcome
# lmer_results$s_ized_prec$CPP$pairs               # its comparison of the two Segments
# names(lmer_results)                              # the six data sets
# names(lmer_results$t_ized_fol)                   # the outcomes available for a data set

# the Segment comparison of every model in one table (set, model, contrast, estimate, p.value, ...)
all_pairs <- map_dfr(lmer_results, function(s) map_dfr(s, ~ .x$pairs, .id = "model"), .id = "set")

# the same table tidied for reading: one row per data set x outcome, rounded, with significance stars
# (each model fits one comparison, so these p-values are not adjusted for the number of models)
pairs_table <- all_pairs %>%
  mutate(sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ ""),
         set = factor(set, levels = names(ized_data)),
         model = factor(model, levels = model_specs$name)) %>%
  arrange(set, model) %>%
  transmute(set = as.character(set), outcome = as.character(model), contrast,
            estimate = round(estimate, 3), SE = round(SE, 3), df = round(df, 1),
            t.ratio = round(t.ratio, 2), p.value = signif(p.value, 3), sig) %>%
  as_tibble()                                      # a tibble, so print(n = Inf) works
print(pairs_table, n = Inf)                       # View(pairs_table) opens it in RStudio
# write.csv(pairs_table, "ized_pairs_table.csv", row.names = FALSE)     # save a copy (UTF-8)

# which models failed, if any: one row per failed model (empty = every model ran)
failed_models <- map_dfr(lmer_results, function(s) {
  map_dfr(s, function(m) if (!is.null(m$error)) tibble(n_obs = m$n_obs, error = m$error), .id = "model")
}, .id = "set")
failed_models


# Figures for the -ized models: box-and-whisker plots with the individual values overlaid.
# One figure per outcome. Each figure is a grid: one column per -ized pair (s vs sˤ, t vs tˤ, d vs dˤ) and one row per vowel
# position (preceding vowels on top, following vowels below). Every panel shows the two categories of the pair on the x axis
# and the acoustic measure on the y axis; all panels share the y axis so they can be compared.
# Each panel is one model (lmer_results$s_ized_prec, $s_ized_fol, ...) and has a bracket between its two categories: *, ** or ***
# when the difference between the two segments was significant in that model (...$pairs), "n.s." when it was not.
# Needs: lmer_results, ized_targets, position_codes, model_specs and all_pairs from ized_models_section.R, base_path, and the tidyverse (ggplot2) loaded.
# The last part (summary figures) draws every model in one heat map and one forest plot.
# The plotted values are exactly the data each model was fitted on (outliers already removed).

# where the figures are saved
fig_dir <- file.path(base_path, "alldata/dissertation/1A/figs")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# font for the figures: it must be installed on this computer and have the IPA symbols.
# The family name is usually written with a space ("Charis SIL"), not "CharisSIL". Find the exact name with
#   systemfonts::system_fonts() %>% filter(str_detect(family, "Charis|Doulos|Gentium")) %>% distinct(family)
plot_font <- "Charis SIL"
installed_fonts <- unique(systemfonts::system_fonts()$family)
if (!plot_font %in% installed_fonts) {
  message("Font '", plot_font, "' is not installed, so the default font is used and the IPA symbols may look wrong.\n",
          "Installed fonts that could work: ",
          paste(grep("Charis|Doulos|Gentium|DejaVu|Noto Sans$|Noto Serif$", installed_fonts, value = TRUE), collapse = ", "))
  plot_font <- ""
}

# colours for the six categories (edit freely)
segment_colors <- c("s" = "#8dd3c7", "sˤ" = "#1b9e77",
                    "t" = "#fdb462", "tˤ" = "#d95f02",
                    "d" = "#bebada", "dˤ" = "#7570b3")

# y-axis labels and titles, named by model_specs$name
y_labels <- c(f0 = "F0 (Hz)", CPP = "CPP (log)", soe = "Strength of excitation (log)",
              F1 = "Normalized F1", F2 = "Normalized F2", H1res = "Residual H1*")
feature_titles <- c(f0 = "F0", CPP = "Cepstral peak prominence", soe = "Strength of excitation",
                    F1 = "F1", F2 = "F2", H1res = "Residual H1*")
panel_titles <- sapply(ized_targets, paste, collapse = " vs ")     # column headings: "s vs sˤ", "t vs tˤ", "d vs dˤ"
position_levels <- unname(position_codes)                           # row order, top to bottom: preceding, following
position_labels <- c(preceding = "Preceding vowels", following = "Following vowels")

ized_plots <- list()                                                # every plot is kept, e.g. ized_plots$f0
audit <- list()                                                     # the p-value and label drawn in every panel
for (feature in model_specs$name) {
  response <- model_specs$response[model_specs$name == feature]
  
  # the values and the p-value of each panel = each model (a model that failed has no panel)
  pts <- list()
  ann <- list()
  for (set_name in names(ized_targets)) {
    for (pos in names(position_codes)) {
      key <- paste0(set_name, "_", pos)
      res <- lmer_results[[key]][[feature]]
      if (is.null(res$model)) next
      pts[[key]] <- res$data %>%
        transmute(set = factor(set_name, levels = names(ized_targets)),         # factors: columns s, t, d; rows preceding, following
                  vowel_position = factor(position_codes[[pos]], levels = position_levels),
                  Segment = factor(Segment, levels = ized_targets[[set_name]]),
                  value = .data[[response]])
      ann[[key]] <- tibble(key = key, set = factor(set_name, levels = names(ized_targets)),
                           vowel_position = factor(position_codes[[pos]], levels = position_levels),
                           p = res$pairs$p.value[1],
                           top = max(pts[[key]]$value, na.rm = TRUE))
    }
  }
  if (length(pts) == 0) next
  plot_df <- bind_rows(pts)
  y_span <- diff(range(plot_df$value, na.rm = TRUE))
  
  # bracket height and label (one per panel). The p-value is res$pairs$p.value[1] of that panel's model (the p in lmer_results ...$pairs):
  # *** below .001, ** below .01, * below .05, otherwise "n.s."
  ann_df <- bind_rows(ann) %>%
    mutate(label = case_when(is.na(p) ~ "NA", p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "n.s."),
           y = top + 0.06 * y_span,
           tick = 0.02 * y_span)
  
  p <- ggplot(plot_df, aes(x = Segment, y = value, fill = Segment)) +
    geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.85) +
    geom_jitter(width = 0.15, size = 1, alpha = 0.3) +
    geom_segment(data = ann_df, aes(x = 1, xend = 2, y = y, yend = y), inherit.aes = FALSE) +
    geom_segment(data = ann_df, aes(x = 1, xend = 1, y = y, yend = y - tick), inherit.aes = FALSE) +
    geom_segment(data = ann_df, aes(x = 2, xend = 2, y = y, yend = y - tick), inherit.aes = FALSE) +
    geom_text(data = ann_df, aes(x = 1.5, y = y, label = label), vjust = -0.5, size = 6, inherit.aes = FALSE) +
    facet_grid(vowel_position ~ set, scales = "free_x",
               labeller = labeller(set = panel_titles, vowel_position = position_labels)) +
    scale_fill_manual(values = segment_colors, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.12))) +
    labs(x = NULL, y = y_labels[[feature]], title = feature_titles[[feature]]) +
    theme_minimal(base_size = 18, base_family = plot_font) +
    theme(panel.spacing.x = unit(1.2, "cm"),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 18),
          plot.title = element_text(hjust = 0.5, size = 24),
          plot.margin = margin(15, 15, 15, 15))
  
  # cairo_pdf (not the default pdf device) so that installed fonts and the IPA symbols work in the PDF
  ggsave(filename = file.path(fig_dir, paste0(feature, ".pdf")), plot = p, width = 14, height = 9,
         device = cairo_pdf)
  ized_plots[[feature]] <- p
  audit[[feature]] <- ann_df %>% mutate(feature = feature)
  cat("Saved:", file.path(fig_dir, paste0(feature, ".pdf")), "\n")
}

## =====================================================================================================
## SUMMARY FIGURES: every model in one picture (a heat map and a forest plot)
## =====================================================================================================
# The outcomes are on different scales (Hz, log units, normalized formants), so each difference is standardized: the model's
# estimate divided by the SD of that outcome in the data the model was fitted on ("SD units"). The sign is emphatic minus plain
# (sˤ - s, tˤ - t, dˤ - d): positive = higher for the pharyngealized segment.
# Significance is the p in lmer_results ...$pairs, the same p as in the box plots. To correct for the number of models instead,
# set p_adjust_method to "holm" or "BH"; "none" keeps the summary identical to the box plots.
p_adjust_method <- "none"
show_values <- TRUE           # print the standardized difference in each heat-map cell (FALSE = stars / n.s. only)
summary_labels <- c(f0 = "F0", CPP = "CPP", soe = "SoE", F1 = "F1", F2 = "F2", H1res = "Residual H1*")

rows <- list()
for (set_name in names(ized_targets)) {
  for (pos in names(position_codes)) {
    for (feature in model_specs$name) {
      res <- lmer_results[[paste0(set_name, "_", pos)]][[feature]]
      if (is.null(res$model)) next                                   # a model that failed has no cell
      response <- model_specs$response[model_specs$name == feature]
      pr <- res$pairs[1, ]
      plain <- ized_targets[[set_name]][1]
      first_term <- str_split_fixed(as.character(pr$contrast), " - ", 2)[, 1]
      flip <- if (first_term == plain) -1 else 1                    # pairs gives plain - emphatic; we want emphatic - plain
      rows[[length(rows) + 1]] <- tibble(set = set_name, vowel_position = position_codes[[pos]], outcome = feature,
                                         estimate = flip * pr$estimate, SE = pr$SE, df = pr$df, p = pr$p.value,
                                         sd_response = sd(res$data[[response]], na.rm = TRUE))
    }
  }
}

summary_df <- bind_rows(rows) %>%
  mutate(p_plot = p.adjust(p, method = p_adjust_method),
         d = estimate / sd_response,                                          # standardized difference
         d_low = (estimate - qt(.975, df) * SE) / sd_response,                # 95% CI
         d_high = (estimate + qt(.975, df) * SE) / sd_response,
         sig_class = factor(ifelse(!is.na(p_plot) & p_plot < .05, "significant", "n.s."), levels = c("significant", "n.s.")),
         stars = case_when(is.na(p_plot) ~ "NA", p_plot < .001 ~ "***", p_plot < .01 ~ "**", p_plot < .05 ~ "*", TRUE ~ "n.s."),
         set = factor(set, levels = names(ized_targets)),
         vowel_position = factor(vowel_position, levels = position_levels),
         outcome = factor(outcome, levels = model_specs$name, labels = unname(summary_labels[model_specs$name])))
print(summary_df %>% transmute(outcome, set, vowel_position, d = round(d, 2), CI = paste0("[", round(d_low, 2), ", ", round(d_high, 2), "]"),
                               p = signif(p_plot, 3), stars) %>% arrange(vowel_position, set, outcome) %>% as_tibble(), n = Inf)

## ---- heat map: one cell per model; colour = standardized difference, faded cells = n.s. -------------------------
d_limit <- max(abs(summary_df$d), na.rm = TRUE)                              # symmetric colour scale around 0
summary_df <- summary_df %>%
  mutate(cell_label = if (show_values) paste0(sprintf("%.2f", d), "\n", stars) else stars,
         text_colour = ifelse(sig_class == "significant" & abs(d) > 0.65 * d_limit, "white", "black"))   # white on the darkest cells

summary_heatmap <- ggplot(summary_df, aes(x = outcome, y = set, fill = d)) +
  geom_tile(aes(alpha = sig_class), colour = "white", linewidth = 1.2) +
  geom_text(aes(label = cell_label, colour = text_colour), size = 5, lineheight = 0.9) +
  scale_colour_identity() +
  facet_grid(vowel_position ~ ., labeller = labeller(vowel_position = position_labels)) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-d_limit, d_limit),
                       name = "Pharyngealized -\nplain\n(SD units)") +
  scale_alpha_manual(values = c(significant = 1, "n.s." = 0.3), guide = "none") +
  scale_y_discrete(limits = rev(names(ized_targets)), labels = panel_titles) +
  labs(x = NULL, y = NULL, title = "Pharyngealized vs plain targets",
       caption = "Faded cells: n.s. (p \u2265 .05). * p < .05, ** p < .01, *** p < .001") +
  theme_minimal(base_size = 18, base_family = plot_font) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24),
        axis.text.x = element_text(size = 16),
        strip.text = element_text(size = 18))
ggsave(filename = file.path(fig_dir, "summary_heatmap.pdf"), plot = summary_heatmap, width = 12, height = 7, device = cairo_pdf)

## ---- forest plot: standardized difference with 95% CI; filled points = significant ----------------------------
pair_colors <- c(s_ized = "#1b9e77", t_ized = "#d95f02", d_ized = "#7570b3")
summary_forest <- ggplot(summary_df, aes(x = d, y = outcome, colour = set, shape = sig_class,
                                         group = factor(set, levels = rev(names(ized_targets))))) +     # group order: s on top within each row
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_linerange(aes(xmin = d_low, xmax = d_high), position = position_dodge(width = 0.6), linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.6), size = 3.8, stroke = 1.1) +
  facet_wrap(~ vowel_position, nrow = 1, labeller = as_labeller(position_labels)) +
  scale_colour_manual(values = pair_colors, labels = panel_titles, name = "Pair") +
  scale_shape_manual(values = c(significant = 16, "n.s." = 1), name = NULL,
                     labels = c(significant = "significant (p < .05)", "n.s." = "n.s.")) +
  scale_y_discrete(limits = rev(levels(summary_df$outcome))) +
  labs(x = "Pharyngealized - plain, in SD units (95% CI)", y = NULL, title = "Pharyngealized vs plain targets") +
  theme_minimal(base_size = 18, base_family = plot_font) +
  theme(panel.spacing.x = unit(1, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24),
        strip.text = element_text(size = 18),
        legend.position = "bottom")
ggsave(filename = file.path(fig_dir, "summary_forest.pdf"), plot = summary_forest, width = 12, height = 6.5, device = cairo_pdf)


# ---- audit: the p-value and label drawn in every panel, against the p in all_pairs / pairs_table -------------------
panel_labels <- bind_rows(audit) %>%
  left_join(all_pairs %>% select(key = set, feature = model, p_in_pairs = p.value), by = c("key", "feature")) %>%
  mutate(label_from_pairs = case_when(is.na(p_in_pairs) ~ "NA", p_in_pairs < .001 ~ "***", p_in_pairs < .01 ~ "**",
                                      p_in_pairs < .05 ~ "*", TRUE ~ "n.s."),
         agrees = label == label_from_pairs & isTRUE(all.equal(p, p_in_pairs))) %>%
  transmute(feature, panel = key, p_drawn = signif(p, 3), p_in_pairs = signif(p_in_pairs, 3), label_drawn = label, agrees) %>%
  as_tibble()
print(panel_labels, n = Inf)                                        # View(panel_labels) opens it in RStudio
cat("Panels whose label disagrees with the p in $pairs:", sum(!panel_labels$agrees), "of", nrow(panel_labels), "\n")


# Models for the other targets (ħ, ʕ, h, ʔ, w, j).
# A separate section from the -ized models, with its own objects (all named other_...) so the two can be changed independently.
# The vowel (interval) is reduced to the categories a, i, u and all three are kept. The data are NOT split by segment: all six targets
# stay in the same model. They are split only by vowel position (preceding / following), giving two data sets, and the same model is
# run on each, for every outcome:
#   outcome ~ Segment + interval + (1|participant) + (1|phrase) + (1|carrierphrase)
# Needs: subset_mean_collapsed (with interval, Segment, vowel_position, participant, phrase, carrierphrase and the outcome / flag
# columns below) and the tidyverse + lmerTest loaded. Results are nested:
#   other_results$other_prec$CPP$summary   other_results$other_prec$CPP$pairs   (also $anova, $model, $emmeans, $n_obs, $data)
# A model that fails is recorded as $error and the loop carries on with the rest.

library(emmeans)

## ---- vowel categories: done once, before the split ---------------------------------------------------------
# The same relabelling as for the -ized models, but all three categories are kept:
# a = ɑ, a, æ    i = i, ɪ    u = u, ʊ     (length marks are ignored). Any other interval, such as the diphthongs aɪ and aʊ, is removed.
other_vowel_key <- c("ɑ" = "a", "a" = "a", "æ" = "a", "i" = "i", "ɪ" = "i", "u" = "u", "ʊ" = "u")
other_vowel_category <- unname(other_vowel_key[str_remove_all(subset_mean_collapsed$interval, "[ːˑ:]")])    # NA = not in the key

cat("\n--- Intervals removed (not in the vowel key) ---\n")
print(subset_mean_collapsed %>% as_tibble() %>% filter(is.na(other_vowel_category)) %>% count(interval, sort = TRUE), n = Inf)

other_vowels <- subset_mean_collapsed %>%
  mutate(interval_detail = interval,                                                        # the original label, kept for reference
         interval = factor(other_vowel_category, levels = c("a", "i", "u"))) %>%            # interval is now a / i / u; a is the reference level
  filter(!is.na(interval))
cat("\nTokens kept, by original label:\n")
print(other_vowels %>% as_tibble() %>% count(interval, interval_detail), n = Inf)

## ---- the data sets -----------------------------------------------------------------------------------------
# the targets, in the order they are listed in the model: the FIRST is the reference level for the Segment coefficients in
# summary() (the pairwise comparisons do not depend on it)
other_targets <- c("ħ", "ʕ", "h", "ʔ", "w", "j")

# vowel position: the short name used in the data-set names, and the value it has in the vowel_position column
other_positions <- c(prec = "preceding", fol = "following")

# the two data sets: other_prec and other_fol (all six targets in each)
other_data <- list()
for (pos in names(other_positions)) {
  other_data[[paste0("other_", pos)]] <- other_vowels %>%
    filter(Segment %in% other_targets, vowel_position == other_positions[[pos]]) %>%
    mutate(Segment = factor(Segment, levels = other_targets))
}

# what each data set holds: a Segment or vowel category with no tokens drops out of the model
other_data_check <- map_dfr(other_data, function(d) {
  tibble(rows = nrow(d),
         tokens_per_segment = paste0(names(table(d$Segment)), " ", as.integer(table(d$Segment)), collapse = " | "),
         tokens_per_interval = paste0(names(table(d$interval)), " ", as.integer(table(d$interval)), collapse = " | "),
         participants = n_distinct(d$participant), phrases = n_distinct(d$phrase))
}, .id = "set")
print(other_data_check, n = Inf)

## ---- the models --------------------------------------------------------------------------------------------
# one row per model: the response and the flag column used to drop outliers
other_specs <- tribble(
  ~name,   ~response,      ~flag,
  "f0",    "strF0_mean",   "strF0_mean_z_outlier",
  "CPP",   "CPP_mean_log", "CPP_mean_log_z_outlier",
  "soe",   "soe_mean_log", "soe_mean_log_z_outlier",
  "F1",    "F1n_mean",     "formant_outlier",           # Mahalanobis flag
  "F2",    "F2n_mean",     "formant_outlier",
  "H1res", "H1res_mean",   "H1res_mean_z_outlier"
)

other_rhs <- "Segment + interval + (1|participant) + (1|phrase) + (1|carrierphrase)"

other_results <- list()
for (set_name in names(other_data)) {
  other_results[[set_name]] <- list()
  for (i in seq_len(nrow(other_specs))) {
    spec <- other_specs[i, ]
    # drop the flagged outliers (for the z-score flags this is the same as == "OK"; formant_outlier keeps its NAs)
    dat <- other_data[[set_name]] %>% filter(is.na(.data[[spec$flag]]) | .data[[spec$flag]] != "outlier")
    other_results[[set_name]][[spec$name]] <- tryCatch({
      f    <- as.formula(paste(spec$response, "~", other_rhs))
      mod  <- eval(bquote(lmer(.(f), data = dat)))      # .(f) puts the formula itself in the call, so summary() prints it
      emms <- emmeans(mod, ~ Segment, lmer.df = "satterthwaite", data = dat)     # averaged over the vowel categories
      list(model = mod,
           summary = summary(mod, correlation = FALSE),   # no correlation block: printing it warns about non-ASCII Segment labels
           anova = anova(mod),                            # Type III F-tests (Satterthwaite): the overall tests of Segment and interval
           emmeans = emms,
           pairs = pairs(emms) %>% as.data.frame() %>% arrange(p.value),     # all 15 pairs of segments (Tukey-adjusted within the model)
           n_obs = nrow(dat), data = dat)
    }, error = function(e) list(error = conditionMessage(e), n_obs = nrow(dat), data = dat))     # data kept so a failure can be inspected
  }
}

## ---- looking at the results -------------------------------------------------------------------------
# other_results$other_prec$CPP$summary             # model summary for one data set x one outcome
# other_results$other_prec$CPP$anova               # the overall tests of Segment and interval
# other_results$other_prec$CPP$pairs               # the 15 pairwise comparisons of the segments
# names(other_results)                             # the two data sets
# names(other_results$other_fol)                   # the outcomes available for a data set

# the overall tests of every model in one table: one row per data set x outcome x term (Segment, interval)
other_effects_table <- map_dfr(other_results, function(s) {
  map_dfr(s, function(m) {
    if (is.null(m$anova)) return(NULL)                 # a model that failed has no anova
    as.data.frame(m$anova) %>% rownames_to_column("term")
  }, .id = "model")
}, .id = "set") %>%
  rename(p.value = `Pr(>F)`) %>%
  as_tibble()
print(other_effects_table %>% transmute(set, outcome = model, term, NumDF, DenDF = round(DenDF, 1),
                                        F.value = round(`F value`, 2), p.value = signif(p.value, 3)), n = Inf)

# the pairwise comparisons of every model in one table (set, model, contrast, estimate, p.value, ...)
other_all_pairs <- map_dfr(other_results, function(s) map_dfr(s, ~ .x$pairs, .id = "model"), .id = "set")

# the same table tidied for reading: rounded, with significance stars (p-values are Tukey-adjusted within each model,
# but not across models)
other_pairs_table <- other_all_pairs %>%
  mutate(sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ ""),
         set = factor(set, levels = names(other_data)),
         model = factor(model, levels = other_specs$name)) %>%
  arrange(set, model) %>%
  transmute(set = as.character(set), outcome = as.character(model), contrast,
            estimate = round(estimate, 3), SE = round(SE, 3), df = round(df, 1),
            t.ratio = round(t.ratio, 2), p.value = signif(p.value, 3), sig) %>%
  as_tibble()
# 15 comparisons x 6 outcomes x 2 data sets = 180 rows, so only the significant ones are printed here;
# View(other_pairs_table) shows all of them
cat("\nSignificant pairwise comparisons (p < .05):\n")
print(other_pairs_table %>% filter(sig %in% c("*", "**", "***")), n = Inf)
# write.csv(other_pairs_table, "other_pairs_table.csv", row.names = FALSE)     # save a copy (UTF-8)

# which models failed, if any: one row per failed model (empty = every model ran)
other_failed_models <- map_dfr(other_results, function(s) {
  map_dfr(s, function(m) if (!is.null(m$error)) tibble(n_obs = m$n_obs, error = m$error), .id = "model")
}, .id = "set")
other_failed_models