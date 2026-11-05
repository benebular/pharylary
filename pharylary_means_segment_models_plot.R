## Modeling for PharyLary
## author: ben lang, blang@ucsd.edu

library(lmerTest)
library(plyr)
library(dplyr) # this checks for normality
# library(ggpubr) # this plots normality
library(magrittr)
library(mgcv)
library(itsadug)
# library(effects)
library(ggplot2)
library(gghalves)
# library(ggsignif)
library(tidyr)
# library(scales)
# library(reshape2)
library(lme4)
library(lmerTest)
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
library(knitr)
library(kableExtra)
emm_options(pbkrtest.limit = 20000, lmerTest.limit = 20000)

# Define data paths - check which volume is available
sync_paths <- c('/Volumes/circe',
                '/Volumes/cassandra')

base_path <- sync_paths[dir.exists(sync_paths)][1]

#paths
orig_data_path <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_subset_mean_truncated.csv')
# orig_data_path <- sprintf('/Volumes/cassandra/alldata/dissertation/vs/output_preproc/pharylary_subset_mean.csv')

subset_mean = read.csv(orig_data_path)

###Plotting means first!

### just grab the first value in the intervals for each word for each participa*nt since it's not the same within interval, word, participant
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
    .groups = "drop"
  )



# combine sonorant label so they are collapsed
unique_data$interval <- str_replace(unique_data$interval, "j|w", "j/w")
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
  plot8, plot9, plot10,
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
subset_mean$interval <- str_replace(subset_mean$interval, "j|w", "j/w")

## run all models with appropriate outlier removal using the flags from z-scoring and outlier flagging

# use sonorant as base since they have stable voicing
subset_mean <- subset_mean %>%
  mutate(interval = relevel(factor(interval), ref = "j/w"))

carrier_ids <- c(
  "Y0409", "Y0395", "Y0388", "Y0401", "Y0396",
  "SD0001", "Y0410", "SD0002", "SD0003", "SD0004", "SD0005",
  "Y0415", "A0249", "Y0208",
  "SD0006", "SD0007", "SD0008", "SD0009", "SD0010",
  "SD0011", "SD0012", "SD0013", "SD0014", "SD0015"
)

subset_mean <- subset_mean %>%
  mutate(carrierphrase = factor(if_else(participant %in% carrier_ids, 1, 2)))

varies <- subset_mean %>%
  group_by(participant, phrase, interval) %>%
  summarise(across(everything(), ~ n_distinct(.x) > 1), .groups = "drop") %>%
  summarise(across(-c(participant, phrase, interval), any)) %>%
  unlist()

names(varies)[varies]   # columns that differ within a cell: these are your ms-level columns

ms_cols <- names(varies)[varies]

subset_mean_collapsed <- subset_mean %>%
  select(-all_of(ms_cols)) %>%
  distinct()

# run models and pairwise comparisons

mod_f0 <- lmer(
  formula = strF0_mean ~
    interval + (1|participant) + (1|phrase) + (1|carrierphrase),
  data = subset_mean_collapsed %>% filter(strF0_mean_z_outlier=="OK")
)
summary(mod_f0)

emms_f0 <- emmeans(mod_f0, ~ interval, lmer.df = "satterthwaite")
pairs_f0 <- pairs(emms_f0) %>% as.data.frame() %>% arrange(p.value)
pairs_f0

mod_CPP <- lmer(
  formula = CPP_mean_log ~
    interval + (1|participant) + (1|phrase) + (1|carrierphrase),
  data = subset_mean_collapsed %>% filter(CPP_mean_log_z_outlier=="OK")
)
summary(mod_CPP)

emms_CPP <- emmeans(mod_CPP, ~ interval, lmer.df = "satterthwaite")
pairs_CPP <- pairs(emms_CPP) %>% as.data.frame() %>% arrange(p.value)
pairs_CPP

mod_soe <- lmer(
  formula = soe_mean_log ~
    interval + (1|participant) + (1|phrase) + (1|carrierphrase),
  data = subset_mean_collapsed %>% filter(soe_mean_log_z_outlier=="OK")
)
summary(mod_soe)

emms_soe <- emmeans(mod_soe, ~ interval, lmer.df = "satterthwaite")
pairs_soe <- pairs(emms_soe) %>% as.data.frame() %>% arrange(p.value)
pairs_soe

mod_F1 <- lmer(
  formula = F1n_mean ~
    interval + (1|participant) + (1|phrase) + (1|carrierphrase),
  data = subset_mean_collapsed %>% filter(formant_outlier != "outlier" | is.na(formant_outlier)) # filter out the outliers from mahalanobis
)
summary(mod_F1)

emms_F1 <- emmeans(mod_F1, ~ interval, lmer.df = "satterthwaite")
pairs_F1 <- pairs(emms_F1) %>% as.data.frame() %>% arrange(p.value)
pairs_F1

### run harmonic model
mod_H1res <- lmer(
  formula = H1res_mean ~ interval  + (1|participant) + (1|phrase) + (1|carrierphrase),
  data = subset_mean_collapsed %>% filter(H1res_mean_z_outlier=="OK")
)
summary(mod_H1res)

emms_H1res <- emmeans(mod_H1res, ~ interval, lmer.df = "satterthwaite")
pairs_H1res <- pairs(emms_H1res) %>% as.data.frame() %>% arrange(p.value)
pairs_H1res


consonant_outcomes <- c("F1","H1res","CPP","soe")          # in table order
consonant_pairs <- mget(paste0("pairs_", consonant_outcomes), envir = globalenv(), ifnotfound = list(NULL))
names(consonant_pairs) <- consonant_outcomes
consonant_pairs <- consonant_pairs[!sapply(consonant_pairs, is.null)]       # outcomes that have no pairs_<name> object are left out
cat("Outcomes in the table:", paste(names(consonant_pairs), collapse = ", "), "\n")

# all comparisons of all models in one table (outcome, contrast, estimate, SE, df, t.ratio, p.value, ...), in the order of the pairs_ objects
consonant_all_pairs <- map_dfr(consonant_pairs, as_tibble, .id = "outcome")

## ---- the tidy table (same layout as pairs_table) ------------------------------------------------------------
# rounded, with significance stars; the p-values are Tukey-adjusted within each model, not across models
consonant_pairs_table <- consonant_all_pairs %>%
  mutate(sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ "")) %>%
  transmute(outcome, contrast = as.character(contrast),
            estimate = round(estimate, 3), SE = round(SE, 3), df = round(df, 1),
            t.ratio = round(t.ratio, 2), p.value = signif(p.value, 3), sig) %>%
  as_tibble()                                      # a tibble, so print(n = Inf) works
print(consonant_pairs_table, n = Inf)             # View(consonant_pairs_table) opens it in RStudio
# write.csv(consonant_pairs_table, "consonant_pairs_table.csv", row.names = FALSE)     # save a copy (UTF-8)

## ---- the LaTeX table ----------------------------------------------------------------------------------------
table_dir <- file.path(base_path, "alldata/dissertation/1A/tables")      # where the .tex file is saved
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
table_outcome_labels <- c(f0 = "F0", CPP = "CPP", soe = "SoE", F1 = "F1", F2 = "F2", H1res = "Residual H1*")

# the order of the comparisons as emmeans lists them before sorting by p, so that every block of the table has them in the same order
# (to list them by p-value instead, use arrange(outcome, p.value) below)
contrast_order <- as.data.frame(pairs(get(paste0("emms_", names(consonant_pairs)[1]))))$contrast
stopifnot(all(as.character(consonant_all_pairs$contrast) %in% as.character(contrast_order)))

# the text that goes into the cells: LaTeX math for the minus sign and "<", p-value stars as superscripts
consonant_latex <- consonant_all_pairs %>%
  mutate(contrast_rank = match(as.character(contrast), as.character(contrast_order)),
         outcome = factor(outcome, levels = names(table_outcome_labels), labels = unname(table_outcome_labels)),
         contrast = str_remove_all(as.character(contrast), "[()]"),                       # emmeans writes (j/w) - h; the brackets are dropped
         contrast = str_replace_all(contrast, " - ", " $-$ "),
         estimate = str_replace(sprintf("%.3f", round(estimate, 3) + 0), "^-", "$-$"),    # + 0 turns -0.000 into 0.000
         SE = sprintf("%.3f", SE),
         df = sprintf("%.1f", df),
         t = str_replace(sprintf("%.2f", round(t.ratio, 2) + 0), "^-", "$-$"),
         p = case_when(is.na(p.value) ~ "--", p.value < .001 ~ "$<$.001", TRUE ~ sub("^0", "", sprintf("%.3f", p.value))),   # .034 (no leading zero)
         stars = case_when(is.na(p.value) ~ "", p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ ""),
         p = ifelse(stars == "", p, paste0(p, "\\textsuperscript{", stars, "}"))) %>%
  arrange(outcome, contrast_rank)

# the sizes of the blocks (one block per outcome), in the order the rows are in, for pack_rows()
block_counts <- table(factor(as.character(consonant_latex$outcome), levels = unique(as.character(consonant_latex$outcome))))
block_index <- setNames(as.integer(block_counts), names(block_counts))

# the note goes under the table as a normal, wrapping paragraph (kableExtra's footnote() would put it in one unwrapped line inside the
# longtable, which makes the table wider than the page)
stars_note <- "\\textsuperscript{.}\\,$p<.1$; \\textsuperscript{*}\\,$p<.05$; \\textsuperscript{**}\\,$p<.01$; \\textsuperscript{***}\\,$p<.001$."
table_note <- paste0("Estimates are the first segment minus the second. ",
                     "The $p$-values are Tukey-adjusted within each model (", nrow(consonant_pairs[[1]]), " comparisons). ",
                     stars_note)

consonant_pairs_latex <- consonant_latex %>%
  select(Contrast = contrast, Estimate = estimate, SE, df, `$t$` = t, `$p$` = p) %>%
  kable(format = "latex", booktabs = TRUE, longtable = TRUE, escape = FALSE, align = "lrrrrr", linesep = "",
        caption = "Pairwise comparisons of the consonant targets, by outcome.", label = "consonant_pairs") %>%
  kable_styling(latex_options = "repeat_header", font_size = 9) %>%
  pack_rows(index = block_index, latex_gap_space = "0.5em")
consonant_pairs_latex <- structure(
  paste0(consonant_pairs_latex, "\n\\par\\smallskip\n{\\footnotesize\\raggedright\\textit{Note.} ", table_note, "}\n"),
  format = "latex", class = "knitr_kable")
cat(consonant_pairs_latex, file = file.path(table_dir, "consonant_pairs_table.tex"))

### VERSION 2 ###

## Consonant analysis for PharyLary: models, pairwise-comparison tables and plots
## author: ben lang, blang@ucsd.edu
#
# Layout (each part uses the objects made by the one before it; no custom functions)
#   1. Libraries
#   2. Load the data
#   3. Prepare the data         -> subset_mean_collapsed (one row per participant x word x consonant)
#   4. Models                   -> one formula template, one row of model_specs per feature  -> consonant_results
#   5. Pairwise comparisons     -> consonant_pairs_table (tidy) and consonant_pairs_latex (kable, saved as .tex)
#   6. Plots                    -> one plot per feature, read in from plot_specs             -> consonant_plots

## ---- 1. Libraries -----------------------------------------------------------------------------------------
library(lmerTest)
library(emmeans)
library(tidyverse)     # dplyr, ggplot2, stringr, purrr, tidyr, tibble
library(gghalves)
library(gridExtra)
library(grid)
library(knitr)
library(kableExtra)
emm_options(pbkrtest.limit = 20000, lmerTest.limit = 20000)

## ---- 2. Load the data -------------------------------------------------------------------------------------
# Define data paths - check which volume is available
sync_paths <- c('/Volumes/circe',
                '/Volumes/cassandra')
base_path <- sync_paths[dir.exists(sync_paths)][1]

orig_data_path <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/pharylary_subset_mean_truncated.csv')
subset_mean <- read.csv(orig_data_path)

## ---- 3. Prepare the data ----------------------------------------------------------------------------------
# `interval` is the consonant (ħ ʕ h ʔ w j). `interval_jw` is the same label with w and j merged into one, j/w; the models and
# plots use `interval_jw`. `interval` is kept as it is, so that w and j can still be left out of a model (see drop_consonants below).
# The reference level is the sonorant j/w (stable voicing).
carrier_ids <- c(
  "Y0409", "Y0395", "Y0388", "Y0401", "Y0396",
  "SD0001", "Y0410", "SD0002", "SD0003", "SD0004", "SD0005",
  "Y0415", "A0249", "Y0208",
  "SD0006", "SD0007", "SD0008", "SD0009", "SD0010",
  "SD0011", "SD0012", "SD0013", "SD0014", "SD0015"
)

subset_mean <- subset_mean %>%
  mutate(interval_jw = if_else(interval %in% c("j", "w"), "j/w", interval),     # computed first, from the original labels
         interval_jw = relevel(factor(interval_jw), ref = "j/w"),
         carrierphrase = factor(if_else(participant %in% carrier_ids, 1, 2)))

# (not used by the models below; kept from the old script)
subset_mean$Position <- factor(subset_mean$Position, levels = c("Initial", "Medial"))
contrasts(subset_mean$Position) <- contr.treatment(2)

# The data has one row per millisecond frame. Every column that differs between the frames of one participant x word x consonant
# (the ms-level columns) is dropped, so what is left is one row per participant x word x consonant: the token means, z-scores and flags.
varies <- subset_mean %>%
  group_by(participant, phrase, interval) %>%
  summarise(across(everything(), ~ n_distinct(.x) > 1), .groups = "drop") %>%
  summarise(across(-c(participant, phrase, interval), any)) %>%
  unlist()
ms_cols <- names(varies)[varies]
cat("Columns that differ within a participant x word x consonant (dropped):", paste(ms_cols, collapse = ", "), "\n")

subset_mean_collapsed <- subset_mean %>%
  select(-all_of(ms_cols)) %>%
  distinct()
cat("Rows after collapsing:", nrow(subset_mean_collapsed), "| distinct participant x word x consonant:",
    n_distinct(subset_mean$participant, subset_mean$phrase, subset_mean$interval), "(these two numbers should be equal)\n")
print(count(subset_mean_collapsed, interval, interval_jw))                      # how the labels map: w and j both become j/w

## ---- 4. Models --------------------------------------------------------------------------------------------
# One row per model: the outcome and the column that flags its outliers.
model_specs <- tribble(
  ~name,   ~response,      ~flag,
  "f0",    "strF0_mean",   "strF0_mean_z_outlier",
  "CPP",   "CPP_mean_log", "CPP_mean_log_z_outlier",
  "soe",   "soe_mean_log", "soe_mean_log_z_outlier",
  "F1",    "F1n_mean",     "formant_outlier",
  "F2",    "F2n_mean",     "formant_outlier",
  "H1res", "H1res_mean",   "H1res_mean_z_outlier"
)

# Consonants that are left out of a model and of its plot, by feature. A feature that is not listed keeps all of them.
drop_consonants <- list(
  F1 = c("j", "w"),               # the formant models do not use j or w
  F2 = c("j", "w")
)

consonant_results <- list()
for (i in seq_len(nrow(model_specs))) {
  spec <- model_specs[i, ]
  
  # the data: flagged outliers removed (the z-score flags are "OK" or "outlier"; formant_outlier is "outlier" or NA),
  # and the consonants this feature leaves out (nothing is removed when the feature is not in drop_consonants)
  dat <- subset_mean_collapsed %>%
    filter(is.na(.data[[spec$flag]]) | .data[[spec$flag]] != "outlier",
           !interval %in% drop_consonants[[spec$name]]) %>%
    droplevels()
  
  # the formula template: the same for every feature, only the outcome changes
  f <- as.formula(paste(spec$response, "~ interval_jw + (1|participant) + (1|phrase) + (1|carrierphrase)"))
  mod <- eval(bquote(lmer(.(f), data = dat)))                    # .(f) puts the formula itself in the call, so summary() prints it
  
  # estimated means per consonant and all pairwise comparisons (Tukey-adjusted within the model), in the order emmeans lists them
  emms <- emmeans(mod, ~ interval_jw, lmer.df = "satterthwaite", data = dat)
  
  consonant_results[[spec$name]] <- list(model = mod,
                                         summary = summary(mod, correlation = FALSE),
                                         emmeans = emms,
                                         pairs = as.data.frame(pairs(emms)),
                                         n_obs = nrow(dat),
                                         data = dat)
}
# consonant_results$F1$summary         # the model summary of one feature
# consonant_results$F1$pairs           # its pairwise comparisons

## ---- 5. Pairwise comparisons: tidy table and LaTeX table ----------------------------------------------------
table_outcomes <- c("F1", "F2", "H1res", "CPP", "soe")          # the features in the table, in table order (add "f0" to include it)
table_outcome_labels <- c(f0 = "F0", CPP = "CPP", soe = "SoE", F1 = "F1", F2 = "F2", H1res = "Residual H1*")

# all comparisons of those models in one table, in the order emmeans lists them
consonant_all_pairs <- map_dfr(consonant_results[table_outcomes], ~ .x$pairs, .id = "outcome")

# the tidy table (same layout as pairs_table): rounded, with stars, sorted by p within each outcome
consonant_pairs_table <- consonant_all_pairs %>%
  mutate(outcome = factor(outcome, levels = table_outcomes),
         contrast = as.character(contrast),
         sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ "")) %>%
  arrange(outcome, p.value) %>%
  transmute(outcome = as.character(outcome), contrast,
            estimate = round(estimate, 3), SE = round(SE, 3), df = round(df, 1),
            t.ratio = round(t.ratio, 2), p.value = signif(p.value, 3), sig) %>%
  as_tibble()
print(consonant_pairs_table, n = Inf)             # View(consonant_pairs_table) opens it in RStudio
# write.csv(consonant_pairs_table, "consonant_pairs_table.csv", row.names = FALSE)     # save a copy (UTF-8)

# The LaTeX table: one block per outcome, the comparisons in the order emmeans lists them (the same order in every block that has
# the same consonants). The text of each cell is built here: LaTeX math for the minus sign and "<", p-value stars as superscripts.
table_dir <- file.path(base_path, "alldata/dissertation/1A/tables")      # where the .tex file is saved
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

consonant_latex <- consonant_all_pairs %>%
  mutate(outcome = factor(outcome, levels = table_outcomes, labels = unname(table_outcome_labels[table_outcomes])),
         contrast = str_remove_all(as.character(contrast), "[()]"),                      # emmeans writes (j/w) - h; the brackets are dropped
         contrast = str_replace_all(contrast, " - ", " $-$ "),
         estimate = str_replace(sprintf("%.3f", round(estimate, 3) + 0), "^-", "$-$"),   # + 0 turns -0.000 into 0.000
         SE = sprintf("%.3f", SE),
         df = sprintf("%.1f", df),
         t = str_replace(sprintf("%.2f", round(t.ratio, 2) + 0), "^-", "$-$"),
         p = case_when(is.na(p.value) ~ "--", p.value < .001 ~ "$<$.001", TRUE ~ sub("^0", "", sprintf("%.3f", p.value))),   # .034 (no leading zero)
         stars = case_when(is.na(p.value) ~ "", p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ ""),
         p = ifelse(stars == "", p, paste0(p, "\\textsuperscript{", stars, "}")))

# the number of rows in each block, in the order the blocks are in (for pack_rows)
block_counts <- table(factor(as.character(consonant_latex$outcome), levels = unique(as.character(consonant_latex$outcome))))
block_index <- setNames(as.integer(block_counts), names(block_counts))

# the note goes under the table as a normal, wrapping paragraph (kableExtra's footnote() would put it in one unwrapped line inside the
# longtable, which makes the table wider than the page)
stars_note <- "\\textsuperscript{.}\\,$p<.1$; \\textsuperscript{*}\\,$p<.05$; \\textsuperscript{**}\\,$p<.01$; \\textsuperscript{***}\\,$p<.001$."
table_note <- paste0("Estimates are the first segment minus the second. ",
                     "The $p$-values are Tukey-adjusted within each model (", paste(unique(block_index), collapse = " or "), " comparisons per model). ",
                     stars_note)

consonant_pairs_latex <- consonant_latex %>%
  select(Contrast = contrast, Estimate = estimate, SE, df, `$t$` = t, `$p$` = p) %>%
  kable(format = "latex", booktabs = TRUE, longtable = TRUE, escape = FALSE, align = "lrrrrr", linesep = "",
        caption = "Pairwise comparisons of Segment for Pharyngeal, Laryngeal, and Approximant Groups", label = "consonant_pairs") %>%
  kable_styling(latex_options = "repeat_header", font_size = 9) %>%
  pack_rows(index = block_index, latex_gap_space = "0.5em")
consonant_pairs_latex <- structure(
  paste0(consonant_pairs_latex, "\n\\par\\smallskip\n{\\footnotesize\\raggedright\\textit{Note.} ", table_note, "}\n"),
  format = "latex", class = "knitr_kable")
cat(consonant_pairs_latex, file = file.path(table_dir, "consonant_pairs_table.tex"))
# consonant_pairs_latex             # type the name to print the LaTeX code

## ---- 6. Plots ---------------------------------------------------------------------------------------------
# One plot per feature: half violin + box plot of the z-scored token values, one per consonant. The data are the same rows the
# model used (same outlier flags, same consonants left out), so a consonant that a model leaves out is missing from its plot too.

interval_colors <- c(
  "ħ"   = "#e7298a",
  "ʕ"   = "#7570b3",
  "h"   = "#1b9e77",
  "ʔ"   = "#66a61e",
  "j/w" = "#d95f02"
)

# the order of the consonants in each plot
# interval_orders <- list(
#   H1res = c("h", "j/w", "ʕ", "ħ", "ʔ"),
#   CPP   = c("j/w", "ʕ", "ʔ", "h", "ħ"),
#   soe   = c("j/w", "ʕ", "h", "ʔ", "ħ"),
#   F1    = c("ʕ", "ħ", "h", "ʔ"),
#   F2    = c("ʕ", "ħ", "h", "ʔ"),
#   f0    = c("h", "j/w", "ʕ", "ħ", "ʔ")
# )

interval_orders <- list(
  H1res = c("h", "j/w", "ʕ", "ħ", "ʔ"),
  CPP   = c("h", "j/w", "ʕ", "ħ", "ʔ"),
  soe   = c("h", "j/w", "ʕ", "ħ", "ʔ"),
  F1    = c("ʕ", "ħ", "h", "ʔ"),
  F2    = c("ʕ", "ħ", "h", "ʔ"),
  f0    = c("h", "j/w", "ʕ", "ħ", "ʔ")
)

# One row per plot: the column that is plotted, its labels, the legend position, text sizes and the file name.
# The outlier flag comes from model_specs and the left-out consonants from drop_consonants, so a plot always matches its model.
plot_specs <- tribble(
  ~name,   ~y_col,           ~y_label,                  ~title,                        ~legend, ~title_size, ~xlab_size, ~file,
  "H1res", "H1res_mean_z",   "Normalized Residual H1*", "Residual H1*",                "none",  34,          20,         "resH1.tif",
  "CPP",   "CPP_mean_log_z", "Normalized CPP",          "CPP",                         "none",  34,          20,         "CPP.tif",
  "soe",   "soe_mean_log_z", "Normalized SoE",          "SoE",                         "none",  34,          20,         "soe.tif",
  "F1",    "F1n_mean",       "Normalized F1",           "F1",                          "none",  34,          20,         "F1.tif",
  "F2",    "F2n_mean",       "Normalized F2",           "F2",                          "none",  34,          20,         "F2.tif",
  "f0",    "strF0_mean_z",   "Normalized f0",           "Fundamental Frequency (f0)",  "left",  34,          20,         "f0.tif"
) %>%
  left_join(select(model_specs, name, flag), by = "name")

consonant_plots <- list()
for (i in seq_len(nrow(plot_specs))) {
  spec <- plot_specs[i, ]
  plot_order <- interval_orders[[spec$name]]
  
  # the rows the model used; `interval_plot` is the consonant (in plot order) and `value` the plotted column
  plot_data <- subset_mean_collapsed %>%
    filter(is.na(.data[[spec$flag]]) | .data[[spec$flag]] != "outlier",
           !interval %in% drop_consonants[[spec$name]]) %>%
    transmute(interval_plot = factor(interval_jw, levels = plot_order),
              value = .data[[spec$y_col]])
  legend_data <- tibble(interval_plot = factor(plot_order, levels = plot_order))      # one row per consonant, for the legend squares
  
  p <- ggplot(plot_data, aes(x = interval_plot, y = value, fill = interval_plot)) +
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
      data = legend_data,
      aes(x = interval_plot, y = NA_real_, fill = interval_plot),      # NA_real_ (not NA) so every ggplot2 version accepts it
      shape = 22, size = 8, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
    ) +
    coord_flip(clip = "off") +
    labs(x = NULL, y = spec$y_label, title = spec$title) +
    scale_fill_manual(values = interval_colors, name = "Segment") +
    guides(
      fill = guide_legend(
        reverse = TRUE,
        override.aes = list(shape = 22, size = 8, colour = "black", alpha = 1)
      )
    ) +
    theme_minimal(base_size = 18) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = spec$legend,
      legend.direction = "vertical",
      legend.background = element_rect(fill = "white", color = "white"),
      legend.key.size = unit(1.2, "cm"),                    # makes legend squares larger
      legend.title = element_text(size = 22),
      legend.text  = element_text(size = 20),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x  = element_text(size = 20),
      axis.title.x = element_text(size = spec$xlab_size, margin = margin(t = 10)),
      plot.margin  = margin(20, 20, 20, 20),
      plot.title = element_text(hjust = 0.5, size = spec$title_size)
    )
  
  ggsave(
    filename = spec$file,
    plot = p,
    width = 8,
    height = 8,
    dpi = 600,          # High resolution for printing
    compression = "lzw" # Keeps file size manageable without losing quality
  )
  consonant_plots[[spec$name]] <- p
}

grid.arrange(
  consonant_plots$H1res, consonant_plots$CPP, consonant_plots$soe, consonant_plots$F1,
  ncol = 2, nrow = 2,
  top = grid::textGrob("Acoustic Feature Means for Laryngeal and Pharyngeal Consonants", gp = grid::gpar(fontsize = 20))
)

# the legend of the f0 plot (the plot that has one), moved to the right and drawn as a fourth item in the same row
legend_grob <- gtable::gtable_filter(ggplotGrob(consonant_plots$f0 + theme(legend.position = "right")), "guide-box")

featuregrid <- grid.arrange(
  consonant_plots$H1res, consonant_plots$CPP, consonant_plots$soe, legend_grob,
  ncol = 4, nrow = 1,
  widths = grid::unit.c(unit(c(1, 1, 1), "null"), sum(legend_grob$widths))     # three equal plots, then a column exactly as wide as the legend
)

ggsave(
  filename = file.path(base_path,'alldata/dissertation/1A/figs/',"resH1_CPP_soe_grid.pdf"),
  plot = featuregrid,
  width = 16,
  height = 8,
  dpi = 600,          # High resolution for printing
  # compression = "lzw", # Keeps file size manageable without losing quality
  device = cairo_pdf
)

# the legend of the F1 plot (F1 and F2 have the same four consonants), moved to the right as a third item in the same row
legend_grob <- gtable::gtable_filter(ggplotGrob(consonant_plots$F1 + theme(legend.position = "right")), "guide-box")

featuregrid <- grid.arrange(
  consonant_plots$F1, consonant_plots$F2, legend_grob,
  ncol = 3, nrow = 1,
  widths = grid::unit.c(unit(c(1, 1), "null"), sum(legend_grob$widths))     # two equal plots, then a column exactly as wide as the legend
)

ggsave(
  filename = file.path(base_path,'alldata/dissertation/1A/figs/',"F1_F2_grid.pdf"),
  plot = featuregrid,
  width = 12,
  height = 8,
  device = cairo_pdf
)

## time series

# ---- data: output of section 9 of the preprocessing script ------------------------------------------------
base_path <- c("/Volumes/circe", "/Volumes/cassandra")[dir.exists(c("/Volumes/circe", "/Volumes/cassandra"))][1]
vseq_path <- file.path(base_path, "alldata/dissertation/vs/output_preproc/pharylary_vsequence_for_gamm.csv")

d <- read_csv(vseq_path) |>
  filter(is.na(formant_outlier)) |>                      # drops whole events flagged by the Mahalanobis step
  mutate(cons = factor(vseq_consonant, levels = c("h", "ʔ", "ħ", "ʕ", "son")),   # h = reference
         # cons = factor(cons_detail, levels = c("h", "ʔ", "ħ", "ʕ", "j", "w")), # use this to split son into j / w
         cons_ord = ordered(cons, levels = levels(cons)),
         participant = factor(participant),
         phrase = factor(phrase),
         event = factor(event)) |>                       # event = token_id: one V-sequence interval in one file
  arrange(event, t_ms)

# ---- surrounding vowels: computed here from the sub-segments (vs_label) of each event ---------------------------
# vs_label / seg_Start / seg_End describe the sub-segments inside a V-sequence interval (e.g. ɑː then ʔ). In each event
# the target is the non-vowel sub-segment; the vowel BEFORE it and the vowel AFTER it are the vowel sub-segments next to it.

vowel_chars <- "aeiouyæɑɔəɛɪʊʌɤɯɜɒ"                    # same vowel symbols as the preprocessing script
strip_marks <- "[ːˑ:]|\\p{M}"                           # length marks and combining marks / tie bars are ignored

# a = ɑ, a, æ    i = i, ɪ    u = u, ʊ. Any other vowel label (diphthongs aɪ / aʊ, e, o ...) maps to NA and is left out.
vowel_key <- c("ɑ" = "a", "a" = "a", "æ" = "a", "i" = "i", "ɪ" = "i", "u" = "u", "ʊ" = "u")

# Swap key: relabel vowels in particular phrases first (copy of vowel_swap_key in the preprocessing script).
vowel_swap_key <- tribble(
  ~phrase, ~from, ~to,
  82,      "ə",   "a",
  121,     "e",   "a",
  122,     "e",   "a",
  123,     "e",   "a"
)
swap_lookup <- setNames(vowel_swap_key$to, paste(vowel_swap_key$phrase, vowel_swap_key$from))
apply_swap <- function(phrase, label) {
  k <- paste(phrase, label)
  hit <- k %in% names(swap_lookup)
  label[hit] <- unname(swap_lookup[k[hit]])
  label
}

# which vowels are kept in each position
edge_vowels   <- c("a", "i", "u")   # initial (CV, the vowel after) and final (VC, the vowel before)
medial_vowels <- "a"                # medial (VCV): only when this same vowel is on BOTH sides of the target

pick_vowel <- function(label, start, ok, which_one) {   # the vowel sub-segment adjacent to the target
  if (!any(ok, na.rm = TRUE)) return(NA_character_)
  i <- which(ok)
  i <- if (which_one == "last") i[which.max(start[i])] else i[which.min(start[i])]
  label[i]
}

tol <- 1   # ms; neighbouring sub-segments share a boundary, so allow a little slack

subseg <- d |>
  distinct(event, vs_label, seg_Start, seg_End) |>
  mutate(label_clean = str_remove_all(stringi::stri_trans_nfc(str_trim(vs_label)), strip_marks),
         is_vowel_seg = str_detect(label_clean, paste0("^[", vowel_chars, "]+$")))

event_vowels <- subseg |>
  group_by(event) |>
  summarise(
    has_target = any(!is_vowel_seg),
    tgt_start  = if (any(!is_vowel_seg)) min(seg_Start[!is_vowel_seg]) else NA_real_,
    tgt_end    = if (any(!is_vowel_seg)) max(seg_End[!is_vowel_seg])   else NA_real_,
    n_before   = sum(is_vowel_seg & seg_End   <= tgt_start + tol, na.rm = TRUE),
    n_after    = sum(is_vowel_seg & seg_Start >= tgt_end   - tol, na.rm = TRUE),
    vowel_before_raw = pick_vowel(label_clean, seg_Start, is_vowel_seg & seg_End   <= tgt_start + tol, "last"),
    vowel_after_raw  = pick_vowel(label_clean, seg_Start, is_vowel_seg & seg_Start >= tgt_end   - tol, "first"),
    .groups = "drop"
  ) |>
  left_join(distinct(d, event, phrase, position_from_label), by = "event") |>
  mutate(
    vowel_before_label = apply_swap(as.character(phrase), vowel_before_raw),
    vowel_after_label  = apply_swap(as.character(phrase), vowel_after_raw),
    vowel_before = unname(vowel_key[vowel_before_label]),     # a / i / u / NA
    vowel_after  = unname(vowel_key[vowel_after_label]),
    vowel_ctx = case_when(
      position_from_label == "VC"  & vowel_before %in% edge_vowels   ~ vowel_before,
      position_from_label == "CV"  & vowel_after  %in% edge_vowels   ~ vowel_after,
      position_from_label == "VCV" & vowel_before %in% medial_vowels & vowel_after == vowel_before ~ vowel_before,
      TRUE ~ NA_character_)
  )

d <- d |> left_join(select(event_vowels, event, vowel_before, vowel_after, vowel_ctx), by = "event")

# ---- checks: read these before modeling ----------------------------------------------------------------------------
cat("\n--- Neighbouring vowel labels -> category (NA = not in vowel_key, so that event is left out) ---\n")
print(bind_rows(event_vowels |> transmute(side = "before", raw = vowel_before_raw, swapped = vowel_before_label, category = vowel_before),
                event_vowels |> transmute(side = "after",  raw = vowel_after_raw,  swapped = vowel_after_label,  category = vowel_after)) |>
        filter(!is.na(raw)) |> count(side, raw, swapped, category, sort = TRUE), n = Inf)

cat("\nEvents with no target sub-segment found:", sum(!event_vowels$has_target), "\n")
cat("Events with more than one vowel sub-segment on a side (the one next to the target is used): before",
    sum(event_vowels$n_before > 1), "| after", sum(event_vowels$n_after > 1), "\n")
cat("VCV events with a different vowel on each side:",
    sum(event_vowels$position_from_label == "VCV" & !is.na(event_vowels$vowel_before) & !is.na(event_vowels$vowel_after) &
          event_vowels$vowel_before != event_vowels$vowel_after), "\n")

cat("\n--- Events kept (vowel_ctx not NA) by position ---\n")
print(event_vowels |> count(position_from_label, kept = !is.na(vowel_ctx)), n = Inf)

cat("\n--- Events per position x vowel context x consonant ---\n")
print(d |> distinct(event, cons, position_from_label, vowel_ctx) |>
        filter(!is.na(vowel_ctx)) |>
        count(position_from_label, vowel_ctx, cons) |>
        pivot_wider(names_from = cons, values_from = n, values_fill = 0), n = Inf)

# ---- choose a response, a position and a vowel ---------------------------------------------------------------------
# Response must be a PER-FRAME column (not *_mean): H1H2c, CPP_log, soe_log, Energy_log, H1res, strF0, HNR05,
# SHR, F1n, F2n, F3n ... (the *_z versions work too if you want everything on a per-speaker z scale).
feature <- "H1H2c"
pos     <- "VCV"          # CV (initial), VC (final) or VCV (medial); one model per position and vowel
vowel   <- "a"            # the vowel context held constant: a, i or u (VCV only has medial_vowels)

dat <- d |>
  filter(position_from_label == pos, vowel_ctx == vowel, !is.na(.data[[feature]])) |>
  droplevels() |>
  as.data.frame() |>                                     # itsadug can't sort tibbles
  start_event(column = "t_ms", event = "event")          # adds start.event for AR.start

# ---- 1. baseline model without AR1 ------------------------------------------------------------------------
f <- as.formula(paste0(feature, " ~ cons_ord +
                         s(t_prop, k = 10) +
                         s(t_prop, by = cons_ord, k = 10) +
                         s(t_prop, participant, bs = 'fs', m = 1, k = 5) +
                         s(phrase, bs = 're')"))

m0 <- bam(f, data = dat, method = "fREML", discrete = TRUE)

# ---- 2. check autocorrelation, then refit with AR1 --------------------------------------------------------
acf_resid(m0)
r1 <- start_value_rho(m0)

m1 <- bam(formula(m0), data = dat, method = "fREML", discrete = TRUE,
          rho = r1, AR.start = dat$start.event)

summary(m1)
gam.check(m1)

# ---- 3. plots -----------------------------------------------------------------------------------------------
plot_smooth(m1, view = "t_prop", plot_all = "cons_ord", rm.ranef = TRUE)
plot_diff(m1, view = "t_prop", comp = list(cons_ord = c("ħ", "ʕ")), rm.ranef = TRUE)





# #### rain clouds
# #### not super functional now
# 
# rain_height <- .1
# 
# # Calculate stagger offsets based on the number of levels in 'interval'
# num_levels <- length(levels(factor(unique_data$interval)))
# stagger_offsets <- seq(-rain_height / 1.5, rain_height / 1.5, length.out = num_levels)
# 
# 
# #### H1*-H2* ####
# plot1 <- ggplot(unique_data, aes(x = "", y = H1H2c_mean_unique, fill = interval)) +
#   # clouds
#   introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
#                                  position = position_nudge(x = rain_height+.05)) +
#   # rain
#   geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
#              position = position_jitter(width = rain_height, height = 0)) +
#   # boxplots
#   geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
#                outlier.shape = NA,
#                # position = position_nudge(x = -rain_height*2) +
#                position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
#   # coord_flip() +
#   # mean and SE point in the cloud
#   # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
#   #              position = position_nudge(x = rain_height * 3)) +
#   # adjust layout
#   scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
#   scale_y_continuous(name = "H1*-H2* (dB)",
#                      # breaks = seq(-30, 2, 30), 
#                      # limits = c(-30, 30)) +
#   ) +
#   coord_flip() +
#   # facet_wrap(~factor(facet_contrast, 
#   #                    levels = c("vcl", "v","son"), 
#   #                    labels = c("ħ", "ʕ","j/w")), 
#   #            nrow = 1) +
#   # custom colours and theme
#   scale_fill_brewer(palette = "Dark2", name = "Segment") +
#   scale_colour_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position.inside = c(0.9, 0.8),
#         legend.background = element_rect(fill = "white", color = "white"),
#         # strip.text = element_text(size = 12), # Adjust font size for facet labels
#         axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
#         axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
#         legend.text = element_text(size = 14), # Adjust font size for legend text
#         legend.title = element_text(size = 14) # Adjust font size for legend title
#   )
# 
# 
# 
# #### CPP ####
# 
# plot2 <- ggplot(unique_data, aes(x = "", y = CPP_mean_unique, fill = interval)) +
#   # clouds
#   introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
#                                  position = position_nudge(x = rain_height+.05)) +
#   # rain
#   geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
#              position = position_jitter(width = rain_height, height = 0)) +
#   # boxplots
#   geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
#                outlier.shape = NA,
#                # position = position_nudge(x = -rain_height*2) +
#                position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
#   # geom_signif(comparisons = list(c('h','ʔ','ħ','ʕ')), 
#   # map_signif_level=TRUE) + 
#   # coord_flip() +
#   # mean and SE point in the cloud
#   # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
#   #              position = position_nudge(x = rain_height * 3)) +
#   # adjust layout
#   scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
#   scale_y_continuous(name = "CPP (dB)",
#                      # breaks = seq(-30, 2, 30), 
#                      # limits = c(-30, 30)) +
#   ) +
#   coord_flip() +
#   # facet_wrap(~factor(facet_contrast, 
#   #                    levels = c("vcl", "v","son"), 
#   #                    labels = c("ħ", "ʕ","j/w")), 
#   #            nrow = 1) +
#   # custom colours and theme
#   scale_fill_brewer(palette = "Dark2", name = "Segment") +
#   scale_colour_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position.inside = c(0.9, 0.8),
#         legend.background = element_rect(fill = "white", color = "white"),
#         axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
#         axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
#         legend.text = element_text(size = 14), # Adjust font size for legend text
#         legend.title = element_text(size = 14) # Adjust font size for legend title
#   ) #+ facet_wrap(~Position, ncol = 2)
# 
# #### SoE ####
# 
# plot3 <- ggplot(unique_data, aes(x = "", y = soe_mean_unique*10, fill = interval)) +
#   # clouds
#   introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
#                                  position = position_nudge(x = rain_height+.05)) +
#   # rain
#   geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
#              position = position_jitter(width = rain_height, height = 0)) +
#   # boxplots
#   geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
#                outlier.shape = NA,
#                # position = position_nudge(x = -rain_height*2) +
#                position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
#   # coord_flip() +
#   # mean and SE point in the cloud
#   # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
#   #              position = position_nudge(x = rain_height * 3)) +
#   # adjust layout
#   scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
#   scale_y_continuous(name = "SoE (dB)",
#                      # breaks = seq(-30, 2, 30), 
#                      # limits = c(-30, 30)) +
#   ) +
#   coord_flip() +
#   # facet_wrap(~factor(facet_contrast, 
#   #                    levels = c("vcl", "v","son"), 
#   #                    labels = c("ħ", "ʕ","j/w")), 
#   #            nrow = 1) +
#   # custom colours and theme
#   scale_fill_brewer(palette = "Dark2", name = "Segment") +
#   scale_colour_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position.inside = c(0.9, 0.8),
#         legend.background = element_rect(fill = "white", color = "white"),
#         axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
#         axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
#         legend.text = element_text(size = 14), # Adjust font size for legend text
#         legend.title = element_text(size = 14) # Adjust font size for legend title
#   )
# 
# 
# #### F1 ####
# 
# plot4 <- ggplot(unique_data, aes(x = "", y = sF1_mean_unique, fill = interval)) +
#   # clouds
#   introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
#                                  position = position_nudge(x = rain_height+.05)) +
#   # rain
#   geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
#              position = position_jitter(width = rain_height, height = 0)) +
#   # boxplots
#   geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
#                outlier.shape = NA,
#                position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
#   # coord_flip() +
#   # mean and SE point in the cloud
#   # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
#   #              position = position_nudge(x = rain_height * 3)) +
#   # adjust layout
#   scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
#   scale_y_continuous(name = "F1 (Hz)",
#                      # breaks = seq(-30, 2, 30), 
#                      # limits = c(-30, 30)) +
#   ) +
#   coord_flip() +
#   # facet_wrap(~factor(facet_contrast, 
#   #                    levels = c("vcl", "v"), 
#   #                    labels = c("h - ħ", "ʔ - ʕ")), 
#   #            nrow = 2) +
#   # custom colours and theme
#   scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
#   scale_colour_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position.inside = c(0.9, 0.8),
#         legend.background = element_rect(fill = "white", color = "white"),
#         axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
#         axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
#         legend.text = element_text(size = 14), # Adjust font size for legend text
#         legend.title = element_text(size = 14) # Adjust font size for legend title
#   )
# 
# #### F2 ####
# 
# plot5 <- ggplot(unique_data, aes(x = "", y = sF2_mean_unique, fill = interval)) +
#   # clouds
#   introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
#                                  position = position_nudge(x = rain_height+.05)) +
#   # rain
#   geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
#              position = position_jitter(width = rain_height, height = 0)) +
#   # boxplots
#   geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
#                outlier.shape = NA,
#                position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
#   # coord_flip() +
#   # mean and SE point in the cloud
#   # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
#   #              position = position_nudge(x = rain_height * 3)) +
#   # adjust layout
#   scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
#   scale_y_continuous(name = "F2 (Hz)",
#                      # breaks = seq(-30, 2, 30), 
#                      # limits = c(-30, 30)) +
#   ) +
#   coord_flip() +
#   # facet_wrap(~factor(facet_contrast, 
#   #                    levels = c("vcl", "v"), 
#   #                    labels = c("h - ħ", "ʔ - ʕ")), 
#   #            nrow = 2) +
#   # custom colours and theme
#   scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
#   scale_colour_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position.inside = c(0.9, 0.9),
#         legend.background = element_rect(fill = "white", color = "white"))
# 
# #### HNR05 ####
# 
# plot6 <- ggplot(unique_data, aes(x = "", y = HNR05_mean_unique, fill = interval)) +
#   # clouds
#   introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
#                                  position = position_nudge(x = rain_height+.05)) +
#   # rain
#   geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
#              position = position_jitter(width = rain_height, height = 0)) +
#   # boxplots
#   geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
#                outlier.shape = NA,
#                position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
#   # coord_flip() +
#   # mean and SE point in the cloud
#   # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
#   #              position = position_nudge(x = rain_height * 3)) +
#   # adjust layout
#   scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
#   scale_y_continuous(name = "HNR05 (dB)",
#                      # breaks = seq(-30, 2, 30), 
#                      # limits = c(-30, 30)) +
#   ) +
#   coord_flip() +
#   # facet_wrap(~factor(facet_contrast, 
#   #                    levels = c("vcl", "v"), 
#   #                    labels = c("h - ħ", "ʔ - ʕ")), 
#   #            nrow = 2) +
#   # custom colours and theme
#   scale_fill_brewer(palette = "Dark2", name = "Contrast Type") +
#   scale_colour_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position.inside = c(0.9, 0.9),
#         legend.background = element_rect(fill = "white", color = "white"))
# 
# #### Residual H1* ####
# 
# plot7 <- ggplot(unique_data, aes(x = "", y = H1c.resid_mean_unique, fill = interval)) +
#   # clouds
#   introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
#                                  position = position_nudge(x = rain_height+.05)) +
#   # rain
#   geom_point(aes(colour = interval), size = 2, alpha = .5, show.legend = FALSE, 
#              position = position_jitter(width = rain_height, height = 0)) +
#   # boxplots
#   geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
#                outlier.shape = NA,
#                # position = position_nudge(x = -rain_height*2) +
#                position = position_nudge(x = -rain_height*2), aes(x = 0.95 + stagger_offsets[as.numeric(factor(interval))])) +
#   # coord_flip() +
#   # mean and SE point in the cloud
#   # stat_summary(fun.data = mean_cl_normal, mapping = aes(color = interval), show.legend = FALSE,
#   #              position = position_nudge(x = rain_height * 3)) +
#   # adjust layout
#   scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
#   scale_y_continuous(name = "SoE (dB)",
#                      # breaks = seq(-30, 2, 30), 
#                      # limits = c(-30, 30)) +
#   ) +
#   coord_flip() +
#   # facet_wrap(~factor(facet_contrast, 
#   #                    levels = c("vcl", "v","son"), 
#   #                    labels = c("ħ", "ʕ","j/w")), 
#   #            nrow = 1) +
#   # custom colours and theme
#   scale_fill_brewer(palette = "Dark2", name = "Segment") +
#   scale_colour_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position.inside = c(0.9, 0.8),
#         legend.background = element_rect(fill = "white", color = "white"),
#         axis.title.x = element_text(size = 18), # Adjust font size for y-axis labels
#         axis.text.x = element_text(size = 14), # Adjust font size for x-axis tick labels
#         legend.text = element_text(size = 14), # Adjust font size for legend text
#         legend.title = element_text(size = 14) # Adjust font size for legend title
#   )


