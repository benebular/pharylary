## convert stimulus tables for appendix
# blang@ucsd.edu

library(tidyr)
library(dplyr)
library(readr)
library(kableExtra)
library(stringr)


## pharyperc
df <- read.csv('/Volumes/cassandra/alldata/dissertation/3/arabic_perception_stimuli - trials_reduced.csv')

df_filtered <- df %>%
  group_by(UniquePairIndex) %>%
  filter(UniquePairIndex == "none" | row_number() == 1) %>%
  ungroup()

df_filtered <- df_filtered %>%
  mutate(TargetSegment = str_replace_all(TargetSegment, "gs", "ʔ"))

df_filtered <- df_filtered %>%
  select(
    'Arabic Orthography' = TargetArabic,
    'Arabic Definition' = TargetDefinition,
    'IPA' = IPA,
    'Target Segment' = TargetSegment,
    'Condition' = Condition,
    'Target Position' = Position,
    'Trial Type' = TrialType
  )

df_filtered <- df_filtered %>%
  mutate(across(-`Arabic Orthography`, ~ str_replace_all(., "_", "\\\\_")))

df_filtered <- df_filtered %>%
  mutate(`Arabic Orthography` = paste0("\\arabicfont{", `Arabic Orthography`, "}"))

kbl(df_filtered, format = "latex", booktabs = TRUE, label = "tab:pharyperc-appendix-stim-table", escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable("/Volumes/cassandra/alldata/dissertation/3/tables/pharyperc_appendix_stim_table.tex")