library(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Load data
dat <- read_csv("'/Users/bcl/GitHub/pharylary/PharyLary Stimuli - pharylary2.csv'")

summary_tbl <- dat %>%
  count(Segment, Position) %>%
  pivot_wider(names_from = Position,
              values_from = n,
              values_fill = 0) %>%
  mutate(Total = rowSums(across(-Segment)), .after = Segment) %>%
  rename(Segment = Segment) %>%
  arrange(desc(Total))

kable(summary_tbl,
      format   = "latex",
      booktabs = TRUE,
      caption  = "Target Segment Counts by Position",
      label    = "segment-counts",
      linesep  = "") %>%
  writeLines("segment_counts_table.tex")