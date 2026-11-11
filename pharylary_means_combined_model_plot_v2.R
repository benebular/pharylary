## PharyLary: models, tables and plots for the consonants and the vowels
## author: ben lang, blang@ucsd.edu
#
#   1. Setup                 libraries, paths, font, and the settings both analyses share
#   2. Data                  both datasets are loaded and prepared the same way -> consonant_data, vowel_data
#   PART A: CONSONANTS       3. models   4. tables   5. box plots   6. heat maps
#   PART B: VOWELS           7. data sets   8. -ized models   9. -ized figures   10. other-target models   11. other-target figures

## ---- 1. Setup ----------------------------------------------------------------------------------------------
library(lmerTest)
library(emmeans)
library(tidyverse)     # dplyr, ggplot2, stringr, purrr, tidyr, tibble
library(gghalves)
library(gridExtra)
library(grid)
library(knitr)
library(kableExtra)
emm_options(pbkrtest.limit = 20000, lmerTest.limit = 20000)

# paths: whichever volume is available
sync_paths <- c('/Volumes/circe',
                '/Volumes/cassandra')
base_path <- sync_paths[dir.exists(sync_paths)][1]
data_dir  <- file.path(base_path, "alldata/dissertation/vs/output_preproc")
fig_dir   <- file.path(base_path, "alldata/dissertation/1A/figs")
table_dir <- file.path(base_path, "alldata/dissertation/1A/tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# font for the figures: must be installed and have the IPA symbols (falls back to the default font with a message)
plot_font <- "Charis SIL"
installed_fonts <- unique(systemfonts::system_fonts()$family)
if (!plot_font %in% installed_fonts) {
  message("Font '", plot_font, "' is not installed, so the default font is used and the IPA symbols may look wrong.\n",
          "Installed fonts that could work: ",
          paste(grep("Charis|Doulos|Gentium|DejaVu|Noto Sans$|Noto Serif$", installed_fonts, value = TRUE), collapse = ", "))
  plot_font <- ""
}

# participants in carrier group 1 (everyone else is group 2)
carrier_ids <- c(
  "Y0409", "Y0395", "Y0388", "Y0401", "Y0396",
  "SD0001", "Y0410", "SD0002", "SD0003", "SD0004", "SD0005",
  "Y0415", "A0249", "Y0208",
  "SD0006", "SD0007", "SD0008", "SD0009", "SD0010",
  "SD0011", "SD0012", "SD0013", "SD0014", "SD0015"
)

# exclusions for BOTH datasets (empty = nothing is excluded)
exclude_participants <- character()                                  # e.g. c("SD0013")
exclude_phrases      <- numeric()                                    # words dropped for everyone, e.g. c(130)
exclude_trials       <- tibble(participant = character(), phrase = numeric())      # one word of one participant, e.g. tibble(participant = "SD0013", phrase = 130)

# one row per feature: the outcome column and the column that flags its outliers (every model below uses this list)
model_specs <- tribble(
  ~name,   ~response,      ~flag,
  "f0",    "strF0_mean",   "strF0_mean_z_outlier",
  "CPP",   "CPP_mean_log", "CPP_mean_log_z_outlier",
  "soe",   "soe_mean_log", "soe_mean_log_z_outlier",
  "F1",    "F1n_mean",     "formant_outlier",
  "F2",    "F2n_mean",     "formant_outlier",
  "H1res", "H1res_mean",   "H1res_mean_z_outlier"
)
outcome_labels <- c(f0 = "F0", CPP = "CPP", soe = "SoE", F1 = "F1", F2 = "F2", H1res = "Residual H1*")

## ---- 2. Load and prepare the data: the same steps for both datasets ---------------------------------------
datasets <- list(
  consonant = read.csv(file.path(data_dir, "pharylary_subset_mean_truncated.csv")),       # interval = the consonant
  vowel     = read.csv(file.path(data_dir, "pharylary_vowel_context_subset_mean.csv"))    # interval = the vowel, Segment = the target next to it
)

for (name in names(datasets)) {
  d <- datasets[[name]]
  
  # the exclusions above, and the carrier group
  rows_before <- nrow(d)
  d <- d %>%
    filter(!participant %in% exclude_participants, !phrase %in% exclude_phrases) %>%
    anti_join(exclude_trials, by = c("participant", "phrase")) %>%
    mutate(carrierphrase = factor(if_else(participant %in% carrier_ids, 1, 2)))
  cat(name, ": excluded", rows_before - nrow(d), "rows\n")
  
  # The data has one row per millisecond frame. Every column that differs between the frames of one unit (a participant x word x
  # consonant, or a vowel x target pairing = context_id) is dropped, leaving one row per unit: the token means, z-scores and flags.
  fallback_id <- paste(d$participant, d$phrase, d$interval, sep = "|")
  d$unit_id <- if ("context_id" %in% names(d)) coalesce(d$context_id, fallback_id) else fallback_id
  varies <- d %>%
    group_by(unit_id) %>%
    summarise(across(everything(), ~ n_distinct(.x) > 1), .groups = "drop") %>%
    summarise(across(-unit_id, any)) %>%
    unlist()
  ms_cols <- names(varies)[varies]
  d <- d %>% select(-all_of(ms_cols)) %>% distinct()
  cat(name, ":", nrow(d), "rows (one per unit); ms-level columns dropped:", paste(ms_cols, collapse = ", "), "\n")
  
  datasets[[name]] <- d
}
consonant_data <- datasets$consonant
vowel_data     <- datasets$vowel
rm(datasets, d)

## ======================================================================================================
## PART A: CONSONANTS
## ======================================================================================================

## ---- 3. Consonant models -----------------------------------------------------------------------------------
# interval = the consonant (ħ ʕ h ʔ w j). interval_jw is the same label with w and j merged into j/w (the reference level, a sonorant);
# the models and plots use interval_jw. interval stays as it is so that w and j can be left out of a model (drop_consonants).
consonant_data <- consonant_data %>%
  mutate(interval_jw = if_else(interval %in% c("j", "w"), "j/w", interval),
         interval_jw = relevel(factor(interval_jw), ref = "j/w"))
print(count(consonant_data, interval, interval_jw))                      # how the labels map: w and j both become j/w

# consonants left out of a model and of its plot, by feature (a feature that is not listed keeps all of them)
drop_consonants <- list(
  F1 = c("j", "w"),
  F2 = c("j", "w")
)

# the right-hand side of every consonant model (also used by the formula table in section 4)
consonant_rhs <- "interval_jw + (1|participant) + (1|phrase) + (1|carrierphrase)"

consonant_results <- list()
for (i in seq_len(nrow(model_specs))) {
  spec <- model_specs[i, ]
  
  # flagged outliers removed (z-score flags are "OK" or "outlier"; formant_outlier is "outlier" or NA), and the consonants this feature leaves out
  dat <- consonant_data %>%
    filter(is.na(.data[[spec$flag]]) | .data[[spec$flag]] != "outlier",
           !interval %in% drop_consonants[[spec$name]]) %>%
    droplevels()
  
  f <- as.formula(paste(spec$response, "~", consonant_rhs))
  mod <- eval(bquote(lmer(.(f), data = dat)))                    # .(f) puts the formula itself in the call, so summary() prints it
  emms <- emmeans(mod, ~ interval_jw, lmer.df = "satterthwaite", data = dat)       # all pairwise comparisons are Tukey-adjusted within the model
  
  consonant_results[[spec$name]] <- list(model = mod,
                                         summary = summary(mod, correlation = FALSE),
                                         emmeans = emms,
                                         pairs = as.data.frame(pairs(emms)),
                                         n_obs = nrow(dat),
                                         data = dat)
}
# consonant_results$F1$summary         # the model summary of one feature
# consonant_results$F1$pairs           # its pairwise comparisons

## ---- 4. Consonant tables: tidy table, two LaTeX tables, model formulas --------------------------------------
table_outcomes <- c("F1", "F2", "H1res", "CPP", "soe")          # the features in the tables, in table order (add "f0" to include it)

# all comparisons of those models in one table, in the order emmeans lists them
consonant_all_pairs <- map_dfr(consonant_results[table_outcomes], ~ .x$pairs, .id = "outcome")

# tidy table: rounded, with stars, sorted by p within each outcome
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

# LaTeX tables: one block per outcome, comparisons in the order emmeans lists them. The text of each cell is built here:
# LaTeX math for the minus sign and "<", p-value stars as superscripts.
consonant_latex <- consonant_all_pairs %>%
  mutate(outcome = factor(outcome, levels = table_outcomes, labels = unname(outcome_labels[table_outcomes])),
         contrast = str_remove_all(as.character(contrast), "[()]"),                      # emmeans writes (j/w) - h; the brackets are dropped
         contrast = str_replace_all(contrast, " - ", " $-$ "),
         estimate = str_replace(sprintf("%.3f", round(estimate, 3) + 0), "^-", "$-$"),   # + 0 turns -0.000 into 0.000
         SE = sprintf("%.3f", SE),
         df = sprintf("%.1f", df),
         t = str_replace(sprintf("%.2f", round(t.ratio, 2) + 0), "^-", "$-$"),
         p = case_when(is.na(p.value) ~ "--", p.value < .001 ~ "$<$.001", TRUE ~ sub("^0", "", sprintf("%.3f", p.value))),   # .034 (no leading zero)
         stars = case_when(is.na(p.value) ~ "", p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ ""),
         p = ifelse(stars == "", p, paste0(p, "\\textsuperscript{", stars, "}")))

# two tables: the formants (F1, F2) and the rest. A group's name is also its label (\ref{tab:<name>}) and the start of its .tex file name.
formant_outcomes <- c("F1", "F2")
table_groups <- list(
  consonant_pairs_formants = formant_outcomes,
  consonant_pairs_other    = setdiff(table_outcomes, formant_outcomes)
)
table_captions <- c(
  consonant_pairs_formants = "Pairwise comparisons of Segment for F1 and F2 (Pharyngeal and Laryngeal Groups)",
  consonant_pairs_other    = "Pairwise comparisons of Segment for Residual H1*, CPP, and SoE (Pharyngeal, Laryngeal, and Approximant Groups)"
)

consonant_pairs_latex <- list()
for (table_name in names(table_groups)) {
  table_rows <- consonant_latex %>%
    filter(outcome %in% outcome_labels[table_groups[[table_name]]])
  
  # the number of rows in each block, in the order the blocks are in (for pack_rows)
  block_counts <- table(factor(as.character(table_rows$outcome), levels = unique(as.character(table_rows$outcome))))
  block_index <- setNames(as.integer(block_counts), names(block_counts))
  
  table_note <- paste0("Estimates are the first segment minus the second. ",
                       "The $p$-values are Tukey-adjusted within each model.")
  
  latex <- table_rows %>%
    select(Contrast = contrast, Estimate = estimate, SE, df, `$t$` = t, `$p$` = p) %>%
    kable(format = "latex", booktabs = TRUE, longtable = TRUE, escape = FALSE, align = "lrrrrr", linesep = "",
          caption = table_captions[[table_name]], label = table_name) %>%
    kable_styling(latex_options = "repeat_header", font_size = 9) %>%
    pack_rows(index = block_index, latex_gap_space = "0.5em")
  # the note is a normal wrapping paragraph under the table (kableExtra's footnote() would stretch a longtable past the page)
  latex <- structure(
    paste0(latex, "\n\\par\\smallskip\n{\\footnotesize\\raggedright\\textit{Note.} ", table_note, "}\n"),
    format = "latex", class = "knitr_kable")
  
  consonant_pairs_latex[[table_name]] <- latex
  cat(latex, file = file.path(table_dir, paste0(table_name, "_table.tex")))
}
# consonant_pairs_latex$consonant_pairs_formants      # type this to print the LaTeX code of a table

# formulas of the consonant models (outcome ~ factor + random effects) and the consonants each model leaves out, as a small LaTeX table
readable_names <- c(                       # name in the code = name in the table (replaced in this order; \\b = whole word only)
  setNames(unname(outcome_labels[model_specs$name]), model_specs$response),
  "interval_jw" = "Segment",
  "\\bparticipant\\b" = "Participant",
  "\\bphrase\\b" = "Word",
  "carrierphrase" = "Carrier phrase"
)

model_formulas <- model_specs %>%
  mutate(model = paste(response, "~", consonant_rhs),
         model = str_replace_all(model, readable_names),
         model = str_replace_all(model, fixed(" ~ "), " $\\sim$ "),           # LaTeX symbols for ~ and |
         model = str_replace_all(model, fixed("|"), " $\\mid$ "),
         left_out = map_chr(name, ~ paste(drop_consonants[[.x]], collapse = ", ")),
         left_out = if_else(left_out == "", "none", left_out))
print(select(model_formulas, name, model, left_out))

model_formulas_latex <- model_formulas %>%
  select(Model = model, `Segments left out` = left_out) %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "ll", linesep = "",
        caption = "Model formulas for the consonant analyses.", label = "model_formulas") %>%
  kable_styling(latex_options = "hold_position", font_size = 9)
cat(model_formulas_latex, file = file.path(table_dir, "consonant_model_formulas_table.tex"))

## ---- 5. Consonant box plots ---------------------------------------------------------------------------------
# One plot per feature: half violin + box plot of the z-scored token values, one per consonant. The data are the rows the model used
# (same outlier flags, same consonants left out).

interval_colors <- c(
  "ħ"   = "#e7298a",
  "ʕ"   = "#7570b3",
  "h"   = "#1b9e77",
  "ʔ"   = "#66a61e",
  "j/w" = "#d95f02"
)

# the order of the consonants in each plot
interval_orders <- list(
  H1res = c("h", "j/w", "ʕ", "ħ", "ʔ"),
  CPP   = c("h", "j/w", "ʕ", "ħ", "ʔ"),
  soe   = c("h", "j/w", "ʕ", "ħ", "ʔ"),
  F1    = c("ʕ", "ħ", "h", "ʔ"),
  F2    = c("ʕ", "ħ", "h", "ʔ"),
  f0    = c("h", "j/w", "ʕ", "ħ", "ʔ")
)

# One row per plot: the column that is plotted, its labels, the legend position, text sizes and the file name
# (the outlier flag comes from model_specs and the left-out consonants from drop_consonants, so a plot always matches its model)
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
  
  # `interval_plot` is the consonant (in plot order) and `value` the plotted column
  plot_data <- consonant_data %>%
    filter(is.na(.data[[spec$flag]]) | .data[[spec$flag]] != "outlier",
           !interval %in% drop_consonants[[spec$name]]) %>%
    transmute(interval_plot = factor(interval_jw, levels = plot_order),
              value = .data[[spec$y_col]])
  legend_data <- tibble(interval_plot = factor(plot_order, levels = plot_order))      # for the legend squares
  
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
  filename = file.path(fig_dir, "resH1_CPP_soe_grid.pdf"),
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
  filename = file.path(fig_dir, "F1_F2_grid.pdf"),
  plot = featuregrid,
  width = 12,
  height = 8,
  device = cairo_pdf
)

## ---- 6. Consonant heat maps ----------------------------------------------------------------------------------
# One tile per pair of consonants, coloured by how significant the pair is (Tukey-adjusted p, as in the tables). Two figures, each one
# row of plots: F1 and F2, then Residual H1*, CPP and SoE (in that order).

# tile colours: one per significance level, from "not significant" to the strongest (any four colours; light to dark reads best)
significance_colors <- c("n.s." = "#f2f0f7",       # p >= .05
                         "*"    = "#cbc9e2",      # p < .05
                         "**"   = "#9e9ac8",      # p < .01
                         "***"  = "#6a51a3")      # p < .001
# other ideas (remove the # to use one):
# significance_colors <- c("n.s." = "grey90", "*" = "#fde0c5", "**" = "#f59e72", "***" = "#c0392b")      # light orange to red
# significance_colors <- c("n.s." = "grey90", "*" = "#d9f0d3", "**" = "#7fbf7b", "***" = "#1b7837")      # light green to dark green

# colour of the writing on each tile: white on dark, black on light (worked out from the colours above)
text_colours <- if_else(colSums(col2rgb(significance_colors) * c(0.299, 0.587, 0.114)) / 255 < 0.55, "white", "black")
names(text_colours) <- names(significance_colors)

# the features in each figure, in the order of the plots from left to right
heatmap_groups <- list(
  consonant_heatmap_formants = c("F1", "F2"),
  consonant_heatmap_other    = c("H1res", "CPP", "soe")
)

heatmap_order <- interval_orders$f0        # the order of the consonants on the axes; a pair is always written "earlier - later" in this order

# one row per pairwise comparison: the earlier consonant (a) and the later one (b) of the pair in heatmap_order, and its significance level
pair_summary <- consonant_all_pairs %>%
  mutate(contrast = str_remove_all(as.character(contrast), "[()]")) %>%       # emmeans writes (j/w) - h; the brackets are dropped
  separate(contrast, into = c("first", "second"), sep = " - ") %>%
  mutate(flip = match(first, heatmap_order) > match(second, heatmap_order),   # TRUE when emmeans lists the later consonant first: swap the pair
         a = if_else(flip, second, first),
         b = if_else(flip, first, second),
         stars = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", TRUE ~ "n.s."),
         sig_level = factor(stars, levels = c("n.s.", "*", "**", "***")))

consonant_heatmaps <- list()
for (group_name in names(heatmap_groups)) {
  outcomes <- heatmap_groups[[group_name]]
  
  # a triangle of tiles for each feature: the earlier consonant of a pair on the x axis, the later one on the y axis
  heat_data <- pair_summary %>%
    filter(outcome %in% outcomes) %>%
    mutate(outcome = factor(outcome, levels = outcomes),
           col = factor(a, levels = heatmap_order),
           row = factor(b, levels = rev(heatmap_order)),
           text_colour = unname(text_colours[as.character(sig_level)]))
  
  p <- ggplot(heat_data, aes(x = col, y = row, fill = sig_level)) +
    geom_tile(colour = "white", linewidth = 1.5) +
    geom_text(aes(label = stars, colour = text_colour), size = 6, family = plot_font) +
    scale_colour_identity() +
    scale_fill_manual(values = significance_colors,
                      labels = c("n.s." = "n.s.", "*" = "p < .05", "**" = "p < .01", "***" = "p < .001"),
                      name = "Tukey-adjusted p", drop = FALSE) +
    facet_wrap(~ outcome, scales = "free", nrow = 1, labeller = labeller(outcome = outcome_labels)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 16, base_family = plot_font) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 22),
          strip.text = element_text(size = 18, face = "bold"),
          axis.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14))
  
  consonant_heatmaps[[group_name]] <- p
  ggsave(filename = file.path(fig_dir, paste0(group_name, ".pdf")), plot = p,
         width = 4 * length(outcomes) + 2, height = 5, device = cairo_pdf)       # 4 inches per plot, plus room for the legend
  print(p)
}

## ======================================================================================================
## PART B: VOWELS
## ======================================================================================================

## ---- 7. Vowel data sets ------------------------------------------------------------------------------------
# vowel labels reduced to a / i / u (length marks ignored); any other vowel (the diphthongs aɪ and aʊ, e ...) is dropped
vowel_key <- c("ɑ" = "a", "a" = "a", "æ" = "a", "i" = "i", "ɪ" = "i", "u" = "u", "ʊ" = "u")
vowel_category <- unname(vowel_key[str_remove_all(vowel_data$interval, "[ːˑ:]")])        # NA = not in the key

cat("\n--- Vowel labels dropped (not in the key) ---\n")
print(vowel_data %>% as_tibble() %>% filter(is.na(vowel_category)) %>% count(interval, sort = TRUE), n = Inf)

vowel_data <- vowel_data %>%
  mutate(interval_detail = interval,                                                      # the original label
         interval = factor(vowel_category, levels = c("a", "i", "u"))) %>%
  filter(!is.na(interval))
cat("\n--- Vowel tokens kept, by original label ---\n")
print(vowel_data %>% as_tibble() %>% count(interval, interval_detail), n = Inf)

# the -ized pairs, the other targets, and the vowel positions (short name used in the data-set names = value in the vowel_position column)
ized_targets <- list(
  s_ized = c("s", "sˤ"),
  t_ized = c("t", "tˤ"),
  d_ized = c("d", "dˤ")
)
other_targets <- c("ħ", "ʕ", "h", "ʔ", "w", "j")        # the first is the reference level for the Segment coefficients in summary()
position_codes <- c(prec = "preceding", fol = "following")
ized_vowels <- "a"                                       # the -ized models use only the vowel a

# -ized: six data sets (s_ized_prec, s_ized_fol, t_ized_prec, ...), each with the two members of one pair
ized_data <- list()
for (set_name in names(ized_targets)) {
  for (pos in names(position_codes)) {
    ized_data[[paste0(set_name, "_", pos)]] <- vowel_data %>%
      filter(interval %in% ized_vowels, Segment %in% ized_targets[[set_name]], vowel_position == position_codes[[pos]])
  }
}

# other targets: two data sets (other_prec, other_fol), all six targets in each
other_data <- list()
for (pos in names(position_codes)) {
  other_data[[paste0("other_", pos)]] <- vowel_data %>%
    filter(Segment %in% other_targets, vowel_position == position_codes[[pos]]) %>%
    mutate(Segment = factor(Segment, levels = other_targets))
}

# what each data set holds (a Segment or vowel with no tokens drops out of its model)
data_check <- map_dfr(c(ized_data, other_data), function(d) {
  tibble(rows = nrow(d),
         tokens_per_segment = paste0(names(table(d$Segment)), " ", as.integer(table(d$Segment)), collapse = " | "),
         tokens_per_vowel = paste0(names(table(d$interval)), " ", as.integer(table(d$interval)), collapse = " | "),
         participants = n_distinct(d$participant), phrases = n_distinct(d$phrase))
}, .id = "set")
print(data_check, n = Inf)

## ---- 8. -ized models (s/sˤ, t/tˤ, d/dˤ) ---------------------------------------------------------------------
# one model per data set x feature; a model that fails is recorded as $error and the loop carries on
ized_rhs <- "Segment + (1|participant) + (1|phrase) + (1|carrierphrase)"

lmer_results <- list()
for (set_name in names(ized_data)) {
  lmer_results[[set_name]] <- list()
  for (i in seq_len(nrow(model_specs))) {
    spec <- model_specs[i, ]
    dat <- ized_data[[set_name]] %>% filter(is.na(.data[[spec$flag]]) | .data[[spec$flag]] != "outlier")
    lmer_results[[set_name]][[spec$name]] <- tryCatch({
      f    <- as.formula(paste(spec$response, "~", ized_rhs))
      mod  <- eval(bquote(lmer(.(f), data = dat)))
      emms <- emmeans(mod, ~ Segment, lmer.df = "satterthwaite", data = dat)
      list(model = mod,
           summary = summary(mod, correlation = FALSE),                       # no correlation block: printing it warns about non-ASCII Segment labels
           emmeans = emms,
           pairs = pairs(emms) %>% as.data.frame() %>% arrange(p.value),      # one comparison: the two Segments
           n_obs = nrow(dat), data = dat)
    }, error = function(e) list(error = conditionMessage(e), n_obs = nrow(dat), data = dat))     # data kept so a failure can be inspected
  }
}
# lmer_results$s_ized_prec$CPP$summary      # model summary of one data set x feature
# lmer_results$s_ized_prec$CPP$pairs        # its comparison of the two Segments

# the comparison of every model in one table, and the same tidied for reading (each model has one comparison, so p-values are not adjusted across models)
all_pairs <- map_dfr(lmer_results, function(s) map_dfr(s, ~ .x$pairs, .id = "model"), .id = "set")

pairs_table <- all_pairs %>%
  mutate(sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ ""),
         set = factor(set, levels = names(ized_data)),
         model = factor(model, levels = model_specs$name)) %>%
  arrange(set, model) %>%
  transmute(set = as.character(set), outcome = as.character(model), contrast,
            estimate = round(estimate, 3), SE = round(SE, 3), df = round(df, 1),
            t.ratio = round(t.ratio, 2), p.value = signif(p.value, 3), sig) %>%
  as_tibble()
print(pairs_table, n = Inf)

# models that failed, if any (empty = every model ran)
failed_models <- map_dfr(lmer_results, function(s) {
  map_dfr(s, function(m) if (!is.null(m$error)) tibble(n_obs = m$n_obs, error = m$error), .id = "model")
}, .id = "set")
failed_models

## ---- 9. -ized figures: box plots, and the summary of all -ized models -------------------------------------
# One figure per feature: a column per pair (s vs sˤ, t vs tˤ, d vs dˤ) and a row per vowel position. Each panel is one model, with a
# bracket marking whether its two segments differed. The plotted values are exactly the data each model was fitted on.

# colours of the six categories
segment_colors <- c("s" = "#8dd3c7", "sˤ" = "#1b9e77",
                    "t" = "#fdb462", "tˤ" = "#d95f02",
                    "d" = "#bebada", "dˤ" = "#7570b3")

# y-axis labels and titles, by feature
y_labels <- c(f0 = "F0 (Hz)", CPP = "CPP (log)", soe = "SoE (log)",
              F1 = "Normalized F1", F2 = "Normalized F2", H1res = "Residual H1*")
feature_titles <- c(f0 = "f0", CPP = "Cepstral Peak Prominence", soe = "Strength of Excitation",
                    F1 = "F1", F2 = "F2", H1res = "Residual H1*")
panel_titles <- sapply(ized_targets, function(x) paste(paste0("/", x, "/"), collapse = " vs. "))     # "/s/ vs. /sˤ/", "/t/ vs. /tˤ/", "/d/ vs. /dˤ/"
position_levels <- unname(position_codes)                           # row order, top to bottom: preceding, following
position_labels <- c(preceding = "Preceding Vowels", following = "Following Vowels")

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
  
  # bracket height and label (one per panel); the p-value is the one in that model's $pairs: *** < .001, ** < .01, * < .05, otherwise "n.s."
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
    scale_x_discrete(labels = function(x) paste0("/", x, "/")) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.12))) +
    labs(x = NULL, y = y_labels[[feature]], title = feature_titles[[feature]]) +
    theme_minimal(base_size = 18, base_family = plot_font) +
    theme(panel.spacing.x = unit(1.2, "cm"),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 18),
          plot.title = element_text(hjust = 0.5, size = 24),
          plot.margin = margin(15, 15, 15, 15))
  
  # cairo_pdf so that installed fonts and the IPA symbols work in the PDF
  ggsave(filename = file.path(fig_dir, paste0(feature, ".pdf")), plot = p, width = 14, height = 9,
         device = cairo_pdf)
  ized_plots[[feature]] <- p
  audit[[feature]] <- ann_df %>% mutate(feature = feature)
  cat("Saved:", file.path(fig_dir, paste0(feature, ".pdf")), "\n")
}

# ---- summary of all the -ized models: a heat map and a forest plot -------------------------------------------
# The features are on different scales, so each difference is standardized (the estimate divided by the SD of that feature in the model's
# data: "SD units"). The sign is emphatic minus plain (sˤ - s ...): positive = higher for the pharyngealized segment.
# Significance is the p in each model's $pairs, as in the box plots; set p_adjust_method to "holm" or "BH" to correct across the models.
p_adjust_method <- "none"
show_values <- TRUE           # print the standardized difference in each heat-map cell (FALSE = stars / n.s. only)

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
         outcome = factor(outcome, levels = model_specs$name, labels = unname(outcome_labels[model_specs$name])))
print(summary_df %>% transmute(outcome, set, vowel_position, d = round(d, 2), CI = paste0("[", round(d_low, 2), ", ", round(d_high, 2), "]"),
                               p = signif(p_plot, 3), stars) %>% arrange(vowel_position, set, outcome) %>% as_tibble(), n = Inf)

# heat map: one cell per model; colour = standardized difference, faded cells = n.s.
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

# forest plot: standardized difference with 95% CI; filled points = significant
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

# check: the p-value and label drawn in every box-plot panel against the p in all_pairs
panel_labels <- bind_rows(audit) %>%
  left_join(all_pairs %>% select(key = set, feature = model, p_in_pairs = p.value), by = c("key", "feature")) %>%
  mutate(label_from_pairs = case_when(is.na(p_in_pairs) ~ "NA", p_in_pairs < .001 ~ "***", p_in_pairs < .01 ~ "**",
                                      p_in_pairs < .05 ~ "*", TRUE ~ "n.s."),
         agrees = label == label_from_pairs & isTRUE(all.equal(p, p_in_pairs))) %>%
  transmute(feature, panel = key, p_drawn = signif(p, 3), p_in_pairs = signif(p_in_pairs, 3), label_drawn = label, agrees) %>%
  as_tibble()
print(panel_labels, n = Inf)
cat("Panels whose label disagrees with the p in $pairs:", sum(!panel_labels$agrees), "of", nrow(panel_labels), "\n")

## ---- 10. Other-target models (ħ ʕ h ʔ w j) -----------------------------------------------------------------
# All six targets are in one model, with the vowel (a / i / u) as a second factor; one model per vowel position x feature.
other_rhs <- "Segment + interval + (1|participant) + (1|phrase) + (1|carrierphrase)"

other_results <- list()
for (set_name in names(other_data)) {
  other_results[[set_name]] <- list()
  for (i in seq_len(nrow(model_specs))) {
    spec <- model_specs[i, ]
    dat <- other_data[[set_name]] %>% filter(is.na(.data[[spec$flag]]) | .data[[spec$flag]] != "outlier")
    other_results[[set_name]][[spec$name]] <- tryCatch({
      f    <- as.formula(paste(spec$response, "~", other_rhs))
      mod  <- eval(bquote(lmer(.(f), data = dat)))
      emms <- emmeans(mod, ~ Segment, lmer.df = "satterthwaite", data = dat)     # averaged over the vowels
      list(model = mod,
           summary = summary(mod, correlation = FALSE),
           anova = anova(mod),                                                    # Type III F-tests: the overall tests of Segment and interval
           emmeans = emms,
           pairs = pairs(emms) %>% as.data.frame() %>% arrange(p.value),          # all 15 pairs of segments (Tukey-adjusted within the model)
           n_obs = nrow(dat), data = dat)
    }, error = function(e) list(error = conditionMessage(e), n_obs = nrow(dat), data = dat))
  }
}
# other_results$other_prec$CPP$anova        # the overall tests of Segment and interval for one model
# other_results$other_prec$CPP$pairs        # its 15 pairwise comparisons

# the overall tests of every model: one row per data set x feature x term (Segment, interval)
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

# the pairwise comparisons of every model, and the same tidied for reading (Tukey-adjusted within each model, not across models)
other_all_pairs <- map_dfr(other_results, function(s) map_dfr(s, ~ .x$pairs, .id = "model"), .id = "set")

other_pairs_table <- other_all_pairs %>%
  mutate(sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", p.value < .1 ~ ".", TRUE ~ ""),
         set = factor(set, levels = names(other_data)),
         model = factor(model, levels = model_specs$name)) %>%
  arrange(set, model) %>%
  transmute(set = as.character(set), outcome = as.character(model), contrast,
            estimate = round(estimate, 3), SE = round(SE, 3), df = round(df, 1),
            t.ratio = round(t.ratio, 2), p.value = signif(p.value, 3), sig) %>%
  as_tibble()
cat("\nSignificant pairwise comparisons (p < .05); View(other_pairs_table) shows all 180:\n")      # 15 comparisons x 6 features x 2 data sets
print(other_pairs_table %>% filter(sig %in% c("*", "**", "***")), n = Inf)

# models that failed, if any (empty = every model ran)
other_failed_models <- map_dfr(other_results, function(s) {
  map_dfr(s, function(m) if (!is.null(m$error)) tibble(n_obs = m$n_obs, error = m$error), .id = "model")
}, .id = "set")
other_failed_models

## ---- 11. Other-target figures: the vowel measurements by consonant ----------------------------------------------
# One figure per feature: a row for the preceding vowels and a row for the following vowels, each with a box-and-whisker plot (and the
# individual values) for each of the six consonants. The values are the rows each model in section 10 was fitted on (outliers removed),
# kept to the vowels in other_plot_vowels.
other_plot_vowels <- "a"                                   # the vowels shown; c("a", "i", "u") includes all three
other_plot_order  <- c("h", "j", "w", "ʕ", "ħ", "ʔ")       # the consonants on the x axis, left to right
other_colors <- c("h" = "#1b9e77", "j" = "#d95f02", "w" = "#e6ab02",
                  "ʕ" = "#7570b3", "ħ" = "#e7298a", "ʔ" = "#66a61e")      # colours of the six consonants

# tokens in each box, before outliers are removed (a row per vowel position)
print(bind_rows(other_data, .id = "set") %>%
        filter(interval %in% other_plot_vowels) %>%
        count(set, Segment) %>%
        pivot_wider(names_from = Segment, values_from = n) %>%
        select(set, all_of(other_plot_order)) %>%
        arrange(match(set, c("other_prec", "other_fol"))))

vowel_note <- paste0(if (length(other_plot_vowels) == 1) "Vowel: " else "Vowels: ", paste0("/", other_plot_vowels, "/", collapse = ", "))

other_plots <- list()
for (feature in model_specs$name) {
  response <- model_specs$response[model_specs$name == feature]
  
  # the values of the two rows, from the data each model used
  pts <- list()
  for (pos in names(position_codes)) {
    pts[[pos]] <- other_results[[paste0("other_", pos)]][[feature]]$data %>%
      filter(interval %in% other_plot_vowels) %>%
      transmute(vowel_position = factor(position_codes[[pos]], levels = position_levels),
                Segment = factor(Segment, levels = other_plot_order),
                value = .data[[response]])
  }
  plot_df <- bind_rows(pts)
  
  p <- ggplot(plot_df, aes(x = Segment, y = value, fill = Segment)) +
    geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.85) +
    geom_jitter(width = 0.15, size = 1, alpha = 0.3) +
    facet_grid(vowel_position ~ ., labeller = labeller(vowel_position = position_labels)) +
    scale_fill_manual(values = other_colors, guide = "none") +
    scale_x_discrete(labels = function(x) paste0("/", x, "/")) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.05))) +
    labs(x = NULL, y = y_labels[[feature]], title = feature_titles[[feature]], subtitle = vowel_note) +
    theme_minimal(base_size = 18, base_family = plot_font) +
    theme(panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 18),
          plot.title = element_text(hjust = 0.5, size = 24),
          plot.subtitle = element_text(hjust = 0.5, size = 18),
          plot.margin = margin(15, 15, 15, 15))
  
  ggsave(filename = file.path(fig_dir, paste0("other_", feature, ".pdf")), plot = p, width = 10, height = 9, device = cairo_pdf)
  other_plots[[feature]] <- p
}