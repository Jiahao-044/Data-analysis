# Redesigned seasonal analysis:
# seasonal bar + line charts, plus monthly line charts as supporting detail.

library(tidyverse)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
this_file <- if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
}

if (!is.na(this_file)) {
  setwd(normalizePath(file.path(dirname(this_file), "..")))
}

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

if (!file.exists("data/cleaned/cleaned_gas_data.csv")) {
  source("code/01_data_cleaning.R")
}

gas_data <- read_csv("data/cleaned/cleaned_gas_data.csv", show_col_types = FALSE)

seasonal_data <- gas_data %>%
  mutate(
    Season = case_when(
      Month_Num %in% c(12, 1, 2) ~ "Summer",
      Month_Num %in% c(3, 4, 5) ~ "Autumn",
      Month_Num %in% c(6, 7, 8) ~ "Winter",
      TRUE ~ "Spring"
    ),
    Season = factor(Season, levels = c("Summer", "Autumn", "Winter", "Spring"))
  )

seasonal_summary <- seasonal_data %>%
  group_by(Gas, Season) %>%
  summarise(
    n = n(),
    mean_concentration = mean(Concentration, na.rm = TRUE),
    sd_concentration = sd(Concentration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Season_Label = recode(
      as.character(Season),
      "Summer" = "Summer (Dec-Feb)",
      "Autumn" = "Autumn (Mar-May)",
      "Winter" = "Winter (Jun-Aug)",
      "Spring" = "Spring (Sep-Nov)"
    ),
    Season_Label = factor(
      Season_Label,
      levels = c("Summer (Dec-Feb)", "Autumn (Mar-May)", "Winter (Jun-Aug)", "Spring (Sep-Nov)")
    )
  )

monthly_summary <- gas_data %>%
  group_by(Gas, Month_Num) %>%
  summarise(
    mean_concentration = mean(Concentration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Month = factor(month.abb[Month_Num], levels = month.abb))

write_csv(seasonal_summary, "outputs/tables/Table_6_redesigned_seasonal_summary.csv")
write_csv(monthly_summary, "outputs/tables/Table_7_monthly_supporting_summary.csv")

cat("\nRedesigned seasonal summary:\n")
print(seasonal_summary)

cat("\nMonthly supporting summary:\n")
print(monthly_summary)

make_seasonal_bar_line <- function(gas_name, color, output_file) {
  plot_data <- seasonal_summary %>% filter(Gas == gas_name)
  y_padding <- sd(plot_data$mean_concentration, na.rm = TRUE) * 1.3
  if (is.na(y_padding) || y_padding == 0) y_padding <- mean(plot_data$mean_concentration) * 0.01

  p <- ggplot(plot_data, aes(x = Season_Label, y = mean_concentration, group = 1)) +
    geom_col(fill = color, alpha = 0.72, width = 0.62) +
    geom_line(color = "black", linewidth = 1.1) +
    geom_point(color = "black", fill = "white", shape = 21, size = 3.2, stroke = 1) +
    geom_text(aes(label = round(mean_concentration, 2)), vjust = -0.8, size = 4) +
    coord_cartesian(
      ylim = c(
        min(plot_data$mean_concentration) - y_padding,
        max(plot_data$mean_concentration) + y_padding
      )
    ) +
    labs(
      title = paste(gas_name, "Seasonal Mean Concentration"),
      subtitle = "Bars show seasonal means; the line highlights the seasonal pattern",
      x = "Southern Hemisphere season (months)",
      y = paste(gas_name, "mean concentration (ppb)")
    ) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank())

  ggsave(output_file, p, width = 7, height = 5, dpi = 300)
  print(p)
}

make_monthly_line <- function(gas_name, color, output_file) {
  plot_data <- monthly_summary %>% filter(Gas == gas_name)

  p <- ggplot(plot_data, aes(x = Month, y = mean_concentration, group = 1)) +
    geom_line(color = color, linewidth = 1.2) +
    geom_point(color = color, size = 2.8) +
    labs(
      title = paste(gas_name, "Monthly Mean Concentration"),
      subtitle = "Monthly means show more detailed within-year movement",
      x = "Month",
      y = paste(gas_name, "mean concentration (ppb)")
    ) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank())

  ggsave(output_file, p, width = 8, height = 5, dpi = 300)
  print(p)
}

make_seasonal_bar_line("CH4", "blue", "outputs/figures/Figure_12_CH4_seasonal_bar_line.png")
make_seasonal_bar_line("N2O", "red", "outputs/figures/Figure_13_N2O_seasonal_bar_line.png")
make_monthly_line("CH4", "blue", "outputs/figures/Figure_14_CH4_monthly_line.png")
make_monthly_line("N2O", "red", "outputs/figures/Figure_15_N2O_monthly_line.png")

cat("\nWhich figure is better?\n")
cat("For the main report, the seasonal bar-line charts are better because they match the seasonal Methods section.\n")
cat("For supporting analysis, the monthly line charts are better because they show the timing of CH4 changes more clearly.\n")
cat("Best choice: use seasonal bar-line charts in the report and monthly line charts as supporting material.\n")

cat("yes\n")

