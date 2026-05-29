library(tidyverse)

setwd("D:/Data analysis")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

gas_data <- read_csv("data/cleaned/cleaned_gas_data.csv", show_col_types = FALSE)

summary_table <- gas_data %>%
  group_by(Gas) %>%
  summarise(
    n = n(),
    mean = mean(Concentration),
    median = median(Concentration),
    sd = sd(Concentration),
    IQR = IQR(Concentration),
    min = min(Concentration),
    max = max(Concentration),
    .groups = "drop"
  )

write_csv(summary_table, "outputs/tables/Table_1_summary_statistics.csv")

cat("\nSummary statistics for report:\n")
print(summary_table)

ch4_histogram <- gas_data %>%
  filter(Gas == "CH4") %>%
  ggplot(aes(x = Concentration)) +
  geom_histogram(bins = 30, fill = "#2878b5", color = "white") +
  labs(title = "CH4 Distribution", x = "CH4 concentration (ppb)", y = "Count") +
  theme_minimal(base_size = 14)
ggsave("outputs/figures/Figure_2_CH4_histogram.png", ch4_histogram, width = 7, height = 5, dpi = 300)
print(ch4_histogram)

n2o_histogram <- gas_data %>%
  filter(Gas == "N2O") %>%
  ggplot(aes(x = Concentration)) +
  geom_histogram(bins = 30, fill = "#d65f5f", color = "white") +
  labs(title = "N2O Distribution", x = "N2O concentration (ppb)", y = "Count") +
  theme_minimal(base_size = 14)
ggsave("outputs/figures/Figure_3_N2O_histogram.png", n2o_histogram, width = 7, height = 5, dpi = 300)
print(n2o_histogram)

ch4_boxplot <- gas_data %>%
  filter(Gas == "CH4") %>%
  ggplot(aes(x = Gas, y = Concentration)) +
  geom_boxplot(fill = "#2878b5", width = 0.45) +
  labs(title = "CH4 Boxplot", x = NULL, y = "CH4 concentration (ppb)") +
  theme_minimal(base_size = 14)
ggsave("outputs/figures/Figure_4_CH4_boxplot.png", ch4_boxplot, width = 6, height = 5, dpi = 300)
print(ch4_boxplot)

n2o_boxplot <- gas_data %>%
  filter(Gas == "N2O") %>%
  ggplot(aes(x = Gas, y = Concentration)) +
  geom_boxplot(fill = "#d65f5f", width = 0.45) +
  labs(title = "N2O Boxplot", x = NULL, y = "N2O concentration (ppb)") +
  theme_minimal(base_size = 14)
ggsave("outputs/figures/Figure_5_N2O_boxplot.png", n2o_boxplot, width = 6, height = 5, dpi = 300)
print(n2o_boxplot)

cat("yes\n")



