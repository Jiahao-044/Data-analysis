library(tidyverse)

setwd("D:/Data analysis")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

gas_data <- read_csv("data/cleaned/cleaned_gas_data.csv", show_col_types = FALSE)

standardised_data <- gas_data %>%
  mutate(Month = as.Date(sprintf("%s-%02d-01", Year, Month_Num))) %>%
  group_by(Gas, Month) %>%
  summarise(Monthly_Mean = mean(Concentration), .groups = "drop") %>%
  group_by(Gas) %>%
  mutate(Z_score = (Monthly_Mean - mean(Monthly_Mean)) / sd(Monthly_Mean)) %>%
  ungroup()

standardised_plot <- ggplot(standardised_data, aes(x = Month, y = Z_score, color = Gas)) +
  geom_line(linewidth = 1) +
  labs(title = "Standardised Monthly Trends", x = "Month", y = "Z-score") +
  scale_color_manual(values = c(CH4 = "blue", N2O = "red")) +
  theme_minimal(base_size = 14)

ggsave("outputs/figures/Figure_10_standardised_trend.png", standardised_plot, width = 8, height = 5, dpi = 300)
print(standardised_plot)

cat("yes\n")


