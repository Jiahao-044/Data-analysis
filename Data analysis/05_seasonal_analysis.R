library(tidyverse)

setwd("D:/Data analysis")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

gas_data <- read_csv("data/cleaned/cleaned_gas_data.csv", show_col_types = FALSE)

season_table <- gas_data %>%
  mutate(
    Season = case_when(
      Month_Num %in% c(12, 1, 2) ~ "Summer",
      Month_Num %in% c(3, 4, 5) ~ "Autumn",
      Month_Num %in% c(6, 7, 8) ~ "Winter",
      TRUE ~ "Spring"
    ),
    Season = factor(Season, levels = c("Summer", "Autumn", "Winter", "Spring"))
  ) %>%
  group_by(Gas, Season) %>%
  summarise(Mean_Concentration = mean(Concentration), .groups = "drop")

write_csv(season_table, "outputs/tables/Table_4_seasonal_means.csv")

cat("\nSeasonal means for report:\n")
print(season_table)

ch4_season <- season_table %>%
  filter(Gas == "CH4") %>%
  ggplot(aes(x = Season, y = Mean_Concentration)) +
  geom_col(fill = "blue", width = 0.65) +
  labs(title = "CH4 Seasonal Means", x = "Season", y = "Mean CH4 (ppb)") +
  theme_minimal(base_size = 14)
ggsave("outputs/figures/Figure_8_CH4_seasonal_means.png", ch4_season, width = 7, height = 5, dpi = 300)
print(ch4_season)

n2o_season <- season_table %>%
  filter(Gas == "N2O") %>%
  ggplot(aes(x = Season, y = Mean_Concentration)) +
  geom_col(fill = "red", width = 0.65) +
  labs(title = "N2O Seasonal Means", x = "Season", y = "Mean N2O (ppb)") +
  theme_minimal(base_size = 14)
ggsave("outputs/figures/Figure_9_N2O_seasonal_means.png", n2o_season, width = 7, height = 5, dpi = 300)
print(n2o_season)

cat("yes\n")



