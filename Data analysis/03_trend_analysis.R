library(tidyverse)

setwd("D:/Data analysis")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

gas_data <- read_csv("data/cleaned/cleaned_gas_data.csv", show_col_types = FALSE)

annual_table <- gas_data %>%
  group_by(Gas, Year) %>%
  summarise(Annual_Mean = mean(Concentration), .groups = "drop")

trend_table <- annual_table %>%
  group_by(Gas) %>%
  group_modify(~ {
    model <- lm(Annual_Mean ~ Year, data = .x)
    tibble(
      slope = coef(model)[["Year"]],
      p_value = summary(model)$coefficients["Year", "Pr(>|t|)"],
      r_squared = summary(model)$r.squared
    )
  }) %>%
  ungroup()

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Ensure output folders exist before writing files.
write_csv(annual_table, "outputs/tables/Table_2_annual_means.csv")
write_csv(trend_table, "outputs/tables/Table_3_trend_results.csv")

ch4_trend <- annual_table %>%
  filter(Gas == "CH4") %>%
  ggplot(aes(x = Year, y = Annual_Mean)) +
  geom_line(color = "blue", linewidth = 1.1) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  labs(title = "CH4 Annual Trend", x = "Year", y = "Annual mean CH4 (ppb)") +
  theme_minimal(base_size = 14)
ggsave("outputs/figures/Figure_6_CH4_annual_trend.png", ch4_trend, width = 7, height = 5, dpi = 300)
print(ch4_trend)

n2o_trend <- annual_table %>%
  filter(Gas == "N2O") %>%
  ggplot(aes(x = Year, y = Annual_Mean)) +
  geom_line(color = "red", linewidth = 1.1) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  labs(title = "N2O Annual Trend", x = "Year", y = "Annual mean N2O (ppb)") +
  theme_minimal(base_size = 14)
ggsave("outputs/figures/Figure_7_N2O_annual_trend.png", n2o_trend, width = 7, height = 5, dpi = 300)
print(n2o_trend)

cat("yes\n")
cat("r_squared")



