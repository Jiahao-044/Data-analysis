library(tidyverse)

setwd("D:/Data analysis")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

gas_data <- read_csv("data/cleaned/cleaned_gas_data.csv", show_col_types = FALSE)

monthly_wide <- gas_data %>%
  mutate(Month = as.Date(sprintf("%s-%02d-01", Year, Month_Num))) %>%
  group_by(Gas, Month) %>%
  summarise(Monthly_Mean = mean(Concentration), .groups = "drop") %>%
  pivot_wider(names_from = Gas, values_from = Monthly_Mean) %>%
  drop_na(CH4, N2O)

correlation_table <- tibble(
  method = c("Pearson", "Spearman"),
  correlation = c(
    cor(monthly_wide$CH4, monthly_wide$N2O, method = "pearson"),
    cor(monthly_wide$CH4, monthly_wide$N2O, method = "spearman")
  )
)

write_csv(correlation_table, "outputs/tables/Table_5_correlation_results.csv")

scatter_plot <- ggplot(monthly_wide, aes(x = CH4, y = N2O)) +
  geom_point(color = "blue", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(title = "CH4 and N2O Association", x = "Monthly mean CH4 (ppb)", y = "Monthly mean N2O (ppb)") +
  theme_minimal(base_size = 14)

ggsave("outputs/figures/Figure_11_CH4_N2O_scatterplot.png", scatter_plot, width = 7, height = 5, dpi = 300)
print(scatter_plot)

cat("yes\n")


