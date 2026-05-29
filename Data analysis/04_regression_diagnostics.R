library(tidyverse)

setwd("D:/Data analysis")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

gas_data <- read_csv("data/cleaned/cleaned_gas_data.csv", show_col_types = FALSE)

annual_table <- gas_data %>%
  group_by(Gas, Year) %>%
  summarise(Annual_Mean = mean(Concentration), .groups = "drop")

make_diagnostics <- function(gas_name, color) {
  data <- annual_table %>% filter(Gas == gas_name)
  model <- lm(Annual_Mean ~ Year, data = data)
  diag_data <- data %>%
    mutate(
      fitted = fitted(model),
      residual = resid(model),
      standard_residual = rstandard(model)
    )

  qq_plot <- ggplot(diag_data, aes(sample = standard_residual)) +
    stat_qq(color = color, size = 2) +
    stat_qq_line(color = "black") +
    labs(title = paste(gas_name, "Q-Q Plot"), x = "Theoretical quantiles", y = "Standard residuals") +
    theme_minimal(base_size = 14)
  ggsave(paste0("outputs/figures/Figure_", gas_name, "_QQ_plot.png"), qq_plot, width = 7, height = 5, dpi = 300)
  print(qq_plot)

  fitted_plot <- ggplot(diag_data, aes(x = fitted, y = residual)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(color = color, size = 2) +
    labs(title = paste(gas_name, "Residuals vs Fitted"), x = "Fitted values", y = "Residuals") +
    theme_minimal(base_size = 14)
  ggsave(paste0("outputs/figures/Figure_", gas_name, "_residuals_vs_fitted.png"), fitted_plot, width = 7, height = 5, dpi = 300)
  print(fitted_plot)

  time_plot <- ggplot(diag_data, aes(x = Year, y = residual)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(color = color, linewidth = 1) +
    geom_point(color = color, size = 2) +
    labs(title = paste(gas_name, "Residuals Over Time"), x = "Year", y = "Residuals") +
    theme_minimal(base_size = 14)
  ggsave(paste0("outputs/figures/Figure_", gas_name, "_residuals_over_time.png"), time_plot, width = 7, height = 5, dpi = 300)
  print(time_plot)
}

make_diagnostics("CH4", "blue")
make_diagnostics("N2O", "red")

cat("yes\n")


