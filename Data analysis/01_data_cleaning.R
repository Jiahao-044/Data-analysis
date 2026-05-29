library(tidyverse)
library(lubridate)

setwd("D:/Data analysis")

dir.create("data/cleaned", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

read_gas_file <- function(file_path, gas_name) {
  if (!file.exists(file_path)) {
    file_path <- file.path("D:/Data analysis", file_path)
  }
  lines <- readLines(file_path, warn = FALSE, encoding = "Latin1")
  header <- lines[str_detect(lines, "^YYYY,MM,DD,DATE,")][1]
  data_lines <- lines[str_detect(lines, "^\\d{4},")]
  raw <- read_csv(I(paste(c(header, data_lines), collapse = "\n")),
                  col_types = cols(.default = col_character()),
                  show_col_types = FALSE)
  gas_column <- paste0(gas_name, "(ppb)")
  raw %>%
    transmute(
      Date = make_date(as.integer(YYYY), as.integer(MM), as.integer(DD)),
      Year = year(Date),
      Month_Num = month(Date),
      Gas = gas_name,
      Concentration = parse_number(.data[[gas_column]])
    ) %>%
    filter(!is.na(Date), !is.na(Concentration))
}

ch4 <- read_gas_file("data/raw/CapeGrim_CH4_data_download.csv", "CH4")
n2o <- read_gas_file("data/raw/CapeGrim_N2O_data_download.csv", "N2O")

gas_data <- bind_rows(ch4, n2o) %>% arrange(Gas, Date)
write_csv(gas_data, "data/cleaned/cleaned_gas_data.csv")

map_plot <- ggplot() +
  annotate("rect", xmin = 143.8, xmax = 148.7, ymin = -43.8, ymax = -39.4,
           fill = "grey92", color = "grey55") +
  geom_point(aes(x = 144.689, y = -40.683), color = "red", size = 4) +
  annotate("text", x = 145.0, y = -40.45, label = "Cape Grim", hjust = 0, size = 5) +
  coord_fixed(xlim = c(143.5, 149.0), ylim = c(-44.0, -39.2)) +
  labs(title = "Study Area Map", x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 14)

ggsave("outputs/figures/Figure_1_study_area_map.png", map_plot, width = 7, height = 5, dpi = 300)
print(map_plot)

cat("yes\n")



