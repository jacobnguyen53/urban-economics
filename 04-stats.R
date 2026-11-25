# for dataviz in latex
install.packages("gt")
library(tidyverse); library(gt)

panel <- read_csv("clean-data/panel.csv", show_col_types = FALSE)

# Table 1: Summary statistics for adopting and non-adopting cities
table1 <- panel |>                                                          # 1
  mutate(group = if_else(treated, "Adopted rent control", "Never adopted")) |>
  group_by(group) |>
  summarise(Cities               = n_distinct(city),
            Obs.                 = n(),
            `Mean rent ($)`      = round(mean(rent)),
            `Min rent ($)`       = min(rent),
            `Max rent ($)`       = max(rent),
            `Mean renter share`  = round(mean(renter_share), 2),
            `Mean stay share`    = round(mean(stay_share, na.rm = TRUE), 2))

# create the images for export
t1 <- table1 |>
  gt() |>
  tab_header(title = "Table 1: Descriptive statistics, 2010–2025") |>
  fmt_number(
    columns = c(Cities, Obs., `Mean rent ($)`, `Min rent ($)`, `Max rent ($)`),
    decimals = 0
  ) |>
  fmt_number(
    columns = c(`Mean renter share`, `Mean stay share`),
    decimals = 2
  ) |>
  tab_source_note("Source: US Census Bureau, ACS 1-year estimates (no 2020)")

gtsave(t1, "figures/table1_descriptives.png")
gtsave(t1, "figures/table1_descriptives.tex")

# Graph 1: Rent trends over time
fig1 <- panel |>
  # give me that sweet sweet index
  group_by(year, group = if_else(treated, "Rent controlled", "Non rent-controlled")) |>
  summarise(mean_log = mean(log_rent), .groups = "drop") |>
  # index on 2010 rent
  group_by(group) |>
  mutate(index = 100 * exp(mean_log - mean_log[year == 2010])) |> 
  ungroup() |>
  # assemble graph
  ggplot(aes(year, index, colour = group)) +
  geom_vline(xintercept = 2020, colour = "red", linetype = "dashed") +
  geom_line() + geom_point() +
  labs(x = NULL, y = "Median contract rent index (2010 baseline)", colour = NULL,
       title = "Figure 1. Rent growth in California cities",
       caption = "Source: US Census Bureau, ACS 1-year estimates (no 2020), 
       Note: Geometric mean rent growth") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("figures/fig1_trends.png", fig1, width = 7, height = 4.5, dpi = 300) 

