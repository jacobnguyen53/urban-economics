library(tidyverse); library(did)
source("R/functions.R")

panel <- read_csv("clean-data/panel_did.csv", show_col_types = FALSE)

# clean panel for mobility analysis due to !stay_share data in Baldwin Park/Inglewood
panel_mob <- panel |>
  filter(!is.na(stay_share)) |>
  group_by(city) |>
  filter(n() == n_distinct(panel$year)) |>  # keeps cities with panel data for stay_share all years
  ungroup()

distinct(panel_mob, city, treat_year) |> filter(treat_year > 0) # keep only treated cities

# Mobility analysis on our balanced panel
mob <- run_cs(panel_mob, "stay_share") # run CS with people staying as y
mob_overall <- aggte(mob, type = "simple")
summary(mob_overall)


# Event study on mobility
es_mob <- aggte(mob, type = "dynamic", min_e = -6, max_e = 4)
fig3 <- ggdid(es_mob) +
  labs(title = "Rent control effects on stability",
       x = "Years relative to adoption",
       y = "Effect on share of renters staying since last year",
       caption = "Source: ACS 1-year (table B07013); Callaway-Sant'Anna estimator")
ggsave("figures/fig3_event_study_mobility.png", fig3, width = 7, height = 4.5, dpi = 300)


# checks for robustness (basically are these other possible explanatory)
checks <- list(
  "Contract rent (main)"         = list(data = panel, y = "log_rent",  ctrl = "notyettreated"),
  "Gross rent"                  = list(data = panel, y = "log_gross", ctrl = "notyettreated"),
  "Never-adopted" = list(data = panel, y = "log_rent",  ctrl = "nevertreated"),
  "Bay Area only (regional)"        = list(data = filter(panel, in_bay), 
                                           y = "log_rent", ctrl = "notyettreated")
)

# what do the estimates for mobility look like given these cases? 
table3 <- imap_dfr(checks, function(x, name) { # iterate through each check
  a <- aggte(run_cs(x$data, x$y, x$ctrl), type = "simple") # run
  tibble(check = name, estimate = a$overall.att, se = a$overall.se)
}) |>
  mutate(ci_low  = estimate - 1.96 * se, # add conf intervals 
         ci_high = estimate + 1.96 * se,
         pct     = 100 * (exp(estimate) - 1)) |> # translate from log
  mutate(across(where(is.numeric), ~ round(.x, 3)))
table3
write_csv(table3, "figures/table3_robustness.csv")
