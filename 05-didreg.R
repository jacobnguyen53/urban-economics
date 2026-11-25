library(tidyverse); library(did); library(fixest)
source("R/functions.R") # import Callaway-Sant'Anna DiD

panel <- read_csv("clean-data/panel.csv", show_col_types = FALSE)
years   <- sort(unique(panel$year))

# Add indices for diff-in-diff regressions and FEs
panel <- panel |>
  mutate(
    city_id = as.integer(factor(city)), # assign integers to each city
    time       = match(year, years),    # renumber years based on position
    adj_yr  = if_else(treat_year == 2020, 2021, treat_year),  
    # omit 2020, and move treated cities to 2021 as a response
    group_id = if_else(adj_yr == 0, 0L, match(adj_yr, years)),
    # cities are grouped by when they adopt rent controls (0 if not)
    dummy   = as.integer(group_id > 0 & time >= group_id)) 
    # dummy for "has the city implemented rent controls yet, if ever?"

write_csv(panel, "clean-data/panel_did.csv")

# do two way fixed effects
twfe <- feols(log_rent ~ dummy | city_id + year, data = panel, cluster = ~city_id)
summary(twfe) # note this is not robust
# b/c twfe is susceptible to staggered treatments, we need a more robust way

# use the method from Callaway-Sant'Anna (2021) / group time avg treatment effects
main <- run_cs(panel, "log_rent")
cs <- main
overall <- aggte(cs, type = "simple") # weighted avg of outcome by group size

# Graph 2: Event study (around when rent-control was adopted)
es <- aggte(cs, type = "dynamic", min_e = -6, max_e = 4)
fig2 <- ggdid(es) +
  labs(title = "Effect of rent control on log median contract rent",
       x = "Years relative to adoption", y = "Effect (log points)",
       caption = "Sources: ACS data (Fig 1); Callaway-Sant'Anna (2021)
       Control group: not-yet-treated cities")
ggsave("figures/fig2_event_study_rent.png", fig2, width = 7, height = 4.5, dpi = 300)
# estimates in att per group year
summary(aggte(cs, type = "group")) 

# Table 2: Compare TWFE and Callaway-Sant'Anna
table2 <- tibble(
  Model    = c("TWFE", "Callaway-Sant'Anna"),
  Estimate = c(unname(coef(twfe)["dummy"]), overall$overall.att),
  SE       = c(unname(se(twfe)["dummy"]),   overall$overall.se)) |>
  mutate(CI_low   = Estimate - 1.96 * SE,
         CI_high  = Estimate + 1.96 * SE,
         Percent  = 100 * (exp(Estimate) - 1)) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
table2
t2 <- table2 |>
  gt() |>
  tab_header(title = "Table 2: Model comparison") |>
  fmt_number(columns = where(is.numeric), decimals = 3) |>
  tab_source_note("Sources: ACS 1-year, Callaway-Sant'Anna (2021), 95% conf. intervals")

gtsave(t2, "figures/table2_descriptives.png")
gtsave(t2, "figures/table2_descriptives.tex")

