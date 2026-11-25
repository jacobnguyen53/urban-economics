# Run Callaway-Sant'Anna (2021) DiD reg
run_cs <- function(data, y, control = "notyettreated", seed = 2000) {
  set.seed(seed)  # random seed for resampling -> errors
  cs <- att_gt(yname = y, # outcome
               tname = "time", # year
               idname = "city_id",
               gname = "group_id", # filter by when groups implemented rent controls
               data = data,  control_group = control,
               # "notyettreated" -> cities prior to implements are part of controls
               base_period = "universal", clustervars = "city_id",
               # treated cities are compared to the immediate preceding year
               allow_unbalanced_panel = TRUE
               )
}