# setup packages
# tidyverse: data work, graphing; tidycensus: download census
# did: difference-in-difference regressions; fixest: fixed-effect regressions
# stargazer: tables

install.packages(c("tidyverse", "tidycensus", "did", "fixest",
                   "stargazer"))

# uncomment, then paste API key here 
tidycensus::census_api_key("yourkey", install = TRUE)