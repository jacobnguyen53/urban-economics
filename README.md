**What is this?**
This is a project for my class in Regional and Urban Economics that aims to assess whether or not rent control was effective in California cities over the last 10 years in increasing renter stability and lowering median rent.

The files can be broken down as stages of the research process:
**functions**: specifically only the method described in Callaway-Sant'Anna for DiD regressions on staggered treatments
**01-setup**: For setting up most necessary packages and accessing census data
**02-data**: Working with the raw datasets
**03-clean**: Cleaning up the data and building the panel for stats work
**04-stats**: Getting summary stats and tracking median rent
**05-didregs**: Methods to determine whether adoption actually had an effect on rent
**06-mob**: Methods to determine whether adoptions had an effect on mobility (and everything else).
...
The folders for holding certain outputs:
**raw-data**: The American Community Surveys from the US Census Bureau, Urban Displacement data from UC Berkeley, and a list of dates and variables for cities that implemented rent control.
**clean-data**: The data (while being cleaned) and the panel data for 04-06
**figures**: Tables, graphs (in this current commit figures was being used to hold intermediate data while cleaning)
...
**Notes for setup / reproduction**
Get a census API key from https://api.census.gov/data/key_signup.html.
Random seed (2000) for resampling and standard errors
