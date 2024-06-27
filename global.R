source("initialise.R")
source("scripts/functions.R")
source("data.R")
source("scripts/get_data.R")
source("scripts/load_plot_functions.R")





###Change year range, this is the display text in the slider 
year_range<- paste0("1990","\U2012", "2019")
years <-1991:2019
min_year=1991
max_year=2019

## change year range, this is the display text in the slider- this 
year_range<- paste("Select year between 1991 and 2018")
###Reading in CSV file 
industry_names<-unique(all_industries$Industry)