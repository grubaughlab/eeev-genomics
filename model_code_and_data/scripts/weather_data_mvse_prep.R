#!/usr/bin/env Rscript

#preps data output from visualcrossing.com to be utilized in the mvse package
rm(list = ls())
#INSTALL PACKAGES
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all")
pacman::p_load(tidyverse, optparse, lubridate, ggpubr)

#CREATE OPTION TO DETERMINE GEOGRAPHICAL RESOLUTION CALCULATING INDEX P
.option_list = list(
  make_option(c("-g", "--geo"), type="character",  
              default= "centroid",
              help = "determine whether to use either the region or state as the grouping variable. Options are region, state, state_year", 
              metavar="character"
  ),
  make_option(c("-m", "--monthlower"), type="numeric",  
              default= 5,
              help = "determine lowest month in year to begin index P", 
              metavar="numeric"
  ),
  make_option(c("-n", "--monthupper"), type="numeric",  
              default= 10,
              help = "determine highest month in year to begin index P", 
              metavar="numeric"
  ),
  make_option(c("-l", "--datelower"), type = "character",
              default = "2003-01-01",
              help = "pick the date lower end for which you want to plot the index P", 
              metavar="character"
  ),
  make_option(c("-u", "--dateupper"), type = "character",
              default = "2020-12-31",
              help = "pick the date upper end for which you want to plot the index P", 
              metavar="character"
  )
)

.opt_parser = OptionParser(option_list=.option_list)
opt = parse_args(.opt_parser)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#visual crossing data import and clean
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
climate_df = read.csv("data_input/weather_data.csv")

climate_clean = function(df, grp1, grp2, dir) {
  df2 = df %>%
    select(name, datetime, temp, humidity, precip) %>%
    drop_na() %>% #remove NA other causes error in MVSE
    mutate(state = str_split(name, '_', simplify = TRUE)[,1]) %>%  #extract the state which is everything before the first "_" in the name
    mutate(region = str_split(name, '_', simplify = TRUE)[,2]) %>% #everything between first and second _
    mutate(date = ymd(datetime)) %>% #convert datetime (character) to a date character to extract the year day and month to match the sample input file from MVSE
    mutate(year = year(date),
           month = month(date)) %>%
    mutate(state_year = paste(state, year, sep = "_")) %>%
    #crucial filtering step!!!!
    filter(date> as.Date(opt$datelow) & date < as.Date(opt$dateupper)) %>%
    filter(month >= opt$monthlower & month <= opt$monthupper) %>%
    group_by_(grp1, grp2) %>%
    summarize(T = mean(temp),
              H = mean(humidity),
              R = mean(precip),
              name = name,
              state = state,
              year = year,
              month = month,
              state_year = state_year) %>%
    mutate(T = if_else(T < 0 , 0, T)) %>% #added per MVSE author recommendation to remove negative temperatures
    ungroup()
  
  #separate variable to keep variables before it splits to get names
  group_list = df2 %>%
    group_by_(grp1) %>%
    group_split(.keep = T) 
 
  #get names from grp1 to name output csv
  state_nm = unique(df2[[grp1]])
   
  group_list2 = list() #create empty vector for for loop
  
  #get X to match the sample csv file
  for(i in seq_along(group_list)) {
    group_list2[[i]] = group_list[[i]] %>%
      mutate(X = 1:nrow(group_list[[i]]),
             date = as.character(date)) %>%
      select(name, state, state_year, X, T, H, year, month, date, R) %>%
      ungroup()
   
    #if grouping by region then use state_region as name if by state use state only
    #extract state name for naming files and plots and stuff
    
  }
  
#Write Files  
  
  #if the output directory doesn't exist create it
  if(!dir.exists(paste0("data_mid/",dir))) {
    dir.create(paste0("data_mid/",dir))
  }
  
  for(i in seq_along(group_list2)) {
    write.csv(group_list2[i], paste0("data_mid/", dir, state_nm[[i]],"_mvse_ready.csv"))
  }
  
}



if(opt$geo[1] == "region") {

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
  #SUMMARIZE BY REGION
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

climate_clean(climate_df, "name", "date", "region_airport/")  
  
} 
#OPTIONAL STATE GROUPED FOR INDEX P ####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(opt$geo[1] == "state") {
  
  climate_clean(climate_df, "state", "date", "state/")  
  
} 

if(opt$geo[1] == "state_year") {
  
  climate_clean(climate_df, "state_year", "date", "state_year/")  
  
} 

#centroid same as region but uses centroid weather data instead
if(opt$geo[1] == "centroid") {
  
    climate_df = rbind( read.csv("data_mid/ne_centroid1_2003-01-01_to_2020-12-31.csv"),
                      read.csv("data_mid/ne_centroids2 2003-01-01 to 2020-12-31.csv"))
  
  climate_clean(climate_df, "name", "date", "centroid/") 
  
} 

if(!opt$geo[1] %in% c("state", "region", "state_year", "centroid")) { 
  print("please pick either `state`, `region`, `state_year`, or `centroid` as inputs for geo argument")
  }

#END OF GROUPED STATE OPTION ####
 






