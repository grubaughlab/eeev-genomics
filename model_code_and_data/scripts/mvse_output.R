#!/usr/bin/env Rscript

#mvse output plot

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#HEADER CODE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm(list = ls())
#INSTALL PACKAGES
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all")
pacman::p_load(tidyverse, optparse, lubridate, ggpubr, 
               sf, sp, maps, maptools) # for mapping

#CREATE OPTION TO RUN GROUPED BY STATE INSTEAD OF REGION FOR CALCULATING INDEX P
#NOT REALLY USED ANYMORE
.option_list = list(
  make_option(c("-g", "--geo"), type="character",  
              default= "centroid",
              help = "determine whether to use either the region or state as the grouping variable. Options are region, state, state_year", 
              metavar="character"
              ),
  make_option(c("-l", "--datelower"), type = "character",
              default = "2000-01-01",
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



source("scripts/colors.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#COMBINE MVSE INDEX P DATAFRAMES AND CLEAN
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  
#get files names to read them in
fn = list.files(path = "data_mid/centroid/", pattern = "indexP.csv")
state_nm = stringr::str_split(fn, '_', simplify = TRUE)[,1] #extract string before first "_"
region_nm = stringr::str_remove(fn, "data_mid/") #remove directory
region_nm = stringr::str_split(region_nm, '_', simplify = TRUE)[,2]  #extract string between "_"
region_nm = stringr::str_split(region_nm, '\\.', simplify = TRUE)[,1]  #extract string between "_"

fn = paste0("data_mid/centroid/", fn)

#loop through list to read files in select columns and add in state name

df = list()

#SELECT VARIABLES AND ADD IN GEOGRAPHICAL IDENTIFIERS AS A VARIABLE
for(i in seq_along(fn)) {
 df[[i]] = read.csv(fn[[i]]) %>%
   select(date, indexP, indexPlower, indexPupper) %>%
   mutate(state = state_nm[[i]],
          region = region_nm[[i]],
          date = as.Date(date))
}

rm(fn, state_nm, region_nm)

#FOR STATE GROUPS
other_st = c("AL","AK", "AZ", "CA", "CO", "AR", "FL","GA","IN","LA","MI","MO","MT","NC","SC","TN","VA","WI", "PA")
other_ne = c("ME","MD", "NJ", "RI", "VT", "NH")
st = c("CT", "NY", "MA")


#COMBING DATAFRAMES, FILTER AND ADD METADATA
mvse = df %>% map_df(rbind.data.frame) %>%
  filter(date > opt$datelow & date < opt$dateupper) %>%
  mutate(year = as.factor(year(date))) %>%
  mutate(state = as.factor(state),
         week = week(date), 
         month = month(date, label = T),
         month_m = floor_date(date, "month"),
         year = year(date),
         day = yday(date),
         comb_name = paste0(state, "_", region)) %>%
  mutate(state_abb = as.character(plyr::mapvalues(state, from = str_to_lower(state.name), to = state.abb))) %>%
  mutate(st_grp =  case_when(
    str_detect(state_abb, paste(st, collapse="|")) ~ state_abb ,
    str_detect(state_abb, paste(other_ne, collapse = "|")) ~ "other NE",
     str_detect(state_abb, paste(other_st, collapse = "|")) ~ "other")
        ) %>%
  mutate(state_abb = as.factor(state_abb)) %>%
  mutate(year_grp = as.factor(if_else(year == 2019, "2019", "other"))) %>%
  mutate(year = as.factor(year),
         year = fct_relevel(year, "2019", after = Inf))


rm(df, other_st, other_ne, st)


write.csv(mvse, "data_mid/mvse_output.csv") 





