#data combine and plot
#PURPOSE:
#THIS CODE COMBINES CASE, AND MOSQUITO ABUNDANCE DATA, BY STATE_GRP, YEAR AND MONTH FOR MODELING


rm(list = ls())
#INSTALL PACKAGES
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all")
pacman::p_load(tidyverse, optparse, lubridate, ggpubr, 
               sf, sp, maps, maptools) # for mapping


mvse0 = read.csv("data_mid/mvse_output.csv") %>%
  select(-X) %>%
  mutate(date = as.Date(date),
         year = as.factor(year),
         month = as.factor(month),
         week = as.factor(week),
         day = as.factor(day)
        )

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#READ IN CLIMATE DATA AND JOIN
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

climate_df = rbind(read.csv("data_mid/ne_centroid1_2003-01-01_to_2020-12-31.csv"),
                   read.csv("data_mid/ne_centroids2 2003-01-01 to 2020-12-31.csv")) %>%
  select(name, datetime, temp, humidity, precip) %>%
  mutate(date = as.Date(datetime)) 

mvse = mvse0 %>% 
  left_join(climate_df, by = c("comb_name" = "name", "date")) %>%
  filter(!is.na(temp)) 

rm(climate_df)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GROUP BY MONTH TO MATCH CASE DATA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mvse_m = mvse %>%
  group_by(st_grp, year, month, month_m, date) %>% #group to remove individual counties
  mutate(threshold_3 = if_else(indexP > 0.3, 1,0),
         threshold_4 = if_else(indexP > 0.4, 1,0),
         threshold_5 = if_else(indexP > 0.5, 1,0)) %>%
  ungroup() %>%
  group_by(st_grp, year, month, month_m) %>%
  summarise_if(is.numeric, ~round(mean(.),3)) %>% 
  arrange(st_grp, year, month_m)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#READ IN CASE DATA FORMAT AND CREATE DF_COMB WITH JOIN
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#read in cases pivot wider for merge and combine other species
cases = read.csv("data_mid/cases.csv") %>% 
  rename(state_abb = state) %>% #rename for merging with mvse
  mutate(year = as.factor(year)) %>% #for the join
  pivot_wider(id_cols = -c(X, species), 
              names_from = species_grp, 
              values_from = count,
              values_fn = sum,
              values_fill = 0) %>%
  select(-none) %>% #drop where year state and month had no cases
  group_by(st_grp, year, month) %>%
  summarise(human = sum(human),
            equine = sum(equine),
            other = sum(other)) %>%
  arrange(st_grp, year, month) %>%
  ungroup()

df_comb = mvse_m %>%
  left_join(cases, by = c("st_grp","year", "month"))
# rename(state_cases_yr = count) %>%

rm(cases)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#READ IN MOSQUITO DATA FORMAT AND JOIN
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mosquito = read.csv("data_mid/mosquito_abundance_by_month_CT_MA.csv") %>%
  select(-X) %>%
  mutate(year = as.factor(year))


df_comb = df_comb %>% 
  left_join(mosquito, by = c("st_grp", "year", "month")) 

rm(mosquito)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#DATA FORMAT ####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
df_comb = df_comb %>%
  mutate(month = ordered(month, levels = month.abb)) %>%
  mutate(month_m = as.Date((month_m))) %>%
  mutate(month_n = month(month_m)) %>%
  mutate(year_grp = as.factor(if_else(year == 2019, "2019", "other"))) %>%
  mutate(all_cases = human + equine + other,
         human_equine = human + equine) %>%
  group_by(st_grp, year) %>%
  mutate(indexP_lag1 = lag(indexP, n = 1, default = NA),
         temp_lag1 = lag(temp, n = 1, default = NA),
         abundance_lag1 = lag(abundance, n = 1, default = NA),
         vector_index_lag1 = lag(vector_index, n = 1, default = NA),
         eeev_detected = if_else(pir > 0, 1, 0) #was any amount of EEEV found in mosquito testing
          ) %>%
  ungroup() %>%
  filter(st_grp != "other NE")

#identify first month that EEEV was introduced
#create a recoded month intro for the model
level_key = c("10" = "1",
              "9"  = "2", 
              "8"  = "3",
              "7"  = "4",
              "6"  = "5",
              "5"  = "6") 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FIRST MONTH OF DETECTION
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
first_mo = df_comb %>%
  filter(pir > 0) %>% # filter out months without infection
  group_by(st_grp, year) %>%
  summarise(month_detected = as.factor(min(month_n))) %>% #find yearly min for state
  mutate(month_detected_inv = recode_factor(month_detected, !!!level_key)) %>%
  ungroup() %>%
  tidyr::complete(st_grp, year) %>%
  mutate(month_detected_inv = as.numeric(month_detected_inv)) %>%
  mutate(month_detected_inv = replace_na(month_detected_inv, 0))

df_comb = df_comb %>%
  left_join(first_mo, by = c("st_grp", "year"))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#TOTAL CASES FOR FOR THE YEAR
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
yr_sum = df_comb %>%
  group_by(st_grp, year) %>%
  summarise(yr_case_tl = sum(human_equine, na.rm = T))

df_comb = df_comb %>%
  left_join(yr_sum, by = c("st_grp", "year"))

rm(yr_sum, first_mo)

write.csv(df_comb, "data_output/combined_mo_data.csv")

save(df_comb, file = "data_output/combined_mo_data.RData")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#YEARLY ANALYSIS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mean_col = c("indexP", "indexP_lag1", "abundance", "pir","vector_index")
sum_col = c("human", "equine", "human_equine", "all_cases")

t1 = df_comb %>%
  group_by(st_grp, year) %>%
  summarise_at(mean_col, mean, na.rm = T)

t2 = df_comb %>%
  group_by(st_grp, year) %>%
  summarise_at(sum_col, sum, na.rm = T)

df_comb_yr = t1 %>% 
  left_join(t2, by = c("st_grp", "year")) %>%
  left_join(first_mo, by = c("st_grp", "year"))

write.csv(df_comb_yr, "data_output/combined_yr_data.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GROUP BY MONTH TO MATCH CASE DATA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mvse_mo = mvse %>%
  group_by(st_grp, year, month, month_m, date) %>% #group to remove individual counties
  mutate(threshold_3 = if_else(indexP > 0.3, 1,0),
         threshold_4 = if_else(indexP > 0.4, 1,0),
         threshold_5 = if_else(indexP > 0.5, 1,0)) %>%
  ungroup() %>%
  group_by(st_grp, year, week, month_m) %>%
  summarise_if(is.numeric, mean) %>% 
  arrange(st_grp, year, month_m)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#READ IN MOSQUITO DATA FORMAT AND JOIN
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mosquito_wk = read.csv("data_mid/mosquito_abundance_by_week_CT_MA.csv") %>%
  select(-X) %>%
  mutate(year = as.factor(year),
         week = as.factor(week)) 


df_comb_wk = mvse %>% 
  left_join(mosquito_wk, by = c("st_grp", "year", "week"))

rm(mosquito_wk)


write.csv(df_comb, "data_output/combined_wk_data.csv")


