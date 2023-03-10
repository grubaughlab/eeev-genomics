
rm(list = ls())
#INSTALL PACKAGES
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all")
pacman::p_load(tidyverse, optparse, lubridate) # for mapping


# * import and clean ####
df_comb = read.csv("data_output/combined_mo_data.csv") %>%
  dplyr::select(-X) %>%
  mutate(all_cases = as.factor(all_cases),
         month_detected_july = as.integer(if_else(month_detected_inv == 4, 1, 0)),
         month_detected_aug = as.integer(if_else(month_detected_inv == 3, 1, 0)),
         month_detected_2 = case_when(month_detected_inv == 4 ~ "Jul",
                                        month_detected_inv == 3 ~ "Aug",
                                                    T ~ "other"),
         month_detected_2 = factor(month_detected_aug, levels = c("other", "Jul", "Aug")),
         vi_grp = cut(vector_index, 
                      breaks = seq(0,35, 5),
                      include.lowest = T),
         pir_bin = if_else(pir > 0, 1, 0),
         month = factor(month, levels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), #Jul first because that is the first month with cases
       #  month_f = factor(month, levels = c("Jul", "May", "Jun", "Aug", "Sep", "Oct", "Nov", "Dec")), #Jul first because that is the first month with cases
         month_f = case_when(
           month_n < 8  ~ "May-Jul",
           month_n == 8 ~ "Aug",
           month_n == 9 ~ "Sep",
           month_n == 10 ~ "Oct",
           TRUE ~ "error")
        ) %>% #for reference group
  mutate(vector_index_log = log(vector_index + 1)) %>%
  mutate(month_f = factor(month_f, levels = c("May-Jul", "Aug", "Sep", "Oct"))) %>%
  filter(!is.na(abundance)) %>%
  mutate_if(function(x) is.numeric(x) & !is.integer(x), ~c(scale(.))) %>%
  mutate(time_index = as.integer(interval(min(.$month_m), .$month_m) %/% months(1))) %>%
  mutate(year_index = as.integer(year - min(year) + 1))# %>%
 # mutate(harmonic = sin(2*pi*time_index/12) +   cos(2*pi*time_index/12))

df_comb = df_comb %>% 
  dplyr::select(st_grp, human_equine, yr_case_tl,
                year, year_grp, month_m, year_index, time_index, month, month_n, month_f, 
                month_detected, month_detected_inv,
                month_detected_july, month_detected_aug,
               # harmonic,
                humidity, indexP, indexP_lag1, temp_lag1, temp,
                vector_index, vector_index_log, abundance, abundance_lag1, pir)

# * write final data####
write.csv(df_comb, "data_output/combined_data_for_model.csv")

save(df_comb, file = "data_output/combined_data_for_model.RData")


# * get Mean and SD for continuous variables
mu_sd = read.csv("data_output/combined_mo_data.csv") %>%
  select_if(function(x) is.numeric(x) & !is.integer(x)) %>%
  gather(factor_key = T) %>%
  group_by(key) %>%
  summarise(mean= mean(value, na.rm = T), 
            sd= sd(value, na.rm = T), 
            max = max(value, na.rm = T), 
            min = min(value, na.rm = T)) %>%
  mutate_if(is.numeric, ~round(., 3))

write.csv(mu_sd, "data_output/summary_stats_of_contin_var.csv")
save(mu_sd, file = "data_output/summary_stats_of_contin_var.RData")
  

