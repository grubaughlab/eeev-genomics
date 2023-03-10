#look at available case data
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all")
pacman::p_load(tidyverse, lubridate, ggpubr)

source("scripts/colors.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CASES NON-HUMAN
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cases_n = read.csv("data_input/case_data/arbonet_cases_nonhuman.csv") %>%
  mutate(onsetdate = dmy(onsetdate),
         year = year(onsetdate),
         month = month(onsetdate, label = T, abbr = F)) %>%
  group_by(year, month, state,species) %>%
  summarize(count = n())

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CASES HUMAN
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cases_h = read.csv("data_input/case_data/arbonet_cases_human.csv") %>%
  mutate(species = "human") # for merge with non human cases


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CASES MERGE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FOR GROUPING VARIABLE
other_st = c("AL","AR", "FL","GA","IN","LA","MI","MO","MT","NC","SC","TN","VA","WI", "PA")
other_ne = c("ME","MD", "NJ", "RI", "VT", "NH")
st = c("CT", "NY", "MA")

 
cases = rbind(cases_h, cases_n) %>%
complete(state, month, year) %>%
mutate(st_grp =  case_when(
           str_detect(state, paste(st, collapse="|")) ~ state,
           str_detect(state, paste(other_ne, collapse = "|")) ~ "other NE",
           str_detect(state, paste(other_st, collapse = "|")) ~ "other"
                          ),
       species_grp = case_when(
           species == "human"  ~ "human",
           species == "Equine" ~ "equine",
           is.na(species)      ~ "none",
            TRUE               ~ "other"
                              )
        ) %>%
  mutate(month = plyr::mapvalues(month, from = month.name, to = month.abb)) %>%
  mutate(month = ordered(month, levels = month.abb)) %>%
  filter(st_grp != "other") %>%
  mutate(species = replace_na(species, "none"),
         count = replace_na(count, 0)) 


write.csv(cases, "data_mid/cases.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CASES PLOT
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#OVER TIME
p_cases = ggplot(cases %>% filter(species_grp != "none"), aes(x = year, y = count, fill = st_grp)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~species_grp, ncol = 1) +
  scale_x_continuous(breaks = seq(2003, 2020, by = 1)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = st_cols)

p_cases


#BY MONTH
ggplot(cases %>% filter(species_grp != "none"), aes(month, count, fill = st_grp)) +
  geom_col() +
  facet_wrap(~species_grp, ncol = 1) +
  theme_classic() +
  scale_fill_manual(values = st_cols)




