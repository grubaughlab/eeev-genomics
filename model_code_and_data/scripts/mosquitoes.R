#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#MOSQUITO DATA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_unload()
pacman::p_load(readr, readxl, tidyverse, janitor, lubridate, PooledInfRate, ggpubr)

#NOTES
#cant use average by trap because rows are the pools within traps and max pool size is 50
#need to use sum for trap by day for abundance which is the accession number.
#

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#clean colnames
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
tk_clean_names = function(x) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(stringr)
    
  if(!is.character(x)) {
    stop("x must be a character. Try using colnames(dataframe)")
  } else {
    x = str_to_lower(x)
    x = str_remove(x, "[[:punct:]]")
    x = str_trim(x)
    x = str_replace(x, " ", "_")
    x = str_replace(x, " ", "_")
    return(x)
  }

}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> --------------------------------------     CONNECTICUT ----------------------------------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> READ DATA AND CLEAN NAMES
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fn = list.files("data_input/mosquito_data/", pattern = "Cumulative", full.names = T)
m = map_df(fn, read_xlsx) 
colnames(m) = tk_clean_names(colnames(m))

m_m = m %>%
  mutate(number_of_traps = 1) %>% #to match the MA data
  mutate(st_grp = "CT") %>%
  dplyr::select(st_grp, #
         species, #
         site, #
         town, #
         county, #
         trap_type, #
         date, #
         accession, #
         mosquitoes, #
         number_of_traps, #
         virus #
        ) 


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> --------------------------------------   MASSACHUSSETTS----------------------------------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#notes about massachussets pool size
# for each line I am going to assume that the number of traps indicates that in that area x nunmber of traps were used to create the 
#pool, which each individual line is a pool. This was determine because there were examples of more traps than there were mosquitos
#this will impact the abundance where the number of mosquitoes will be divided by the number of traps, but not the pir where each line
#will be considered a pool

m_ma = readxl::read_excel("data_input/mosquito_data/MA_mosquito.xlsm", sheet = "Raw Data")
colnames(m_ma) = tk_clean_names(colnames(m_ma))

m_ma = m_ma %>%
  rename(date = collection_date, #match names of CT data
         site = trap_site,
         accession = case_id,
         mosquitoes = pool_size
         ) %>%
  mutate(number_of_traps = replace_na(number_of_traps, 1),
         virus = case_when(   #match format of CT
        result == "Positive"   ~ "EEE",
        result == "Negative"   ~ "0",
        result == "Not Tested" ~ "0"
                           )
           ) %>%
  mutate(st_grp = "MA") %>%
  dplyr::select(st_grp, #
         species, #
         site, #
         town, #
         county, #
         trap_type, #
         date, #
         accession, #
         mosquitoes, #
         number_of_traps, #
         virus #
         )


m0 = rbind.data.frame(m_ma, m_m)
  

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> ADD GROUPING VARIABLES AND SPECIES
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#filter for melanura and create grouping variables
m1 <- m0 %>%
 filter(species %in% c("Culiseta melanura", "Coquillettidia perturbans")) %>%
 mutate(year = year(date),
        month = month(date, label = T, abbr = T),
        week = as.factor(week(floor_date(date, "week", 7))),
        virus = replace_na(virus, "none"),
        pos = if_else(str_detect(virus, "EEE"), 1, 0),
        number_of_pools = 1)  #for grouping


# #sites to be used 
# sites = m1 %>%
#   distinct(st_grp, site, trap_type) %>%
#   count(site)
# 
# #number of traps total
# traps = sum(sites$n)
# #number of years
# years = max(year(m1$date)) - min(year(m1$date))
# #number of epi weeks
# weeks = m1 %>% distinct(cdc_week) %>% count()
# 
# #rough calculation of max total observations for abundance
# #will be less given every trap isnt tested every single week I imagine
# traps*years*weeks

#number of pools per trap per day
#pools = m1 %>% get_dupes(date, site, trap_type, species)

#traps per cdc_week
traps_wk= m1 %>%
  #  group_by(st_grp, year, month, month2, date_m, pos, #variables to keep
  #           date, site, trap_type, species) %>% #actual grouping variables
  group_by(st_grp, year, week) %>% #true grouping variable 24968 just accession
  summarize(no_traps = sum(number_of_traps))

#PLOT TO CHECK FOR ANY WEIRDNESS
ggplot(traps_wk, aes(week, no_traps, fill = st_grp)) +
  geom_col() +
  facet_wrap(~year)


#ABUNDANCE
#total mosquito's per trap per night
m2 = m1 %>%
  group_by(st_grp, date, site, trap_type, species) %>% #true grouping variable 24968 just accession
  summarize(mosq = sum(mosquitoes),
            n_traps = sum(number_of_traps),
            n_pools = sum(number_of_pools)
            ) %>%
  ungroup() %>%
  mutate(n_traps = if_else(st_grp == "CT", #converting back to one because CT broke up pools from same trap as different observations
                           1,
                           n_traps),
         mosq_per_trap = mosq/n_traps, 
         week = as.factor(week(floor_date(date, "week", 7))),
         year = year(date),
         month = month(date, label = T, abbr = T)) %>%
  arrange(st_grp, date, site, trap_type)

ggplot(m2, aes(mosq_per_trap, fill = st_grp)) +
  geom_density(alpha = 0.8)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> ANALYSIS BY WEEK
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#abundance drop site info
m_abund_wk = m2 %>%
  group_by(st_grp,
           year, week, species) %>% #true grouping variable 24968 just accession)
  summarize(abundance = mean(mosq_per_trap, na.rm = T)) %>%
  ungroup()


#mean and variance
m_abund_wk %>% 
  group_by(species) %>%
  summarize(mean(abundance),
            sd(abundance))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> DESCRIPTIVE PLOTS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#boxplot mosquitoes per trap per week 2000-2019
ggplot(m_abund_wk, aes(week,abundance, fill = species, color = species)) +
  geom_boxplot(alpha = 0.5) +
  ggtitle("Average mosquitoes per trap per week 2000-2019") +
  theme_classic() +
  facet_wrap(~st_grp, ncol =1 )

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CALCULATE POOLED INFECTIVITY RATE BY WEEK
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#split to map pIR over month pools
m1_list = m1 %>%
  group_by(st_grp, year, week, species) %>%
  group_split()

#iterat
mle = purrr::map(m1_list, ~ pIR(pos ~ mosquitoes, data = ., pt.method = "mle"))

#extract values from pIR 
pir = sapply(mle,"[[",1)
lci =sapply(mle,"[[",2)
uci =sapply(mle,"[[",3)

m_abund_wk2 = m_abund_wk %>%
  mutate(pir = round(pir,4),
         pir_lci = round(lci,4),
         pir_uci = round(uci,4),
         vector_index = round(abundance * pir,4)
  )


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PLOT PIR 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ALL YEARS BY MONTH
ggplot(m_abund_wk2, aes(x = week, y = pir, fill = species, color = species)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic() +
  facet_wrap(~st_grp, nrow = 1) +
  theme(axis.text.x = element_text(angle = 90))


m_abund_wk2_mel = m_abund_wk2 %>%
  filter(species == "Culiseta melanura") %>%
  mutate(year = as.factor(year))

source("scripts/colors.R")
#line plot by year for melanura
p_mosq_eeev_wk = ggarrange(
  ggplot(m_abund_wk2_mel, aes(x = week)) +
    geom_line(aes(y = abundance, group = year, color = year)) + 
    theme_classic() +
    scale_color_manual(values = yr_col) +
    facet_wrap(~st_grp, ncol = 1) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90)),
  ggplot(m_abund_wk2_mel, aes(x = week, y = pir, group = year, color = year)) +
    geom_line() +
    theme_classic() +
    scale_color_manual(values = yr_col) +
    facet_wrap(~st_grp, ncol = 1) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90)),
  
  ggplot(m_abund_wk2_mel, aes(x = week, y = vector_index, group = year,color = year)) +
    geom_line() +
    theme_classic() +
    scale_color_manual(values = yr_col) +
    facet_wrap(~st_grp, ncol = 1) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90)),
  nrow = 1
  
)

ggsave("data_output/mosquito_eeev_data_by_week.png")

write.csv(m_abund_wk2_mel, "data_mid/mosquito_abundance_by_week_CT_MA.csv")

write.csv(m_abund_wk2, "data_mid/mosquito_abundance_by_week_CT_MA_all_mosq.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> ANALYSIS BY MONTH
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#abundance drop site info
m_abund_mo = m2 %>%
  group_by(st_grp,
          year, month, species) %>% #true grouping variable 24968 just accession)
  summarize(abundance = mean(mosq_per_trap)) %>%
  ungroup()


#mean and variance
m_abund_mo %>% 
  group_by(species) %>%
  summarize(mean(abundance),
            sd(abundance))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> DESCRIPTIVE PLOTS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#boxplot mosquitoes per trap per week 2000-2019
ggplot(m_abund_mo, aes(month,abundance, fill = species, color = species)) +
  geom_boxplot(alpha = 0.5) +
  ggtitle("Average mosquitoes per trap per Month 2000-2019") +
  facet_wrap(~st_grp, ncol = 1) +
  theme_classic()

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CALCULATE POOLED INFECTIVITY RATE BY MONTH
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#split to map pIR over month pools
m1_list = m1 %>%
  group_by(st_grp, year, month, species) %>%
  group_split()

#iterat
mle = map(m1_list, ~ pIR(pos ~ mosquitoes, data = ., pt.method = "mle"))

#extract values from pIR 
pir = sapply(mle,"[[",1)
lci =sapply(mle,"[[",2)
uci =sapply(mle,"[[",3)

m_abund_mo2 = m_abund_mo %>%
  ungroup() %>%
  mutate(pir = round(pir,4),
         pir_lci = round(lci,4),
         pir_uci = round(uci,4),
         vector_index = round(abundance * pir,4)
  )


m_abund_mo2_mel = m_abund_mo2 %>%
  filter(species == "Culiseta melanura")

write.csv(m_abund_mo2_mel, "data_mid/mosquito_abundance_by_month_CT_MA.csv")
write.csv(m_abund_mo2, "data_mid/mosquito_abundance_by_month_CT_MA_all_mosq.csv")



