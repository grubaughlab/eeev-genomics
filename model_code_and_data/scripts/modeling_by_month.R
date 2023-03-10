#modeling tests on monthly time scale

#HEADER ####
#source any updates to final data for model
source("scripts/data_clean_for_model.R")

rm(list = ls())

load("data_output/combined_data_for_model.RData")
load("data_output/summary_stats_of_contin_var.RData")

#seeing whether removing the months with 0's did much to the model and it didn't really
# df_comb = df_comb %>% filter(month != "May",
#                              month != "Jun")

df_unscale = read.csv("data_output/combined_mo_data.csv")

# * packages ####
if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all")
pacman::p_load(readr, readxl, tidyverse, lubridate, ggpubr, purrr,
               broom, broom.helpers, easystats, MASS, pscl, boot,
               DHARMa, #for checking residuals
               modelr, ciTools) #for models

# * split by st_grp ####

MA = df_comb %>%
  filter(st_grp == "MA")

CT = df_comb %>%
  filter(st_grp == "CT")


#FUNCTIONS ####

mod_check = function(mod) {
  
#extract the name of the model
mod_nm = deparse(substitute(mod))

t = list(
  "check_model" = check_model(mod),
  "collinearity" = check_collinearity(mod),
  "distribution" = check_distribution(mod),
  "dispersion" = check_overdispersion(mod),
  "zeroinflation" = check_zeroinflation(mod),
  #"heteroskedasticity" = check_heteroskedasticity(mod),
  "aurocorrelation" = check_autocorrelation(mod)

        )
  assign(paste0(mod_nm, "_checks"), t, envir = globalenv())
  
}

mod_clean = function(x, mod) {
  #x = the previously existing compiled model data
  #mod = the model you want to add
  x_nm = deparse(substitute(x)) #get object as a string
  mod_nm = deparse(substitute(mod))
  formula = paste(mod$call$formula[2],mod$call$formula[1], mod$call$formula[3])
  
  if(exists(x_nm)) {
   t = mod %>%
     
      broom::tidy(., conf.int = T, exponentiate = T) %>%
      mutate(sig = if_else(p.value < 0.05, 1, 0)) %>%
      mutate_if(is.numeric, round, 3) %>%
      mutate(model = mod_nm) %>%
      mutate(formula = formula) %>%
      mutate(family = case_when(
                str_detect(model, "modp|mod_p|pois") ~ "pois",
                str_detect(model, "modnb|modnb|negbin") ~ "neg_bin",
                 str_detect(model, "modlm|mod_lm") ~ "lm",
                 TRUE ~ "unknown")
            ) %>%
      rbind(x) %>% #append to existing dataframe
      distinct_all(.) #remove any models you tried to run twice
    
  }else{
    mod %>%
      broom::tidy(., conf.int = T, exponentiate = T) %>%
      mutate(sig = if_else(p.value < 0.05, 1, 0)) %>%
      mutate_if(is.numeric, round, 3) %>%
      mutate(model = mod_nm) %>%
      mutate(formula = formula) %>%      
      mutate(family = case_when(
        str_detect(model, "modp|mod_p|pois") ~ "pois",
        str_detect(model, "modnb|modnb|negbin") ~ "neg_bin",
        str_detect(model, "modlm|mod_lm") ~ "lm",
        TRUE ~ "unknown")
      )
    
  } 

}

mod_fit = function(x, mod) {
  #x = the previously existing compiled model data
  #mod the model you want to add
  x_nm = deparse(substitute(x)) #get object as a string
  mod_nm = deparse(substitute(mod))
  formula = paste(mod$call$formula[2],mod$call$formula[1], mod$call$formula[3])
  
      #if the x object to write to doesn't exist create one
  if(exists(x_nm)){
    glance(mod) %>%
      mutate(model = mod_nm) %>%
      mutate(formula = formula) %>%
      rbind(x) %>%
      arrange(AIC) %>%
      distinct_all(.) %>% #incase a model is run and added twice it will remove it
      dplyr::select(model,formula, everything())
  } else {
    glance(mod) %>%
      mutate(model = mod_nm) %>%
      mutate(formula = formula) %>%
      dplyr::select(model,formula, everything())
  } 

}

purrr_lm = function(y, df) {
  
  uni = df %>% 
    dplyr::select(-y) %>%  # exclude outcome, leave only predictors 
    map(~glm.nb(df$y ~ .x, data = df))
  
  t = uni %>%
    map_df(~tidy(.x, conf.int = T, exponentiate = T))
  
  t2 = uni %>%
    map("coefficients")
  
  t2 =  as.data.frame(unlist(t2)) %>%
    transmute(term = rownames(.),
              estimate = exp(`unlist(t2)`))
  
  rownames(t2) = NULL
  
  #merge by estimate
  uni = left_join(t,t2, by = "estimate") %>%
    filter(term.x != "(Intercept)") %>%
    mutate(term.x = term.y) %>%
    dplyr::select(-term.y) %>%
    mutate(term.x = str_remove(term.x, "\\.\\.x")) %>%
    rename(term = term.x)
  
  rm(t,t2)
  
  p_uni = ggplot(uni, aes(x = term, y = estimate)) +
    geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width=0.2)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Univariate")
  
  #compare to multinomial
  full = glm.nb(df$y ~ ., data = df)
  
  full = tidy(full, conf.int = T, exponentiate = T) %>%
    filter(term != "(Intercept)")
  
  p_full = ggplot(full, aes(x = term, y = estimate)) +
    geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width=0.2)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    theme_classic() +
    ggtitle("Multivariate") +
    theme(axis.text.x = element_text(angle = 90)) 
  
  data = list("uni" = uni, 
              "multivariate" = full,
              "plot" = ggpubr::ggarrange(p_uni, p_full))
  return(data)
  
}

plot_estimates = function(mod){
  
  if(is.data.frame(mod)) {
    ggplot(mod, aes(x = term, y = estimate, color = term)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width=0.2)) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      theme_bw()+
      theme(axis.text.x = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, size = 7),
            legend.position = "none")+
      facet_grid(family~model)
  } else {
    print("mod must be a dataframe compiled using mod_clean function")
  }
  
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#NEG BIN VECTOR INDEX Only ####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ALL VARIABLES


# * Model: full ####
modnb_vi = glm.nb(human_equine ~  vector_index + st_grp,
                   data = df_comb,
                   control = glm.control(maxit=100))

summary(modnb_vi)
glance(modnb_vi)

mod_check(modnb_vi)
mod_estimates = mod_clean(mod_estimates, modnb_vi)
fit = mod_fit(fit, modnb_vi)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#NEG BIN INDEXP Only  ####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ALL VARIABLES

# * Model: full ####
modnb_ip = glm.nb(human_equine ~  indexP_lag1 + st_grp,
                  data = df_comb,
                  control = glm.control(maxit=100))

summary(modnb_ip)
glance(modnb_ip)

mod_check(modnb_ip)
mod_estimates = mod_clean(mod_estimates, modnb_ip)
fit = mod_fit(fit, modnb_ip)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#NEG BIN ALL ####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ALL VARIABLES

# * Model: full ####
modnb_all = glm.nb(human_equine ~  vector_index + month_detected_july + year_index + month_f + indexP_lag1 + st_grp,
                      data = df_comb,
                      control = glm.control(maxit=100))

summary(modnb_all)
mod_check(modnb_all)
mod_estimates = mod_clean(mod_estimates, modnb_all)
fit = mod_fit(fit, modnb_all)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#NEG BIN No Index P ####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ALL VARIABLES

# * Model: full ####
modnb_Nip = glm.nb(human_equine ~  vector_index + month_detected_july + year_index + month_f + st_grp,
                   data = df_comb,
                   control = glm.control(maxit=100))

summary(modnb_Nip)
mod_check(modnb_Nip)
mod_estimates = mod_clean(mod_estimates, modnb_Nip)
fit = mod_fit(fit, modnb_Nip)

# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# NEG BIN ALL temp ####
# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
modnb_all_temp = glm.nb(human_equine ~ vector_index + month_detected_july + year_index + month_f + temp_lag1 + st_grp,
                    data = df_comb,
                    control = glm.control(maxit=100))

mod_check(modnb_all_temp)
mod_estimates = mod_clean(mod_estimates, modnb_all_temp)
fit = mod_fit(fit, modnb_all_temp)

# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# NEG BIN ALL Abundance ####
# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
modnb_all_abun = glm.nb(human_equine ~ vector_index + month_detected_july + year_index + month_f + abundance_lag1 + st_grp,
                        data = df_comb,
                        control = glm.control(maxit=100))

mod_check(modnb_all_abun)
mod_estimates = mod_clean(mod_estimates, modnb_all_abun)
fit = mod_fit(fit, modnb_all_abun)


# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# NEG BIN ALL month detect EF ####
# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
modnb_all_EF = glm.nb(human_equine ~ vector_index * month_detected_july + year_index + indexP_lag1 + st_grp + month_f,
                    data = df_comb,
                    control = glm.control(maxit=100))

summary(modnb_all_EF)
mod_check(modnb_all_EF)
mod_estimates = mod_clean(mod_estimates, modnb_all_EF)
fit = mod_fit(fit, modnb_all_EF)

plot_estimates(mod_estimates)


# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# NEG BIN ALL month detect EF2 ####
# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
modnb_all_EF2 = glm.nb(human_equine ~ vector_index * month_detected_aug + year_index + indexP_lag1 + st_grp + month_f,
                      data = df_comb,
                      control = glm.control(maxit=100))

summary(modnb_all_EF2)
mod_check(modnb_all_EF2)
mod_estimates = mod_clean(mod_estimates, modnb_all_EF2)
fit = mod_fit(fit, modnb_all_EF2)

plot_estimates(mod_estimates)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CHECK RESIDUALS DHARMa####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

simulationOutput <- simulateResiduals(fittedModel = modnb_all, plot = F, n = 1000)
plot(simulationOutput)
#KS test: p = 0.17 distribution appropriate
#dispersion test: p = 0.47 no dispersion detected
#outlier test p =1
plotResiduals(simulationOutput, form = df_comb$vector_index)
testZeroInflation(simulationOutput)
#zero inflation not detected p = 0.776
testDispersion(simulationOutput)


 #testing autocorrelation index P lag is removed because more appropriate for testing
modnb_all_MA = glm.nb(human_equine ~ vector_index * month_detected_aug + year_index + indexP + month_f,
                      data = MA,
                     control = glm.control(maxit=100))

simulationOutput <- simulateResiduals(fittedModel = modnb_all_MA, plot = F)
plot(simulationOutput)
#no abnormalities detected
testTemporalAutocorrelation(simulationOutput, time = MA$time_index)
#DW = 1.59 within bounds for rule of thumb that values between 1.5 and 2.5 are considered normal
#p-value = 0.051

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CHECK PERFORMANCE####
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

p_fit = ggplot(fit, aes(x = model, y = AIC)) +
  geom_col() +
  geom_text(aes(label = round(AIC,2)), color = "white", vjust = 1.5) +
  coord_cartesian(ylim = c(220, 280)) +
  theme_classic()

p_fit

ggsave("figures/AIC_model_comparison.png")

write.csv(fit, "data_output/model_fit_stats.csv")

t = compare_performance(modnb_all, modnb_all_EF, modnb_all_abun, modnb_Nip, modnb_ip, modnb_all_temp) %>%
  dplyr::select(-AIC, -BIC)
fit2 = left_join(fit, t, by = c("model" = "Name"))

plot(compare_performance(modnb_all, modnb_all_EF, modnb_all_abun, modnb_Nip, modnb_ip, modnb_all_temp))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#clean and write estimate and fit files
term_nm = c(st_grpMA = "MA",
            vector_index = "Vector Index",
             abundance = "Abundance",
            abundance_lag1 = "Abundance 1 Month Lag",
             year_index = "Year",
             month_fAug = "Aug",
             month_fSep = "Sep",
             month_fOct = "Oct",
             VIxMA = "vector_index:st_grpMA",
             pir_bin = "EEEV Detected During Season",
             indexP_lag1 = "IndexP 1 Month Lag",
             temp_lag1 = "Temp 1 Month Lag",
             month_detected_inv = "First Month EEEV Detected",
            `vector_index:month_detected_inv_fVI detected July` = "VI x Month detected July",
            `vector_index:month_detected_july` = "VI x Month detected Jul",
            `vector_index:month_detected_aug` = "VI x Month detected Aug",
             month_detected_aug = "Month detected Aug",
             month_detected_july = "Month detected Jul")
  
mod_estimates2 = mod_estimates %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term, !!!term_nm)) %>%
  mutate(term = factor(term, level = c("MA",
                                       "Vector Index",
                                       "IndexP 1 Month Lag","Temp 1 Month Lag","Abundance 1 Month Lag",
                                       "Month detected Jul", 
                                       "VI x Month detected Jul",
                                       "Month detected Aug", 
                                       "VI x Month detected Aug",
                                       "VIx1st_Month_detected",
                                       "Year",
                                        "Aug","Sep","Oct"
                                       )))


write.csv(mod_estimates2, "data_output/mod_estimates.csv")
write.csv(fit, "data_output/fit.csv")


#PLOT ESTIMATES####

# * plot estimates ####

p_estimates = plot_estimates(mod_estimates2)

p_estimates

ggsave(filename = "figures/mod_estimates_all.png",
       width = 10,
       height = 6)

p_estimates = plot_estimates(mod_estimates2 %>% 
                             #filter(term != "EEEV Detected During Season") %>%
                              filter(term != "MA") %>%
                              filter(family != "pois") %>%
                               filter(model != "MA_modnb_all_EF")
                              )
 
p_estimates + 
  labs(caption  = "State (MA) Term removed for scale") +
  theme(plot.caption = element_text(face = "bold"))

#combine with p_fit (AIC)

p_aic_estimates = 
  ggarrange(p_fit, p_estimates, 
            ncol = 1,
            heights = c(0.3,0.7)
            )

ggsave(filename = "figures/p_aic_estimates.png",
       plot = p_aic_estimates,
       width = 10,
       height = 6)

ggsave(filename = "figures/p_aic_estimates.svg",
       plot = p_aic_estimates,
       width = 10,
       height = 6)

# * plot estimates without state ####
plot_estimates_final = function(mod, final){
  
  if(is.data.frame(mod)) {
    ggplot(mod %>% filter(model == final), aes(x = term, y = estimate)) +
      geom_point(size = 4, color = "grey30") +
      geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width=0.2), color = "grey30") +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      theme_classic()
  } else {
    print("mod must be a dataframe compiled using mod_clean function")
  }
}

# * plot estimates without state ####
p_estimate_final = plot_estimates_final(mod_estimates2 %>% filter(term != "MA"),
                                         "modnb_all")

p_estimate_final


ggsave(filename = "figures/mod_estimates_nostate_final.png",
       width = 10,
       height = 6)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PREDICTIONS####
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pred_plot = function(df){
  ggplot(df, aes(factor(month))) +
    geom_line(aes(y = pred, group = 1), linewidth = 1.05, color = "red", alpha = 0.8) +
    geom_ribbon(aes(ymin = pred_lwr, ymax = pred_upr, group = 1), fill = "red", alpha = 0.2) +
    geom_line(aes(y = human_equine, group = 1), linewidth = 1.05, color = "grey30", alpha = 0.8) +
    theme_classic() +
    coord_cartesian(ylim = c(0, max(df_comb_pred$pred, df_comb$human_equine) +1)) +
    ylab("Human + Equine Cases") +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank())
}

resid_plot = function(df){
  ggplot(df_comb_pred, aes(x = month_n)) +
    geom_col(aes(y = pred-human_equine), color = "grey30", size = 2, alpha = 0.6) +
    geom_point(aes(y = resid), color = "red", alpha = 0.6) +
    geom_line(aes(y = resid), color = "red", alpha = 0.6) +
    geom_hline(yintercept = 0, color = "grey30") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank()) +
    facet_grid(st_grp~year)
  
}

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# * ALL predictons ####
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
df_comb_pred = df_comb %>% 
          modelr::add_predictions(modnb_all, 
                          type = "response") %>%
          ciTools::add_ci(modnb_all, names = c("pred_lwr", "pred_upr")) %>%
           modelr::add_residuals(modnb_all)

write.csv(df_comb_pred, "data_output/combined_data_mod_pred.csv")

# #plot vector index against cases
# p_pred_pt_all = ggplot(df_comb_pred, aes(x = vector_index)) +
#   geom_point(aes(y = human_equine), color = "grey30", size = 3, alpha = 0.6) +
#   geom_point(aes(y = pred), color = "red", size = 3, alpha = 0.6) +
#   theme_classic() +
#   facet_wrap(~st_grp)
# 
# p_pred_pt_all


#MODEL FOR ALL
p_pred_all = pred_plot(df_comb_pred)
p_pred_all + facet_grid(st_grp ~ year) 

ggsave(filename = "figures/predict_timeseries_all.png", 
       width = 12, height = 4)

ggsave(filename = "figures/predict_timeseries_all.svg", 
       width = 12, height = 4)

p_resid_all = resid_plot(df_comb_pred)
p_resid_all

ggsave(filename = "figures/predict_residual_all.png", 
       width = 12, height = 4)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# * ALL predictons EF ####
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
df_comb_pred2 = df_comb %>% 
  modelr::add_predictions(modnb_all_EF, 
                          type = "response") %>%
  ciTools::add_ci(modnb_all, names = c("pred_lwr", "pred_upr")) %>%
  modelr::add_residuals(modnb_all)

write.csv(df_comb_pred2, "data_output/combined_data_mod_pred_EF.csv")

# #plot vector index against cases
# p_pred_pt_all = ggplot(df_comb_pred, aes(x = vector_index)) +
#   geom_point(aes(y = human_equine), color = "grey30", size = 3, alpha = 0.6) +
#   geom_point(aes(y = pred), color = "red", size = 3, alpha = 0.6) +
#   theme_classic() +
#   facet_wrap(~st_grp)
# 
# p_pred_pt_all


#MODEL FOR ALL
p_pred_all2 = pred_plot(df_comb_pred2)
p_pred_all2 + facet_grid(st_grp ~ year) 

ggsave(filename = "figures/predict_timeseries_all_ef.png", 
       width = 12, height = 4)

ggsave(filename = "figures/predict_timeseries_all_ef.svg", 
       width = 12, height = 4)

p_resid_all2 = resid_plot(df_comb_pred2)
p_resid_all2

ggsave(filename = "figures/predict_residual_all_ef.png", 
       width = 12, height = 4)


ggplot(df_comb_pred2, aes(x = vector_index, y = pred)) +
  geom_point() +
  facet_grid(st_grp~month_detected_july) +
  theme_bw()

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#SIMULATION VI####
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

dummy_data = function(month){
  
  vi_range = rep(seq(0,7,0.1),8)
  l = length(vi_range)
  
  data.frame(vector_index = vi_range,
             month_detected_july = c(rep(0,l/2), 
                                     rep(1,l/2)),
             st_grp = c(rep("CT", l/4),
                        rep("MA", l/4),
                        rep("CT", l/4),
                        rep("MA", l/4)
             ),
             year_index = 17,
             month_f = factor(month),
             # indexP_lag1 = c(rep(-0.007, l/4),
             #                 rep(1.08, l/4),
             #                 rep(0.882, l/4),
             #                 rep(-0.229, l/4)),
             human_equine = NA
  )
  
}

x = unique(df_comb$month_f)
x = as.character(x)
vi_sd = 0.043
vi_mu = 0.021

test = map_df(x, dummy_data) %>%
  distinct_all()

rm(x)

#get indexp averages
t = df_comb %>%
  group_by(month_f) %>% 
  summarize(indexP_lag1 = mean(indexP_lag1))

df_t = df_comb %>%
  mutate(vector_index = (vector_index * vi_sd) + vi_mu)
  

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# * SIM NO EF####
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
test1 = test %>% 
  left_join(t, by = "month_f") %>%
  modelr::add_predictions(modnb_all, 
                          type = "response") %>%
  ciTools::add_ci(modnb_all, names = c("pred_lwr", "pred_upr")) %>%
  mutate(year_grp = if_else(year_index == 17, "2019", "other")) %>%
  mutate(vector_index = (vector_index * vi_sd) + vi_mu)

write.csv(test1, "data_output/simulation_data_modnb_all.csv")

test_tl = test1 %>%
  group_by(st_grp, month_detected_july, vector_ind-ex) %>%
  summarise(total = sum(pred),
            total_upr = sum(pred_upr),
            total_lwr = sum(pred_lwr))

case_tl = df_t %>%
  group_by(st_grp, month_detected_july, year) %>%
  summarize(human_equine = sum(human_equine),
            vector_index = mean(vector_index),
            indexP_lag1 = mean(indexP_lag1)) %>%
  mutate(year_grp = if_else(year == 2019, "2019", "other"))

p_sim = ggplot() +
 # geom_line(data = test_tl, aes(x = vector_index, y = total), color = "red", size = 1) +
 # geom_ribbon(data = test_tl, aes(x = vector_index, y = total, ymin = total_lwr, ymax = total_upr, group = 1), alpha = 0.1) +
  geom_line(data = test1, aes(x = vector_index, y = pred, color = month_f), size = 1.05, alpha = 1) +
  geom_ribbon(data = test1, aes(x = vector_index, y = pred, ymin = pred_lwr, ymax = pred_upr, group = month_f, fill = month_f), alpha = 0.1) +
  geom_point(data = df_t, aes(x = vector_index, y =  human_equine, color = month_f, shape = year_grp, alpha = indexP_lag1, text = year), size = 3) +
  facet_grid(st_grp~month_detected_july+month_f) +
  coord_cartesian(ylim = c(0,15)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(caption = "IndexP lag 1 held constant using the average IndexP for that month from 2003 to 2019")

p_sim
ggsave(plot = p_sim, "figures/no_ef_mod_sim.svg", height = 5, width = 10)
ggsave(plot = p_sim, "figures/no_ef_mod_sim.png", height = 5, width = 10)

p_simly = plotly::ggplotly(p_sim)
p_simly


p_sim_tl = ggplot() +
   geom_line(data = test_tl, aes(x = vector_index, y = total), color = "red", size = 1) +
   geom_ribbon(data = test_tl, aes(x = vector_index, y = total, ymin = total_lwr, ymax = total_upr, group = 1, fill = "red"), alpha = 0.2) +
 # geom_line(data = test, aes(x = vector_index, y = pred, color = month_f), size = 1.05, alpha = 0.8) +
 # geom_ribbon(data = test, aes(x = vector_index, y = pred, ymin = pred_lwr, ymax = pred_upr, group = month_f, fill = month_f), alpha = 0.1) +
  geom_point(data = case_tl, aes(x = vector_index, y =  human_equine, shape = year_grp, alpha = indexP_lag1), size = 4) +
  facet_grid(st_grp~month_detected_july) +
  coord_cartesian(ylim = c(0,20)) +
  theme_classic() +
  labs(caption = "IndexP lag 1 is held constant for the predictions. \n It uses the average IndexP for that month from 2003 to 2019")

p_sim_tl

p_sim_tlly = plotly::ggplotly(p_sim_tl)
p_sim_tlly

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# * SIM EF####
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
test2 = test %>% 
  left_join(t, by = "month_f") %>%
  modelr::add_predictions(modnb_all_EF, 
                          type = "response") %>%
  ciTools::add_ci(modnb_all_EF, names = c("pred_lwr", "pred_upr")) %>%
  mutate(year_grp = if_else(year_index == 17, "2019", "other")) %>%
  mutate(vector_index = (vector_index * vi_sd) + vi_mu)



test_tl = test2 %>%
  group_by(st_grp, month_detected_july, vector_index) %>%
  summarise(total = sum(pred),
            total_upr = sum(pred_upr),
            total_lwr = sum(pred_lwr))

p_sim2 = ggplot() +
  # geom_line(data = test_tl, aes(x = vector_index, y = total), color = "red", size = 1) +
  # geom_ribbon(data = test_tl, aes(x = vector_index, y = total, ymin = total_lwr, ymax = total_upr, group = 1), alpha = 0.1) +
  geom_line(data = test2, aes(x = vector_index, y = pred, color = month_f), size = 1.05, alpha = 1) +
  geom_ribbon(data = test2, aes(x = vector_index, y = pred, ymin = pred_lwr, ymax = pred_upr, group = month_f, fill = month_f), alpha = 0.2) +
  geom_point(data = df_t, aes(x = vector_index, y =  human_equine, color = month_f, shape = year_grp, alpha = indexP_lag1, text = year), size = 3) +
  facet_grid(st_grp~month_detected_july+month_f) +
  coord_cartesian(ylim = c(0,15)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(caption = "IndexP lag 1 held constant using the average IndexP for that month from 2003 to 2019")

p_sim2

p_simly2 = plotly::ggplotly(p_sim2)
p_simly2


p_sim_tl2 = ggplot() +
  geom_line(data = test_tl, aes(x = vector_index, y = total), color = "red", size = 1) +
  geom_ribbon(data = test_tl, aes(x = vector_index, y = total, ymin = total_lwr, ymax = total_upr, group = 1, fill = "red"), alpha = 0.2) +
  # geom_line(data = test, aes(x = vector_index, y = pred, color = month_f), size = 1.05, alpha = 0.8) +
  # geom_ribbon(data = test, aes(x = vector_index, y = pred, ymin = pred_lwr, ymax = pred_upr, group = month_f, fill = month_f), alpha = 0.1) +
  geom_point(data = case_tl, aes(x = vector_index, y =  human_equine, shape = year_grp, alpha = indexP_lag1), size = 4) +
  facet_grid(st_grp~month_detected_july) +
  coord_cartesian(ylim = c(0,20)) +
  theme_classic() +
  labs(caption = "IndexP lag 1 held constant using the average IndexP for that month from 2003 to 2019 \n Red Line is the Total")

p_sim_tl2

p_sim_tlly2 = plotly::ggplotly(p_sim_tl2)
p_sim_tlly2


