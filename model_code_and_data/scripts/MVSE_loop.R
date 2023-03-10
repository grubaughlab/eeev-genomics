#!/usr/bin/env Rscript

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_unload("all")
pacman::p_load("pbapply", "scales", "genlasso", "stringr", "optparse")


#CREATE OPTION TO DETERMINE GEOGRAPHICAL RESOLUTION CALCULATING INDEX P
.option_list = list(
  make_option(c("-d", "--directory"), type="character",  
              default= "centroid/",
              help = "determine whether to use either the region or state as the grouping variable. Options are region, state, state_year", 
              metavar="character")
                  )

.opt_parser = OptionParser(option_list=.option_list)
opt = parse_args(.opt_parser)


#if mvse_ready.csv files don't exist then run the MSVE.R script

#set the directory with the specific geographic resolution
work_dir = paste0("data_mid/", opt$dir)

if(length(list.files(path = work_dir, pattern = "mvse_ready.csv")) == 0 ) {
 source("scripts/batch_weather_station_prep.R")
 print("no mvse_ready.csv files were in data_mid directory so running batch_weather_station_prep.R script")
} 

#"load" in MVSE package
source('scripts/MVSE.R')


fn = list.files(path = work_dir, pattern = "mvse_ready.csv")
fn2 = paste0(work_dir, fn)
fn3 = stringr::str_replace(fn, work_dir, "")
fn3 = stringr::str_replace(fn3, "_mvse_ready.csv", "")

for(i in seq_along(fn2)) {
  
  setEmpiricalClimateSeries(fn2[[i]])
  setOutputFilePathAndTag(paste0(work_dir,fn3[i]))
  
  #climate independent variables
  # bird lifespan
  setHumanLifeExpPrior(pmean=12, psd=2, pdist='gamma')    
  
  # transmission p from infected bird to mosquito per bit
  setHumanMosqTransProbPrior(pmean=0.5, psd=0.01, pdist='gamma') 
  #bird infectious period
  setHumanInfPerPrior(pmean=6, psd=1, pdist='gamma') 
  # bird incubation period
  setHumanIncPerPrior(pmean=1.5, psd=1, pdist='gamma') 
  
  #climate dependent variables
  
  #mosquito lifespan
  setMosqLifeExpPrior(pmean=10, psd=2, pdist='gamma') 
  
  # Extrinstic incubation period PREVIOUS
  setMosqIncPerPrior(pmean=4, psd=1, pdist='gamma')  
  
  # Mosquito biting Rate Prior
  setMosqBitingPrior(pmean=0.14, psd=0.02, pdist='gamma')  
  
  #setMosqBitingPrior(pmean=0.02, psd=0.01, pdist='gamma')   # daily biting rate PROPOSED from paper but seems low
  #setMosqIncPerPrior(pmean=5.1, psd=1.3, pdist='gamma')  # Extrinstic incubation period PROPOSED

  #p of transmission from infected mosquito to bird per bite  PREVIOUS
  
 
  estimateEcoCoefficients(nMCMC=100000, bMCMC=0.5, cRho=1, cEta=1, gauJump=0.75)
  
  simulateEmpiricalIndexP(nSample=1000, smoothing=c(7,15,30,60))
  
  exportEmpiricalIndexP()
  
  #extra
  exportEcoCoefficients()
  getTempEffAquaDev()
  getTempEffAquaLifeSpan()
  getTempEffProbTransVH()
  
  
}





