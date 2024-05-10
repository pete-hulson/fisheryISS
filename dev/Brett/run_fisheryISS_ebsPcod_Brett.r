# Script for practicing the basics of fishery length composition expansion and ISS calculation for EBS P.cod
# 
# Import Matt C. .RDS data query that should mimic what Steve's code does. I checked the code he included that produces the data file and it looks good. All that needed to be changed was "region" and "species" in the sql call file. The sql call file appears to be generic to be changed as needed for region and species.
# The required species and region inputs were from lines 49 and 59 here: https://github.com/afsc-assessments/EBS_PCOD/blob/dfb30720357630e95b2a8d2366afd346c1a7e8a1/2023_ASSESSMENT/Functions/R/MAIN_BS_PCOD.r
# Pick and choose code from Steve's functions to reproduce length expansion for easiest case

# Libraries from: https://github.com/afsc-assessments/EBS_PCOD/blob/dfb30720357630e95b2a8d2366afd346c1a7e8a1/2023_ASSESSMENT/Functions/R/LENGTH_BY_CATCH_BS_short.r
library(data.table)
library(reshape2)
# library(rgdal)
library(dplyr)
library(lubridate)
library(swo)
library(vcdExtra)
library(misty)

# code needed to run below code


# Import Matt C. .RDS data
Dspcomp.matt = readRDS("C:/Users/bstacy2/OneDrive - UW/UW Postdoc/Data/From Matt C/Dspcomp.RDS")

# plug into code from: https://github.com/afsc-assessments/EBS_PCOD/blob/dfb30720357630e95b2a8d2366afd346c1a7e8a1/2023_ASSESSMENT/Functions/R/LENGTH_BY_CATCH_BS_short.r
# This code is pulled out from a function so needs some code from other scripts to be run. That code is above.
Dspcomp=Dspcomp.matt %>% 
  dplyr::rename_all(toupper) %>% subset(EXTRAPOLATED_WEIGHT > 0 & NUMB > 0)

Dspcomp <- subset(Dspcomp,EXTRAPOLATED_WEIGHT > 0)
Dspcomp<- subset(Dspcomp,NUMB>0)   
Dspcomp$QUARTER<-trunc((as.numeric(Dspcomp$MONTH)/3)-0.3)+1






