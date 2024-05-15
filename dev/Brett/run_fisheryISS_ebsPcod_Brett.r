# Script for practicing the basics of fishery length composition expansion and ISS calculation for EBS P.cod
# 


# Questions
  # which weights should theoretically add up to 1? should there be any? the annual weights for the total length observations don't add to 1, they add to 1.9. I'm pretty sure these should add up to 1.





# Import Matt C. .RDS data query that should mimic what Steve's code does. I checked the code he included that produces the data file and it looks good. All that needed to be changed was "region" and "species" in the sql call file. The sql call file appears to be generic to be changed as needed for region and species.
# The required species and region inputs were from lines 49 and 59 here: https://github.com/afsc-assessments/EBS_PCOD/blob/dfb30720357630e95b2a8d2366afd346c1a7e8a1/2023_ASSESSMENT/Functions/R/MAIN_BS_PCOD.r
# Pick and choose code from Steve's functions to reproduce length expansion for easiest case:
  # Only look at BS onboard observer length data and catch for domestic fishery for combined sex. 
  # Do not include foreign observer data. This data comes from afsc.
  # Do not include age data. Steve doesn't use this data and the small section in the script playing with age data is commented out.
  # Do not include port data. i.e. only include onboard observer data. Port data comes from afsc and is complicated (see Steve's notes in script)
  # Do not include foreign catch. This data comes from afsc.
  # Do not include sex-separated analysis.

# Expansion ####
# This section uses relevant lines of code of the single function in
# https://github.com/afsc-assessments/EBS_PCOD/blob/dfb30720357630e95b2a8d2366afd346c1a7e8a1/2023_ASSESSMENT/Functions/R/LENGTH_BY_CATCH_BS_short.r

# That function is:
# LENGTH_BY_CATCH_short<-function(species=fsh_sp_str ,species_catch=fsh_sp_label, for_species_catch=for_sp_label,sp_area=fsh_sp_area ,ly=final_year, SEX=TRUE, PORT=TRUE ){
# With the following function arguments from MAIN_BS_PCOD.r:
# species=fsh_sp_str , -> 202
species = 202 # this is called down the code
# species_catch=fsh_sp_label, -> 'PCOD'
# for_species_catch=for_sp_label, -> 'PACIFIC COD'
# sp_area=fsh_sp_area , -> 'BS'
# ly=final_year, -> 2022
# SEX=TRUE, ->FALSE
SEX = TRUE # this is called in the expansion section conditional statement
# PORT=TRUE. ->FALSE


# Libraries from LENGTH_BY_CATCH_BS_short.r
library(data.table)
library(reshape2)
# library(rgdal)
library(dplyr)
library(lubridate)
library(swo)
library(vcdExtra)
library(misty)



# Import Matt C. .RDS data
Dspcomp.matt = readRDS("C:/Users/bstacy2/OneDrive - UW/UW Postdoc/Data/From Matt C/Dspcomp.RDS")
CATCH.matt = readRDS("C:/Users/bstacy2/OneDrive - UW/UW Postdoc/Data/From Matt C/CATCH.RDS")

# Observer Data ####
# Dspcomp I think is debriefed species composition
# plug into code from: https://github.com/afsc-assessments/EBS_PCOD/blob/dfb30720357630e95b2a8d2366afd346c1a7e8a1/2023_ASSESSMENT/Functions/R/LENGTH_BY_CATCH_BS_short.r
# This code is pulled out from a function so needs some code from other scripts to be run. That code is above.
Dspcomp=Dspcomp.matt %>%  # line 57. Edited this line to circumvent the direct data download
  dplyr::rename_all(toupper) %>% subset(EXTRAPOLATED_WEIGHT > 0 & NUMB > 0) # Rename all the variable names upper case. Then only keep data where EXTRAPOLATED_WEIGHT and NUMB are greater than zero.

Dspcomp <- subset(Dspcomp,EXTRAPOLATED_WEIGHT > 0) # line 60. Repeated code to above
Dspcomp<- subset(Dspcomp,NUMB>0)   # repeated code to above
Dspcomp$QUARTER<-trunc((as.numeric(Dspcomp$MONTH)/3)-0.3)+1 # add QUARTER that gives the quarter of the year the data is in. 1-3, 4-6, etc. The -0.3 and trunc() business is just to deal with when month=1

Tspcomp<-data.table(Dspcomp) # line 75. Edited this line to not include the foreign length data rbind

source("C:/Users/bstacy2/OneDrive - UW/UW Postdoc/GitHub Repos/EBS_PCOD/2023_ASSESSMENT/Functions/R/utils.r") # needed for below line
# Tspcomp$WED<-WED(Tspcomp$HDAY) # line 76
# begin testing #
# the above line of code did not work so I copied and pasted and adjusted the WED command sourced above in the utils.r file. 
x = Tspcomp$HDAY
WED <- function(x) {
  y <- data.table(
    weekday = base::weekdays(x), # create a column weekday associated with the name of the weekday in the date string HDAY. e.g. first entry is "Thursday" 
    wed = lubridate::ceiling_date(x, "week"), # create a column wed that is a date string of HDAY rounded up by week. this is the sunday associated with the week HDAY is in.
    plus = base::ifelse(weekdays(x) == "Sunday", 6, -1), # create a column plus that equals 6 if the weekday is Sunday and -1 if it is not. 
    YR = lubridate::year(x) # creates a column YR that is the year associated with HDAY
  )
  
  y$next_saturday <- y$wed + lubridate::days(y$plus) # creates a column next_saturday that uses plus to adjust the date wed to end up with the Saturday associated with the data. The name "next_saturday" is partially a misnomer because if the data falls on a Sunday, this value is the day before. 
  # y[YR < 1993, next_saturday := y$wed] 
  y[YR < 1993, next_saturday := wed] # Had to adjust above commented-out line because it was returning an error due to y$wed not being just wed. I suspect it was a syntax issue. uses data.table function := for "assignment". I think this line is saying: for data associated with years less than 1993, make the next_saturday column entries equal to wed. In other words, prior to 1993, next_saturday should actually be the sunday of that week. 
  y[YR != lubridate::year(next_saturday), next_saturday := as.Date(paste0(YR, "-12-31"))] # for data associated with years of HDAY not equal to the year nex_saturday is in, make next_saturday equal to the last day of the callendar year associated with HDAY year.
  
  # so really what this is doing is some sort of "next Saturday" identifier for each data entry, where the rules for defining nex Saturday differ slightly prior to 1993. 
  return(y$next_saturday) # return a WED column for Tspcomp that represents the next_saturday vector
}
# end testing #
Tspcomp$WED<-WED(Tspcomp$HDAY) # apply the WED function to add a new column to Tspcomp that contains the next_saturday date
Tspcomp$MONTH_WED<-month(Tspcomp$WED) # the month WED is in
Tspcomp$MONTH<-as.numeric(Tspcomp$MONTH) # change the existing MONTH column to be numeric instead of character

Tspcomp<-Tspcomp[EXTRAPOLATED_WEIGHT>0] # only keep observations associated with greater than zero extrapolated weight. There were no entries discarded
Tspcomp<-Tspcomp[NUMB>0] # don't know what NUMB is, but there were no entries discarded here either. These lines of code repeat the initial commands after the data was imported. They are repeated here because it is assumed the additional data from foreign sources was added. 

Tspcomp[is.na(VES_AKR_ADFG)]$VES_AKR_ADFG<- Tspcomp[is.na(VES_AKR_ADFG)]$PERMIT # there was a warning here but the next line is exactly the same, the code writer must have kept both and the second converts to factor then numeric to avoid the warning. 
Tspcomp[is.na(VES_AKR_ADFG)]$VES_AKR_ADFG<- as.numeric(as.factor(Tspcomp[is.na(VES_AKR_ADFG)]$PERMIT)) # replace where VES_AKR_ADFG is na with PERMIT. I think these are vessel or permit numbers.
Tspcomp[is.na(PERMIT)]$PERMIT<- Tspcomp[is.na(PERMIT)]$VES_AKR_ADFG # Similarly, replace where PERMIT is na with VES_AKR_ADFG. The result is that these vairables will only have NA's where they both share NA's

Tspcomp$AREA2<-trunc(Tspcomp$AREA/10)*10 # creates a coluimn AREA2 that consolidates AREA into three Areas 500, 510, 520
Tspcomp[AREA2==500]$AREA2<-510 # For some reason, rename AREA2==500 510?

OBS_DATA<-Tspcomp[,c("SPECIES","YEAR","GEAR","AREA2","AREA","MONTH","QUARTER","CRUISE","VES_AKR_ADFG","HAUL_JOIN","SEX","LENGTH","SUM_FREQUENCY","EXTRAPOLATED_WEIGHT","NUMB","SOURCE")] # this is called in the port data section to join it together if PORT=T. It equals false in our case

## Calculate average weights to calculate number of fish in landings data, which are only in weight

YAGM_AVWT=data.table(Tspcomp)[,list(YAGM_AVE_WT=sum(EXTRAPOLATED_WEIGHT)/sum(NUMB)),by=c('YEAR','AREA2','MONTH','GEAR')] #YAGM - year area gear month. These are all the average weights of a P.cod according to the specified variables
YAGM_AVWT$YEAR<-as.numeric(YAGM_AVWT$YEAR)
YAM_AVWT=data.table(Tspcomp)[,list(YAM_AVE_WT=sum(EXTRAPOLATED_WEIGHT)/sum(NUMB)),by=c('YEAR','AREA2','MONTH')]
YAM_AVWT$YEAR<-as.numeric(YAM_AVWT$YEAR)
YGM_AVWT=data.table(Tspcomp)[,list(YGM_AVE_WT=sum(EXTRAPOLATED_WEIGHT)/sum(NUMB)),by=c('YEAR','GEAR','MONTH')]
YGM_AVWT$YEAR<-as.numeric(YGM_AVWT$YEAR)
YGQ_AVWT=data.table(Tspcomp)[,list(YGQ_AVE_WT=sum(EXTRAPOLATED_WEIGHT)/sum(NUMB)),by=c('YEAR','GEAR','QUARTER')]
YGQ_AVWT$YEAR<-as.numeric(YGQ_AVWT$YEAR)
YAQ_AVWT=data.table(Tspcomp)[,list(YAQ_AVE_WT=sum(EXTRAPOLATED_WEIGHT)/sum(NUMB)),by=c('YEAR','AREA2','QUARTER')]
YAQ_AVWT$YEAR<-as.numeric(YAQ_AVWT$YEAR)
YG_AVWT=data.table(Tspcomp)[,list(YG_AVE_WT=sum(EXTRAPOLATED_WEIGHT)/sum(NUMB)),by=c('YEAR','GEAR')]
YG_AVWT$YEAR<-as.numeric(YG_AVWT$YEAR)

YAGM_AVWT$MONTH<-as.numeric(YAGM_AVWT$MONTH) # making them numeric because before they are characters
YGM_AVWT$MONTH<-as.numeric(YGM_AVWT$MONTH)
YGQ_AVWT$QUARTER<-as.numeric(YGQ_AVWT$QUARTER)
YAM_AVWT$MONTH<-as.numeric(YAM_AVWT$MONTH)
YAQ_AVWT$QUARTER<-as.numeric(YAQ_AVWT$QUARTER)


# Catch ####
# this section uses the catch in tons from a different source than the observer estimated catch above (extrapolated_weight). The extrapolated weight above is available at a haul level because it was estimated by the observer. The below weight I think is more accurate than adding up all the estimated weights from above but it is at a much coarser resolution.


## pull blend information on domestic catch

CATCH=CATCH.matt %>%  # line 350. Edited this line to circumvent the direct data download
  dplyr::rename_all(toupper)


##Combine foriegn and domestic catch    
# CATCHT<-data.table(rbind(FCATCH,CATCH)) # original
CATCHT<-data.table(rbind(CATCH)) # edited out FCATCH from rbind. CATCHT is catch in tons


CATCHT<-CATCHT[TONS>0] # don't use catch data with no catch. There were 12 of these observations and I'm not sure if it means fishing occurred but they didn't catch anything OR no fishing occurred. 
CATCHT$AREA<-as.numeric(CATCHT$AREA) # same stuff as for observer data above

CATCHT$MONTH<-as.numeric(CATCHT$MONTH_WED)
CATCHT$QUARTER<-trunc((CATCHT$MONTH)/3-0.3)+1


CATCHT$AREA2<-trunc(CATCHT$AREA/10)*10
CATCHT[AREA2==500]$AREA2<-510

CATCHT<-CATCHT[GEAR%in%c("POT","TRW","HAL")] # exclude other gear types, there were 4 others 
CATCHT$YEAR<-as.numeric(CATCHT$YEAR)

## use average weights to calculate number of fish caught

CATCHT2<-data.table(CATCHT) 
CATCHT2<-merge(CATCHT2,YAGM_AVWT,all.x=T,by=c('YEAR','AREA2','MONTH','GEAR'))  ## first is year, area, gear, month
CATCHT2<-merge(CATCHT2,YGM_AVWT,all.x=T,by=c('YEAR','GEAR','MONTH'))          ## second choice is year, gear, month
CATCHT2<-merge(CATCHT2,YGQ_AVWT,all.x=T,by=c('YEAR','GEAR','QUARTER'))        ## third choice is year, gear, quarter
CATCHT2<-merge(CATCHT2,YAM_AVWT,all.x=T,by=c('YEAR','AREA2','MONTH'))          ## fourth choice by year, area, month
CATCHT2<-merge(CATCHT2,YG_AVWT,all.x=T,by=c('YEAR','GEAR'))                   ## fill in any more missing by year and gear
CATCHT2$AVEWT<-CATCHT2$YAGM_AVE_WT # copy YAGM to a new variable AVEWT, remember this is the observer based average weights
CATCHT2[is.na(AVEWT)]$AVEWT<-CATCHT2[is.na(AVEWT)]$YGM_AVE_WT # *Imputing steps! Where AVEWT (YAGM) is NA, fill it in with the values for YGM. In other words, if there is no data for an AREA2 for a particular year, gear, month, fill it in with the average from the year, gear, month (averaged over areas). 
CATCHT2[is.na(AVEWT)]$AVEWT<-CATCHT2[is.na(AVEWT)]$YGQ_AVE_WT # If there are still NAs (i.e., for particular months), fill them in with data averaged over months to the quarter year resolution.
CATCHT2[is.na(AVEWT)]$AVEWT<-CATCHT2[is.na(AVEWT)]$YAM_AVE_WT # If there are still NAs (i.e., for particular gears), fill them in with data averaged over gears.
CATCHT2[is.na(AVEWT)]$AVEWT<-CATCHT2[is.na(AVEWT)]$YG_AVE_WT # If there are still NAs (i.e., for particular months), fill them in with data averaged over all the months. 

CATCHT3<-data.table(data.frame(CATCHT2)[,names(CATCHT)]) # new object CATCHT3 only keep the names from catcht. 
CATCHT3$NUMBER<-CATCHT3$TONS/(CATCHT2$AVEWT/1000)        ## estimate the number of fish caught using average weights # new variable NUMBER calculated by dividing the weight from the catch download by the average weight per fish by the observer data. 
CATCHT3$SPECIES=species
CATCHT3$SPECIES=as.numeric(CATCHT3$SPECIES)
CATCHT3<-CATCHT3[!is.na(NUMBER)] # did not exclude any data

CATCHT4<-CATCHT3[,list(YAGM_TONS=sum(TONS),YAGM_TNUM=sum(NUMBER)),by=c("SPECIES","YEAR","GEAR","AREA2","MONTH")] # new variables YAGM_TONS and YAGM_TNUM. They are summed tons and number of fish over YAGM

xt_YAG<-CATCHT4[,list(YAG_TONS=sum(YAGM_TONS),YAG_TNUM=sum(YAGM_TNUM)),by=c("AREA2","GEAR","YEAR")] # Similar process to above where things are merged
CATCHT4<-merge(CATCHT4,xt_YAG,by=c("YEAR","AREA2","GEAR"), all.x=T)
xt_YG<-CATCHT4[,list(YG_TONS=sum(YAGM_TONS),YG_TNUM=sum(YAGM_TNUM)),by=c("GEAR","YEAR")]
CATCHT4<-merge(CATCHT4,xt_YG,by=c("YEAR","GEAR"), all.x=T)
xt_Y<-CATCHT4[,list(Y_TONS=sum(YAGM_TONS),Y_TNUM=sum(YAGM_TNUM)),by=c("YEAR")] # this gives the total weight and number of fish caught per year.
CATCHT4<-merge(CATCHT4,xt_Y,by=c("YEAR"), all.x=T)


# condense a Length data.table from the observer data that is a bit more organized to be used later
ALL_DATA <- OBS_DATA # pulled from line 319, which is part of the PORT conditional code (not run in this example)
Length<-ALL_DATA[GEAR%in%c("POT","TRW","HAL")] # just keep the relevant gear types
Length$YEAR<-as.numeric(Length$YEAR)
Length$MONTH<-as.numeric(Length$MONTH)

Length$YAGMH_STONS<-Length$EXTRAPOLATED_WEIGHT/1000 # convert to tons. haul level (YAGMH)
Length$YAGMH_SNUM<-Length$NUMB # number of fish in the haul. this is just a renaming of NUMB



# Expansion ####



# if(!SEX){
  Length<-Length[,list(SUM_FREQUENCY=sum(SUM_FREQUENCY)),by=c("SPECIES","YEAR","AREA2","GEAR","MONTH","CRUISE","VES_AKR_ADFG","HAUL_JOIN","LENGTH","YAGMH_STONS","YAGMH_SNUM")] # sum the frequency of each observed length at haul level, summed over the excluded variables from this list: SEX. Also get rid of variables no longer needed that are the same for each flattened observation: EXTRAPOLATED_WEIGHT, SOURCE, AREA, QUARTER, NUMB
  L_YAGMH<-Length[,list(YAGMH_SFREQ=sum(SUM_FREQUENCY)),by=c("CRUISE","VES_AKR_ADFG","HAUL_JOIN")] # Sum again, aggregating over year, gear, month, cruise, ves_akr_adfg, length, yagmh_stons, yagmh_snum. This gives the amount to fish sampled for length for each vessel, cruise and haul.
  Length<-merge(Length,L_YAGMH,by=c("CRUISE","VES_AKR_ADFG","HAUL_JOIN"), all.x=T) # join the amount of fish sampled calculated previous in the big table Length. this spreads the aggregated number of fish sampled over their respective cruise, ves_akr_adfg, and haul_join observations.
  L_YAGM<-Length[,list(YAGM_STONS=sum(YAGMH_STONS),YAGM_SNUM=sum(YAGMH_SNUM),YAGM_SFREQ=sum(SUM_FREQUENCY)),by=c("AREA2","GEAR","MONTH","YEAR")] # Sum STONS, SNUM, Frequency over haul_join and vessel
  Length<-merge(Length,L_YAGM,by=c("YEAR","AREA2","GEAR","MONTH"), all.x=T) # join this to Length, spreading the sums over their observations.
  L_YAG<-Length[,list(YAG_STONS=sum(YAGMH_STONS),YAG_SNUM=sum(YAGMH_SNUM),YAG_SFREQ=sum(SUM_FREQUENCY)),by=c("AREA2","GEAR","YEAR")] # same as previous but aggregating over months
  Length<-merge(Length,L_YAG,by=c("YEAR","AREA2","GEAR"), all.x=T) # join it in
  L_YG<-Length[,list(YG_STONS=sum(YAGMH_STONS),YG_SNUM=sum(YAGMH_SNUM),YG_SFREQ=sum(SUM_FREQUENCY)),by=c("GEAR","YEAR")] # same as previous but aggregating over area2
  Length<-merge(Length,L_YG,by=c("YEAR","GEAR"), all.x=T) # join it in
  L_Y<-Length[,list(Y_STONS=sum(YAGMH_STONS),Y_SNUM=sum(YAGMH_SNUM),Y_SFREQ=sum(SUM_FREQUENCY)),by=c("YEAR")] # same as previous but aggregating over gear. This gives a total weight, number, and frequency of each length observation for every year.
  Length<-merge(Length,L_Y,by=c("YEAR"), all.x=T) # join it in
  
  x<-merge(Length,CATCHT4,by=c("SPECIES","YEAR","AREA2","GEAR","MONTH"),all.x=T) # merge the observer length observations with catch4: aggregations with the same levels as Length. 
  
  y2<-x[!is.na(YAGM_TNUM)] # exclude ~175,000 observations with NA in YAGM_TNUM. i.e., where there is no total number of fish for the year, area, gear, month aggregation from the fishery (CATCH4). 
  
  
  y2$WEIGHT1<-y2$SUM_FREQUENCY/y2$YAGMH_SFREQ  ## individual weight of length in length sample at the haul level. # weight here refers to the leverage weight the observation will have in later analysis I think. i.e., the proportion of lengths l observed to the total number of lengths for a year, area, gear, month, haul. 
  y2$WEIGHT2<-y2$YAGMH_SNUM/y2$YAGM_SNUM        ## weight of haul numbers for the year, area, gear, month strata (proportion of total observer samples for strata). # i.e., each haul will have a different weight proportional to the number of samples taken on that haul relative to the number taken across all hauls
  y2$WEIGHT3<-y2$YAGM_TNUM/y2$YG_TNUM           ## weight of total number of catch by year/area/gear/month to year/gear catch for 3 fishery models. # i.e., each month will have a different weight for combined gear types: the proportion (contribution) of each month catch numbers to the annual catch.
  y2$WEIGHT4<-y2$YAGM_TNUM/y2$Y_TNUM             ## weight of total number of catch by year/area/gear/month to year catch for single fishery models. # i.e., the proportion of each area, gear, month catch to yearly catch. 
  
  
  y3<- y2[,c("YEAR","GEAR","AREA2","MONTH","CRUISE",
             "HAUL_JOIN", "LENGTH", "SUM_FREQUENCY", "YAGMH_SNUM", 
             "YAGMH_SFREQ","YAGM_SFREQ", "YG_SFREQ","Y_SFREQ","YAGM_TNUM","YG_TNUM","Y_TNUM","YAGMH_SNUM",
             "YAGM_SNUM","YG_SNUM","YG_SNUM","Y_SNUM","WEIGHT1","WEIGHT2","WEIGHT3","WEIGHT4")]     # get rid of some unneeded variables
  
  y3$WEIGHTX<-y3$WEIGHT1*y3$WEIGHT2*y3$WEIGHT4   ## weight of individual length sample for single fishery model. # multiply individual observation weight at haul level by weight of the haul by the weight of the year/area/gear/month. So this should give the weight each observation has scaled by the haul weight and month/gear/year/area weight 
  y3$WEIGHTX_GEAR<-y3$WEIGHT1*y3$WEIGHT2*y3$WEIGHT3 # similar to previous but including gear weights
  y3$STRATA<-paste(y3$AREA2,y3$MONTH,y3$GEAR,sep="_")  ## simple strata of area, month, gear for clustered bootstrap. # create a unique identifier (strata) for area, month, and gear. 
  y3$STRATA1<-as.numeric(as.factor(y3$STRATA)) # forces an index to each unique factor (strata)
  y3$HAUL_JOIN1<-as.numeric(as.factor(y3$HAUL_JOIN)) # same as previous for haul_join
  
  
  y4<-y3[YAGM_SFREQ>30][,list(WEIGHT=sum(WEIGHTX)),by=c("LENGTH","YEAR")]  ## setting minumal sample size to 30 lengths for Year, area, gear, month strata. # this reduced nrow by about 1000. this line also creates a new variable WEIGHT, which is the sum of WEIGHTX across length and year. i.e., the weight of each length for every year. 
  y4.1<-y3[YAGM_SFREQ>30][,list(WEIGHT_GEAR=sum(WEIGHTX_GEAR)),by=c("LENGTH","GEAR","YEAR")]  ## setting minumal sample size to 30 lengths for Year, area, gear, month strata. to be used in the multiple gear fisheries section below.
  
  
  y5<-y4[,list(TWEIGHT=sum(WEIGHT)),by=c("YEAR")] # TWEIGHT is I think the total weight summed across length bins for every year. sum(TWEIGHT) = 1.9. Shouldn't this sum to 1??
  y5=merge(y4,y5) # merge the individual length weights with their respective year weights.
  y5$FREQ<-y5$WEIGHT/y5$TWEIGHT # the FREQuency, or relative weight, between length bins to the total annual weight. 
  y6<-y5[,-c("WEIGHT","TWEIGHT")] # saving a version with just FREQ
  
  grid<-data.table(expand.grid(YEAR=unique(y5$YEAR),LENGTH=1:max(y5$LENGTH))) # make a grid for every year, have a length bin by centemeter from 1 to max length
  y7<-merge(grid,y6,all.x=TRUE,by=c("YEAR","LENGTH")) # merge the grid with y6, which is the expanded frequency of proportions for each length bin observed for each year.
  y7[is.na(FREQ)]$FREQ <-0                                  ## this is the proportion at length for a single fishery. # The NAs are where there were no observations for that length bin in that year so call them zero. This is what must be input into the assessment model.
  
  ## calculations for multiple gear fisheries. # same as above but with gear in the grid too.
  
  y5.1<-y4.1[,list(TWEIGHT=sum(WEIGHT_GEAR)),by=c("GEAR","YEAR")] 
  y5.1=merge(y4.1,y5.1)
  y5.1$FREQ<-y5.1$WEIGHT_GEAR/y5.1$TWEIGHT
  y6.1<-y5.1[,-c("WEIGHT_GEAR","TWEIGHT")]
  
  grid<-data.table(expand.grid(YEAR=unique(y6.1$YEAR),GEAR=unique(y6.1$GEAR),LENGTH=1:max(y6.1$LENGTH)))
  y7.1<-merge(grid,y6.1,all.x=TRUE,by=c("YEAR","GEAR","LENGTH"))
  y7.1[is.na(FREQ)]$FREQ <-0   ## this is the proportion at length by gear
  
  # this and below must be for the bootstrap attempt
  y3.1<- y3[,c("YEAR","HAUL_JOIN1","STRATA1","LENGTH", "SUM_FREQUENCY", "YAGMH_SFREQ", 
               "YAGMH_SNUM","YAGM_SFREQ", "YG_SFREQ","Y_SFREQ","YAGM_TNUM","YG_TNUM","Y_TNUM",
               "YAGM_SNUM","YG_SNUM","Y_SNUM")] 
  
  y3.1<-y3.1[YAGM_SFREQ>30] 
  
  
  
  years<-unique(y3.1$YEAR)
  
  ESS<-vector("list",length=length(years))
  
  for(i in 1:length(years)){ # looks like this is counting the number of observed lengths (N), number of unique hauls, and unique strata (unique area/month/gear combinations) as estimates of ISS
    data<-y3.1[YEAR==years[i]]
    N = sum(data$SUM_FREQUENCY)
    H = length(unique(data$HAUL_JOIN1))
    S = length(unique(data$STRATA1))
    
    #d_EXP<- vcdExtra::expand.dft(data, freq="SUM_FREQUENCY")
    #cm   <- fishmethods::clus.mean(popchar =d_EXP$LENGTH , cluster = d_EXP$STRATA1, clustotal = round(d_EXP$YAGM_TNUM), rho=clus.rho(popchar =d_EXP$LENGTH , cluster = d_EXP$STRATA1)$icc[1],nboot = 10000)
    
    ESS[[i]] <-data.table(YEAR=years[i],BootESS=NA,df=NA,NSAMP=N,NHAUL=H,NSTRATA=S)
    print(years[i])
  }
  
  
  y3.2<- y3[,c("YEAR","GEAR","HAUL_JOIN1","STRATA1","LENGTH", "SUM_FREQUENCY", "YAGMH_SFREQ", 
               "YAGM_SFREQ", "YG_SFREQ","YAGM_TNUM","YG_TNUM","YAGMH_SNUM","YAGM_SNUM","YG_SNUM")] 
  
  y3.2<-y3.2[YAGM_SFREQ>30] 
  
  years<-unique(y3.2$YEAR)
  gears=unique(y3.2$GEAR)
  
  b=1
  ESS.1<-vector("list",length=length(years)*length(gears))
  for(j in 1:length(gears)){ # same as previous loop but for the three gear categories separately
    for(i in 1:length(years)){
      
      data<-y3.2[YEAR==years[i]&GEAR==gears[[j]]]
      if(nrow(data>0)){
        N = sum(data$SUM_FREQUENCY)
        H = length(unique(data$HAUL_JOIN1))
        S = length(unique(data$STRATA1))
        
        
        #d_EXP<-vcdExtra::expand.dft(data, freq="SUM_FREQUENCY")
        
        #if(S>1){
        #        cm=fishmethods::clus.mean(popchar =d_EXP$LENGTH , cluster = d_EXP$STRATA1, clustotal = round(d_EXP$YAGM_SNUM), rho=clus.rho(popchar =d_EXP$LENGTH , cluster = d_EXP$STRATA1)$icc[1],nboot = 10000)
        #        } else {cm <- rep(NA,12)
        #        print("Only one strata for this year and gear combination")} 
      }
      
      ESS.1[[b]] <-data.table(YEAR=years[i],GEAR=gears[[j]],BootESS=NA,df=NA,NSAMP=N,NHAUL=H,NSTRATA=S)
      b <- b+1
      print(paste0(gears[[j]]," in ",years[i]))
    }
  }
  
  
  ESS=do.call(rbind, ESS)
  ESS.1=do.call(rbind, ESS.1)
  LF1<-merge(y7,ESS,by="YEAR")
  LF1.1<-merge(y7.1,ESS.1,by=c("YEAR","GEAR"))
  LF1.1<-LF1.1[NSAMP>0]
  LF<-list(LF1,LF1.1)
# }




