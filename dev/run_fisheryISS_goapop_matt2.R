# example script to obtain age/length input sample size for production run

# load surveyISS library
#devtools::install_github("afsc-assessments/surveyISS", force = TRUE)
#library(surveyISS)

# load/source libraries/functions for testing
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)

# devtools::install_github("afsc-assessments/afscdata", force = TRUE)
library(afscdata)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# set number of desired bootstrap iterations (suggested here: 10 for testing, 500 for running)
# iters = 500
iters = 10

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# pull data ----
query = TRUE
species_code = "301"
species_group = "POPA"
year = 2023
area = "GOA"

if(isTRUE(query)){
  query_data_all(year, 
                 species_code, 
                 species_group,
                 area)
}


catch <- vroom::vroom(here::here("data", "fsh_catch_data.txt"),
                         delim = ",")

specimen <- vroom::vroom(here::here("data", "fsh_specimen_data.txt"),
                         delim = ",",
                         col_type = c(join_key="c", haul_join="c", port_join="c"))

lfreq <- vroom::vroom(here::here("data", "fsh_length_data.txt"),
                      delim = ",",
                      col_type = c(haul_join="c", port_join="c"))

obs_catch <- vroom::vroom(here::here("data", "fsh_obs_data.txt"),
                          delim = ",",
                          col_type = c(join_key = "c", haul_join = "c"))

read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species, species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age')

# set sim parameters
yrs = 2015 # >= year filter

# for testing
# lfreq_data = lfreq
# specimen_data = specimen
# catch_data = catch
# r_t = read_test
# yrs = 2015
# bin = 1
# join = 'haul'
# exp_meth = 'expanded'
# boot_primes = TRUE
# boot_lengths = TRUE
# boot_ages = TRUE
# al_var = TRUE
# al_var_ann = TRUE
# age_err = TRUE


# run at haul level ----
fsh_iss(iters = iters, 
        lfreq_data = lfreq, 
        specimen_data = specimen, 
        catch_data = obs_catch, 
        r_t = read_test, 
        yrs = yrs, 
        bin = 1, 
        join = 'haul', 
        exp_meth = 'expanded', 
        boot_primes = TRUE, 
        boot_lengths = TRUE, 
        boot_ages = TRUE, 
        al_var = TRUE, 
        al_var_ann = TRUE, 
        age_err = TRUE, 
        region = area, 
        save_interm = FALSE,
        save = 'haul')


# run at trip level ----

# run function to convert haul to trip level
trip_data <- haul_to_trip(catch, 
                          specimen, 
                          lfreq,
                          yrs) 

# get catch data reorg
catch_t <- trip_data$pete_c %>% 
  tidytable::rename(haul_join = 'trip_join',
                    extrapolated_number = 'ext_num',
                    species_key = 'species') %>% 
  tidytable::select(-num_hls, -cruise, -permit)

# get specimen data reorg
specimen_t <- trip_data$pete_s %>% 
  tidytable::rename(haul_join = 'trip_join')

# get length freq data reorg
lfreq_t <- trip_data$pete_l %>% 
  tidytable::rename(haul_join = 'trip_join')

# run bootstraps
fsh_iss(iters = iters, 
        lfreq_data = lfreq_t, 
        specimen_data = specimen_t, 
        catch_data = catch_t, 
        r_t = read_test, 
        yrs = yrs, 
        bin = 1, 
        join = 'haul', 
        exp_meth = 'expanded', 
        boot_primes = TRUE, 
        boot_lengths = TRUE, 
        boot_ages = TRUE, 
        al_var = TRUE, 
        al_var_ann = TRUE, 
        age_err = TRUE, 
        region = area, 
        save_interm = FALSE,
        save = 'trip')

# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

