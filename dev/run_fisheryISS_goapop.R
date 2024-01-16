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
#iters = 500
iters = 50

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# run for gulf of alaska pacific ocean perch ----

# pull data
query = FALSE
species = 301
year = 2023
area = "GOA"

if(isTRUE(query)){
  query_data(species, year, area)
}

specimen <- vroom::vroom(here::here("data", "fsh_specimen_data.txt"),
                         delim = ",",
                         col_type = c(join_key="c", haul_join="c", port_join="c"))

lfreq <- vroom::vroom(here::here("data", "fsh_length_data.txt"),
                      delim = ",",
                      col_type = c(haul_join="c", port_join="c"))

catch <- vroom::vroom(here::here("data", "fsh_obs_data.txt"),
                      delim = ",",
                      col_type = c(join_key = "c", haul_join = "c"))


# for development testing
# test marginal comps
fsh_iss(iters = iters, 
        lfreq_data = lfreq, 
        specimen_data = specimen, 
        catch_data = catch, 
        r_t = NULL, 
        yrs = 2015, 
        bin = 1, 
        join = 'both', 
        exp_meth = 'marginal', 
        boot_primes = TRUE, 
        boot_lengths = TRUE, 
        boot_ages = TRUE, 
        al_var = FALSE, 
        al_var_ann = FALSE, 
        age_err = FALSE, 
        region = area, 
        save = 'marg')

# test expanded comps
fsh_iss(iters = iters, 
        lfreq_data = lfreq, 
        specimen_data = specimen, 
        catch_data = catch, 
        r_t = NULL, 
        yrs = 2015, 
        bin = 1, 
        join = 'both', 
        exp_meth = 'expanded', 
        boot_primes = TRUE, 
        boot_lengths = TRUE, 
        boot_ages = TRUE, 
        al_var = FALSE, 
        al_var_ann = FALSE, 
        age_err = FALSE, 
        region = area, 
        save = 'exp')

# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

