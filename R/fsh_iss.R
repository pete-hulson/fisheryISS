#' replicate fishery input sample size function
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param catch_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param join use 'haul' only, or 'both' haul and port samples for age/length (default NULL)
#' @param exp_meth compute age/length comps as 'marginal' or 'expanded' (default NULL)
#' @param boot_primes resample primary sampling unit w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save name to save output
#'
#' @return
#' @export fsh_iss
#'
#' @examples

fsh_iss <- function(iters = 1, lfreq_data, specimen_data, catch_data, r_t, yrs = NULL, bin = 1, 
                    join, exp_meth, boot_primes = FALSE, boot_lengths = FALSE, boot_ages = FALSE, 
                    al_var = FALSE, al_var_ann = FALSE, age_err = FALSE, region = NULL, save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  # create storage location
  if(!dir.exists(here::here('output', region, 'dev')) & save != 'prod'){
    dir.create(here::here('output', region, 'dev'), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  catch_data <- tidytable::as_tidytable(catch_data) 
  
  # get original age/length pop'n values
  og <- smpl_fsh_comps(lfreq_data, 
                       specimen_data, 
                       catch_data, 
                       r_t, 
                       yrs, 
                       bin, 
                       join, 
                       exp_meth,
                       boot_primes = FALSE, 
                       boot_lengths = FALSE, 
                       boot_ages = FALSE, 
                       al_var = FALSE, 
                       al_var_ann = FALSE, 
                       age_err = FALSE) 
  
  oga <- og$age %>% 
    select(-type)
  ogl <- og$length %>% 
    select(-type)

  # run resampling iterations
  rr <- purrr::map(1:iters, ~ smpl_fsh_comps(lfreq_data, 
                                             specimen_data, 
                                             catch_data, 
                                             r_t, 
                                             yrs, 
                                             bin, 
                                             join, 
                                             exp_meth,
                                             boot_primes = boot_primes, 
                                             boot_lengths = boot_lengths, 
                                             boot_ages = boot_ages,
                                             al_var = al_var,
                                             al_var_ann = al_var_ann,
                                             age_err = age_err))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # compute effective sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") -> .ess_age
  r_length %>%
    tidytable::map(., ~ess_size(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") -> .ess_size

  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  .ess_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(ess, na.rm=T),
                         .by = c(year, species, comp_type, type)) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) -> iss_age
  
  .ess_age %>%
    tidytable::pivot_wider(names_from = type, values_from = ess) -> .ess_age1
  
  .ess_size %>% 
    tidytable::summarise(iss = psych::harmonic.mean(ess, na.rm=T),
                         .by = c(year, species, comp_type, type)) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) -> iss_size
  
  .ess_size %>%
    tidytable::pivot_wider(names_from = type, values_from = ess) -> .ess_size1
  
  # write input sample size results
  if(save == 'prod'){
    vroom::vroom_write(.ess_size1, here::here("output", region, paste0(save, "_iter_ess_sz.csv")), delim = ",")
    vroom::vroom_write(.ess_age1, here::here("output", region, paste0(save, "_iter_ess_ag.csv")), delim = ",")
    vroom::vroom_write(iss_size, here::here("output", region, paste0(save, "_iss_sz.csv")), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag.csv")), delim = ",")
  } else if(save != 'prod'){
    vroom::vroom_write(.ess_size1, here::here("output", region, 'dev', paste0(save, "_iter_ess_sz.csv")), delim = ",")
    vroom::vroom_write(.ess_age1, here::here("output", region, 'dev', paste0(save, "_iter_ess_ag.csv")), delim = ",")
    vroom::vroom_write(iss_size, here::here("output", region, 'dev', paste0(save, "_iss_sz.csv")), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, 'dev', paste0(save, "_iss_ag.csv")), delim = ",")
  }
}