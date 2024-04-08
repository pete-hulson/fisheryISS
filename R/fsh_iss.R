#' replicate fishery input sample size function
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data length frequency data
#' @param specimen_data age-length specimen data
#' @param catch_data abundance by length data 
#' @param r_t reader/tester ageing data 
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
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export fsh_iss
#'
#' @examples

fsh_iss <- function(iters = 1, 
                    lfreq_data, 
                    specimen_data, 
                    catch_data,
                    r_t, 
                    yrs = NULL,
                    bin = 1, 
                    join = NULL, 
                    exp_meth = NULL, 
                    boot_primes = FALSE, 
                    boot_lengths = FALSE, 
                    boot_ages = FALSE, 
                    al_var = FALSE, 
                    al_var_ann = FALSE, 
                    age_err = FALSE,
                    region = NULL, 
                    save_interm = FALSE,
                    save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  catch_data <- tidytable::as_tidytable(catch_data) 
  
  # get original age/length comps ----
  og <- smpl_fsh_comps(lfreq_data = lfreq_data, 
                       specimen_data = specimen_data, 
                       catch_data = catch_data, 
                       r_t = r_t, 
                       yrs = yrs, 
                       bin = bin, 
                       join = join, 
                       exp_meth = exp_meth,
                       boot_primes = FALSE, 
                       boot_lengths = FALSE, 
                       boot_ages = FALSE, 
                       al_var = FALSE, 
                       al_var_ann = FALSE, 
                       age_err = FALSE) 
  
  oga <- og$age
  ogl <- og$length

  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ smpl_fsh_comps(lfreq_data = lfreq_data, 
                                             specimen_data = specimen_data, 
                                             catch_data = catch_data, 
                                             r_t = r_t, 
                                             yrs = yrs, 
                                             bin = bin, 
                                             join = join, 
                                             exp_meth = exp_meth,
                                             boot_primes = boot_primes, 
                                             boot_lengths = boot_lengths, 
                                             boot_ages = boot_ages,
                                             al_var = al_var,
                                             al_var_ann = al_var_ann,
                                             age_err = age_err))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") -> .rss_age
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") -> .rss_length

  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  #   and compute average relative bias in pop'n estimates (avg relative bias across age or length)
  .rss_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species, comp_type)) %>% 
    tidytable::left_join(r_age %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::left_join(oga %>% 
                                                  tidytable::rename(og_acomp = 'acomp')) %>% 
                           tidytable::mutate(bias = (acomp - og_acomp)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species, comp_type))) %>% 
    tidytable::filter(iss > 0) -> iss_age
  
  .rss_length %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species, comp_type)) %>% 
    tidytable::left_join(r_length %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::left_join(ogl %>% 
                                                  tidytable::rename(og_lcomp = 'lcomp')) %>% 
                           tidytable::mutate(bias = (lcomp - og_lcomp)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species, comp_type))) %>% 
    tidytable::filter(iss > 0) -> iss_length

  # write results ----
  # input sample size
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln.csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag.csv")), delim = ",")
  # base age & length comp
  vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_acomp.csv")), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_lcomp.csv")), delim = ",")
  # if desired, write out bootstrapped age & length comps and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_length.csv"), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age.csv"), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln.csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag.csv")), delim = ",")
  }

}