#' primary fishery age/length bootstrap function to compute comps
#'
#' @param lfreq_data length frequency data
#' @param specimen_data age-length specimen data
#' @param catch_data abundance by length data 
#' @param r_t reader/tester ageing data 
#' @param yrs age filter returns years >= (default = NULL)
#' @param bin length bin size (default = 1 cm)
#' @param join use 'haul' only, or 'both' haul and port samples for age/length (default NULL)
#' @param exp_meth compute age/length comps as 'marginal' or 'expanded' (default NULL)
#' @param boot_primes switch for resampling primary sampling unit, like hauls or trip (default = FALSE)
#' @param boot_lengths switch for resampling lengths (default = FALSE)
#' @param boot_ages switch for resampling ages (default = FALSE)
#' @param al_var switch for including age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err switch for including ageing error (default = FALSE)
#'
#' @return
#' @export smpl_fsh_comps
#'
#' @examples
#' 

smpl_fsh_comps <- function(lfreq_data, specimen_data, catch_data, r_t, yrs, bin, join, exp_meth,
                       boot_primes, boot_lengths, boot_ages, al_var, al_var_ann, age_err) {
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # first pass of filtering and combine port/haul joins to single join value ('prime_join')
  data.table::setDT(catch_data) %>%
      tidytable::filter(year >= yrs) %>% 
      tidytable::mutate(prime_join = haul_join,
                        species = species_key) -> .catch
  
  data.table::setDT(lfreq_data) %>%
    tidytable::filter(year >= yrs) -> lfreq1
  
  lfreq1 %>% 
    tidytable::bind_cols(lfreq1 %>% 
                           tidytable::select(haul_join) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::rename(prime_join = 'haul_join') %>% 
                           tidytable::bind_rows(lfreq1 %>% 
                                                  tidytable::select(port_join) %>% 
                                                  tidytable::drop_na() %>% 
                                                  tidytable::rename(prime_join = 'port_join'))) -> .lfreq
  
  .lfreq %>% 
    tidytable::uncount(frequency) -> .lfreq_un
  
  data.table::setDT(specimen_data) %>%
    tidytable::filter(year >= yrs) -> agedat1
  
  agedat1 %>% 
    tidytable::bind_cols(agedat1 %>% 
                           tidytable::select(haul_join) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::rename(prime_join = 'haul_join') %>% 
                           tidytable::bind_rows(agedat1 %>% 
                                                  tidytable::select(port_join) %>% 
                                                  tidytable::drop_na() %>% 
                                                  tidytable::rename(prime_join = 'port_join')))  -> .agedat

  # randomize primary sampling unit (hauls/trips) ----  
  if(isTRUE(boot_primes)) {
    
    boot_prime(.lfreq) -> .hls_len
    
    boot_prime(.agedat) -> .hls_age

    .hls_len %>% 
      tidytable::left_join(.lfreq_un) -> .lfreq_un_hl
    
    .hls_age %>% 
      tidytable::left_join(.agedat)  -> .agedat_hl
    
  } else{
    
    .lfreq_un_hl <- .lfreq_un
    .agedat_hl <- .agedat
    
  }
  
  # randomize lengths ----
  if(isTRUE(boot_lengths)) {
    boot_length(.lfreq_un_hl) %>% 
      tidytable::mutate(type = 'base') -> .lfreq_un_hlen
  } else{
    .lfreq_un_hl %>% 
      tidytable::mutate(type = 'base') -> .lfreq_un_hlen
  }
  
  # bin length data ----
  .lfreq_un_hlen %>% 
    tidytable::mutate(length = bin * ceiling(length / bin)) -> .lfreq_un_hlen_bin
  
  # length comp ----
  
  # clean data and determine if haul, or both haul and port data to be used
  if(join == 'haul'){
    .lfreq_un_hlen_bin %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::drop_na(haul_join) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) %>% 
      tidytable::summarise(frequency = tidytable::n(), .by = c(year, prime_join, type, species, sex, length)) -> .lfreq_samp
  }
  if(join == 'both'){
    .lfreq_un_hlen_bin %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) %>% 
      tidytable::summarise(frequency = tidytable::n(), .by = c(year, prime_join, type, species, sex, length)) -> .lfreq_samp
  }
  

  lcomp(lfreq = .lfreq_samp, catch = .catch, exp_meth = exp_meth) -> .lcomp

  # randomize age ----
  if(isTRUE(boot_ages)) {
    boot_age(.agedat_hl) %>% 
      tidytable::mutate(type = 'base') -> .agedat_hlage
  } else{
    .agedat_hl %>% 
      tidytable::mutate(type = 'base') -> .agedat_hlage
  }
  
  # # add age-length variability ----
  # if(isTRUE(al_var)) {
  #   al_variab(.agedat_hl, annual = al_var_ann)  %>%
  #     tidytable::mutate(type = 'al') -> .agedat_al
  # }
  # 
  # # add ageing error ----
  # if(isTRUE(age_err)) {
  #   age_error(.agedat_hl, r_t)  %>%
  #     tidytable::mutate(type = 'ae') -> .agedat_ae
  # }
  # 
  # # with age-length and ageing error ----
  # if(isTRUE(al_var) & isTRUE(age_err)) {
  #   age_error(.agedat_al, r_t)  %>%
  #     tidytable::mutate(type = 'ae_al') %>%
  #     tidytable::bind_rows(.agedat_al) %>%
  #     tidytable::bind_rows(.agedat_ae) %>%
  #     tidytable::bind_rows(.agedat_hlage) -> .agedat_hlage
  # } else if(isTRUE(al_var) & !isTRUE(age_err)){
  #   .agedat_hlage %>%
  #     tidytable::bind_rows(.agedat_al) -> .agedat_hlage
  # } else if(!isTRUE(al_var) & isTRUE(age_err)){
  #   .agedat_hlage %>%
  #     tidytable::bind_rows(.agedat_ae) -> .agedat_hlage
  # }
  
  # bin age data ----
  .agedat_hlage %>% 
    tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .agedat_hlage_bin
  
  # age comp ----
  
  # clean data and determine if haul, or both haul and port data to be used
  if(join == 'haul'){
    .agedat_hlage_bin %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::drop_na(haul_join) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> .agedat_samp
  }
  if(join == 'both'){
    .agedat_hlage_bin %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> .agedat_samp
  }
  
  
  acomp(agedat = .agedat_samp, lfreq = .lfreq_samp, catch = .catch, exp_meth = exp_meth) -> .acomp
  

  list(age = .acomp, length = .lcomp)
  
}