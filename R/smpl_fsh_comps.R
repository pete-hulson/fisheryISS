#' primary fishery age/length bootstrap function to compute comps
#'
#' @param lfreq_data length frequency data
#' @param specimen_data age-length specimen data
#' @param catch_data abundance by length data 
#' @param r_t reader/tester ageing data 
#' @param yrs year filter returns years >= (default = NULL)
#' @param bin length bin size (default = 1 cm)
#' @param join use 'haul' only, or 'both' haul and port samples for age/length (default NULL)
#' @param exp_meth compute age/length comps as 'marginal' or 'expanded' (default NULL)
#' @param boot_primes switch for resampling primary sampling unit, like hauls or trip (default = FALSE)
#' @param boot_lengths switch for resampling lengths (default = FALSE)
#' @param boot_ages switch for resampling ages (default = FALSE)
#' @param al_var switch for including age-length variability (default = FALSE), note: only makes sense if expanding through an age-length key
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err switch for including ageing error (default = FALSE)
#'
#' @return
#' @export smpl_fsh_comps
#'
#' @examples
#' 

smpl_fsh_comps <- function(lfreq_data,
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
                           age_err = FALSE) {
  
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # first pass of filtering and combine port/haul joins to single join value ('prime_join')
  
  # catch data
  data.table::setDT(catch_data) %>%
      tidytable::filter(year >= yrs) %>% 
      tidytable::mutate(prime_join = haul_join,
                        species = species_key) -> .catch
  
  # length frequency data
  data.table::setDT(lfreq_data) %>%
    tidytable::filter(year >= yrs) -> lfreq1
  
  if(join == 'both'){
    lfreq1 %>% 
      tidytable::bind_cols(lfreq1 %>% 
                             tidytable::select(haul_join) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::rename(prime_join = 'haul_join') %>% 
                             tidytable::bind_rows(lfreq1 %>% 
                                                    tidytable::select(port_join) %>% 
                                                    tidytable::drop_na() %>% 
                                                    tidytable::rename(prime_join = 'port_join'))) %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::select(-haul_join, -port_join) -> .lfreq
  }
  if(join == 'haul'){
    lfreq1 %>%
      tidytable::bind_cols(lfreq1 %>% 
                             tidytable::select(haul_join) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::rename(prime_join = 'haul_join')) %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::select(-haul_join) -> .lfreq
  }
  
  .lfreq %>% 
    tidytable::uncount(frequency) -> .lfreq_un
  
  # specimen data
  data.table::setDT(specimen_data) %>%
    tidytable::filter(year >= yrs) -> agedat1
  
  if(join == 'both'){
    agedat1 %>% 
      tidytable::bind_cols(agedat1 %>% 
                             tidytable::select(haul_join) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::rename(prime_join = 'haul_join') %>% 
                             tidytable::bind_rows(agedat1 %>% 
                                                    tidytable::select(port_join) %>% 
                                                    tidytable::drop_na() %>% 
                                                    tidytable::rename(prime_join = 'port_join'))) %>%
      tidytable::filter(!is.na(age),
                        !is.na(length),
                        !is.na(performance)) %>% 
      tidytable::select(-haul_join, -port_join)  -> .agedat
  }
  if(join == 'haul'){
    agedat1 %>%
      tidytable::bind_cols(agedat1 %>% 
                             tidytable::select(haul_join) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::rename(prime_join = 'haul_join')) %>%
      tidytable::filter(!is.na(age),
                        !is.na(length),
                        !is.na(performance)) %>% 
      tidytable::select(-haul_join) -> .agedat
  }
  
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
    boot_length(.lfreq_un_hl) -> .lfreq_un_hlen
  } else{
    .lfreq_un_hl -> .lfreq_un_hlen
  }
  
  # bin length data ----
  .lfreq_un_hlen %>% 
    tidytable::mutate(length = bin * ceiling(length / bin)) -> .lfreq_un_hlen_bin
  
  # length comp ----
  
  # convert from uncounted to frequency data and rename sex category descriptions
  .lfreq_un_hlen_bin %>%
    tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                 sex == 'U' ~ 'unknown',
                                                 sex == 'M' ~ 'male')) %>% 
    tidytable::summarise(frequency = tidytable::n(), .by = c(year, prime_join, species, sex, length)) -> .lfreq_samp
  
  # compute length comp
  fsh_lcomp(lfreq = .lfreq_samp, 
            catch = .catch, 
            exp_meth = exp_meth) -> .lcomp
  
  # randomize age ----
  if(isTRUE(boot_ages)) {
    boot_age(.agedat_hl) -> .agedat
  } else{
    .agedat_hl -> .agedat
  }
  
  # with age-length and ageing error ----
  if(isTRUE(al_var) & isTRUE(age_err)) {
    al_variab(.agedat, annual = al_var_ann) -> .agedat_al
    age_error(.agedat_al, r_t) -> .agedat
  } else if(isTRUE(al_var) & !isTRUE(age_err)){
    al_variab(.agedat, annual = al_var_ann) -> .agedat
  } else if(!isTRUE(al_var) & isTRUE(age_err)){
    age_error(.agedat, r_t) -> .agedat
  }
  
  # bin age data ----
  .agedat %>% 
    tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .agedat
  
  # age comp ----
  
  # rename sex category descriptions
  .agedat %>%
    tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                 sex == 'U' ~ 'unknown',
                                                 sex == 'M' ~ 'male')) -> .agedat_samp
  
  fsh_acomp(agedat = .agedat_samp, 
            lfreq = .lfreq_samp, 
            catch = .catch, 
            exp_meth = exp_meth) -> .acomp
  
  
  list(age = .acomp, length = .lcomp)
  
}