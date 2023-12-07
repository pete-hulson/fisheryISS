#' primary fishery age/length bootstrap function to compute comps
#'
#' @param lfreq_data length frequency data
#' @param specimen_data age-length specimen data
#' @param catch_data abundance by length data 
#' @param r_t reader/tester ageing data 
#' @param yrs age filter returns years >= (default = NULL)
#' @param bin length bin size (default = 1 cm)
#' @param boot_primes switch for resampling primary sampling unit, like hauls or trip (default = FALSE)
#' @param boot_lengths switch for resampling lengths (default = FALSE)
#' @param boot_ages switch for resampling ages (default = FALSE)
#' @param sex_spec determine whether to do sex specific or total comps (default = TRUE)
#' @param al_var switch for including age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err switch for including ageing error (default = FALSE)
#'
#' @return
#' @export fsh_comps
#'
#' @examples
#' 

fsh_comps <- function(lfreq_data, specimen_data, catch_data, r_t, yrs, bin,
                       boot_primes, boot_lengths, boot_ages, sex_spec, al_var, al_var_ann, age_err) {
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # first pass of filtering and combine port/haul joins to single join value ('prime_join')
  data.table::setDT(catch_data) %>%
    tidytable::filter(year >= yrs) -> .catch
  
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
    
    boot_prime(.lfreq) %>% 
      tidytable::mutate(primejoin_unq = .I) -> .hls_len
    
    boot_prime(.agedat) %>% 
      tidytable::mutate(primejoin_unq = .I) -> .hls_age

    .hls_len %>% 
      tidytable::left_join(.lfreq_un) %>% 
      tidytable::rename(primejoin_orig = 'prime_join',
                        prime_join = 'primejoin_unq') -> .lfreq_un
    
    .hls_age %>% 
      tidytable::left_join(.agedat) %>% 
      tidytable::rename(primejoin_orig = 'prime_join',
                        prime_join = 'primejoin_unq')  -> .agedat
    
  } 
  
  # randomize lengths ----
  if(isTRUE(boot_lengths)) {
    boot_length(.lfreq_un) %>% 
      tidytable::mutate(type = 'base') -> .lfreq_un
  } else{
    .lfreq_un %>% 
      tidytable::mutate(type = 'base') -> .lfreq_un
  }
  
  # bin length data ----
  .lfreq_un %>% 
    tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .lfreq_un
  
  # length comp ----
  lcomp(.lfreq_un) -> .lcomp
  
  # length population ----
  lpop(.lcomp, .cpue, .lngs) -> .lpop
  
  # randomize age ----
  if(isTRUE(boot_ages)) {
    boot_age(.agedat) %>% 
      tidytable::mutate(type = 'base') -> .agedat
  } else{
    .agedat %>% 
      tidytable::mutate(type = 'base') -> .agedat
  }
  
  # add age-length variability ----
  if(isTRUE(al_var)) {
    al_variab(.agedat, annual = al_var_ann)  %>% 
      tidytable::mutate(type = 'al') -> .agedat_al
  }
  
  # add ageing error ----
  if(isTRUE(age_err)) {
    age_error(.agedat, r_t)  %>% 
      tidytable::mutate(type = 'ae') -> .agedat_ae
  }
  
  # with age-length and ageing error ----
  if(isTRUE(al_var) & isTRUE(age_err)) {
    age_error(.agedat_al, r_t)  %>% 
      tidytable::mutate(type = 'ae_al') %>% 
      tidytable::bind_rows(.agedat_al) %>% 
      tidytable::bind_rows(.agedat_ae) %>% 
      tidytable::bind_rows(.agedat) -> .agedat
  } else if(isTRUE(al_var) & !isTRUE(age_err)){
    .agedat %>% 
      tidytable::bind_rows(.agedat_al) -> .agedat
  } else if(!isTRUE(al_var) & isTRUE(age_err)){
    .agedat %>% 
      tidytable::bind_rows(.agedat_ae) -> .agedat
  }
  
  .agedat %>% 
    tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .agedat
  
  # age population ----
  apop(.lpop, .agedat, sex_spec = sex_spec) -> .apop
  
  list(age = .apop, length = .lpop)
  
}