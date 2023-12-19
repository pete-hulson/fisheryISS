#' compute fishery age comp
#'
#' @param agedat age specimen data
#' @param lfreq length frequency data
#' @param catch observer catch data
#' @param exp_meth compute age comps as 'marginal' or 'expanded'
#'
#' @return
#' @export
#'
#' @examples
acomp <- function(agedat, lfreq, catch, exp_meth){
  
  # compute marginal age comps ----
  if(exp_meth == 'marginal'){
  # combined sex 'total' age comp
  agedat %>%
    tidytable::summarise(afreq = tidytable::n(), .by = c(age, species, type, year)) %>% 
    tidytable::mutate(tot = sum(afreq), .by = c(year, species)) %>% 
    tidytable::mutate(acomp = afreq / tot) %>% 
    tidytable::select(year, species, type, age, acomp) %>% 
    tidytable::mutate(comp_type = 'total') %>% 
    # sex-specific age comp
    tidytable::bind_rows(agedat %>%
                           tidytable::filter(sex!= 'unknown') %>% 
                           tidytable::summarise(afreq = tidytable::n(), .by = c(age, species, type, year, sex)) %>% 
                           tidytable::mutate(tot = sum(afreq), .by = c(year, species, sex)) %>% 
                           tidytable::mutate(acomp = afreq / tot) %>% 
                           tidytable::select(year, species, type, age, sex, acomp) %>% 
                           tidytable::rename(comp_type = 'sex')) -> acomp
  }
    
  
  # compute expanded age comps (expanded with age-length key) ----
  if(exp_meth == 'expanded'){
    
    # first compute marginal length frequencies for missing years for expanded frequencies
    lfreq %>% 
      tidytable::summarise(lfreq = sum(frequency), .by = c(year, species, type, length)) %>% 
      tidytable::mutate(comp_type = 'total') %>% 
      tidytable::select(year, species, type, length, comp_type, lfreq) %>% 
      tidytable::bind_rows(lfreq %>% 
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::summarise(lfreq = sum(frequency), .by = c(year, species, type, length, sex)) %>% 
                             tidytable::select(year, species, type, length, sex, lfreq) %>% 
                             tidytable::rename(comp_type = 'sex')) -> lfreq_marg

    # next compute expanded length frequencies (weighted by observer catch)
    # combined sex 'total' length frequencies
    lfreq %>%
      tidytable::select(year, species, type, prime_join, length, frequency) %>% 
      tidytable::summarize(freq = sum(frequency), .by = c(year, species, type, prime_join, length)) %>% 
      tidytable::left_join(catch %>% 
                             tidytable::select(prime_join, species, extrapolated_number)) %>%
      tidytable::drop_na() %>% 
      tidytable::mutate(tot_freq = sum(freq), .by = c(year, species, type, prime_join)) %>% 
      tidytable::mutate(hl_lcomp = freq / tot_freq, # compute haul length comps
                        len_extrap = hl_lcomp * extrapolated_number) %>%  # compute weighted length frequencies per haul
      tidytable::summarize(lfreq = sum(len_extrap), .by = c(year, species, type, length)) %>% 
      tidytable::mutate(comp_type = 'total') %>%
      tidytable::select(year, species, type, length, comp_type, lfreq) %>% 
      # sex-specific length comps
      tidytable::bind_rows(lfreq %>%
                             tidytable::select(year, species, type, prime_join, length, sex, frequency) %>%
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::summarize(freq = sum(frequency), .by = c(year, species, type, prime_join, length, sex)) %>% 
                             tidytable::left_join(catch %>% 
                                                    tidytable::select(prime_join, species, extrapolated_number)) %>%
                             tidytable::drop_na() %>% 
                             tidytable::mutate(tot_freq = sum(freq), .by = c(year, species, type, prime_join, sex)) %>% 
                             tidytable::mutate(hl_lcomp = freq / tot_freq, # compute haul length comps
                                               len_extrap = hl_lcomp * extrapolated_number) %>%  # compute weighted length frequencies per haul
                             tidytable::summarize(lfreq = sum(len_extrap), .by = c(year, species, type, length, sex)) %>% 
                             tidytable::rename(comp_type = 'sex') %>% 
                             tidytable::select(year, species, type, length, comp_type, lfreq)) -> lfreq_exp
    
    # finally, bind expanded with marginal frequencies
    lfreq_exp %>% 
      tidytable::distinct(year) -> yrs_exp
    
    lfreq_marg %>% 
      tidytable::filter(!(year %in% yrs_exp$year)) %>% 
      tidytable::bind_rows(lfreq_exp) -> lfreq2
    
    
    # compute length comps associated with observed lengths in age data
    agedat %>% 
      tidytable::select(year, species, type, length) %>% 
      tidytable::summarise(length = unique(length), .by = c(year, species, type)) %>% 
      tidytable::mutate(comp_type = 'total') %>% 
      tidytable::bind_rows(agedat %>% 
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::select(year, species, type, length, sex) %>% 
                             tidytable::summarise(length = unique(length), .by = c(year, species, type, sex)) %>% 
                             tidytable::rename(comp_type = 'sex')) %>% 
      tidytable::left_join(lfreq2) %>% 
      tidytable::drop_na() %>% 
      tidytable::mutate(tot_lfreq = sum(lfreq), .by = c(year, species, type, comp_type)) %>% 
      tidytable::mutate(lcomp = lfreq / tot_lfreq) %>% 
      tidytable::select(year, species, type, length, comp_type, lcomp) -> lencomp
    
    # combined sex 'total' age comp
    agedat %>%
      tidytable::mutate(n_l = tidytable::n(), .by = c(year, species, type, age, length)) %>%
      tidytable::select(year, species, type, age, length, n_l) %>%
      tidytable::summarise(n_l = mean(n_l), .by = c(year, species, type, age, length)) %>%
      tidytable::mutate(N_l = sum(n_l), .by = c(year, species, type, length)) %>%
      tidytable::mutate(prop_al = n_l / N_l) %>% # age-length key
      tidytable::left_join(lencomp %>% 
                             tidytable::filter(comp_type == 'total') %>% 
                             tidytable::select(year, species, type, length, lcomp)) %>%
      tidytable::drop_na() %>%
      tidytable::mutate(prop = prop_al * lcomp) %>%
      tidytable::summarise(acomp = sum(prop), .by = c(year, species, type, age)) %>% 
      tidytable::mutate(comp_type = 'total') %>% 
      # sex-specific age comp
      tidytable::bind_rows(agedat %>%
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::mutate(n_l = tidytable::n(), .by = c(year, species, type, age, length, sex)) %>%
                             tidytable::select(year, species, type, age, length, sex, n_l) %>%
                             tidytable::arrange(age, length) %>%
                             tidytable::summarise(n_l = mean(n_l), .by = c(year, species, type, age, length, sex)) %>%
                             tidytable::mutate(N_l = sum(n_l), .by = c(year, species, type, length, sex)) %>%
                             tidytable::mutate(prop_al = n_l / N_l) %>% # age-length key
                             tidytable::left_join(lencomp %>% 
                                                    tidytable::filter(comp_type != 'total') %>% 
                                                    tidytable::rename(sex = 'comp_type')) %>%
                             tidytable::drop_na() %>%
                             tidytable::mutate(prop = prop_al * lcomp) %>%
                             tidytable::summarise(acomp = sum(prop), .by = c(year, species, type, age, sex)) %>% 
                             tidytable::rename(comp_type = 'sex')) -> acomp
    
  }
  
  acomp

}