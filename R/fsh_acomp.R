#' compute fishery age comp
#'
#' @param agedat age specimen data
#' @param lfreq length frequency data
#' @param catch observer catch data
#' @param join use 'haul' only, or 'both' haul and port samples (default NULL)
#' @param exp_meth compute age comps as 'marginal' or 'expanded' (default NULL)
#'
#' @return
#' @export
#'
#' @examples
acomp <- function(agedat, lfreq, catch, join, exp_meth){
  
  
  if(join == 'haul'){
    agedat %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::drop_na(haul_join) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> agedat1
    lfreq %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::drop_na(haul_join) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> lfreq1
  }
  if(join == 'both'){
    agedat %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> agedat1
    lfreq %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> lfreq1
  }

  
  
  # compute marginal age comps ----
  if(exp_meth == 'marginal'){
  # combined sex 'total' age comp
  agedat1 %>%
    tidytable::summarise(afreq = tidytable::n(), .by = c(age, year)) %>% 
    tidytable::mutate(tot = sum(afreq), .by = year) %>% 
    tidytable::mutate(acomp = afreq / tot) %>% 
    tidytable::select(year, age, acomp) %>% 
    tidytable::mutate(comp_type = 'total') %>% 
    # sex-specific age comp
    tidytable::bind_rows(agedat1 %>%
                           tidytable::filter(sex!= 'unknown') %>% 
                           tidytable::summarise(afreq = tidytable::n(), .by = c(age, year, sex)) %>% 
                           tidytable::mutate(tot = sum(afreq), .by = c(year, sex)) %>% 
                           tidytable::mutate(acomp = afreq / tot) %>% 
                           tidytable::select(year, age, sex, acomp) %>% 
                           tidytable::rename(comp_type = 'sex')) -> acomp
  }
    
  
  # compute expanded age comps (expanded with age-length key) ----
  if(exp_meth == 'expanded'){
    
    # first compute marginal length frequencies for missing years for expanded frequencies
    lfreq1 %>% 
      tidytable::summarise(lfreq = sum(frequency), .by = c(year, length)) %>% 
      tidytable::mutate(comp_type = 'total') %>% 
      tidytable::select(year, length, comp_type, lfreq) %>% 
      tidytable::bind_rows(lfreq1 %>% 
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::summarise(lfreq = sum(frequency), .by = c(year, length, sex)) %>% 
                             tidytable::select(year, length, sex, lfreq) %>% 
                             tidytable::rename(comp_type = 'sex')) -> lfreq_marg

    # next compute expanded length comp (weighted by observer catch)
    # combined sex 'total' length comps
    lfreq1 %>%
      tidytable::select(year, haul_join, length, frequency) %>% 
      tidytable::summarize(freq = sum(frequency), .by = c(year, haul_join, length)) %>% 
      tidytable::left_join(catch %>% 
                             tidytable::select(haul_join, extrapolated_number)) %>%
      tidytable::drop_na() %>% 
      tidytable::mutate(tot_freq = sum(freq), .by = c(year, haul_join)) %>% 
      tidytable::mutate(hl_lcomp = freq / tot_freq, # compute haul length comps
                        len_extrap = hl_lcomp * extrapolated_number) %>%  # compute weighted length frequencies per haul
      tidytable::summarize(lfreq = sum(len_extrap), .by = c(year, length)) %>% 
      tidytable::mutate(comp_type = 'total') %>%
      tidytable::select(year, length, comp_type, lfreq) %>% 
      # sex-specific length comps
      tidytable::bind_rows(lfreq1 %>%
                             tidytable::select(year, haul_join, length, sex, frequency) %>%
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::summarize(freq = sum(frequency), .by = c(year, haul_join, length, sex)) %>% 
                             tidytable::left_join(catch %>% 
                                                    tidytable::select(haul_join, extrapolated_number)) %>%
                             tidytable::drop_na() %>% 
                             tidytable::mutate(tot_freq = sum(freq), .by = c(year, haul_join, sex)) %>% 
                             tidytable::mutate(hl_lcomp = freq / tot_freq, # compute haul length comps
                                               len_extrap = hl_lcomp * extrapolated_number) %>%  # compute weighted length frequencies per haul
                             tidytable::summarize(lfreq = sum(len_extrap), .by = c(year, length, sex)) %>% 
                             tidytable::rename(comp_type = 'sex') %>% 
                             tidytable::select(year, length, comp_type, lfreq)) -> lfreq_exp
    
    # finally, bind expanded with marginal frequencies
    lfreq_exp %>% 
      tidytable::distinct(year) -> yrs_exp
    
    lfreq_marg %>% 
      tidytable::filter(!(year %in% yrs_exp$year)) %>% 
      tidytable::bind_rows(lfreq_exp) -> lfreq2
    
    
    # compute length comps associated with observed lengths in age data
    agedat1 %>% 
      tidytable::select(year, length) %>% 
      tidytable::summarise(length = unique(length), .by = c(year)) %>% 
      tidytable::mutate(comp_type = 'total') %>% 
      tidytable::bind_rows(agedat1 %>% 
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::select(year, length, sex) %>% 
                             tidytable::summarise(length = unique(length), .by = c(year, sex)) %>% 
                             tidytable::rename(comp_type = 'sex')) %>% 
      tidytable::left_join(lfreq2) %>% 
      tidytable::drop_na() %>% 
      tidytable::mutate(tot_lfreq = sum(lfreq), .by = c(year, comp_type)) %>% 
      tidytable::mutate(lcomp = lfreq / tot_lfreq) %>% 
      tidytable::select(year, length, comp_type, lcomp) -> lencomp
    
    # combined sex 'total' age comp
    agedat1 %>%
      tidytable::mutate(n_l = tidytable::n(), .by = c(year, age, length)) %>%
      tidytable::select(year, age, length, n_l) %>%
      tidytable::summarise(n_l = mean(n_l), .by = c(year, age, length)) %>%
      tidytable::mutate(N_l = sum(n_l), .by = c(year, length)) %>%
      tidytable::mutate(prop_al = n_l / N_l) %>% # age-length key
      tidytable::left_join(lencomp %>% 
                             tidytable::filter(comp_type == 'total') %>% 
                             tidytable::select(year, length, lcomp)) %>%
      tidytable::drop_na() %>%
      tidytable::mutate(prop = prop_al * lcomp) %>%
      tidytable::summarise(acomp = sum(prop), .by = c(year, age)) %>% 
      tidytable::mutate(comp_type = 'total') %>% 
      # sex-specific age comp
      tidytable::bind_rows(agedat1 %>%
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::mutate(n_l = tidytable::n(), .by = c(year, age, length, sex)) %>%
                             tidytable::select(year, age, length, sex, n_l) %>%
                             tidytable::arrange(age, length) %>%
                             tidytable::summarise(n_l = mean(n_l), .by = c(year, age, length, sex)) %>%
                             tidytable::mutate(N_l = sum(n_l), .by = c(year, length, sex)) %>%
                             tidytable::mutate(prop_al = n_l / N_l) %>% # age-length key
                             tidytable::left_join(lencomp %>% 
                                                    tidytable::filter(comp_type != 'total') %>% 
                                                    tidytable::rename(sex = 'comp_type')) %>%
                             tidytable::drop_na() %>%
                             tidytable::mutate(prop = prop_al * lcomp) %>%
                             tidytable::summarise(acomp = sum(prop), .by = c(year, age, sex)) %>% 
                             tidytable::rename(comp_type = 'sex')) -> acomp
    
  }
  
  acomp

}