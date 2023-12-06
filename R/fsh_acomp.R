#' compute fishery age comp
#'
#' @param agedat age specimen data
#' @param lencomp length composition
#' @param join use 'haul' only, or 'both' haul and port samples (default NULL)
#' @param exp_meth compute age comps as 'marginal' or 'expanded' (default NULL)
#'
#' @return
#' @export
#'
#' @examples
acomp <- function(agedat, lencomp, join, exp_meth){
  
  
  if(join == 'haul'){
    agedat %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::drop_na(haul_join) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> agedat1
  }
  if(join == 'both'){
    agedat %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> agedat1
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
      tidytable::mutate(acomp_tot = sum(acomp), .by = c(year)) %>% # restandardize to sum to 1
      tidytable::mutate(acomp_corr = acomp / acomp_tot) %>% 
      tidytable::select(year, age, acomp_corr) %>% 
      tidytable::rename(acomp = 'acomp_corr') %>% 
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
                             tidytable::mutate(acomp_tot = sum(acomp), .by = c(year, sex)) %>%  # restandardize to sum to 1
                             tidytable::mutate(acomp_corr = acomp / acomp_tot) %>% 
                             tidytable::select(year, age, sex, acomp_corr) %>% 
                             tidytable::rename(acomp = 'acomp_corr',
                                               comp_type = 'sex')) -> acomp
  }
  
  acomp

}