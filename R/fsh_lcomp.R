#' compute fishery length comp
#'
#' @param lfreq length comp data
#' @param catch haul catch data
#' @param join use 'haul' only, or 'both' haul and port samples (default NULL)
#' @param exp_meth compute length comps as 'marginal' or 'expanded' (default NULL)
#'
#' @return
#' @export lcomp
#'
#' @examples
lcomp <- function(lfreq, catch, join = NULL, exp_meth = NULL) {
  
  # clean data and determine if haul, or both haul and port data to be used
  if(join == 'haul'){
    lfreq %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::drop_na(haul_join) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> lfreq1
  }
  if(join == 'both'){
    lfreq %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) %>% 
      tidytable::mutate(sex = tidytable::case_when(sex == 'F' ~ 'female',
                                                   sex == 'U' ~ 'unknown',
                                                   sex == 'M' ~ 'male')) -> lfreq1
  }
  
  if(exp_meth == 'marginal'){
    # compute marginal length comps
    lfreq1 %>% 
      tidytable::summarise(freq = sum(frequency), .by = c(year, length)) %>% 
      tidytable::mutate(tot_freq = sum(freq), .by = c(year),
                        lcomp = freq / tot_freq,
                        comp_type = 'total') %>% 
      tidytable::select(year, length, comp_type, lcomp) %>% 
      tidytable::bind_rows(lfreq1 %>% 
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::summarise(freq = sum(frequency), .by = c(year, length, sex)) %>% 
                             tidytable::mutate(tot_freq = sum(freq), .by = c(year, sex),
                                               lcomp = freq / tot_freq) %>% 
                             tidytable::select(year, length, sex, lcomp) %>% 
                             tidytable::rename(comp_type = 'sex')) -> lcomp
  }
  
  if(exp_meth == 'expanded'){
    # compute expanded length comp (weighted by observer catch)
    
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
      tidytable::summarize(wtd_freq = sum(len_extrap), .by = c(year, length)) %>% 
      tidytable::mutate(tot_wtd_freq = sum(wtd_freq), .by = c(year)) %>% 
      tidytable::mutate(lcomp = wtd_freq / tot_wtd_freq) %>%   # compute catch weighted length composition
      tidytable::mutate(comp_type = 'total') %>%
      tidytable::select(year, length, comp_type, lcomp) %>% 
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
                             tidytable::summarize(wtd_freq = sum(len_extrap), .by = c(year, length, sex)) %>% 
                             tidytable::mutate(tot_wtd_freq = sum(wtd_freq), .by = c(year, sex)) %>% 
                             tidytable::mutate(lcomp = wtd_freq / tot_wtd_freq) %>%   # compute catch weighted length composition
                             tidytable::rename(comp_type = 'sex') %>% 
                             tidytable::select(year, length, comp_type, lcomp)) -> lcomp
  }
  
  lcomp
}