#' compute fishery length comp
#'
#' @param lfreq length comp data
#' @param catch haul catch data
#' @param exp_meth compute length comps as 'marginal' or 'expanded' (default NULL)
#'
#' @return
#' @export fsh_lcomp
#'
#' @examples
fsh_lcomp <- function(lfreq, 
                      catch, 
                      exp_meth = NULL) {
  
  # compute marginal length comps ----
  if(exp_meth == 'marginal'){
    # combined sex 'total' length comps
    lfreq %>% 
      tidytable::summarise(freq = sum(frequency), .by = c(year, species, length)) %>% 
      tidytable::mutate(tot_freq = sum(freq),
                        lcomp = freq / tot_freq, .by = c(year)) %>% 
      tidytable::mutate(comp_type = 'total') %>% 
      tidytable::select(year, species, length, comp_type, lcomp) %>% 
      # sex-specific length comps
      tidytable::bind_rows(lfreq %>% 
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::summarise(freq = sum(frequency), .by = c(year, species, length, sex)) %>% 
                             tidytable::mutate(tot_freq = sum(freq),
                                               lcomp = freq / tot_freq, .by = c(year, sex)) %>% 
                             tidytable::select(year, species, length, sex, lcomp) %>% 
                             tidytable::rename(comp_type = 'sex')) -> lcomp
  }
  
  # compute expanded length comp ----
  if(exp_meth == 'expanded'){
    # first compute marginal length comps for missing years for expanded comps
    lfreq %>% 
      tidytable::summarise(freq = sum(frequency), .by = c(year, species, length)) %>% 
      tidytable::mutate(tot_freq = sum(freq),
                        lcomp = freq / tot_freq, .by = c(year)) %>% 
    tidytable::mutate(comp_type = 'total') %>% 
      tidytable::select(year, species, length, comp_type, lcomp) %>% 
      # sex-specific length comps
      tidytable::bind_rows(lfreq %>% 
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::summarise(freq = sum(frequency), .by = c(year, species, length, sex)) %>% 
                             tidytable::mutate(tot_freq = sum(freq),
                                               lcomp = freq / tot_freq, .by = c(year, sex)) %>% 
                             tidytable::select(year, species, length, sex, lcomp) %>% 
                             tidytable::rename(comp_type = 'sex')) -> lcomp_marg
    
    # next compute expanded length comp (weighted by observer catch)
    # combined sex 'total' length comps
    lfreq %>%
      tidytable::summarize(freq = sum(frequency), .by = c(year, prime_join, species, length)) %>% 
      tidytable::left_join(catch %>% 
                             tidytable::select(prime_join, species, extrapolated_number)) %>%
      tidytable::drop_na() %>% 
      tidytable::mutate(tot_freq = sum(freq), .by = c(year, prime_join, species)) %>% 
      tidytable::mutate(hl_lcomp = freq / tot_freq, # compute haul length comps
                        len_extrap = hl_lcomp * extrapolated_number) %>%  # compute weighted length frequencies per haul
      tidytable::summarize(wtd_freq = sum(len_extrap), .by = c(year, species, length)) %>% 
      tidytable::mutate(tot_wtd_freq = sum(wtd_freq), .by = c(year, species)) %>% 
      tidytable::mutate(lcomp = wtd_freq / tot_wtd_freq) %>%   # compute catch weighted length composition
      tidytable::mutate(comp_type = 'total') %>%
      tidytable::select(year, species, length, comp_type, lcomp) %>% 
      # sex-specific length comps
      tidytable::bind_rows(lfreq %>%
                             tidytable::filter(sex != 'unknown') %>% 
                             tidytable::summarize(freq = sum(frequency), .by = c(year, prime_join, species, length, sex)) %>% 
                             tidytable::left_join(catch %>% 
                                                    tidytable::select(prime_join, species, extrapolated_number)) %>%
                             tidytable::drop_na() %>% 
                             tidytable::mutate(tot_freq = sum(freq), .by = c(year, prime_join, species, sex)) %>% 
                             tidytable::mutate(hl_lcomp = freq / tot_freq, # compute haul length comps
                                               len_extrap = hl_lcomp * extrapolated_number) %>%  # compute weighted length frequencies per haul
                             tidytable::summarize(wtd_freq = sum(len_extrap), .by = c(year, species, length, sex)) %>% 
                             tidytable::mutate(tot_wtd_freq = sum(wtd_freq), .by = c(year, species, sex)) %>% 
                             tidytable::mutate(lcomp = wtd_freq / tot_wtd_freq) %>%   # compute catch weighted length composition
                             tidytable::rename(comp_type = 'sex') %>% 
                             tidytable::select(year, species, length, comp_type, lcomp)) -> lcomp_exp
    
    # finally, bind expanded with marginal comps
    lcomp_exp %>% 
      tidytable::distinct(year) -> yrs_exp
    
    lcomp_marg %>% 
      tidytable::filter(!(year %in% yrs_exp$year)) %>% 
      tidytable::bind_rows(lcomp_exp) -> lcomp

    
  }
  
  lcomp
}