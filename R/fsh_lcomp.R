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
      tidytable::drop_na(haul_join) -> lfreq1
  }
  if(join == 'both'){
    lfreq %>%
      tidytable::filter(!is.na(length),
                        !is.na(performance)) -> lfreq1
  }
  
  if(exp_meth == 'marginal'){
    # compute marginal length comps
    lfreq1 %>% 
      tidytable::summarise(freq = sum(frequency), .by = c(year, length)) %>% 
      tidytable::mutate(tot_freq = sum(freq), .by = c(year),
                        lcomp = freq / tot_freq) %>% 
      tidytable::select(year, length, lcomp) -> lcomp
  }
  
  if(exp_meth == 'expanded'){
    # compute expanded length comp (weighted by observer catch)
    lfreq1 %>% 
      tidytable::left_join(catch %>% 
                             tidytable::select(haul_join, extrapolated_number)) %>%
      tidytable::select(year, haul_join, length, frequency, extrapolated_number) %>%
      tidytable::drop_na() %>%
      tidytable::distinct(year, haul_join, extrapolated_number) %>%
      tidytable::mutate(p_haul = extrapolated_number / sum(extrapolated_number),
                        .by = c(year)) -> p_haul # proportion of catch across hauls sampled for length
    
    lfreq1 %>% 
      tidytable::left_join(p_haul) %>%
      tidytable::select(year, haul_join, length, frequency, p_haul) %>%
      tidytable::drop_na() %>%
      tidytable::mutate(p_hlen = frequency / sum(frequency), .by = c (year, haul_join)) %>% # compute haul length comps
      tidytable::mutate(n_len = sum(frequency), .by = c(year, length)) %>% # compute number of samples by length bin
      tidytable::mutate(wtd_freq = p_haul * p_hlen * n_len) %>% # compute weighted length frequencies per haul
      tidytable::summarise(length_tot = sum(wtd_freq), .by = c(year, length)) -> lcomp
  }

  lcomp
}