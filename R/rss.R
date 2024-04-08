#' calculate realized sample size for age comps
#'
#' @param sim_data list of abundance by age data
#' @param og_data original abundance by age data (single list)
#'
#' @return
#' @export rss_age
#'
#' @examples
rss_age <- function(sim_data, 
                    og_data){
  
  # compute realized sample size
  sim_data %>% 
    tidytable::full_join(og_data %>% 
                           tidytable::rename(og_acomp = 'acomp')) %>% 
    tidytable::replace_na(list(acomp = 0)) %>% 
    tidytable::replace_na(list(og_acomp = 0)) %>%
    tidytable::summarise(rss = sum(og_acomp * (1 - og_acomp)) / sum((acomp - og_acomp)^2),
                         .by = c(year, species, comp_type)) %>% 
    tidytable::drop_na()
  
}

#' calculate realized sample size for length comps
#'
#' @param sim_data list of abundance by length data
#' @param og_data original abundance by length data (single list)
#'
#' @return
#' @export rss_length
#'
#' @examples
rss_length <- function(sim_data,
                       og_data) {
  
  # compute realized sample size
  sim_data %>% 
    tidytable::full_join(og_data %>% 
                           tidytable::rename(og_lcomp = 'lcomp')) %>% 
    tidytable::replace_na(list(lcomp = 0)) %>% 
    tidytable::replace_na(list(og_lcomp = 0)) %>% 
    tidytable::summarise(rss = sum(og_lcomp * (1 - og_lcomp)) / sum((lcomp - og_lcomp)^2),
                         .by = c(year, species, comp_type)) %>% 
    tidytable::drop_na()
  
}