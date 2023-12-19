#' calculate effective sample size for age comps
#'
#' @param sim_data list of abundance by age data
#' @param og_data original abundance by age data (single list)
#'
#' @return
#' @export
#'
#' @examples
ess_age <- function(sim_data, og_data){
  
  sim_data %>% 
    tidytable::left_join(og_data %>% 
                           tidytable::rename(og_acomp = 'acomp')) %>% 
      tidytable::mutate(ess = sum(og_acomp * (1 - og_acomp)) / sum((acomp - og_acomp) ^ 2),
                         .by = c(year, species, type, comp_type)) %>%
      tidytable::distinct(year, species, type, comp_type, ess) 
  
}