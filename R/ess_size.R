#' calculate effective sample size for length comps
#'
#' @param sim_data list of abundance by length data
#' @param og_data original abundance by length data (single list)
#'
#' @return
#' @export
#'
#' @examples
ess_size <- function(sim_data, og_data) {
  
  sim_data %>% 
    tidytable::left_join(og_data %>% 
                           tidytable::rename(og_lcomp = 'lcomp')) %>% 
    tidytable::mutate(ess = sum(og_lcomp * (1 - og_lcomp)) / sum((lcomp - og_lcomp) ^ 2),
                      .by = c(year, species, type, comp_type)) %>%
    tidytable::distinct(year, species, type, comp_type, ess) 

}
