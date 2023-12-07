#' resample primary sampling unit w/replacement
#'
#' @param data dataframe within which to resample primary sampling unit (i.e., haul or trip)
#'
#' @return
#' @export boot_prime
#'
#' @examples
boot_prime <- function(data) {
  
  data %>% 
    tidytable::select(year, species, prime_join) %>% 
    tidytable::distinct() %>% 
    tidytable::mutate(prime_join = sample(prime_join, .N, replace = TRUE),
                      .by = c(year, species))
}