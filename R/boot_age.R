#' resample age data w/replacement
#'
#' @param age_dat age specimen data 
#'
#' @return
#' @export
#'
#' @examples
boot_age <- function(age_dat) {

  # combine sex length age to common id - bootstrao based on year, species, haul
  # then split back apart
  
  age_dat %>%
    tidytable::mutate(sex_ln_ag = paste0(sex, "-", length, "-", age)) %>%
    tidytable::mutate(sex_ln_ag = sample(sex_ln_ag, .N, replace = TRUE), 
                      .by = c(year, species, prime_join)) %>%
    tidytable::separate(sex_ln_ag, c('sex', 'length', "age"), sep = '-', convert = TRUE)   
}