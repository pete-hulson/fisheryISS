#' resample length data w/replacement
#'
#' @param lfreq_un expanded length frequency data 
#'
#' @return
#' @export boot_length
#'
#' @examples
boot_length <- function(lfreq_un) {

  # combine sex-length to common id - bootstrap based on year, species, haul then split back apart
  lfreq_un %>%
    tidytable::mutate(sex_ln = paste0(sex, "-", length)) %>%
    group_by(year, species, prime_join) %>% 
    tidytable::mutate(sex_ln = sample(sex_ln, .N, replace = TRUE)) %>%
    ungroup %>% 
    tidytable::separate(sex_ln, c('sex', 'length'), sep = '-', convert = TRUE)
}