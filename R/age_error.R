#' resample reader/tester data to implement ageing error
#'
#' @param age_dat age specimen data 
#' @param r_t reader/tester data 
#'
#' @return
#' @export age_error
#'
#' @examples
age_error <- function(age_dat, 
                      r_t) {

  # add id
  age_dat %>% 
    tidytable::mutate(id = .I) -> age_dat
  
  age_dat %>% 
    tidytable::left_join(age_dat %>% 
                           tidytable::summarise(aged = .N,
                                                .by = c(species, year, age)) %>% 
                           tidytable::left_join(r_t %>% 
                                                  tidytable::summarise(count = .N, .by = c(species, age, test_age)) %>%
                                                  tidytable::mutate(p_a = count / sum(count), .by = c(species, age))) %>% 
                           tidytable::drop_na() %>% 
                           dplyr::group_by(species, year, age) %>% 
                           dplyr::mutate(new_age = rmultinom(1, aged, p_a)) %>% 
                           ungroup %>% 
                           tidytable::filter(new_age[,1] != 0) %>% 
                           tidytable::select(species, year, age, test_age, new_age) %>% 
                           tidytable::uncount(., new_age)) %>% 
    group_by(id) %>% 
    tidytable::slice_sample(n = 1) %>% 
    ungroup -> agerr

  # remove the old ages, replace with new ones and bind back with samples that were not tested
  agerr %>% 
    tidytable::select(-age, age = test_age) %>% 
    tidytable::bind_rows(anti_join(age_dat, agerr, by = "id")) %>% 
    tidytable::select(-id)
  
  

}
