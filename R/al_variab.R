#' resample age-length data to implement growth variability
#'
#' @param age_dat age specimen data 
#' @param annual resample age data annually or pooled across years
#'
#' @return
#' @export al_variab
#'
#' @examples
al_variab <- function(age_dat, annual = FALSE) {

  if(isTRUE(annual)){
    age_dat %>% 
      tidytable::mutate(id = .I) %>% 
      tidytable::left_join(age_dat %>% 
                             tidytable::summarise(lengthed = .N, 
                                                  .by = c(species, year, sex, age, length)) %>% 
                             tidytable::mutate(p_l = lengthed / sum(lengthed), 
                                               .by = c(species, year, sex, age)) %>% 
                             tidytable::drop_na() %>% 
                             dplyr::group_by(species, year, sex, age) %>% 
                             dplyr::mutate(samp_length = rmultinom(1, sum(lengthed), p_l)) %>% 
                             ungroup %>% 
                             tidytable::filter(samp_length[,1] != 0) %>% 
                             tidytable::select(species, year, sex, age, length, samp_length) %>% 
                             tidytable::uncount(., samp_length) %>% 
                             tidytable::rename(new_length = length)) %>% 
      group_by(id) %>% 
      tidytable::slice_sample(n = 1) %>% 
      ungroup %>% 
      tidytable::select(-length, length = new_length) %>% 
      tidytable::select(-id)
  } else{
    age_dat%>% 
      tidytable::mutate(id = .I) %>% 
      tidytable::left_join(age_dat %>% 
                             tidytable::summarise(lengthed = .N,
                                                  .by = c(species, sex, age, length)) %>% 
                             tidytable::mutate(p_l = lengthed / sum(lengthed), 
                                               .by = c(species, sex, age)) %>% 
                             tidytable::drop_na() %>% 
                             dplyr::group_by(species, sex, age) %>% 
                             dplyr::mutate(samp_length = rmultinom(1, sum(lengthed), p_l)) %>% 
                             ungroup %>% 
                             tidytable::filter(samp_length[,1] != 0) %>% 
                             tidytable::select(species, sex, age, length, samp_length) %>% 
                             tidytable::uncount(., samp_length) %>% 
                             tidytable::rename(new_length = length)) %>% 
      group_by(id) %>% 
      tidytable::slice_sample(n = 1) %>% 
      ungroup %>% 
      tidytable::select(-length, length = new_length) %>% 
      tidytable::select(-id)
  }
  
}
