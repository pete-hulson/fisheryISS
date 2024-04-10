#' fishery data query for all columns
#'
#' @param yr final year of data (default: NULL)
#' @param species_code norpac species_codes (default: NULL)
#' @param species_group norpac species group (default: NULL)
#' @param area specific region for data ('GOA', 'AI')
#' 
#' @return
#' @export query_data_all
#'
#' @examples
#'            
query_data_all <- function(yr = NULL,
                           species_code = NULL,
                           species_group = NULL,
                           area = NULL) {
  
  # create folders
  if (!dir.exists("data")) {dir.create("data")}
  
  # connect to database
  akfin = connect()
  
  # globals
  area = toupper(area)
  area = if(isTRUE(area == "GOA")){
    area = c("WG", "CG", "WY", "EY", "SE")
  } else if(isTRUE(area=="BSAI")){
    area = c("BS", "AI")
  } else if(sum(sapply(c("BSAI", "GOA"), grepl, area))==2){
    area = c("WG", "CG", "WY", "EY", "SE", "BS", "AI")
  } else {
    area
  }
  
  # get landings data ----
  
  table <- dplyr::tbl(akfin, dplyr::sql("council.comprehensive_blend_ca")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::filter(year <= yr,
                  species_group_code %in% species_group,
                  fmp_subarea %in% area)
  
  # output
  dplyr::collect(table) %>% 
    vroom::vroom_write(here::here("data", "fsh_catch_data.txt"), 
                       delim = ",")
  
  
  # get observer haul catch data ----
  
  table <- dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_spcomp_mv")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>%
    dplyr::left_join(dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                       dplyr::rename_with(tolower) %>% 
                       dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>%
                       dplyr::select(fmp_subarea, gear_type, join_key, haul_seq)) %>% 
    dplyr::filter(year<=yr, 
                  species %in% species_code, 
                  fmp_subarea %in% area) 
  
  # output
  dplyr::collect(table) %>% 
    vroom::vroom_write(here::here("data", "fsh_obs_data.txt"), 
                       delim = ",")
  
  # get specimen data ----
  
  table <- dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_age_mv")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::mutate(dplyr::across(c(join_key, haul_join, port_join), as.character)) %>%
    dplyr::left_join(dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                       dplyr::rename_with(tolower) %>% 
                       dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>%
                       dplyr::select(fmp_subarea, gear_type, join_key, haul_seq)) %>% 
    dplyr::filter(year <= yr & year > 0, 
                  fmp_subarea %in% area, 
                  species %in% species_code,
                  !is.na(age)) %>% 
    dplyr::arrange(year)
  
  # output
  dplyr::collect(table) %>% 
    vroom::vroom_write(here::here("data", "fsh_specimen_data.txt"), 
                       delim = ",")
  
  # get length data ----
  
  table <- dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_length_mv")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::mutate(dplyr::across(c(join_key, haul_join, port_join), as.character)) %>%
    dplyr::left_join(dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                       dplyr::rename_with(tolower) %>% 
                       dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>%
                       dplyr::select(fmp_subarea, gear_type, join_key, haul_seq)) %>% 
    dplyr::filter(year <= yr & year > 0, 
                  fmp_subarea %in% area, 
                  species %in% species_code,
                  !is.na(length)) %>% 
    dplyr::arrange(year)
  
  dplyr::collect(table) %>% 
    vroom::vroom_write(here::here("data", "fsh_length_data.txt"), 
                       delim = ",")
  
}


