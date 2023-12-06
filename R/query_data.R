#' fishery observer data query
#'
#' @param species norpac species_codes
#' @param year final year of data (default: NULL)
#' @param area specific region for data ('GOA', 'AI')
#' 
#' @return
#' @export query_data
#'
#' @examples
#'            
query_data <- function(species, year = NULL, area) {
  
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
  yr = year
  sp = species
  
  # get catch data ----
  table <- dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_spcomp_mv")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>%
    dplyr::left_join(dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                       dplyr::rename_with(tolower) %>% 
                       dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>%
                       dplyr::select(fmp_subarea, gear_type, join_key, haul_seq)) %>% 
    dplyr::filter(year<=yr, 
                  species %in% sp, 
                  fmp_subarea %in% area) 
  
  # output
  dplyr::collect(table) %>% 
    vroom::vroom_write(here::here("data", "fsh_obs_data.txt"), 
                       delim = ",")
  
  # get specimen data ----
  cols = c("year", "performance", "specimen_type", "join_key", "haul_join", "port_join",
           "species", "fmp_gear", "fmp_area", "fmp_subarea", 
           "age", "length", "weight", "sex")
  
  table <- dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_age_mv")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::select(!!!cols) %>% 
    dplyr::mutate(dplyr::across(c(join_key, haul_join, port_join), as.character)) %>%
    dplyr::left_join(dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                       dplyr::rename_with(tolower) %>% 
                       dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>%
                       dplyr::select(fmp_subarea, gear_type, join_key, haul_seq)) %>% 
    dplyr::filter(year <= yr & year > 0, 
                  fmp_subarea %in% area, 
                  species %in% sp,
                  !is.na(age)) %>% 
    dplyr::arrange(year)
  
  dplyr::collect(table) %>% 
    vroom::vroom_write(here::here("data", "fsh_specimen_data.txt"), 
                       delim = ",")
  
  # get length data ----
  cols = c("year", "performance", "haul_join", "port_join",
           "species", "fmp_gear", "fmp_area", "fmp_subarea", 
           "sex", "length", "frequency")
  
  table <- dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_length_mv")) %>% 
    dplyr::rename_with(tolower) %>% 
    dplyr::mutate(dplyr::across(c(join_key, haul_join, port_join), as.character)) %>%
    dplyr::left_join(dplyr::tbl(akfin, dplyr::sql("norpac.debriefed_haul_mv")) %>% 
                       dplyr::rename_with(tolower) %>% 
                       dplyr::mutate(dplyr::across(c(join_key, haul_join), as.character)) %>%
                       dplyr::select(fmp_subarea, gear_type, join_key, haul_seq)) %>% 
    dplyr::select(!!!cols) %>% 
    dplyr::filter(year <= yr & year > 0, 
                  fmp_subarea %in% area, 
                  species %in% sp,
                  !is.na(length)) %>% 
    dplyr::arrange(year)
  
  dplyr::collect(table) %>% 
    vroom::vroom_write(here::here("data", "fsh_length_data.txt"), 
                       delim = ",")
  
}


