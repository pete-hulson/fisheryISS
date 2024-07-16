#' transform haul level data to trip level data
#'
#' @param catch haul catch observer data 
#' @param specimen specimen age-length data 
#' @param lfreq length frequency data
#' @param yrs filter for years more recent than this defined year
#'
#' @return
#' @export haul_to_trip
#'
#' @examples
haul_to_trip <- function(catch, 
                         specimen, 
                         lfreq,
                         yrs) {
  
  # filter years
  catch <- catch %>%
    filter(year >= yrs)
  
  obs_catch <- obs_catch %>%
    filter(year >= yrs)
  
  lfreq <- lfreq %>%
    filter(year >= yrs)
  
  specimen <- specimen %>%
    filter(year >= yrs)
  
  
  # Creating unique "trip_join" values
  catch <- catch %>%
    tidytable::select(year, vessel_id, species_name, deployment_trip_start_date,
                      deployment_trip_end_date, weight_posted) %>%
    tidytable::rename(permit = vessel_id) %>%
    tidytable::mutate(trip_join = cur_group_id(), .by = c(permit, 
                                                          deployment_trip_start_date, 
                                                          deployment_trip_end_date)) %>%
    tidytable::distinct(weight_posted, .keep_all = TRUE) %>%
    tidytable::summarise(trip_weight = sum(weight_posted), 
                         .by = c(permit, 
                                 deployment_trip_start_date, 
                                 deployment_trip_end_date,
                                 trip_join))
  
  
  # Join obs_catch with catch based on permit and haul_date; assign trip_join values
  obs_catch %>%
    tidytable::left_join(catch, by = "permit") %>%
    tidytable::filter(haul_date >= deployment_trip_start_date & haul_date <= deployment_trip_end_date) %>%
    tidytable::select(-c(deployment_trip_start_date, deployment_trip_end_date, trip_weight)) -> catch
  
  # Grouping and translating catch tibble
  #catch.t <- catch %>%
    #group_by(cruise, permit, year) %>%
    #mutate(haul_join_group = first(haul_join), 
           #extrapolated_number_sum = sum(extrapolated_number)) %>%
    #ungroup()
  
  catch %>%
    tidytable::summarize(#trip_join = first(trip_join), 
                         num_hls = .N,
                         ext_num = sum(extrapolated_number), .by = c(year, species, cruise, permit, trip_join))-> catch.p

  # Matching haul_join values from catch to specimen tibble
  #specimen.t <- specimen %>%
    #left_join(catch.t %>% select(haul_join, haul_join_group), by = "haul_join") %>%
    #filter(!is.na(haul_join_group)) %>%
    #mutate(haul_join = haul_join_group) %>%
    #select(-haul_join_group)
  
  specimen %>% 
    tidytable::select(year, species, haul_join, sex, age, length, performance) %>% 
    tidytable::left_join(catch %>% 
                           tidytable::select(year, species, haul_join, cruise, permit, trip_join)) %>% 
    tidytable::drop_na() %>%
    #tidytable::mutate(trip_join = first(haul_join), .by = c(year, species, cruise, permit)) %>% 
    tidytable::select(year, species, trip_join, sex, age, length, performance) -> specimen.p
  
  # Matching haul_join values from catch to lfreq tibble
  #lfreq.t <- lfreq %>%
    #left_join(catch.t %>% select(haul_join, haul_join_group), by = "haul_join") %>%
    #filter(!is.na(haul_join_group)) %>%
    #mutate(haul_join = haul_join_group) %>%
    #select(-haul_join_group)
   
  lfreq %>% 
    tidytable::select(year, species, haul_join, sex, length, frequency, performance) %>% 
    tidytable::left_join(catch %>% 
                           tidytable::select(year, species, haul_join, cruise, permit, trip_join)) %>% 
    tidytable::drop_na() %>% 
    #tidytable::mutate(trip_join = first(haul_join), .by = c(year, species, cruise, permit)) %>% 
    tidytable::summarise(frequency = sum(frequency),
                         performance = median(performance), .by = c(year, species, trip_join, sex, length)) -> lfreq.p

  # Cleaning up catch tibble
  #catch.t <- catch.t %>%
    #group_by(haul_join) %>%
    #mutate(haul_join = haul_join_group,
           #extrapolated_number = extrapolated_number_sum) %>%
    #select(-haul_join_group, -extrapolated_number_sum)
  
  # Summarizing lfreq observations by haul_join/sex/length
  #lfreq.t <- lfreq.t %>%
    #group_by(year, haul_join, sex, length) %>%
    #summarize(frequency = sum(frequency),
              #performance = first(performance),
              #port_join = first(port_join),
              #species = first(species),
              #fmp_gear = first(fmp_gear),
              #fmp_area = first(fmp_area),
              #fmp_subarea = first(fmp_subarea))
  
  # Return the translated tibbles
  return(list(pete_c = catch.p, pete_s = specimen.p, pete_l = lfreq.p))
}

