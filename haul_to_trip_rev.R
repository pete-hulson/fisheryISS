
haul_to_trip <- function(catch, 
                         specimen, 
                         lfreq,
                         yrs) {
  
  # filter years
  catch <- catch %>%
    filter(year >= yrs)
  
# catch1 <- catch1 %>%
  # filter(lubridate::ymd(catch_activity_date) == as.Date("2015-05-11"))  
  
  obs_catch <- obs_catch %>%
    filter(year >= yrs)
  
  lfreq <- lfreq %>%
    filter(year >= yrs)
  
  specimen <- specimen %>%
    filter(year >= yrs)
  
  # Creating unique "trip_join" values
  catch1 <- catch %>%
    tidytable::select(year, vessel_id, species_name, deployment_trip_start_date,
           deployment_trip_end_date, weight_posted) %>%
    tidytable::rename(permit = vessel_id) %>%
    tidytable::mutate(trip_join = cur_group_id(), .by = c(permit, 
                                                          deployment_trip_start_date, 
                                                          deployment_trip_end_date)) %>%
    tidytable::summarise(trip_total_weight = sum(weight_posted), 
                                              .by = c(permit, 
                                                      deployment_trip_start_date, 
                                                      deployment_trip_end_date,
                                                      trip_join))
  # Inner join based on 'permit'
  result <- inner_join(obs_catch, catch, by = "permit")
  
  # Assigning trip_join across obs_catch based on matching rows from catch
  result <- result %>%
    filter(haul_date >= deployment_trip_start_date & haul_date <= deployment_trip_end_date) %>%
    group_by(unique_id_table2) %>%
    mutate(trip_join = first(trip_join)) %>%
    ungroup()
  
  
  # Joining with the "catch" data frame based on year and permit variables
  
  # Initialize a vector to store trip_join values
  trip_join_values <- numeric(nrow(obs_catch))
  
  # Iterate over each row of another_df
  for (i in 1:nrow(obs_catch)) {
    haul_date <- obs_catch$haul_date[i]
    year <- obs_catch$year[i]
    permit <- obs_catch$permit[i]
    
    # Filter catch data frame for matching year and permit
    catch_filtered <- catch %>%
      filter(year == year, permit == permit)
    
    # Check if haul_date falls within any date range in catch_filtered
    within_range <- haul_date >= catch_filtered$deployment_trip_start_date & haul_date <= catch_filtered$deployment_trip_end_date
    
    # If within range, assign corresponding trip_join value
    if (any(within_range)) {
      trip_join_values[i] <- catch_filtered$trip_join[which.max(within_range)]
    } else {
      trip_join_values[i] <- NA  # If not within any range, assign NA
    }
  }
  
  # Assign trip_join values to another_df
  obs_catch$trip_join <- trip_join_values
  
  
  
  
  # Grouping and translating catch tibble
  obs_catch %>%
    tidytable::summarize(trip_join = first(haul_join), 
                         num_hls = .N,
                         ext_num = sum(extrapolated_number), .by = c(year, species, cruise, permit))-> catch.p
  
  # Matching haul_join values from catch to specimen tibble
  specimen %>% 
    tidytable::select(year, species, haul_join, sex, age, length, performance) %>% 
    tidytable::left_join(catch %>% 
                           tidytable::select(year, species, haul_join, cruise, permit)) %>% 
    tidytable::drop_na() %>%
    tidytable::mutate(trip_join = first(haul_join), .by = c(year, species, cruise, permit)) %>% 
    tidytable::select(year, species, trip_join, sex, age, length, performance) -> specimen.p
  
  # Matching haul_join values from catch to lfreq tibble
  lfreq %>% 
    tidytable::select(year, species, haul_join, sex, length, frequency, performance) %>% 
    tidytable::left_join(catch %>% 
                           tidytable::select(year, species, haul_join, cruise, permit)) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(trip_join = first(haul_join), .by = c(year, species, cruise, permit)) %>% 
    tidytable::summarise(frequency = sum(frequency),
                         performance = median(performance), .by = c(year, species, trip_join, sex, length)) -> lfreq.p
  
  # Return the translated tibbles
  return(list(pete_c = catch.p, pete_s = specimen.p, pete_l = lfreq.p))
}

