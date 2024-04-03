library(dplyr)

catch <- catch %>%
  filter(year >= 2015)

lfreq <- lfreq %>%
  filter(year >= 2015)

specimen <- specimen %>%
  filter(year >= 2015)

haul_to_trip <- function(catch, specimen, lfreq) {
  # Grouping and translating catch tibble
  catch.t <- catch %>%
    group_by(cruise, permit, year) %>%
    mutate(haul_join_group = first(haul_join), 
           extrapolated_number_sum = sum(extrapolated_number)) %>%
    ungroup()
  
  # Matching haul_join values from catch to specimen tibble
  specimen.t <- specimen %>%
    left_join(catch.t %>% select(haul_join, haul_join_group), by = "haul_join") %>%
    filter(!is.na(haul_join_group)) %>%
    mutate(haul_join = haul_join_group) %>%
    select(-haul_join_group)
  
  # Matching haul_join values from catch to lfreq tibble
  lfreq.t <- lfreq %>%
    left_join(catch.t %>% select(haul_join, haul_join_group), by = "haul_join") %>%
    filter(!is.na(haul_join_group)) %>%
    mutate(haul_join = haul_join_group) %>%
    select(-haul_join_group)
   
  # Cleaning up catch tibble
  catch.t <- catch.t %>%
    group_by(haul_join) %>%
    mutate(haul_join = haul_join_group,
           extrapolated_number = extrapolated_number_sum) %>%
    select(-haul_join_group, -extrapolated_number_sum)
  
  # Summarizing lfreq observations by haul_join/sex/length
  lfreq.t <- lfreq.t %>%
    group_by(year, haul_join, sex, length) %>%
    summarize(frequency = sum(frequency),
              performance = first(performance),
              port_join = first(port_join),
              species = first(species),
              fmp_gear = first(fmp_gear),
              fmp_area = first(fmp_area),
              fmp_subarea = first(fmp_subarea))
  
  # Return the translated tibbles
  return(list(catch.t, specimen.t, lfreq.t))
}

catch.t <- haul_to_trip(catch, specimen, lfreq)[[1]]
specimen.t <- haul_to_trip(catch, specimen, lfreq)[[2]]
lfreq.t <- haul_to_trip(catch, specimen, lfreq)[[3]]

catch = catch.t
specimen = specimen.t
lfreq = lfreq.t




