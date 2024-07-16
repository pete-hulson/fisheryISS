library(dplyr)
library(ggplot2)
library(gridExtra)


# Read in .csv files
trip_iss_ag <- read.csv("trip_iss_ag.csv")
trip_iss_ln <- read.csv("trip_iss_ln.csv")

haul_iss_ag <- read.csv("haul_iss_ag.csv")
haul_iss_ln <- read.csv("haul_iss_ln.csv")


# Add source column
trip_iss_ag$source <- "Trip"
trip_iss_ln$source <- "Trip"

haul_iss_ag$source <- "Haul"
haul_iss_ln$source <- "Haul"


# Combine trip and haul data frames
exp_iss_ag_combined <- rbind(trip_iss_ag, haul_iss_ag)
exp_iss_ln_combined <- rbind(trip_iss_ln, haul_iss_ln)


# Function to create boxplot
create_boxplot <- function(data, comp_type) {
  ggplot(data, aes(x = comp_type, y = iss, fill = source)) +
    geom_boxplot() +
    labs(fill = "Source") +
    theme_minimal()
}

# Create boxplots for each combination of trip and haul data
boxplot_exp_ag <- create_boxplot(exp_iss_ag_combined, "comp_type")
boxplot_exp_ln <- create_boxplot(exp_iss_ln_combined, "comp_type")


# Create a list of boxplots with specific titles
boxplots <- list(
  boxplot_exp_ag + ggtitle("Expanded Age ISS") + ylab("Base ISS"),
  boxplot_exp_ln + ggtitle("Expanded Length ISS") + ylab("Base ISS")
)

# Combine the list of plots into a 2x2 grid
grid.arrange(grobs = boxplots, ncol = 2)



