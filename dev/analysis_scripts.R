library(dplyr)
library(ggplot2)
library(gridExtra)


# Read trip level CSV files
marg_iss_sz_trip <- read.csv("marg_iss_sz_trip.csv")
exp_iss_sz_trip <- read.csv("exp_iss_sz_trip.csv")
marg_iss_ag_trip <- read.csv("marg_iss_ag_trip.csv")
exp_iss_ag_trip <- read.csv("exp_iss_ag_trip.csv")

# Read haul level CSV files
marg_iss_sz <- read.csv("marg_iss_sz.csv")
exp_iss_sz <- read.csv("exp_iss_sz.csv")
marg_iss_ag <- read.csv("marg_iss_ag.csv")
exp_iss_ag <- read.csv("exp_iss_ag.csv")

# Add source column
marg_iss_sz_trip$source <- "Trip"
exp_iss_sz_trip$source <- "Trip"
marg_iss_ag_trip$source <- "Trip"
exp_iss_ag_trip$source <- "Trip"

marg_iss_sz$source <- "Haul"
exp_iss_sz$source <- "Haul"
marg_iss_ag$source <- "Haul"
exp_iss_ag$source <- "Haul"

# Combine trip and haul data frames
marg_iss_sz_combined <- rbind(marg_iss_sz_trip, marg_iss_sz)
exp_iss_sz_combined <- rbind(exp_iss_sz_trip, exp_iss_sz)
marg_iss_ag_combined <- rbind(marg_iss_ag_trip, marg_iss_ag)
exp_iss_ag_combined <- rbind(exp_iss_ag_trip, exp_iss_ag)

# Function to create boxplot
create_boxplot <- function(data, comp_type) {
  ggplot(data, aes(x = comp_type, y = base, fill = source)) +
    geom_boxplot() +
    labs(fill = "Source") +
    theme_minimal()
}

# Create boxplots for each combination of trip and haul data
boxplot_marg_sz <- create_boxplot(marg_iss_sz_combined, "comp_type")
boxplot_exp_sz <- create_boxplot(exp_iss_sz_combined, "comp_type")
boxplot_marg_ag <- create_boxplot(marg_iss_ag_combined, "comp_type")
boxplot_exp_ag <- create_boxplot(exp_iss_ag_combined, "comp_type")

# Create a list of boxplots with specific titles
boxplots <- list(
  boxplot_marg_sz + ggtitle("Marginal Length ISS") + ylab("Base ISS"),
  boxplot_exp_sz + ggtitle("Expanded Length ISS") + ylab("Base ISS"),
  boxplot_marg_ag + ggtitle("Marginal Age ISS") + ylab("Base ISS"),
  boxplot_exp_ag + ggtitle("Expanded Age ISS") + ylab("Base ISS")
)

# Combine the list of plots into a 2x2 grid
grid.arrange(grobs = boxplots, ncol = 2)



