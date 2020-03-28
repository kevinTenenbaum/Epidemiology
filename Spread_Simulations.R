library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(rvest)
library(dtplyr)
library(ggplot2)
library(ggforce)
library(tidyr)

source('~/R/Epidemiology/Spread_Helper_Functions.R') # Source helper functions

#' TODO: Add the ability to have people congregate at one local place
#' TODO: Add congregation at multiple places in each city
#' TODO: Add a social distancing component
#' TODO: Make cities vary in size
#' TODO: Model number of hospital beds as a function of the number infected
#' TODO: Model mortality rates as a function of the number of hospital beds and the number infected
#' TODO: Allow user to input real cities and populations and coordinates with cross-contamination related to distance


## Set paramters
BaselineRate <-  0.005
InfectionRadius <- 2
InfectionRate <- .2
InfectionDays <- 7
InfectionDaySD <- 1
N <- 15000
SideLength <- 50
MoveSD = 4
NumDays <- 40 
NCities <- 3
CommunityCenters <- 1
CommCenterRate <- 0
CityMoveRate <- 0.1


simulates <- simDays(NumDays)

CityStats <- simulates$CityStats
CityPivot <- CityStats %>% pivot_longer(cols = Susceptible:Removed, names_to = "Status", values_to = "Count")

DayStats <- simulates$DayStats
DayPivot <- DayStats %>% pivot_longer(cols = Susceptible:Removed, names_to = "Status", values_to = "Count")

### Plot Results

# Plot Population Shares by City
ggplot(CityPivot, aes(x = Day, y = Count, colour = Status)) + geom_line() + 
  facet_wrap(~City)

# Plot Global Population Share
ggplot(DayPivot, aes(x = Day, y = Count, colour = Status)) + geom_line()

# Plot global Reff over time
ggplot(DayStats, aes(x = Day, y = R)) + geom_line()

# Plot Reff by City over time
ggplot(CityStats %>% mutate(City = as.factor(City)), aes(x = Day, y = R, colour = City)) + geom_line()
s


