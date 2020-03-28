library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(rvest)
library(dtplyr)
library(ggplot2)
library(ggforce)

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
NumDays <- 100 
NCities <- 3
CommunityCenters <- 1
CommCenterRate <- 0
CityMoveRate <- 0.1


simDays(5)


# 
ggplot() + geom_line(data = CityStats, aes(x = Day, y = Infected), color = 'red') + 
  geom_line(data = CityStats, aes(x = Day, y = Susceptible), color  = 'black') + 
  geom_line(data = CityStats, aes(x = Day, y = Removed), color = 'green') + facet_wrap(~City)




