

source('Spread_Helper_Functions.R') # Source helper functions

#' TODO: Allow user to input real cities and populations and coordinates with cross-contamination related to distance
#' TODO: Publish simulation tool as a shiny app on ShinyApps.io
#' TODO: Add a readME.md file 
#' TODO: Create default parameter setting for different scenarios
#' TODO: Allow for only a portion of people to socially distance

## Set paramters
# BaselineRate <-  0.005
# InfectionRadius <- 1
# InfectionRate <- .2
# InfectionDays <- 7
# InfectionDaySD <- 1
# N <- 15000
# SideLength <- 100
# MoveSD = 4
# NumDays <- 40
# NCities <- 3
# CityPopShareShape1 <- 2
# CityPopShareShape2 <- 60
# CommunityCenters <- 1
# CommCenterRate <- 0
# CityMoveRate <- 0
# HospitalBaseline <- N/5
# MortalityBaseline <- .15 # Mortality rate for cases warranting hospitalization
# MortalityFullHospitals <- .3 # Mortality rate when there are no empty beds
# HospitalizationRate <- .1 # Rate that cases need hospital beds
# HospitalMinDays <- 3
# MortalityMinDays <- 5


# Simulate Disease Spread
simulates <- simDays(NumDays = 50, N = 10000, BaselineRate = 0.05, verbose = T)

# Pivot metrics for easier plotting
CityStats <- simulates$CityStats
CityPivot <- CityStats %>% pivot_longer(cols = Susceptible:Severe, names_to = "Status", values_to = "Count")

DayStats <- simulates$DayStats
DayPivot <- DayStats %>% pivot_longer(cols = Susceptible:Severe, names_to = "Status", values_to = "Count")

### Plot Results

# Plot Population Shares by City
ggplot(CityPivot, aes(x = Day, y = Count, colour = Status)) + geom_line() + facet_wrap(~City) +  
  geom_line(data = CityStats, aes(x = Day, y = NumBeds),colour = 'black', lty = 2) + 
  theme_bw() 

# Plot Global Population Share
ggplot(DayPivot, aes(x = Day, y = Count, colour = Status)) + geom_line() + theme_bw() + 
  geom_line(data = DayStats, aes(x = Day, y = NumBeds), colour = 'black', lty = 2)

# Plot global Reff over time
ggplot(DayStats, aes(x = Day, y = R)) + geom_line() + theme_bw()

# Plot Reff by City over time
ggplot(CityStats %>% mutate(City = as.factor(City)), aes(x = Day, y = R, colour = City)) + geom_line() + theme_bw()



