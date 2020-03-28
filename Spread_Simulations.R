library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(rvest)
library(dtplyr)
library(ggplot2)
library(ggforce)


## Set paramters
BaselineRate <-  0.005
InfectionRadius <- 2
InfectionRate <- .2
InfectionDays <- 7
InfectionDaySD <- 1
N <- 10000
SideLength <- 50
MoveSD = 3
NumDays <- 100 
NCities <- 1
CityMoveRate <- 0.1
DayStats <- data.frame(Day = 1:NumDays, 
                       Susceptible = rep(0, NumDays),
                       Infected = rep(0, NumDays),
                       Removed = rep(0, NumDays))
CityStats <- bind_rows(lapply(1:NCities, function(x) DayStats)) %>% arrange(Day)
CityStats$City <- 1:NCities
CityStats <- CityStats[, c('Day','City','Susceptible','Infected','Removed')]



people <- tibble(ID = 1:N,
       InitialInfected = base::sample(c(0,1), size = N, replace = TRUE, prob = c(1-BaselineRate, BaselineRate)),
       Status = ifelse(InitialInfected == 1, 'I', 'S'),
       StartingLocationX = runif(N, min = -SideLength/2, max = SideLength/2), 
       StartingLocationY = runif(N, min = -SideLength/2, max = SideLength/2),
       NDays = 0,
       City = sample(1:NCities, size = N, replace = TRUE)
       )
people <- data.table(people)
setkey(people, ID)

getSummaryStats <- function(people){
  c(Susceptible = sum(people$Status == 'S'),
    Infected = sum(people$Status == 'I'),
    Removed = sum(people$Status == 'R'))
  
}

getCityStats <- function(people){
  cs <- people[,.(Susceptible = sum(Status == 'S'),
            Infected = sum(Status == 'I'),
            Removed = sum(Status == 'R')), .(City)]
  setorder(cs, City)
  return(cs)
}

newSim <- list(people = people, summary = getSummaryStats(people), cities = getCityStats(people))
DayStats[1, -1] <- newSim$summary
CityStats[CityStats$Day == 1, -1] <- newSim$cities

pb <- txtProgressBar(min = 2, max = NumDays, style = 3)
for(i in 2:NumDays){
  setTxtProgressBar(pb, i)
  newSim <- simDay(newSim$people)
  DayStats[i, -1] <- newSim$summary
  CityStats[CityStats$Day == i, -1] <- newSim$cities
}

ggplot() + geom_line(data = CityStats, aes(x = Day, y = Infected), color = 'red') + 
  geom_line(data = CityStats, aes(x = Day, y = Susceptible), color  = 'black') + 
  geom_line(data = CityStats, aes(x = Day, y = Removed), color = 'green') + facet_wrap(~City)


simDay <- function(people){
  
  ## Check to see who should be infected
  infected <- people[Status == 'I']
  
  
  # Merge infected population with Susceptible to see who came into contact
  merged <- CJ(Infected = people[Status == 'I', ID],
     ID = people[Status == 'S',ID]) # Created cross-joined data frame
  
  setkey(merged, Infected) # Merge infected people data in
  merged = merged[infected[,list(ID, City, StartingLocationX, StartingLocationY)], nomatch = 0]
  
  setkey(merged, ID) # Merge susceptible people data in
  merged = merged[people[,list(ID, City, StartingLocationX, StartingLocationY)], nomatch = 0]
  
  # Check if they were in radius
  merged[, Distance := sqrt((StartingLocationX - i.StartingLocationX)^2 + (StartingLocationY + i.StartingLocationY)^2)]
  merged[, inRadius := Distance <= InfectionRadius & City == i.City]
  
  # Aggregate to only people that were in radius
  aggMerged <- merged[,.(NumContacts = sum(inRadius)), .(ID)]
  toInfect <- aggMerged[NumContacts > 0]
  
  # Randomly sample exposed people to be infected
  toInfect[, Infected := sample(c(0,1), size = nrow(toInfect), replace = TRUE, prob = c(1-InfectionRate, InfectionRate))]
  setkey(toInfect, ID)
  ## Change Status 
  people <- toInfect[people] # Join infection data
  people[,NewStatus := Status] # Set new status as the same as current status
  people[Infected == 1, NewStatus := 'I'] # Change infected people to new status as infected
  people[Status != NewStatus, NDays := 0] # Reset NDays in current status counter
  # Sample the number of days someone should have the virus for
  people[, InfectionDayLimit := rnorm(nrow(people), mean = InfectionDays, sd = InfectionDaySD)]
  # Move infected people to removed given sufficient number of days
  people[Status == 'I' & NDays >= InfectionDayLimit, NewStatus := 'R']
  people[,NDays := NDays + 1] # Increase number of days in current status
  people[,Status := NewStatus] # Finalize status changes
    
  # Move Everyone to new locations
  people[, c("StartingLocationX", "StartingLocationY") := list(rnorm(N, mean = StartingLocationX, sd = MoveSD), rnorm(N, mean = StartingLocationY, sd = MoveSD))]
  people <- people[, c("ID", "City", "Status", "StartingLocationX", "StartingLocationY", "NDays")]
  if(NCities > 1){
    # Simulate who should move cities
    people[, MoveCity := runif(nrow(people)) <= CityMoveRate]
    # Move people to new cities
    people[, CityIndex := sample(1:(NCities-1), size = nrow(people), replace = TRUE)]
    people[,NewCity := City]
    people[MoveCity == TRUE, NewCity := ifelse(CityIndex < City, CityIndex, CityIndex + 1)]
    people[,City := NewCity]
  }
  # Calculate summary stats
  sumStats <- getSummaryStats(people)
  cityStats <- getCityStats(people)
  
  return(list(people = people, summary = sumStats, cities = cityStats))
}
