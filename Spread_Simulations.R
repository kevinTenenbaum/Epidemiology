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
N <- 1000
SideLength <- 50
MoveSD = 3
NumDays <- 100 
DayStats <- data.frame(Day = 1:NumDays, 
                       Susceptible = rep(0, NumDays),
                       Infected = rep(0, NumDays),
                       Removed = rep(0, NumDays))



people <- tibble(ID = 1:N,
       InitialInfected = base::sample(c(0,1), size = N, replace = TRUE, prob = c(1-BaselineRate, BaselineRate)),
       Status = ifelse(InitialInfected == 1, 'I', 'S'),
       StartingLocationX = runif(N, min = -SideLength/2, max = SideLength/2), 
       StartingLocationY = runif(N, min = -SideLength/2, max = SideLength/2),
       NDays = 0
       )
people <- data.table(people)
setkey(people, ID)

getSummaryStats <- function(people){
  c(Susceptible = sum(people$Status == 'S'),
    Infected = sum(people$Status == 'I'),
    Removed = sum(people$Status == 'R'))
  
}

newSim <- list(people = people, summary = getSummaryStats(people))
DayStats[1, -1] <- getSummaryStats(people)
pb <- txtProgressBar(min = 2, max = NumDays, style = 3)
for(i in 2:NumDays){
  setTxtProgressBar(pb, i)
  newSim <- simDay(newSim$people)
  DayStats[i, -1] <- newSim$summary
}

ggplot() + geom_line(data = DayStats, aes(x = Day, y = Infected), color = 'red') + 
  geom_line(data = DayStats, aes(x = Day, y = Susceptible), color  = 'black') + 
  geom_line(data = DayStats, aes(x = Day, y = Removed), color = 'green') 


simDay <- function(people){
  
  ## Check to see who should be infected
  infected <- people[Status == 'I']
  
  
  # Merge infected population with Susceptible to see who came into contact
  merged <- CJ(Infected = people[Status == 'I', ID],
     ID = people[Status == 'S',ID]) # Created cross-joined data frame
  
  setkey(merged, Infected) # Merge infected people data in
  merged = merged[infected[,list(ID, StartingLocationX, StartingLocationY)], nomatch = 0]
  
  setkey(merged, ID) # Merge susceptible people data in
  merged = merged[people[,list(ID, StartingLocationX, StartingLocationY)], nomatch = 0]
  
  # Check if they were in radius
  merged[, Distance := sqrt((StartingLocationX - i.StartingLocationX)^2 + (StartingLocationY + i.StartingLocationY)^2)]
  merged[, inRadius := Distance <= InfectionRadius]
  
  # Aggregate to only people that were in radius
  aggMerged <- merged[,.(NumContacts = sum(inRadius)), .(ID)]
  toInfect <- aggMerged[NumContacts > 0]
  
  # Randomly sample exposed people to be infected
  toInfect[, Infected := sample(c(0,1), size = nrow(toInfect), replace = TRUE, prob = c(1-InfectionRate, InfectionRate))]
  
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
  people[, c("StartingLocationX", "StartingLocationY") := list(rnorm(1000, mean = StartingLocationX, sd = MoveSD), rnorm(1000, mean = StartingLocationY, sd = MoveSD))]
  people <- people[, c("ID", "Status", "StartingLocationX", "StartingLocationY", "NDays")]
  
  # Calculate summary stats
  sumStats <- getSummaryStats(people)
  
  return(list(people = people, summary = sumStats))
}
