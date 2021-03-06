require(data.table)
require(dplyr, warn.conflicts = FALSE)
require(dtplyr)
require(ggplot2)
require(ggforce)
require(tidyr)

getSummaryStats <- function(people){
  
  c(Susceptible = sum(people$Status == 'S'),
    Infected = sum(people$Status == 'I'),
    Recovered = sum(people$Status == 'R'),
    Dead = sum(people$Status == 'D'), 
    Severe = sum(people$Severe),
    NumBeds = sum(people[,.(.N), by = City][,N]))
}

getCityStats <- function(people){
  
  cs <- people[,.(Susceptible = sum(Status == 'S'),
                  Infected = sum(Status == 'I'),
                  Recovered = sum(Status == 'R'),
                  Dead = sum(Status == 'D'),
                  Severe = sum(Severe),
                  NumBeds = mean(numBeds)), .(City)]
  setorder(cs, City)
  return(cs)
}

simDay <- function(people
                   ,InfectionRadius = 1
                   ,InfectionRate = .2
                   ,N = 15000
                   ,SideLength = 100
                   ,MoveSD = 4
                   ,NCities = 3
                   ,CommunityCenters = FALSE
                   ,CommCenterRate = 0
                   ,CityMoveRate = 0
                   ,HospitalBaseline = N/5
                   ,MortalityBaseline = .15 # Mortality rate for cases warranting hospitalization
                   ,MortalityFullHospitals = .3 # Mortality rate when there are no empty beds
                   ,HospitalizationRate = .1 # Rate that cases need hospital beds
                   ,HospitalMinDays = 3
                   ,MortalityMinDays = 5
                   , verbose = FALSE){
  
  ## Check to see who should be infected
  infected <- people[Status == 'I']
  
  
  # Merge infected population with Susceptible to see who came into contact
  merged <- CJ(Infected = people[Status == 'I', ID],
               ID = people[Status == 'S',ID]) # Created cross-joined data frame
  
  setkey(merged, Infected) # Merge infected people data in
  merged = merged[infected[,list(ID, City, StartingLocationX, StartingLocationY, atStore)], nomatch = 0]
  
  setkey(merged, ID) # Merge susceptible people data in
  merged = merged[people[,list(ID, City, StartingLocationX, StartingLocationY, atStore)], nomatch = 0]
  
  # Check if they were in radius
  merged[, Distance := sqrt((StartingLocationX - i.StartingLocationX)^2 + (StartingLocationY + i.StartingLocationY)^2)]
  merged[, inRadius := (Distance <= InfectionRadius | (atStore & i.atStore)) & City == i.City]
  
  # Aggregate to only people that were in radius
  aggMerged <- merged[,.(NumContacts = sum(inRadius)), .(ID, City)]
  toInfect <- aggMerged[NumContacts > 0]
  
  # Randomly sample exposed people to be infected
  toInfect[, Infected := sample(c(0,1), size = nrow(toInfect), replace = TRUE, prob = c(1-InfectionRate, InfectionRate))]
  setkey(toInfect, ID)
  
  ## Change Status 
  people <- toInfect[people] # Join infection data
  people[,NewStatus := Status] # Set new status as the same as current status
  people[Infected == 1, NewStatus := 'I'] # Change infected people to new status as infected
  people[Status != NewStatus, NDays := 0] # Reset NDays in current status counter
  # Move infected people to removed given sufficient number of days
  people[Status == 'I' & NDays >= InfectionDayLimit, NewStatus := 'R']
  people[,NDays := NDays + 1] # Increase number of days in current status
  people[,Status := NewStatus] # Finalize status changes
  
  # Resolve Cases for Mortality in hospitals
  NEligible <- nrow(people[Status == 'I' & Severe == TRUE & Hospital == TRUE & NDays >= MortalityMinDays])
  if(NEligible > 0){
    people[Status == 'I' & Severe == TRUE & Hospital == TRUE & NDays >= MortalityMinDays, Status := ifelse(runif(NEligible) <= MortalityBaseline, 'D', Status)]  
  }
  
  
  # Resolve cases for mortality when not in hostital but should be
  NEligible <- nrow(people[Status == 'I' & Severe == TRUE & Hospital == FALSE & NDays >= MortalityMinDays])
  if(NEligible > 0){
    people[Status == 'I' & Severe == TRUE & Hospital == TRUE & NDays >= MortalityMinDays, Status := ifelse(runif(NEligible) <= MortalityFullHospitals, 'D', Status)]  
  }
  
  
  
  
  
  # Remove people from hospital
  people[Hospital == TRUE & NDays > InfectionDayLimit, Hospital := FALSE]
  people[Severe == TRUE & NDays > InfectionDayLimit, Severe := FALSE]
  people[Status != 'I', Hospital := FALSE]
  people[Status != 'I', Severe := FALSE]
  
  ### Resolve cases to hospital
  # Sample severe cases
  NEligible <- people[Status == 'I' & Severe == FALSE & NDays >= HospitalMinDays] %>% nrow()
  people[Status == 'I' & Severe == FALSE & NDays >= HospitalMinDays, Severe := runif(NEligible) <= HospitalizationRate]
  ## Find number of severe cases and beds by City
  
  # Find number of beds in each city
  cityBeds <- people[,.(numBeds = .N*HospitalBaseline/N), by = i.City]
  setkey(cityBeds, i.City)
  people$numBeds <- NULL
  people <- merge(people, cityBeds, by = 'i.City')
  setkey(people, ID)
  
  
  # Find cumulative sum of Severe cases by City and join back to people
  cumSevere <- people[,.(NSevere = cumsum(Severe),
                         ID = ID), by = i.City]
  setkey(cumSevere, ID)
  people <- cumSevere[people]
  
  # Move people to Hospital if they are severe and NSevere < # of Beds
  people[Status == 'I' & Severe == TRUE & numBeds - NSevere > 0, Hospital := TRUE]
  
  
  # Move Everyone to new locations
  # people[, c("StartingLocationX", "StartingLocationY") := list(rnorm(N, mean = StartingLocationX, sd = MoveSD), rnorm(N, mean = StartingLocationY, sd = MoveSD))]
  if(CommunityCenters){
    people[, atStore := runif(N) <= CommCenterRate]  
  } else{
    people[,atStore := FALSE]
  }
  
  people[, c("StartingLocationX", "StartingLocationY") := list(pmax(-SideLength/2, pmin(SideLength/2, rnorm(N, mean = StartingLocationX, sd = MoveSD))), pmax(-SideLength/2, pmin(SideLength/2, rnorm(N, mean = StartingLocationY, sd = MoveSD))))]
  
  people <- people[, c("ID", "i.City", "Status", "StartingLocationX", "StartingLocationY", "NDays", "atStore", "Severe", "numBeds", "Hospital", "InfectionDayLimit")]
  setnames(people, "i.City", "City")
  if(NCities > 1){
    # Simulate who should move cities
    people[, MoveCity := runif(nrow(people)) <= CityMoveRate]
    # Move people to new cities
    people[, CityIndex := sample(1:(NCities-1), size = nrow(people), replace = TRUE)]
    people[,NewCity := City]
    people[MoveCity == TRUE, NewCity := ifelse(CityIndex < City, CityIndex, CityIndex + 1)]
    people[,City := NewCity]
  }
  
  if(nrow(toInfect) > 0){
    # Calculate Effective Reproduction Numbers globally and by city
    Rglobal <- sum(toInfect$Infected, na.rm = TRUE)/nrow(infected)
    
    InitCity <- infected[,.(numInfected = sum(Status == 'I')), .(City)]
    setkey(InitCity, City)
    InfectCity <- toInfect[,.(numtoInfect = sum(Infected)),.(City)]
    setkey(InfectCity, City)
    RCity <- InfectCity[InitCity]
    RCity[,R := (numtoInfect/numInfected)]
    RCity <- RCity[,c("City","R")]
    
    
    
  } else {
    Rglobal <- 0
    RCity <- data.table(City = 1:NCities,
                        R = 0)
    setkey(RCity, City)
  }
  
  # Calculate summary stats
  sumStats <- getSummaryStats(people)
  sumStats <- c(sumStats, R = Rglobal)
  cityStats <- getCityStats(people)
  cityStats <- merge(cityStats, RCity, by = 'City', all.x = TRUE) 
  cityStats[is.na(R), R := 0]
  
  if(verbose){
    cat('S: ', sumStats['Susceptible'], ' I: ', sumStats['Infected'], ' V: ', sumStats['Severe'], ' R: ', sumStats['Recovered'], ' Reff: ', round(sumStats['R'], 2))
  }
  
  return(list(people = people, summary = sumStats, cities = cityStats))
}

# Create Initial Population
initializePop <- function(N = 15000, BaselineRate = 0.005, SideLength = 50, CommCenterRate = 0, InfectionDays = 7, InfectionDaySD = 1.5, NCities = 1,
                          CityPopShareShape1 = 2, CityPopShareShape2 = 50, HospitalBaseline = N/5){
  people <- tibble(ID = 1:N,
                   InitialInfected = base::sample(c(0,1), size = N, replace = TRUE, prob = c(1-BaselineRate, BaselineRate)),
                   Status = ifelse(InitialInfected == 1, 'I', 'S'),
                   StartingLocationX = runif(N, min = -SideLength/2, max = SideLength/2), 
                   StartingLocationY = runif(N, min = -SideLength/2, max = SideLength/2),
                   atStore = runif(N) <= CommCenterRate, 
                   NDays = 0,
                   Severe = FALSE,
                   Hospital  = FALSE,
                   InfectionDayLimit = rnorm(N, mean = InfectionDays, sd = InfectionDaySD),
                   City = sample(1:NCities, size = N, replace = TRUE, prob = N*prop.table(rbeta(NCities, shape1 = CityPopShareShape1, shape2 = CityPopShareShape2)))
  )
  people <- data.table(people)
  
  
  InitialBeds <- people[,.(numBeds = HospitalBaseline*.N/N), by = City]
  people <- merge(people, InitialBeds, by = "City")
  setkey(people, ID)
  return(people)  
}



## Simulate days
simDays <- function( BaselineRate =  0.005
                     ,InfectionRadius = 1
                     ,InfectionRate = .2
                     ,InfectionDays = 7
                     ,InfectionDaySD = 1
                     ,N = 13000
                     ,SideLength = 100
                     ,MoveSD = 4
                     ,NumDays = 10
                     ,NCities = 3
                     ,CityPopShareShape1 = 2
                     ,CityPopShareShape2 = 60
                     ,CommunityCenters = FALSE
                     ,CommCenterRate = 0
                     ,CityMoveRate = 0
                     ,HospitalBaseline = N/5
                     ,MortalityBaseline = .15 # Mortality rate for cases warranting hospitalization
                     ,MortalityFullHospitals = .3 # Mortality rate when there are no empty beds
                     ,HospitalizationRate = .1 # Rate that cases need hospital beds
                     ,HospitalMinDays = 3
                     ,MortalityMinDays = 5, verbose = FALSE, shiny = FALSE){
  
  
  initPeople <- initializePop(N = N, BaselineRate = BaselineRate, SideLength = SideLength, CommCenterRate = CommCenterRate, InfectionDays = InfectionDays, 
                          InfectionDaySD = InfectionDaySD, NCities = NCities,
                          CityPopShareShape1 = CityPopShareShape1, CityPopShareShape2 = CityPopShareShape2, HospitalBaseline = HospitalBaseline)
  
  # Build data frames to track spread
  DayStats <- data.frame(Day = 1:NumDays, 
                         Susceptible = rep(0, NumDays),
                         Infected = rep(0, NumDays),
                         Recovered = rep(0, NumDays),
                         Dead = rep(0, NumDays),
                         Severe = 0,
                         NumBeds = 0,
                         R = 0)
  CityStats <- bind_rows(lapply(1:NCities, function(x) DayStats)) %>% arrange(Day)
  CityStats$City <- 1:NCities
  CityStats <- CityStats[, c('Day','City','Susceptible','Infected','Recovered','Dead', 'Severe','NumBeds', 'R')]
  
  
  newSim <- list(people = initPeople, summary = getSummaryStats(initPeople), cities = getCityStats(initPeople))
  DayStats[1, -1] <- c(newSim$summary, R = NA)
  CityStats[CityStats$Day == 1, -1] <- cbind(newSim$cities, R = NA)
  
  
  if(!shiny){
    pb <- txtProgressBar(min = 2, max = NumDays, style = 3)
  }
  for(i in 2:NumDays){
    if(shiny){
        incProgress(i, detail = paste("Simulating Day", i, " of ", NumDays))
    } else {
      setTxtProgressBar(pb, i)
    }
    
    newSim <- simDay(newSim$people
                     ,InfectionRadius = InfectionRadius
                     ,InfectionRate = InfectionRate
                     ,N = N
                     ,SideLength = SideLength
                     ,MoveSD = MoveSD
                     ,NCities = NCities
                     ,CommunityCenters = CommunityCenters
                     ,CommCenterRate = CommCenterRate
                     ,CityMoveRate = CityMoveRate
                     ,HospitalBaseline = HospitalBaseline
                     ,MortalityBaseline = MortalityBaseline # Mortality rate for cases warranting hospitalization
                     ,MortalityFullHospitals = MortalityFullHospitals # Mortality rate when there are no empty beds
                     ,HospitalizationRate = HospitalizationRate # Rate that cases need hospital beds
                     ,HospitalMinDays = HospitalMinDays
                     ,MortalityMinDays = MortalityMinDays
                     , verbose = verbose)
    DayStats[i, -1] <- newSim$summary
    CityStats[CityStats$Day == i, -1] <- newSim$cities
  }
  
  return(list(people = newSim$people, DayStats = DayStats, CityStats = CityStats))
}

