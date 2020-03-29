library(shiny)


source('~/R/Epidemiology/Spread_Helper_Functions.R') # Source helper functions


# Set baseline parameters
InfectionRadius <- 1
InfectionRate <- .2
InfectionDays <- 7
InfectionDaySD <- 1
N <- 15000
SideLength <- 100
MoveSD = 4
NumDays <- 40 
NCities <- 3
CityPopShareShape1 <- 2
CityPopShareShape2 <- 60
# CommunityCente  rs <- 1
CommCenterRate <- 0
CityMoveRate <- 0
HospitalBaseline <- N/5
MortalityBaseline <- .15 # Mortality rate for cases warranting hospitalization
MortalityFullHospitals <- .3 # Mortality rate when there are no empty beds
HospitalizationRate <- .1 # Rate that cases need hospital beds
HospitalMinDays <- 3
MortalityMinDays <- 5

ui <- fluidPage(
  titlePanel("Infectious Disease Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(BaselineRate, label = "Probability for each person to begin infected", value = 0.005)
    ),
    mainPanel(
      plotOutput("allPlots")
    )
  )
) 



server <- function(input, output){
  output$allPlots <- renderPlot({
    BaselineRate <- input$BaselineRate
    # Set baseline parameters
    InfectionRadius <- 1
    InfectionRate <- .2
    InfectionDays <- 7
    InfectionDaySD <- 1
    N <- 15000
    SideLength <- 100
    MoveSD = 4
    NumDays <- 40 
    NCities <- 3
    CityPopShareShape1 <- 2
    CityPopShareShape2 <- 60
    # CommunityCente  rs <- 1
    CommCenterRate <- 0
    CityMoveRate <- 0
    HospitalBaseline <- N/5
    MortalityBaseline <- .15 # Mortality rate for cases warranting hospitalization
    MortalityFullHospitals <- .3 # Mortality rate when there are no empty beds
    HospitalizationRate <- .1 # Rate that cases need hospital beds
    HospitalMinDays <- 3
    MortalityMinDays <- 5
    
    
    simulates <- simDays(NumDays)
    
    # Pivot metrics for easier plotting
    CityStats <- simulates$CityStats
    CityPivot <- CityStats %>% pivot_longer(cols = Susceptible:Severe, names_to = "Status", values_to = "Count")
    
    DayStats <- simulates$DayStats
    DayPivot <- DayStats %>% pivot_longer(cols = Susceptible:Severe, names_to = "Status", values_to = "Count")
    
    ggplot(CityPivot, aes(x = Day, y = Count, colour = Status)) + geom_line() + facet_wrap(~City) +  
      geom_line(data = CityStats, aes(x = Day, y = NumBeds),colour = 'black', lty = 2) + 
      theme_bw()
  })  
}


shinyApp(ui = ui, server = server)

