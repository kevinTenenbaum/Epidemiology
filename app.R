library(shiny)


source('Spread_Helper_Functions.R') # Source helper functions



ui <- fluidPage(
  titlePanel("Infectious Disease Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      
      actionButton("Button", "Run Simulation"),
      br(),
      
      numericInput("N", label = "Global Population", value = 10000),
      numericInput("BaselineRate", label = "Probability for each person to begin infected", value = 0.005),
      numericInput("NCities", label = "Number of Cities", value = 1),
      numericInput("SideLength", label = "City Side Length", value = 50),
      numericInput("InfectionRadius", label = "Radius for infection Circle", value = 2),
      numericInput("InfectionRate", label = "Rate that people become infected once exposed", value = .2),
      numericInput("InfectionDays", label = "Average number of days that the infection lasts", value = 7),
      numericInput("InfectionDaySD", label = "Standard Deviation of days the infections lasts", value = 1.5),
      numericInput("MoveSD", label = "Standard Deviation for movement distribution each day for each direction", value = 2),
      checkboxInput("CommunityCenters", label = "Include Community Centers", value = FALSE),
      numericInput("CommCenterRate", label = "Probablity someone visits a community center on a given day", value = 0),
      numericInput("CityMoveRate", label = "Probability someone moves to another city each day", value = 0),
      numericInput("HospitalBaseline", label = "Number of total hospital beds available", value = 3000),
      numericInput("MortalityBaseline", label = "Mortality Rate for severe cases with hospital care", value = 0.15),
      numericInput("MortalityFullHospitals", label = "Mortality Rate for severe cases without hospital care", value = 0.3),
      numericInput("HospitalizationRate", label = "Percent of cases severe enough for hospitalization", value = .1),
      numericInput("HospitalMinDays", label  = "Min days infected before cases can become severe", value = 3),
      numericInput("MortalityMinDays", label = "Min days infected before cases can lead to mortality", value = 5),
      sliderInput("NumDays", label = "Number of Days", min = 1, max = 200, value = 50)
    ),
    mainPanel(
      plotOutput("allPlots")
    )
  )
) 



server <- function(input, output){
  output$allPlots <- renderPlot({
    
    if(input$Button){
    
    isolate({
      withProgress(message = 'Simulating Infections', value = 0, min = 2, max = input$NumDays, {
        simulates <- simDays( BaselineRate =  input$BaselineRate
                              ,InfectionRadius = input$InfectionRadius
                              ,InfectionRate = input$InfectionRate
                              ,InfectionDays = input$InfectionDays
                              ,InfectionDaySD = input$InfectionDaySD
                              ,N = input$N
                              ,SideLength = input$SideLength
                              ,MoveSD = input$MoveSD
                              ,NumDays = input$NumDays
                              ,NCities = input$NCities
                              ,CityPopShareShape1 = 2
                              ,CityPopShareShape2 = 60
                              ,CommunityCenters = input$CommunityCenters
                              ,CommCenterRate = input$CommCenterRate
                              ,CityMoveRate = input$CityMoveRate
                              ,HospitalBaseline = input$HospitalBaseline
                              ,MortalityBaseline = input$MortalityBaseline # Mortality rate for cases warranting hospitalization
                              ,MortalityFullHospitals = input$MortalityFullHospitals # Mortality rate when there are no empty beds
                              ,HospitalizationRate = input$HospitalizationRate # Rate that cases need hospital beds
                              ,HospitalMinDays = input$HospitalMinDays
                              ,MortalityMinDays = input$MortalityMinDays, shiny = TRUE)
      })
      # Pivot metrics for easier plotting
      CityStats <- simulates$CityStats
      CityPivot <- CityStats %>% pivot_longer(cols = Susceptible:Severe, names_to = "Status", values_to = "Count")
      
      DayStats <- simulates$DayStats
      DayPivot <- DayStats %>% pivot_longer(cols = Susceptible:Severe, names_to = "Status", values_to = "Count")
      
      ggplot(CityPivot, aes(x = Day, y = Count, colour = Status)) + geom_line() + facet_wrap(~City) +  
        geom_line(data = CityStats, aes(x = Day, y = NumBeds),colour = 'black', lty = 2) + 
        theme_bw()
    })
    
    } else {
      ggplot(data.frame(x = 0, y= 0,Text = "Adjust parameters and hit button to start simulation..."), aes(x= x, y = y, label = Text)) + 
        geom_text() + xlab("") + ylab("") + theme(panel.background = element_blank(),
                            axis.line = element_blank(),
                            axis.text = element_blank(),
                            axis.ticks = element_blank())
    }
    
  })  
}


shinyApp(ui = ui, server = server)

