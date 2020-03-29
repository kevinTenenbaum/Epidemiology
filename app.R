library(shiny)


source('~/R/Epidemiology/Spread_Helper_Functions.R') # Source helper functions



ui <- fluidPage(
  titlePanel("Infectious Disease Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N", label = "Global Population", value = 10000),
      numericInput("BaselineRate", label = "Probability for each person to begin infected", value = 0.005),
      numericInput("NCities", label = "Number of Cities", value = 1),
      numericInput("SideLength", label = "City Side Length", value = 50),
      numericInput("InfectionRadius", label = "Radius for infection Circle", value = 2),
      sliderInput("NumDays", label = "Number of Days", min = 1, max = 200, value = 50),
      br(),
      actionButton("Button", "Run Simulation")
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
                              ,InfectionRate = .2
                              ,InfectionDays = 7
                              ,InfectionDaySD = 1
                              ,N = input$N
                              ,SideLength = input$SideLength
                              ,MoveSD = 4
                              ,NumDays = input$NumDays
                              ,NCities = input$NCities
                              ,CityPopShareShape1 = 2
                              ,CityPopShareShape2 = 60
                              ,CommunityCenters = FALSE
                              ,CommCenterRate = 0
                              ,CityMoveRate = 0
                              ,HospitalBaseline = input$N/5
                              ,MortalityBaseline = .15 # Mortality rate for cases warranting hospitalization
                              ,MortalityFullHospitals = .3 # Mortality rate when there are no empty beds
                              ,HospitalizationRate = .1 # Rate that cases need hospital beds
                              ,HospitalMinDays = 3
                              ,MortalityMinDays = 5, shiny = TRUE)
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

