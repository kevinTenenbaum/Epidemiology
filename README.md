---
output: 
  html_document:
    keep_md: true
---


In an effort to better understand the spread of diseases like COVID-19, I built a simulation model that tries to create a semi-realistic agent-based simulation for the spread of disesases. it relies on a SIHRD model (Susceptible, Infected, Hospitalized, Recovered, Dead). It takes in a number of custom paramters that you can alter in the accompanying shiny app to see how they impact disease growth/spread. These variables include:

* **N**: Population Size
* **Baseline** Rate: Percentage of population infected originally
* **Infection Radius**: The distance that you need to come within an infected person to run chance of becoming infected.
* **InfectionRate**: The probability you become infected if you are exposed to an infected person
* **InfectionDays**: The average number of days the infection lasts.
* **InfectionDaysSD**: The standard deviation for distribution of infection length.
* **SideLength**: Each "city" is a square with this side length
* **MoveSD**: Standard deviation of normal distribution centered at 0 that generates agent movement in the space each day.
* **NumDays**: Number of days in the simulation
* **CityPopShareShape1**, **CityPopShareShape2**: City population shares of N are determined by a beta distribution determined by these parameters.
* **Community Centers**: The number of local gathering places in each city
* **CommCenterRate**: The probability that a person visits the community center rather than their typical random movement each day.
* **CityMoveRate**: The probablity that a person moves to another random city on a given day.
* **HospitalBaseline**: The number of hospital beds in the simulation
* **MortalityBaseline**: Baseline mortality rate for the disease when not in the hospital
* **MortalityFullHospitals**: Mortality rate for the disease when hospitals are full
* **HospitalMinDays**: The minimum number of days that must have passed before someone can be severe enough to be admitted to the hospital.
* **MortalityMinDays**: The minimum number of days that must have passed before someone can be severe enough to die from disease.



