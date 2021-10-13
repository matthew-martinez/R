library(tidycensus)
library(stringr)
library(tidyverse)

# State, regions, and divisions from GitHub
states <- read.csv('https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv')

# State and county populations from Census Bureau
statePops <- get_estimates(geography = "state", product = "population", output="wide")
countyPops <- get_estimates(geography = "county", product = "population", output="wide")
metroPops <- get_estimates(geography = "place", product = "population", output="wide")
usPops <- get_estimates(geography = "us", product = "population", output="wide")

# State housing and economic data, ACS 2015-2019
housingDataUs <- get_acs(geography = "us", 
                            variables = c(medIncome = "B19013_001", 
                                          gini="B19083_001", 
                                          rent="B25064_001", 
                                          rentPerc="B25071_001", 
                                          ownPerc = "B25092_001", 
                                          mortPerc="B25092_002", 
                                          noMortPerc ="B25092_003"), 
                            year = 2019,
                            output="wide")

housingDataUs<- cbind(housingDataUs, usPops)

housingDataUs <- housingDataUs %>%
  mutate(totalDiff = rentPercE - ownPercE,
         mortDiff = rentPercE - mortPercE,
         noMortDiff = rentPercE - noMortPercE)

# State housing and economic data, ACS 2015-2019
housingDataState <- get_acs(geography = "state", 
                    variables = c(medIncome = "B19013_001", 
                                  gini="B19083_001", 
                                  rent="B25064_001", 
                                  rentPerc="B25071_001", 
                                  ownPerc = "B25092_001", 
                                  mortPerc="B25092_002", 
                                  noMortPerc ="B25092_003"), 
                    year = 2019,
                    output="wide")

housingDataState <- merge(housingDataState, statePops, by="GEOID")
housingDataState <- merge(states, housingDataState, by.x="State", by.y="NAME")

housingDataState <- housingDataState %>%
  mutate(totalDiff = rentPercE - ownPercE,
         mortDiff = rentPercE - mortPercE,
         noMortDiff = rentPercE - noMortPercE) %>%
  select(-NAME)


# Housing by State, Yearly
housingDataStateYearlyFull <- NULL
i <- 0
for (i in 2006:2019){
housingDataStateYearly <- get_acs(geography = "state", 
                            variables = c(medIncome = "B19013_001", 
                                          gini="B19083_001", 
                                          rent="B25064_001", 
                                          rentPerc="B25071_001", 
                                          ownPerc = "B25092_001", 
                                          mortPerc="B25092_002", 
                                          noMortPerc ="B25092_003"), 
                            year = i,
                            survey="acs1",
                            output="wide")
housingDataStateYearly$Year <- i
housingDataStateYearlyFull <- rbind(housingDataStateYearlyFull, housingDataStateYearly)

}
housingDataStateYearlyFull <- merge(states, housingDataStateYearlyFull , by.x="State", by.y="NAME")

housingDataStateYearlyFull <- housingDataStateYearlyFull %>%
  arrange(State, Year)

# County housing and economic data, ACS 2015-2019
housingDataCounty <- get_acs(geography = "county", 
                            variables = c(medIncome = "B19013_001", 
                                          gini="B19083_001", 
                                          rent="B25064_001", 
                                          rentPerc="B25071_001", 
                                          ownPerc = "B25092_001", 
                                          mortPerc="B25092_002", 
                                          noMortPerc ="B25092_003"), 
                            year = 2019,
                            output="wide")

# Splitting the county and state NAME into two separate columns, County and State
housingDataCounty$County<- str_split(housingDataCounty$NAME,", ") %>% sapply("[", 1)
housingDataCounty$State <- str_split(housingDataCounty$NAME,", ") %>% sapply("[", 2)

housingDataCounty <- merge(states, housingDataCounty, by="State")
housingDataCounty <- merge(housingDataCounty, countyPops, by="GEOID")

housingDataCounty<- housingDataCounty %>%
  mutate(totalDiff = rentPercE - ownPercE,
         mortDiff = rentPercE - mortPercE,
         noMortDiff = rentPercE - noMortPercE)%>% 
  select(County, GEOID, State, Region, Division, POP, DENSITY, medIncomeE,  medIncomeM,  giniE,giniM,rentE,      
         rentM,rentPercE,rentPercM,ownPercE,ownPercM ,mortPercE, mortPercM,  
         noMortPercE,noMortPercM,totalDiff,mortDiff,noMortDiff)

# Metro areas
# County housing and economic data, ACS 2015-2019
housingDataCounty <- get_acs(geography = "county", 
                             variables = c(medIncome = "B19013_001", 
                                           gini="B19083_001", 
                                           rent="B25064_001", 
                                           rentPerc="B25071_001", 
                                           ownPerc = "B25092_001", 
                                           mortPerc="B25092_002", 
                                           noMortPerc ="B25092_003"), 
                             year = 2019,
                             output="wide")

# Splitting the county and state NAME into two separate columns, County and State
housingDataMetro$County<- str_split(housingDataCounty$NAME,", ") %>% sapply("[", 1)
housingDataMetro$State <- str_split(housingDataCounty$NAME,", ") %>% sapply("[", 2)

housingDataMetro <- merge(states, housingDataMetro, by="State")
housingDataMetro <- merge(housingDataMetro, countyPops, by="GEOID")

housingDataMetro<- housingDataMetro %>%
  mutate(totalDiff = rentPercE - ownPercE,
         mortDiff = rentPercE - mortPercE,
         noMortDiff = rentPercE - noMortPercE)%>% 
  select(County, GEOID, State, POP, DENSITY, medIncomeE,  medIncomeM,  giniE,giniM,rentE,      
         rentM,rentPercE,rentPercM,ownPercE,ownPercM ,mortPercE, mortPercM,  
         noMortPercE,noMortPercM,totalDiff,mortDiff,noMortDiff)


write.csv(housingDataStateYearlyFull,"/home/m/Documents/Data/ACSHousing/housingDataStateYearly.csv")
write.csv(housingDataState,"/home/m/Documents/Data/ACSHousing/housingDataState.csv")
write.csv(housingDataCounty,"/home/m/Documents/Data/ACSHousing/housingDataCounty.csv")
