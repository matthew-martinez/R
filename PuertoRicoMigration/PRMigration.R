library(tidyverse)
library(tidycensus)


prFlowsFull <- NULL

for (i in 2010:2018){
  prFlows <- get_flows(
    geography="county",
    state="Puerto Rico",
    year = i
  )
  #prFlows$Year <- i
  prFlows <- prFlows %>% 
    filter(variable == "MOVEDOUT") %>%
    mutate(Year = i)
  prFlowsFull <- rbind(prFlowsFull, prFlows)
}