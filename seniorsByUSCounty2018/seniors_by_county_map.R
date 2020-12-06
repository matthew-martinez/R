library(tidyverse)
library(tidycensus)
library(highcharter)
library(htmlwidgets)

# loading CSV w/ population data
d <- read.csv("C:\\Users\\mmartinez\\Desktop\\covds\\county map\\countypop60plus_2018.csv")

# cleaning up a couple fips codes and padding them with a leading 0
d$FIPS.Code[d$FIPS.Code == "46102"] <- "46113"
d$FIPS.Code[d$FIPS.Code == "2158"] <- "02270"
d$FIPS.Code <- str_pad(d$FIPS.Code, 5, pad = "0")

d$PopulationAges60andOver2018_formatted <- prettyNum(d$PopulationAges60andOver2018,big.mark=",", preserve.width="none")

quantile(d$PercentAges60andOver)

# downloading high charts data for mapping
mapdata_county <- get_data_from_map(download_map_data("countries/us/us-all-all"))

map_counties <- download_map_data("countries/us/us-all-all")

### Creating breakpoints for the choropleth
d$pop_cat[d$PopulationAges60andOver2018 < 1000] <- "A"
d$pop_cat[d$PopulationAges60andOver2018 >= 1000 & d$PopulationAges60andOver2018 < 10000] <- "B"
d$pop_cat[d$PopulationAges60andOver2018 >= 10000 & d$PopulationAges60andOver2018 < 100000] <- "C"
d$pop_cat[d$PopulationAges60andOver2018 >= 100000] <- "D"

PopulationAges60andOver2018_A <- d %>% filter(pop_cat == "A")
PopulationAges60andOver2018_B <- d %>% filter(pop_cat == "B")
PopulationAges60andOver2018_C <- d %>% filter(pop_cat == "C")
PopulationAges60andOver2018_D <- d %>% filter(pop_cat == "D")

# building high chart map
hc <- highchart(type = "map") %>% 
  hc_plotOptions(map = list(
    allAreas = FALSE,
    joinBy = c("fips", "FIPS.Code"),
    mapData = map_counties
  )) %>% 
  hc_add_series(name = "Fewer than 1,000", data = PopulationAges60andOver2018_A, value="PopulationAges60andOver2018", 
                color = "rgb(169,212,208)", borderWidth=.01) %>% 
  hc_add_series(name = "1,000 to 9,999", data = PopulationAges60andOver2018_B, value="PopulationAges60andOver2018", 
                color = "rgb(131,175,185)", borderWidth=.01) %>% 
  hc_add_series(name = "10,000 to 99,999", data = PopulationAges60andOver2018_C, value="PopulationAges60andOver2018", 
                color = "rgb(88,133,160)", borderWidth=.01) %>% 
  hc_add_series(name = "100,000 or more", data = PopulationAges60andOver2018_D, value="PopulationAges60andOver2018", 
                color = "rgb(51,96,138)", borderWidth=.01) %>% 
  hc_add_series(mapData = map_counties, type = "mapline",
                lineWidth = 1,showInLegend = F) %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "<b>County:</b> {point.name} <br> <b>Population ages 60+:</b> {point.PopulationAges60andOver2018_formatted}") %>%
  hc_title(text="<b>U.S. Population Ages 60 and Older in 2018, by County</b>") %>%
  hc_credits(enabled=T, text="<b>Source: PRB analysis of data from the U.S. Census Bureau, Population Estimates Program.</b>")

hc