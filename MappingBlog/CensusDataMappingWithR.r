###############################
##### Spatial Analysis in R
##### Matthew Martinez
##### PRB
##### October 21, 2021
###############################

library(tidyverse)
library(tidycensus)
library(mapview)
library(leaflet)
library(htmlwidgets)
library(htmltools)

###### Data extraction #######

# extract data from U.S. Census Bureau API with tidycensus
# tidycensus allows you to easily pull in Census data along with geometric data for mapping
# get_acs pulls in ACS data from the API.

dataNoSpatial <- get_acs(geography="tract",
                state="DC",
                variables=c(
                  medianIncome="B19013_001"),
                year = 2019,
                output="wide")

head(dataNoSpatial) # display the first five rows of the median income data

# the data returned from tidycensus provides the GEOID, name of location, and the estimate and margin of error for each requested table

# adding the argument geometry=TRUE will also pull in the underlying geometric data for the data request
# this will allow for easily mapping US Census data

data <- get_acs(geography="tract",
                  state="DC",
                  variables=c(
                    medianIncome="B19013_001"),
                  year = 2019,
                  output="wide",
                  geometry=TRUE) # specify TRUE on geometry to get the corresponding geometry for your data

# recode medianIncomeE into categories
# obtain sample quantiles to determine the breakpoints for our income categories
quantile(data$medianIncomeE, probs=seq(0, 1, 1/4), na.rm=T)

# recoding the median income variable into four income categories, based on quantiles
data$medIncCat[data$medianIncomeE < 49000] <- "< $49,000"
data$medIncCat[data$medianIncomeE >= 49000 & data$medianIncomeE < 94000] <- "$49,000 - $93,999"
data$medIncCat[data$medianIncomeE >= 94000 & data$medianIncomeE < 120000] <- "$94,000 - $119,999"
data$medIncCat[data$medianIncomeE >= 120000] <- "> $119,999"

# turn the categories into an ordered factor variable
data$medIncCat <- factor(data$medIncCat, levels=(c("< $49,000",
                                                   "$49,000 - $93,999", 
                                                   "$94,000 - $119,999",
                                                   "> $119,999")))



####### Mapview ########

# using mapview to create a quick map of our data

mapviewOptions(fgb = FALSE) # setting an option for mapview due to a pandoc error that occurs when trying to save the mapview object

mapviewMap <- mapview(data, zcol=c("medianIncomeE"), legend = TRUE, hide = TRUE) # building the mapview map object

# the zcol argument with concatenated variable names lets you plot as many variables on one map as possible
# you are able to switch between the variable displayed using the layers button on the left side of the map.

mapviewMap # viewing the mapview map

# saveWidget from the htmlwidgets package lets you save the mapview object as an .html file in a line of code
# selfcontained=TRUE will make the .html file include all additional code (JavaScript) without it, additional JS code will be saved outside the .html file
# @map needs to be appended to the end of the mapview object name to save it using saveWidget
saveWidget(mapviewMap@map, file="medianIncomeMapview.html", selfcontained=TRUE, title="Mapview - Median Income in Washington D.C.")



###### Leaflet ######

# using leaflet to create a map of our data

# load in tracts using readOGR (rgdal package)
# tracts <- readOGR(dsn="PATH", layer="FILENAME", stringsAsFactors=FALSE)

# using geo_join (tigris package) function from tigris to merge shapefile object (tracts) with ACS data (data)
# tractsDataMerge <- geo_join(tracts, data, "GEOID", "GEOID", how="left")

# creating mouse over labels for the map, this information will show up when the user hovers over a census tract
labels <- sprintf(
  "<strong> %s </strong><br/><strong>Median Income:</strong> $%s",
  data$NAME, prettyNum(data$medianIncomeE, big.mark=",")) %>% 
  lapply(htmltools::HTML)

# create colors for our income categories and the legend
pal <- colorFactor(palette = c("#bae4bc", "#7bccc4", "#2b8cbe", "#0868ac"), # hex colors used
                   levels=c("< $49,000",
                            "$49,000 - $93,999", 
                            "$94,000 - $119,999",
                            "> $119,999"), # income categories, these match the category labels made earlier
                   ordered=TRUE, # labels are in order (small to large)
                   reverse=T, # reverse the color and labels correspondence
                   na.color="transparent")

# creating a leaflet map
leafletMap <-leaflet() %>%
  addProviderTiles("CartoDB.Positron", group = "Positron") %>% # choosing the basemap
  addPolygons(data = data, # adding the sf object where our data (income data and geometric data)
              fillColor = ~pal(data$medIncCat), # selecting the column with our income categories to fill in polygons
              color = "#5e5c5c", # border color - darkish grey
              fillOpacity = 0.55,
              weight = 0.4, # border weight
              smoothFactor = 0.2,
              label = labels, # hover over label using the labels list earlier
              highlightOptions = highlightOptions( # highlight polygon on mouse over
                color="#666", # darker border
                bringToFront = T, # bring it to the front
                weight = 2 # thicken border on mouse over
              )) %>% # selecting the popup text created earlier to display when a polygon is clicked
  addLegend(pal = pal, # adding legend, using the pal object from above with our colors and categories
            values = data$medIncCat, # selecting the values for the legend, same as the fillColor above
            position = "bottomright")

# display the leaflet map
leafletMap

# code for saving the .html file
saveWidget(leafletMap, file="medianIncomeLeaflet.html", selfcontained=TRUE, title="Leaflet - Median Income in Washington D.C.")
