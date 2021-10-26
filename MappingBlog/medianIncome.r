library(tidyverse)
library(tidycensus)
library(tigris)
library(mapview)
library(leaflet)
library(rgdal)
library(htmlwidgets)

# extract data from U.S. Census Bureau API with tidycensus
data <- get_acs(geography="tract",
                   state="DC",
                   variables=c(
                     medianIncome="B19013_001",
                     gini="B19083_001"
                   ),
                   year = 2019,
                   output="wide",
                  geometry=TRUE)

# recode medianIncomeE into categories

quantile(data$medianIncomeE, probs=seq(0, 1, 1/4), na.rm=T)

data$medIncCat[data$medianIncomeE < 49000] <- "< $49,000"
data$medIncCat[data$medianIncomeE >= 49000 & data$medianIncomeE < 94000] <- "$49,000 - $93,999"
data$medIncCat[data$medianIncomeE >= 94000 & data$medianIncomeE < 120000] <- "$94,000 - $119,999"
data$medIncCat[data$medianIncomeE >= 120000] <- "> $119,999"


data$medIncCat <- factor(data$medIncCat, levels=(c("< $49,000",
                                                   "$49,000 - $93,999", 
                                                   "$94,000 - $119,999",
                                                   "> $119,999")))

# create map using mapview
mapviewMap <- mapview(data, zcol=c("medianIncomeE", "giniE"), legend = TRUE, hide = TRUE)

mapviewMap

saveWidget(mapviewMap, file=".//Projects//blog//medianIncomeMapview.html", selfcontained=TRUE, title="Median Income in Washington D.C.") # code for saving the HTML file

# load in tracts
# tracts <- readOGR(dsn="./Projects/blog/dc_tracts", layer="tl_2019_11_tract", stringsAsFactors=FALSE)

# using geo_join function from tigris to merge shapefile object (tracts) with ACS data (data)
# tractsDataMerge <- geo_join(tracts, data, "GEOID", "GEOID", how="left")

# create map using leaflet
popup <- paste0("Tract FIPS: ", data$GEOID, "<br>",
                "Median Income: ", data$medianIncomeE)

# create corresponding legend colors
pal <- colorFactor(palette = c("#bae4bc", "#7bccc4", "#2b8cbe", "#0868ac"),
                   levels=c("< $49,000",
                            "$49,000 - $93,999", 
                            "$94,000 - $119,999",
                            "> $119,999"), 
                   ordered=TRUE, 
                   reverse=T, 
                   na.color="transparent")

# creating a leaflet map
leafletMap <-leaflet() %>%
  addProviderTiles("CartoDB.Positron", group = "Positron") %>% # choosing the basemap
  addPolygons(data = data, # adding the SpatialPolygonsDataFrame
              fillColor = ~pal(data$medIncCat),# selecting the column with our income categories to fill in polygons
              color = "#757373",
              fillOpacity = 0.55, 
              weight = 0.4, 
              smoothFactor = 0.2,
              popup = popup) %>% # selecting the popup text created earlier to display when a polygon is clicked
  addLegend(pal = pal, # adding legend, using the pal object from above with our colors and categories
            values = data$medIncCat, # selecting the values for the legend, same as the fillColor above
            position = "bottomright")

css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- htmltools::tags$style(type = "text/css", css_fix)
leafletMap <- leafletMap %>% htmlwidgets::prependContent(html_fix)
leafletMap

saveWidget(leafletMap, file=".//Projects//blog//medianIncomeLeaflet.html", selfcontained=TRUE, title="Median Income in Washington D.C.") # code for saving the HTML file
