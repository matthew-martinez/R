# Title: DC map for web article
# Author: Matthew Martinez
# Created: 06/23/2020
# Last updated: 06/23/2020

library(tidycensus)
library(leaflet)
library(tigris)
library(sp)
library(tidyverse)
library(rgeos)
library(maptools)
library(mapproj)
library(htmlwidgets)
library(jsonlite)
library(ggspatial)
library(rgdal)
library(openxlsx)

# making stable directories for loading/saving
table_dir <- paste0("C:\\Users\\mmartinez\\Desktop\\census_undercount\\Response Rate\\Output\\Tables\\")
map_dir <- paste0("C:\\Users\\mmartinez\\Desktop\\census_undercount\\Response Rate\\value add\\web article\\")

# data loading, merging, cleanup, and reorganizing
d2 <- read.xlsx("C:\\Users\\mmartinez\\Desktop\\census_undercount\\Response Rate\\Output\\Tables\\2020-06-25_response_rate_full.xlsx")

survey_date <- d2$RESP_DATE[1]

tract_filter <- read.csv("C:\\Users\\mmartinez\\Desktop\\census_undercount\\Response Rate\\Data\\decennialrr2020_crosswalkfile.csv", stringsAsFactors = FALSE)
tract_filter <- tract_filter %>%
  filter(Geo_Type == "Census Tract")

d_tracts <- right_join(d2, tract_filter, by="GEO_ID")

d_tracts <- separate(d_tracts, GEO_ID, c("GEOID10", "GEOID20"), sep = "US")

riskcats <- read.csv("C:\\Users\\mmartinez\\Desktop\\census_undercount\\Response Rate\\Data\\HTCmap_2020Boundaries_20200527.txt", stringsAsFactors=FALSE)
riskcats$GEOID20 <- as.character(str_pad(riskcats$GEOID20, 11, pad="0"))

riskcats2 <- riskcats %>%
  filter(largecnty == 1)

response_rates <- inner_join(d_tracts, riskcats, by="GEOID20")

response_rates$risk_cat[response_rates$YC_UNDERCNT_RISK2020 == 0] <- "Unknown"
response_rates$risk_cat[response_rates$YC_UNDERCNT_RISK2020 == 1] <- "Low"
response_rates$risk_cat[response_rates$YC_UNDERCNT_RISK2020 == 2] <- "High"
response_rates$risk_cat[response_rates$YC_UNDERCNT_RISK2020 == 3] <- "Very High"

rr_check <- response_rates

response_rates2 <- response_rates %>%
  filter(largecnty == 1)

response_rates <- response_rates %>%
  filter(largecnty == 1) %>% filter(risk_cat != "Unknown")

# determining breaks on the map
rr_sd <- round(sd(response_rates2$CRRALL, na.rm=TRUE))
rr_mean <- round(mean(response_rates2$CRRALL, na.rm=TRUE))
sdDOWN <- round(mean(response_rates2$CRRALL, na.rm=TRUE)) - round(sd(response_rates2$CRRALL, na.rm=TRUE))
sdDOWNDOWN <- round(mean(response_rates2$CRRALL, na.rm=TRUE)) - round(sd(response_rates2$CRRALL, na.rm=TRUE)) - round(sd(response_rates2$CRRALL, na.rm=TRUE))
sdUP <- round(mean(response_rates2$CRRALL, na.rm=TRUE)) + round(sd(response_rates2$CRRALL, na.rm=TRUE))

response_rates$crrall_perc_cat[response_rates$CRRALL < sdDOWN & response_rates$risk_cat == "Very High"] <- "Very high risk of child undercount - Very low response rate (<49%)"
response_rates$crrall_perc_cat[response_rates$CRRALL < rr_mean & response_rates$CRRALL >= sdDOWN & response_rates$risk_cat == "Very High"] <- "Very high risk of child undercount - Lower response rate (49% to 62.99%)"
response_rates$crrall_perc_cat[response_rates$CRRALL < rr_mean & response_rates$risk_cat == "High"] <- "High risk of child undercount - Lower response rate (<63%)"
response_rates$crrall_perc_cat[response_rates$CRRALL < rr_mean & response_rates$risk_cat == "Low"] <- "Low risk of child undercount - Lower response rate (<63%)"
response_rates$crrall_perc_cat[response_rates$CRRALL >= rr_mean] <- "Higher response rate (63% or more)"

response_rates$crrall_perc_cat <- factor(response_rates$crrall_perc_cat, levels=(c("Very high risk of child undercount - Very low response rate (<49%)",
                                                                                   "Very high risk of child undercount - Lower response rate (49% to 62.99%)", 
                                                                                   "High risk of child undercount - Lower response rate (<63%)",
                                                                                   "Low risk of child undercount - Lower response rate (<63%)",
                                                                                   "Higher response rate (63% or more)")))

response_rates$GEOID <- paste0(response_rates$GEOID10, "US", response_rates$GEOID20)

# Loading 2020 Shape File and joining it with the above data frame
tracts_us <- readOGR(dsn="./Shapefiles/Census Tracts 2020", layer="tract_bas20_sr_500k", stringsAsFactors=FALSE)

tracts_us <- spTransform(tracts_us, CRS("+init=epsg:4326"))

response_rates_dc <- response_rates %>%
  filter(State == "District of Columbia")

tracts_data_dc <- geo_join(tracts_us, response_rates_dc, "GEO_ID", "GEOID", how="inner")

# setting options for the leaflet map
popup_crrall_map <- paste0("Tract FIPS: ", tracts_data_dc$GEOID20, "<br>",
                           "Risk of undercounting young children: ", tracts_data_dc$risk_cat, "<br>",
                           "2020 Census self-response rate: ", tracts_data_dc$CRRALL, "%<br>",
                           "Children ages 0-4: ", formatC(tracts_data_dc$rpop04_1418_2020, big.mark=","))

pal <- colorFactor(palette = c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#92c5de"),
                   levels=c("Very high risk of child undercount - Very low response rate (<49%)",
                            "Very high risk of child undercount - Lower response rate (49% to 62.99%)", 
                            "High risk of child undercount - Lower response rate (<63%)",
                            "Low risk of child undercount - Lower response rate (<63%)",
                            "Higher response rate (63% or more)"), ordered=TRUE, reverse=FALSE, na.color="transparent")

# building the leaflet map
crrall_map_dc <-leaflet() %>%
  addProviderTiles("CartoDB.Positron", group = "Positron") %>%
  addPolygons(data = tracts_data_dc, 
              fillColor = ~pal(tracts_data_dc$crrall_perc_cat), 
              color = "#757373",
              fillOpacity = 0.55, 
              weight = 0.4, 
              smoothFactor = 0.2,
              popup = popup_crrall_map, group = "High Risk") %>%
  addLegend(pal = pal, 
            values = tracts_data_dc$crrall_perc_cat, 
            position = "bottomright")

css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- htmltools::tags$style(type = "text/css", css_fix)
crrall_map_dc <- crrall_map_dc %>% htmlwidgets::prependContent(html_fix)
crrall_map_dc
saveWidget(crrall_map_dc, file=paste0(map_dir, survey_date, "-DC_map.html"), selfcontained=TRUE, title="Response Rates - DC")