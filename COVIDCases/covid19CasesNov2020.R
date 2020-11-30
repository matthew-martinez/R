library(tidyverse)
library(plotly)
library(maps)

# Read in CSV from the NYT GitHub
d <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", stringsAsFactors = FALSE)

d$fips <- str_pad(d$fips, 5, pad="0")

d$fips[d$county == "New York City"] <- "36061"

d_grouped_cases <- d %>%
  group_by(county, fips) %>%
  filter(date == d$date[dim(d)[1]])

gcounty <- map_data("county")

fipstab <-
  transmute(maps::county.fips, fips, county = sub(":.*", "", polyname)) %>%
  unique() %>%
  separate(county, c("region", "subregion"), sep = ",")

fipstab$fips <- str_pad(fipstab$fips, 5, pad="0")

gcounty <- left_join(gcounty, fipstab, c("region", "subregion"))

# Creating centroids for point placements on the map using the long and lats from the data set
county_centroids <- summarize(group_by(gcounty, fips),
                              x = mean(range(long)), y = mean(range(lat)))

d_grouped_cases_ll <- left_join(d_grouped_cases, county_centroids, by="fips", keep.x=TRUE)

# Manually placing points for locations without available data
d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Fairbanks North Star Borough"] <- -147.7164
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Fairbanks North Star Borough"] <- 64.8378

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Alexandria city"] <- -77.0469
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Alexandria city"] <- 38.8048

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Anchorage"] <- -149.9003
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Anchorage"] <- 61.2181
  
d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Bethel Census Area"] <- -160.8641
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Bethel Census Area"] <- 61.0934
  
d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Bristol city"] <- -82.1887
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Bristol city"] <- 36.5951

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Buena Vista city"] <- -79.3539
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Buena Vista city"] <- 37.7343

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Charlottesville city"] <- -78.4767
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Charlottesville city"] <- 38.0293

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Chesapeake city"] <- -76.2875
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Chesapeake city"] <- 36.7682

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Covington city"] <- -77.4103
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Covington city"] <- 37.2440

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Danville city"] <- -79.3950
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Danville city"] <- 36.5860

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Emporia city"] <- -77.5425
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Emporia city"] <- 36.6860

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Fairfax city"] <- -77.3064
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Fairfax city"] <- 38.8462

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Falls Church city"] <- -77.1711
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Falls Church city"] <- 38.8823

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Franklin city"] <- - 76.9225
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Franklin city"] <- 36.6777

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Galax city"] <- -80.9240
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Galax city"] <- 36.6612

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Harrisonburg city"] <- -78.8689
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Harrisonburg city"] <- 38.4496

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Hopewell city"] <- -77.2872
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Hopewell city"] <- 37.3043
  
d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Lexington city"] <- -79.4428
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Lexington city"] <- 37.7840

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Lynchburg city"] <- -79.1422
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Lynchburg city"] <- 37.4138

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Manassas city"] <- -77.4753
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Manassas city"] <- 38.7509

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Manassas Park city"] <- -77.4697
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Manassas Park city"] <- 38.7840

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Norton city"] <- -82.6290
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Norton city"] <- 36.9334

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Petersburg city"] <- -77.4019
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Petersburg city"] <- 37.2279

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Poquoson city"] <- -76.3458
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Poquoson city"] <- 37.1224

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Portsmouth city"] <- -76.2983
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Portsmouth city"] <- 36.8354

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Radford city"] <- -80.5764
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Radford city"] <- 37.1318

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Richmond city"] <- -77.4360
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Richmond city"] <- 37.5407

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Roanoke city"] <- -79.9414
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Roanoke city"] <- 37.2710

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Honolulu"] <- -157.8581
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Honolulu"] <- 21.3156

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Juneau City and Borough"] <- -134.4197
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Juneau City and Borough"] <- 58.3019

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Kansas City"] <- -94.5786
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Kansas City"] <- 39.0997

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Kauai"] <- -159.5261
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Kauai"] <- 22.0964

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Kenai Peninsula Borough"] <- -151.3823
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Kenai Peninsula Borough"] <- 60.0858

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Ketchikan Gateway Borough"] <- -131.3893
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Ketchikan Gateway Borough"] <- 55.5103

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Kodiak Island Borough"] <- -153.2659
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Kodiak Island Borough"] <- 57.5369

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Matanuska-Susitna Borough"] <- -150.1729
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Matanuska-Susitna Borough"] <- 62.2599

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Maui"] <- -156.3319
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Maui"] <- 20.7984

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Nome Census Area"] <- -164.2566
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Nome Census Area"] <- 64.8330

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Oglala Lakota"] <- -102.6216
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Oglala Lakota"] <- 43.2437

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Petersburg Borough"] <- -132.9556
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Petersburg Borough"] <- 56.8125

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Prince of Wales-Hyder Census Area"] <- -132.6045
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Prince of Wales-Hyder Census Area"] <- 55.3242

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Salem city"] <- -80.0548
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Salem city"] <- 37.2935

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Sitka City and Borough"] <- -135.3300
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Sitka City and Borough"] <- 57.0531

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Southeast Fairbanks Census Area"] <- -143.6297
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Southeast Fairbanks Census Area"] <- 63.9604

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Staunton city"] <- -79.0717
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Staunton city"] <- 38.1496

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Waynesboro city"] <- -78.8895
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Waynesboro city"] <- 38.0685

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Williamsburg city"] <- -76.7075
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Williamsburg city"] <- 37.2707

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Winchester city"] <- -78.1633
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Winchester city"] <- 39.1857

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Yukon-Koyukuk Census Area"] <- -153.4303
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Yukon-Koyukuk Census Area"] <- 65.8444

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Colonial Heights city"] <- -77.4103
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Colonial Heights city"] <- 37.2440

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Fredericksburg city"] <- -77.4605
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Fredericksburg city"] <- 38.3032

d_grouped_cases_ll$x[d_grouped_cases_ll$county == "Hawaii"] <- -155.475761
d_grouped_cases_ll$y[d_grouped_cases_ll$county == "Hawaii"] <- 19.666365

# Creating settings for the map using Plotly
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  showlakes = TRUE,
  lakecolor = toRGB('white'),
  landcolor = "#DBE9EE",
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

# Plotly map
fig <- plot_geo(d_grouped_cases_ll, locationmode = 'USA-states', sizes = c(2, 3500),
                marker = list(color = 'rgb(82, 154, 166)', line = list(width = 0, color = 'rgb(82, 154, 166)')))
fig <- fig %>% add_markers(
  x = ~x, y = ~y, size = ~cases, hoverinfo = "text", hoverlabel=list(bgcolor="#FFFFFF"),
  text = ~paste(d_grouped_cases_ll$county, "<br />", prettyNum(d_grouped_cases_ll$cases,big.mark=",",scientific=FALSE), "cases")) %>% 
  # add_text(x=-87.6298, y=41.8781, mode="text", text="Chicago", hoverinfo="none", marker = list(opacity=.1),
  #          textfont=list(family="Roboto Condensed", size=12, color="#000000")) %>% 
  # add_text(x=-80.1918, y=25.7617, mode="text", text="Miami", hoverinfo="none",marker = list(opacity=.1),textposition="bottomright",
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-122.4194, y=37.7749, mode="text", text="San Francisco", hoverinfo="none",marker = list(opacity=.1),textposition="topleft",
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-122.3321, y=47.6062, mode="text", text="Seattle", hoverinfo="none",marker = list(opacity=.1),textposition="topleft",
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-74.0060, y=40.7128, mode="text", text="New York City", hoverinfo="none",marker = list(opacity=.1),
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-95.3698, y=29.7604, mode="text", text="Houston", hoverinfo="none",marker = list(opacity=.1),textposition="bottomright",
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-96.7970, y=32.7767, mode="text", text="Dallas", hoverinfo="none",marker = list(opacity=.1),textposition="topright",
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-83.0458, y=42.3314, mode="text", text="Detroit", hoverinfo="none",marker = list(opacity=.1),
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-118.2437, y=34.0522, mode="text", text="Los Angeles", hoverinfo="none",marker = list(opacity=.1),textposition="bottomleft",
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-90.0715, y=29.9511, mode="text", text="New Orleans", hoverinfo="none",marker = list(opacity=.1),textposition="bottomright",
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>% 
  # add_text(x=-77.0369, y=38.9072, mode="text", text="D.C.", hoverinfo="none",marker = list(opacity=.1),
  #          textfont=list(family="Roboto Condensed", size=12, color="#00467F")) %>%
  layout(geo = g, showlegend=FALSE, title="Coronavirus Cases Across the United States, 11/29/2020", font="Roboto Condensed") %>% config(displayModeBar = F,
                                                                                                                            scrollZoom=FALSE)

fig