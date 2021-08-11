library(shiny)
#download Leandro data
Leandro <- read.csv("~/Downloads/2021LeandroCostEstimateswv.csv")
library(shinydashboard)
#basic architecture for shiny app

ui <- dashboardPage(
  dashboardHeader(title = "Leandro Budget Tool"),
  dashboardSidebar(),
  dashboardBody(leaflet(mergedshapeL) %>%
                  addProviderTiles("HikeBike.HikeBike") %>%
                  addPolygons(fillColor = "white",
                              color = "black",
                              weight = 0.5, 
                              popup = paste(mergedshapeL$NAME, "<br>",
                                            "Textbooks", mergedshapeL$Textbks, "<br>",
                                            "At-risk", mergedshapeL$At_Risk, "<br>",
                                            "Classroom Supply/Mat.", mergedshapeL$C_S___M, "<br>",
                                            "LEP", mergedshapeL$Lmt_E_P, "<br>",
                                            "Chi. w/ Dis.", mergedshapeL$Chld__D))),)

server <-function(input, output){}


shinyApp(ui, server)

# map example (built on the standard North Carolina shapefile):

library(leaflet)
library(dplyr)
library(Rcpp)
library(sf)

# NC counties - a shapefile shipped with the sf package
#shape <- st_read("~/Users/sarahpedonti/Downloads/tl_2017_37_unsd/tl_2017_37_unsd.shp") %>% 
#st_transform(shape, crs = 4326)
#another way 
#aoi_boundary_NCDPI <- st_read(
#"~/Downloads/tl_2017_37_unsd/tl_2017_37_unsd.shp")

library(ggplot2)
ggplot() + 
  geom_sf(data = aoi_boundary_NCDPI, size = 1, color = "grey", fill = "blue") + 
  ggtitle("NCPI Boundary Plot") + 
  coord_sf()

#a third way #THIS WORKS, USE THIS
library(tigris)
library(leaflet)

schools <- school_districts("North Carolina")

schools <- write.csv(schools, "schools.csv")

#get simple maps with popup for districtname
leaflet(schools) %>%
  addProviderTiles("HikeBike.HikeBike") %>%
  addPolygons(fillColor = "white",
              color = "black",
              weight = 0.5, 
              popup = paste("LEA", schools$NAME, "<br>",
                            "Lowest Grade", schools$LOGRADE))

# ProviderTiles options: "CartoDB.Positron", "Thunderforest.Landscape", "HikeBike.HikeBike"

#now try to merge shapefile and LEANDRO so pop up boxes will have info
#first need to add word "schools" to end of LEA in Leandro file (just did in Excel)

#now merge shapefile and budget numbers for pop ups.THIS DOESN'T WORK

mergedshapeL <- merge(Leandro, schools, by.x= "LEA", by.y= "NAME")

# tryinganotherway STILL NOT WORKING
#library(rgdal)
#Leandro <- read.csv("~/Downloads/2021LeandroCostEstimateswv.csv")
#myspdf <- readOGR("~/Downloads/tl_2017_37_unsd/tl_2017_37_unsd.shp")
#mergedshapeL <- merge(Leandro, myspdf, by.x= "LEA", by.y= "NAME")

library(raster)

raster::shapefile(mergedshapeL, "mergedshapeL.shp")

county_info <- read.csv("~/Downloads/2021LeandroCostEstimateswv_wGEOID.csv")
county_shape <- read_sf("~/Downloads/tl_2017_37_unsd/tl_2017_37_unsd.shp")


library(sf)
library(dplyr)

county_info <- read.csv("~/rStuff/Leandro/2021LeandroCostEstimateswv_wGEOID.csv")
county_info$GEOID <- as.character(county_info$GEOID)
county_shape <- read_sf("~/Downloads/tl_2017_37_unsd/tl_2017_37_unsd.shp")
county_info %>% full_join(y = county_shape, by = 'GEOID') %>%
  st_as_sf() %>%
  sf::st_write("~/Downloads/output.shp") 

mergedshapeL <- read_sf("~/Downloads/output.shp")

#save merge back out as shapefile for leaflet to read
#library(raster)
s#hapefile(mergedshapeL, "path/merged.shp")

#now get map with additional finance info in popups
leaflet(mergedshapeL) %>%
  addProviderTiles("HikeBike.HikeBike") %>%
  addPolygons(fillColor = "white",
              color = "black",
              weight = 0.5, 
              popup = ~NAME)
