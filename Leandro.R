#download Leandro data (only need if not working from Github and reading data locally)
#Leandro <- read.csv("~/Downloads/2021LeandroCostEstimateswv.csv")
read.csv("2021Leandrocostestimatesfullfund_wGEOID.csv")
read.csv("2027Leandrocostestimateswfullfunding_wGEOID.csv")
library(sf)
library(dplyr)
#readingin and merging shape file and finance info (no longer need after initial creation)
#county_info <- read.csv("~/Leandro1/2021LeandroCostEstimateswv_wGEOID.csv")
#county_info$GEOID <- as.character(county_info$GEOID)
#county_shape <- read_sf("~/Leandro1/tl_2017_37_unsd.shp")
#county_info %>% full_join(y = county_shape, by = 'GEOID') %>%
  #st_as_sf() %>%
  #sf::st_write("~/Leandro1/output.shp") 

#reading in merged shape file that was written from above
mergedshapeL <- read_sf("output.shp")

#get rid of commas so that R can read as.numeric
install.packages("eeptools")
library(eeptools)
mergedshapeL$Total <- decomma(mergedshapeL$Total)


library(shiny)
library(shinydashboard)
library(leaflet)
library(Rcpp)
library(htmlwidgets)
library(htmltools)

#set color bins for chloropeth mapping
library(RColorBrewer)
pal <- colorNumeric(
  palette = "Blues", 
  domain = mergedshapeL$Total)
library(colorspace)
library(rgdal)




#basic architecture for shiny app (with chloropeth mapping; change this to PPE?)

ui <- fluidPage(#theme="bootstrap.css",
  tags$head(
    tags$style(HTML(" @import url('http://fonts.cdnfonts.com/css/itc-avant-garde-gothic-std-book');
      body {
        background-color: #1E355E;
        color: white;
      }
      /* Change font of header text */
      h2 {
          font-family: 'ITC Avant Garde Gothic Std Bold', sans-serif;
      }
      /* Make text visible on inputs */
      .shiny-input-container {
        color: #F99D20;
      }"))
  ),
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: #1E355E;
    font-weight: bold;
    font-size: 18px;
  }
")),
  
  title <- tags$div(
    tag.map.title, HTML("To see allocations broken down, click your district")
  ),
  titlePanel(title="Leandro Budget Tool"),
  sidebarLayout(
    sidebarPanel(
      helpText("View your districts anticipated funding
               by current or projected ('27-28) year"),
      
      selectInput("var", 
                  label = "Choose a year to display",
                  choices = list("Current", 
                                 "Current with Leandro",
                                 "2027-2028 Projected", 
                                 "2027-2028 with Leandro"),
                  selected = "Current")),
                mainPanel(leaflet(mergedshapeL) %>%
                            addProviderTiles("MapBox", options = providerTileOptions(
                              id = "mapbox.light",
                              accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                            addPolygons(fillColor = ~pal(Total),
                                        weight = .5,
                                        opacity = 1,
                                        color = "black",
                                        fillOpacity = 0.7,
                                        popup = paste(mergedshapeL$NAME, "<br>",
                                                      "Textbooks", mergedshapeL$Textbks, "<br>",
                                                      "At-risk", mergedshapeL$At_Risk, "<br>",
                                                      "Classroom Supply/Mat.", mergedshapeL$C_S___M, "<br>",
                                                      "LEP", mergedshapeL$Lmt_E_P, "<br>",
                                                      "Chi. w/ Dis.", mergedshapeL$Chld__D))
                                        %>%
                                          addLegend(pal = pal, values = ~Total, opacity = 0.7, title = 'Total 21-22 Funding',
                                                    position = "bottomright") %>%
                            addTiles() %>%
                            addControl(title, position = "topleft", className="map-title"),
                         
                          
                          
                          ),))

server <-function(input, output){}

shinyApp(ui, server)

#run shiny connect to publish

install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='sarahpedonti',
                          token='0C7EF3A322A741CDDD35E5366C22897F',
                          secret='JrUrKknfyeFCs0br4ytrYGg9T6HAikB1nBlo+f1o')
#to deploy
library(rsconnect)
rsconnect::deployApp('path/to/your/app')





#setting up a continuous color palette for orange
#f99d20
#F9A636
#FAB04C
#FABA62
#FBC479
#FBCE8F
#FCD7A5
#FDE1BC
#FDEBD2
#FEF5E8
#FEFFFF

#setting up a continuous color palette for green
#08ad8d
#20B598
#39BDA3
#52C5AF
#6ACDBA
#83D5C6
#9CDED1
#B4E6DC
#CDEEE8
#E6F6F3
#FEFEFF



#colors for quick references
  #navy = "#1E355E",
  #green = "#08AD8D",
  #orange = " #F99D20",



