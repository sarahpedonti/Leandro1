#download Leandro data (only need if not working from Github and reading data locally)
#Leandro <- read.csv("~/Downloads/2021LeandroCostEstimateswv.csv")
`2021-2022 Projected` <-read.csv("2021Leandrocostestimateswv_wGEOID.csv")
`2021-2022 with Leandro` <- read.csv("2021Leandrocostestimatesfullfund_wGEOID.csv")
`2027-2028 with Leandro` <- read.csv("2027Leandrocostestimateswfullfunding_wGEOID.csv")

library(sf)
library(dplyr)

#added this in to make district comparisons clearer
#thispart is done.
#ADM <- read.csv("~/Downloads/Copy of 21-22 Allotted ADM.csv")
#merge ADM and shapefile from below

#mergedshapeL2 <- merge(ADM, mergedshapeL, by.x = "Name", by.y = "LEA")
#adding PPE variable
#maketotalADM numeric
library(eeptools)
#mergedshapeL2$TOTAL <- decomma(mergedshapeL2$TOTAL)
#mergedshapeL2$TOTAL <- as.numeric(mergedshapeL2$TOTAL)
#mergedshapeL2$"Per Pupil Funding" <- mergedshapeL2$Total/mergedshapeL2$TOTAL
#mergedshapeL2 <- mergedshapeL2[, -c(1:17)] # delete columns 2 through 17
#mergedshapeL2 <- mergedshapeL2[, -c(33:50)] # delete columns 33 through 50
#mergedshapeL2 <- mergedshapeL2[, -c(33:42)] # delete columns 33 through 42

#readingin and merging shape file and finance info (no longer need after initial creation)
#county_info <- read.csv("~/Leandro1/2021LeandroCostEstimateswv_wGEOID.csv")
#county_info$GEOID <- as.character(county_info$GEOID)
#county_shape <- read_sf("~/Leandro1/tl_2017_37_unsd.shp")
#county_info %>% full_join(y = county_shape, by = 'GEOID') 
#mergedshapeL2%>%
#st_as_sf() %>%
#sf::st_write("~/Leandro1/output1.shp") 

#reading in merged shape file that was written from above
mergedshapeL <- read_sf("output1.shp")

mergedshapeL <- rename(mergedshapeL, "Per Pupil Funding" = PrPplFn)


library(shiny)
library(shinydashboard)
library(leaflet)
library(Rcpp)
library(htmlwidgets)
library(htmltools)

#making labels for map below
labels <- sprintf(
  mergedshapeL$NAME
) %>% 
  lapply(htmltools::HTML)



#set color bins for chloropeth mapping
library(RColorBrewer)
pal <- colorNumeric(
  palette = "Blues", 
  domain = mergedshapeL$`Per Pupil Funding`)
library(colorspace)
library(rgdal)




#basic architecture for shiny app (with chloropeth mapping; change this to PPE?)
# add a comment
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
    tag.map.title, HTML("Click your district of interest to view breakdown of allocations.")
  ),
  titlePanel(title="Leandro Budget Tool"),

  
sidebarLayout(
    sidebarPanel(
        helpText("North Carolina school districts/LEAs are displayed according to geographical location.
Each LEA contains pop up boxes with budget categories calculated for either the 21-22 or 27-28 school yearc
The dropdown box below will allow you to select the year.First, choose your year.Then,click your district of interest to view breakdown of allocations,
                 or select an allocation category from the box below to see a graph of categories"),
      
      selectInput("dataset", 
                  label = "Choose a year to display",
                  choices = c("2021-2022 Projected" = "2021-2022 Projected", 
                                 "2021-2022 with Leandro" = "2021-2022 with Leandro",
                                 "2027-2028 with Leandro"= "2027-2028 with Leandro"),
                  selected = "2021-2022 Projected"),
      selectInput("var", 
                  label = "Choose a an allottment category",
                  choices = c("Textbooks" = "Textbooks",
                              "At Risk" = "At.Risk",
                              "Classroom Supplies"= "Classroom.Supplies...Materials",
                              "LEP" = "Limited.English.Proficiency", 
                              "Children with Dis." = "Children.with.Disabilities", 
                              "Low Wealth" = "Low.Wealth", 
                              "Teacher Assistants" = "Teacher.Assistants", 
                              "Supp. Funding" = "Disadvantaged.Students.Supplemental.Funding", 
                              "Instructional Support" = "Instructional.Support.Dollars", 
                              "Assistant Principals" = "Assistant.Principals.Dollars", 
                              "Non-Instructional" = "Non-Instructional Supports", 
                              "Central Office" = "Central.Office", 
                              "Professional Development" = "Professional.Development...Mentoring", 
                              "Community School Coord." =  "Community.School.Coordinator..Dollars.",
                              "NC Virtual" = "NC.Virtual.Public.Dollars", 
                              "Career Development Coordinators" = "Career.Development.Coordinators..Dollar.", 
                              "Classroom Teacher" = "Classroom.Teachers..Dollar.", 
                              "Principals" = "Principals...Dollar.", 
                              "Transportation" = "Transportation", 
                              "Small County" = "Small County", 
                              "CTE Teachers" = "CTE.Teachers..Dollar.", 
                              "CTE Program Supports" = "CTE.Program.Support", 
                              "Driver Training" = "Driver Training", 
                              "Total" = "Total"
                              ),
                  selected = "2021-2022 Projected")),
                mainPanel(
                  
                  mergedshapeL %>%
                            sf::st_transform(4326) %>% 
                            leaflet() %>%
                            addProviderTiles("MapBox", options = providerTileOptions(
                              id = "mapbox.light",
                              accessToken = 'pk.eyJ1Ijoic2ZmODA5IiwiYSI6ImNrc2ZocmQwZTFhZ2kyb255eTdqZnhraDQifQ.R6fe2zZx8SVrWtYSZjfGrg
')) %>%
                            addPolygons(fillColor = ~pal(`Per Pupil Funding`),
                                        weight = .5,
                                        opacity = 1,
                                        color = "black",
                                        fillOpacity = 0.7,
                                        popup = paste(
                                                      "Textbooks:", mergedshapeL$Textbks, "<br>",
                                                      "At-risk:", mergedshapeL$At_Risk, "<br>",
                                                      "Classroom Supply/Mat.:", mergedshapeL$C_S___M, "<br>",
                                                      "LEP:", mergedshapeL$Lmt_E_P, "<br>",
                                                      "Chi. w/ Dis.:", mergedshapeL$Chld__D, "<br>", 
                                                      "Low Wealth:", mergedshapeL$Lw_Wlth, "<br>",
                                                      "Teacher Assist.:", mergedshapeL$Tchr_As, "<br>",
                                                      "Supp. Funding:", mergedshapeL$D_S_S_F, "<br", 
                                                      "Instructional Support:", mergedshapeL$I_S__D_, "<br>", 
                                                      "Assistant Principal:", mergedshapeL$A_P__D_, "<br>", 
                                                      "Non-Inst. Support:", mergedshapeL$Nn_In_S, "<br>", 
                                                      "Central Office:", mergedshapeL$Cntrl_O, "<br>",
                                                      "NC Virtual:", mergedshapeL$NC_V_P_, "<br>",
                                                      "Classroom Teachers:", mergedshapeL$C_T___D, "<br>",
                                                      "Principals:", mergedshapeL$Pr___D_, "<br>", 
                                                      "Transportation:", mergedshapeL$Trnsprt, "<br>",
                                                      "Small County:", mergedshapeL$Smll_Cn, "<br>", 
                                                      "CTE Teachers:", mergedshapeL$CTE_T__D, "<br>",
                                                      "CTE Prog. Supp.:", mergedshapeL$CTE_P_S, "<br>", 
                                                      "Driver Training:", mergedshapeL$Drvr_Tr, "<br>"), 
                                        highlight = highlightOptions(weight = 5,
                                                                     color = "#666",
                                                                     dashArray = NULL,
                                                                     fillOpacity = 0.7,
                                                                     bringToFront = TRUE), 
                                        label = labels,
                                        labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                                                 padding = "3px 8px"),
                                                                    textsize = "15px",
                                                                    direction = "auto")) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = mergedshapeL$`Per Pupil Funding`,
                  title = "Total Funding",
                  opacity = 1) %>%
                         
        addTiles() %>%
        addControl(title, position = "topleft", className="map-title"),
                          
                          
        plotOutput("plotfunding")                 )))

server <-function(input, output){

  function(input, output, session) {
    
    data1 <- reactive({
      input$`2021-2022 with Leandro`
    })
    data2 <- reactive({
      input$`2027-2028 with Leandro`
    })
    
    output$plotfunding <- renderPlot({
      boxplot(get(data2()) ~ get(data1()) )
    })
    
    output$Hist <- renderPlot({
      req(data1())
      hist(data1())})}}
    

shinyApp(ui, server)



#run shiny connect to publish

#library(rsconnect)
#rsconnect::setAccountInfo(name='sarahpedonti',
                          #token='0C7EF3A322A741CDDD35E5366C22897F',
                          #secret='JrUrKknfyeFCs0br4ytrYGg9T6HAikB1nBlo+f1o')
#to deploy
#library(rsconnect)
#rsconnect::deployApp('app.R')





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



