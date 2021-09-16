library(sf)
library(tidyverse)
library(shiny)
library(leaflet)
library(eeptools)
library(shiny)
library(shinydashboard)
library(leaflet)
library(Rcpp)
library(htmlwidgets)
library(htmltools)
library(colorspace)
library(rgdal)
library(RColorBrewer)

# read in datasets
`2021-2022 Projected` <- read_csv("2021Leandrocostestimateswv_wGEOID.csv")
`2021-2022 with Leandro` <- read_csv("2021Leandrocostestimatesfullfund_wGEOID.csv")
`2027-2028 with Leandro` <- read_csv("2027Leandrocostestimateswfullfunding_wGEOID.csv")

# Adding 4 dashes to the end of this comment allows you to collapse the UI section! ----
ui <- fluidPage(
    
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
      }")),
    
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
                                    "At Risk" = "At-Risk",
                                    "Classroom Supplies"= "Classroom Supplies & Materials",
                                    "LEP" = "Limited English Proficiency", 
                                    "Children with Dis." = "Children with Disabilities", 
                                    "Low Wealth" = "Low Wealth", 
                                    "Teacher Assistants" = "Teacher Assistants", 
                                    "Supp. Funding" = "Disadvantaged Students Supplemental Funding", 
                                    "Instructional Support" = "Instructional Support Dollars", 
                                    "Assistant Principals" = "Assistant Principals Dollars", 
                                    "Non-Instructional" = "Non-Instructional Supports", 
                                    "Central Office" = "Central Office", 
                                    "Professional Development" = "Professional Development & Mentoring", 
                                    "Community School Coord." =  "Community School Coordinator (Dollars)",
                                    "NC Virtual" = "NC Virtual Public Dollars", 
                                    "Career Development Coordinators" = "Career Development Coordinators (Dollar)", 
                                    "Classroom Teacher" = "Classroom Teachers \n(Dollar)", 
                                    "Principals" = "Principals \n(Dollar)", 
                                    "Transportation" = "Transportation", 
                                    "Small County" = "Small County", 
                                    "CTE Teachers" = "CTE Teachers (Dollar)", 
                                    "CTE Program Supports" = "CTE Program Support", 
                                    "Driver Training" = "Driver Training", 
                                    "Total" = "Total"
                        ),
                        
                        selected = "NC Virtual")
        ),
        
        mainPanel(
            leafletOutput("map", width = "100%", height = 400),
            
            # Output: Histogram ----
            plotOutput(outputId = "AllotPlot")
        )
    )
    
    
)

# Adding 4 dashes to the end of this comment allows you to collapse the server section! ----
server <- function(input, output, session) {
    
    data1 <- reactive({
        if(input$dataset == "2021-2022 Projected") {
            return(`2021-2022 Projected`)
        }else if(input$dataset == "2021-2022 with Leandro") {
            return(`2021-2022 with Leandro`)
        }else{input$dataset == "2027-2028 with Leandro"
            return(`2027-2028 with Leandro`)
        }
    })
    
    # get selected column from selected dataset
    var <- reactive({
      data1()[[input$var]]
    })
    
    output$map <- renderLeaflet({
      
      # use dataset and allotment choice made in dropdown menus 
      county_info <- data1()
      allotment <- var()
      
      # uncomment the lines below for non-web testing
      # remember to comment them out again before running the app!!!
      #county_info <- `2021-2022 Projected`
      #allotment <- "At-Risk"
      
      # merge shapefile with dataset
      county_info$GEOID <- as.character(county_info$GEOID)
      county_shape <- read_sf("tl_2017_37_unsd.shp") %>% 
        st_transform("+proj=longlat +datum=WGS84")
      county_info <- county_info %>%
        full_join(y = county_shape, by = 'GEOID')
      
      # clean merged data
      mergedshapeL2 <- county_info
      # Add any data cleaning, formatting, creation of new columns, etc. HERE!!
      
      # create labels
      labels <- sprintf(mergedshapeL2$NAME) %>%
        lapply(htmltools::HTML)
      
      # set color bins for choropleth mapping
      pal <- colorNumeric(
        palette = "Blues",
        domain = allotment
        )
      
      # generate leaflet map
      leaflet() %>%
        
        addTiles() %>%
        
        addPolygons(data = mergedshapeL2$geometry,
                    fillColor = pal(allotment),
                    weight = .5,
                    opacity = 1,
                    color = "black",
                    fillOpacity = 0.7,
                    label = labels,
                    labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                             textsize = "15px",
                                                             direction = "auto")),
                    highlight = highlightOptions(weight = 5,
                                                 color = "#666",
                                                 dashArray = NULL,
                                                 fillOpacity = 0.7,
                                                 bringToFront = TRUE),
                    popup = paste(
                      "Textbooks:", mergedshapeL2$Textbooks, "<br>",
                      "At-risk:", mergedshapeL2$`At-Risk`, "<br>",
                      "Classroom Supply/Mat.:", mergedshapeL2$`Classroom Supplies & Materials`, "<br>",
                      "LEP:", mergedshapeL2$`Limited English Proficiency`, "<br>",
                      "Chi. w/ Dis.:", mergedshapeL2$`Children with Disabilities`, "<br>", 
                      "Low Wealth:", mergedshapeL2$`Low Wealth`, "<br>",
                      "Teacher Assist.:", mergedshapeL2$`Teacher Assistants`, "<br>",
                      "Supp. Funding:", mergedshapeL2$`Disadvantaged Student Supplemental Funding`, "<br", 
                      "Instructional Support:", mergedshapeL2$`Instructional Support (Dollars)`, "<br>", 
                      "Assistant Principal:", mergedshapeL2$`Assistant Principals (Dollars)`, "<br>", 
                      "Non-Inst. Support:", mergedshapeL2$`Non-Instructional Support`, "<br>", 
                      "Central Office:", mergedshapeL2$`Central Office`, "<br>",
                      "NC Virtual:", mergedshapeL2$`NC Virtual Public School`, "<br>",
                      "Classroom Teachers:", mergedshapeL2$`Classroom Teachers \n(Dollar)`, "<br>",
                      "Principals:", mergedshapeL2$`Principals \n(Dollar)`, "<br>", 
                      "Transportation:", mergedshapeL2$`Transportation`, "<br>",
                      "Small County:", mergedshapeL2$`Small County`, "<br>", 
                      "CTE Teachers:", mergedshapeL2$`CTE Teachers (Dollar)`, "<br>",
                      "CTE Prog. Supp.:", mergedshapeL2$`CTE Program Support`, "<br>", 
                      "Driver Training:", mergedshapeL2$`Driver Training`, "<br>"
                      )

        ) %>%
        
        addLegend(pal = pal, 
                  values = allotment,
                  position = "bottomright",
                  title = "Total Funding",
                  opacity = 1)
      })
    
      output$AllotPlot <- renderPlot({

        bins <- seq(min(var(), na.rm = TRUE), max(var(), na.rm = TRUE), length.out = 5)

        hist(var(), breaks = bins, col = "#75AADB", border = "white",
             xlab = "Comparisons of allottments (in dollars)",
             main = "Histogram of allotments")
        })
    }

shinyApp(ui, server)
