# UI for Gun and Suicide/Homicide info interactive map of US States
# 
# By Doug Duffy 2016

# Load packages
library(leaflet)
library(shiny)
library(RColorBrewer)

#
# Shiny UI Function
# 

shinyUI(fluidPage(
  # Add a little CSS to define the styling for the lower left display box
  tags$head(tags$style("
    .floater { background-color: white; padding: 8px; 
opacity: 0.8; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
  ")),
  
  # Leaflet Output Map
  leafletOutput("map", 1000, 500),
  
  
  # Panel for lower left display box
  absolutePanel(
    left = 25, top=335, width = 300,  class = "floater",
    # Title
    h4("US Gun and Violence Stats"),
    # Hovered State info
    uiOutput("stateInfo")
  ),
  
  # Panel for data set input
  absolutePanel(top = 10, left = 865,width = 150,
                selectInput("data", "Data Plotted :",
                            c("Gun Ownership","Homicide Rate","Suicide Rate"),
                            selected = "Gun Ownership"
                )
                
  ),
  
  # Panel for check box for legend and reset view button
  absolutePanel(top = 75, left = 895, width = 125,
              checkboxInput("legend", "Show legend", TRUE),
                actionButton("resetview", "Reset View")
  ),
  
  # Panel for  Header and Logo and References
  absolutePanel(top=432, left = 815, width = 200,
                # Header and Logo Div
                div(style="background:white; opacity:0.7",
                    # Logo
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Logo.png", width = "40px", height= "35px")),
                    # Header
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Header.png",width = "155px", height= "35px"))
                ),
                # References Div
                div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:7px", 
                    "Sources :", 
                    # FBI Homicide Rate Data
                    a( href = "http://www.ucrdatatool.gov/Search/Crime/Crime.cfm",
                       style ="text-decoration:none",target = "_blank",
                       "FBI"),
                    ", ",
                    # CDC Suicide Rate Data
                    a( href = "http://www.cdc.gov/mmwr/preview/mmwrhtml/mm6345a10.htm",
                       style ="text-decoration:none",target = "_blank",
                       "CDC"),
                    ", ",
                    # British Medical Journal Gun Ownership % data
                    a( href = "http://injuryprevention.bmj.com/content/early/2015/06/09/injuryprev-2015-041586.full.pdf?keytype=ref&ijkey=doj6vx0laFZMsQ2",
                       style ="text-decoration:none",target = "_blank",
                       "BMJ")
                    ))
)
)


