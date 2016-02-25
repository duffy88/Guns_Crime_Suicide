# UI for Gun and Suicide/Crime info interactive map for Country data
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
  
  # Leaflet map output
  leafletOutput("map", 1000, 500),
  
  # Panel for lower left display box
  absolutePanel(
    left = 25, top=355, width = 300, class = "floater",
    # Title
    h4("Country Gun and Violence Stats"),
    # Country Data
    uiOutput("countryInfo")
  ),
  
  # Panel for Data set select input
  absolutePanel(top = 10, left = 865,width = 150,
                selectInput("data", "Data Plotted :",
                            c("Gun Ownership","Homicide Rate","Suicide Rate"),
                            selected = "Gun Ownership"
                )
                
  ),
  
  # Panel for legend check box and reset view button
  absolutePanel(top = 75, left = 900, width = 110,
              checkboxInput("legend", "Show legend", TRUE),
                actionButton("resetview", "Reset View")
  ),
  
  # Panel for Header and Logo
  absolutePanel(top=432, left = 815,width = 200, 
                # Div for main styling
                div(style="background:white; opacity:0.7",
                    # Logo Link
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Logo.png", width = "40px", height= "35px")),
                    # Header Link
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Header.png",width = "155px", height= "35px"))
                )
                ),
  
  # Panel for References
  absolutePanel(top=467, left = 675, width = 340, 
                # Div for main styling
                div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:7px", 
                    "Sources :", 
                    # UN Homicide Rate Data reference and link
                    a( href = "http://www.unodc.org/documents/gsh/pdfs/2014_GLOBAL_HOMICIDE_BOOK_web.pdf",
                       style ="text-decoration:none",target = "_blank",
                       "UN(Homicide)"),
                    ", ",
                    # UN Guns per capita data reference and link
                    a( href = "https://www.unodc.org/documents/firearms-protocol/UNODC_Study_on_Firearms_WEB.pdf",
                       style ="text-decoration:none",target = "_blank",
                       "UN(Guns)"),
                    ", ",
                    # WHO Suicide Rate data reference and link
                    a( href = "http://apps.who.int/gho/data/node.main.MHSUICIDE?lang=en",
                       style ="text-decoration:none",target = "_blank",
                       "WHO"),
                    ", ",
                    # Open Knowledge Country GeoJSON reference and link
                    a( href = "http://data.okfn.org/data/datasets/geo-boundaries-world-110m",
                       style ="text-decoration:none",target = "_blank",
                       "Open Knowledge")
                ))
)
)


