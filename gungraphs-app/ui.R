# UI for Gun and Suicide/Crime info interactive graphs.
# 
# By Doug Duffy 2016

# Load packages
library(shiny)
library(ggvis)

# Shiny UI function
shinyUI(fluidPage(
  
  # ggvis Output
  ggvisOutput("ggvis"),
  
  # Panel for various drop down menu inputs
  absolutePanel(top = 10, left = 810,width = 200,
                
                # Dataset choice
                selectInput("dataset", "Dataset :",
                            c("US State","Country"),
                            selected = "US State"
                ),
                
                # y-axis
                uiOutput("yvar2"), 
                
                # point size
                uiOutput("size"),
                
                # Group of nations
                uiOutput("cgroups")
  ),
  # Panel for checkboxes
  absolutePanel(top = 325, left = 885, width = 125,
                checkboxInput("trend", "Show Trendline", FALSE),
                checkboxInput("text", "Show Labels", FALSE)
  ),
  
  # Panel for header and logo
  absolutePanel(top=442, left = 815, width = 200,
                div(style="background:white; opacity:0.7",
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Logo.png", width = "40px", height= "35px")),
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Header.png",width = "155px", height= "35px"))
                )
  ),
  
  # Panel for references and links
  uiOutput("refs")
  
  
  )
  )







