# Server for Gun and Suicide/Homicide info interactive map of US States
# 
# By Doug Duffy 2016

# Load packages
library(shiny)
library(leaflet)
library(jsonlite)

# Load prepared datasets
statesJson <- readRDS("data/statesJson.rds")
statenames <- readRDS("data/statenames.rds")
gunown <- readRDS("data/gunown.rds")
homrate <- readRDS("data/homrate.rds")
suirate <- readRDS("data/suirate.rds")

#
# Shiny server function
#
shinyServer(
  function(input, output) {
    
    # Define color palette based on plotted data selected by user
    colorpal <- reactive({
      if(input$data =="Gun Ownership"){
        return(colorNumeric("OrRd", gunown))
      }else if(input$data =="Homicide Rate"){
        return(colorNumeric("OrRd", homrate))
      }else if(input$data == "Suicide Rate"){
        return(colorNumeric("OrRd", suirate))
      }
      
    })
    
    
    # Make default map with Toner Lite tiles from Stamen, zoom to US
    output$map <- renderLeaflet({
      leaflet(statesJson) %>% 
        addGeoJSON(statesJson) %>% 
        addProviderTiles("Stamen.TonerLite") %>%
        setView(-98.85, 43, zoom =4)
    })
    
    # Observe reset view button
    observe({
      input$resetview
      leafletProxy("map") %>% setView(-98.85, 43, zoom =4)
    })
    
    
    
    # Observe changes in data set to be plotted
    observe({
      pal <- colorpal()
     
      # Add a properties$style list to each feature based on color scheme
      statesJson$features <- lapply(statesJson$features, function(feat) {
         if(input$data =="Gun Ownership"){
           feat$properties$style <- list(fillColor = pal(feat$properties$gun_own))
         }else if(input$data =="Homicide Rate"){
           feat$properties$style <- list(fillColor = pal(feat$properties$homicide_rate))
         }else if(input$data =="Suicide Rate"){
           feat$properties$style <- list(fillColor = pal(feat$properties$suicide_rate))
         }
        feat
      })
      
      # Clear previous plot of map and replot with new colors
      leafletProxy("map", data = statesJson) %>%
        clearGeoJSON() %>%
        addGeoJSON(statesJson)
        
    })
    
    # Remove and add legend based on user input
    observe({
      proxy <- leafletProxy("map", data = statesJson)
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        # Define variables for legend based on data plotted
        
        # Gun Ownership
        if(input$data =="Gun Ownership"){
          # variable plotted
          valplot <- gunown 
          # suffix for legend label
          sufplot <- " % Residents"
          # title for legend
          titplot <- input$data
          
          # Homicide Rate
        }else if(input$data =="Homicide Rate"){
          valplot <- homrate
          sufplot <- " per 100,000 Residents"
          titplot <- input$data
          
          # Suicide Rate
        }else if(input$data =="Suicide Rate"){
          valplot <- suirate
          sufplot <- " per 100,000 Residents"
          titplot <- input$data
        }
        
        # Add/Remove legend from plot
        proxy %>% addLegend(position = "topleft",
                            pal = pal, values = ~valplot,
                            labFormat = labelFormat(suffix = sufplot),
                            title = titplot, 
                            na.label = "Data Not Avail."
        )
      }
    })
    
    # Create values object to store info related to highlighted state
    values <- reactiveValues(highlight = c())
    
    # Observe mouseover of state
    observe({
      # Store info from geojson with mouseover
      values$highlight <- input$map_geojson_mouseover
    })
    
    
    # Dynamically render the display box in the lower-left
    output$stateInfo <- renderUI({
      # Check if highlighting is null
      if (is.null(values$highlight)) {
        # HTML if no state has been hovered over
        return("Hover mouse over a state")
      } else {
        # HTML for lower left display box
        return(tags$div(
          # State name
          tags$strong(values$highlight$properties$name),
          # Population
          tags$div(ifelse((length(values$highlight$properties$population)==0),"NA",round(values$highlight$properties$population/1000000,2)), 
                   "mil Population (Rank: ",values$highlight$properties$population_rank,")" ),
          # Gun Owner Info
          tags$div(ifelse((length(values$highlight$properties$gun_own)==0),"NA",values$highlight$properties$gun_own),
                   "% Residents Gun Owners",
                   "(",values$highlight$properties$gun_own_rank,")"),
          # Homicide Info
          tags$div(ifelse((length(values$highlight$properties$homicide_rate)==0),"NA",values$highlight$properties$homicide_rate),
                   "Homicides per 100,000 Residents",
                   "(",values$highlight$properties$homicide_rate_rank,")"),
          # Suicide Info
          tags$div(ifelse((length(values$highlight$properties$suicide_rate)==0),"NA",values$highlight$properties$suicide_rate),
                   "Suicides per 100,000 Residents",
                   "(",values$highlight$properties$suicide_rate_rank,")")
        ))
        
      }
    })
    
    # Add hightlighting as state is hovered over
    observe({
      values$highlight$properties$name
      # Check if highted state is null
      if(!is.null(values$highlight$properties$name)){
        # if highlighted object is a state
        if(values$highlight$properties$name %in% statenames){
          # find index of the state which is highlighted
          hl <- which(values$highlight$properties$name == statenames)
          # Add new GeoJSON for the highlighted state with no fill and thick black border
          # define group so that the added object is easily removed later
          leafletProxy("map") %>% 
            addGeoJSON(statesJson$features[[hl]], weight = 4, opacity =1,
                       fillOpacity = 0, group = values$highlight$properties$name, color = "black")
          
        }
      }
    })
    
    
    
    # Remove highlighting for all states not being hovered over
    observe({
      values$highlight$properties$name
      # Check if null
      if(!is.null(values$highlight$properties$name)){
        if(values$highlight$properties$name %in% statenames){
          # Which states aren't being hovered over
          unhl <- which(!(values$highlight$properties$name == statenames))
          
          # Remove highlighting by referencing group name from above
          leafletProxy("map") %>% 
            clearGroup(group = statenames[unhl])
          
        }
      }
      
      
    })
  }
)
