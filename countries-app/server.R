# Server for Gun and Suicide/Crime info interactive map for Country data
# 
# By Doug Duffy 2016

# Load Packages
library(shiny)
library(leaflet)
library(leaflet)
library(jsonlite)

# Load prepared datasets
countriesJson <- readRDS("data/countriesJson.rds")
countrynames <- readRDS("data/countrynames.rds")
gunown <- readRDS("data/gunown.rds")
suirate <- readRDS("data/suirate.rds")
homrate <- readRDS("data/homrate.rds")

#
# Shiny Server Function
#

shinyServer(
  function(input, output) {
    
    # Define color palette based on data selected to be plotted by user
    colorpal <- reactive({
      if(input$data =="Gun Ownership"){
        return(colorNumeric("OrRd", gunown))
      }else if(input$data =="Homicide Rate"){
        return(colorNumeric("OrRd", homrate))
      }else if(input$data == "Suicide Rate"){
        return(colorNumeric("OrRd", suirate))
      }
      
    })
    
    
    # Plot default map with Stamen Toner Lite tiles, zoom to show only one Earth
    output$map <- renderLeaflet({
      leaflet(countriesJson) %>% 
        addGeoJSON(countriesJson) %>% 
        addProviderTiles("Stamen.TonerLite") %>% 
        setView(0, 0, zoom =2)
    })
    
    # Observe reset view button
    observe({
      input$resetview
      leafletProxy("map") %>% setView(0, 0, zoom =2)
    })
    
    
    # Observe changes in data set to be plotted
    observe({
      pal <- colorpal()
     
      # Add a properties$style list to each feature based on color scheme
      countriesJson$features <- lapply(countriesJson$features, function(feat) {
         if(input$data =="Gun Ownership"){
           feat$properties$style <- list(fillColor = pal(feat$properties$gun_own))
         }else if(input$data =="Homicide Rate"){
           feat$properties$style <- list(fillColor = pal(feat$properties$homicide_rate))
         }else if(input$data =="Suicide Rate"){
           feat$properties$style <- list(fillColor = pal(feat$properties$suicide_rate))
         }
        feat
      })
      
      # Clear previous plot and add plot with new colors
      leafletProxy("map", data = countriesJson) %>%
        clearGeoJSON() %>%
        addGeoJSON(countriesJson)
        
    })
    
    # Add or Remove Legend based on input
    observe({
      proxy <- leafletProxy("map", data = countriesJson)
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        # Legend data for 
        # Gun Ownership
        if(input$data =="Gun Ownership"){
          # Variable plotted
          valplot <- gunown 
          # Suffix for legend label
          sufplot <- " per 100 Residents"
          # Title for legend
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
        
        # Add new legend
        proxy %>% addLegend(position = "topleft",
                            pal = pal, values = ~valplot,
                            labFormat = labelFormat(suffix = sufplot),
                            title = titplot, 
                            na.label = "Data Not Avail."
        )
      }
    })
    
    # Define object to store info for hightlighted country
    values <- reactiveValues(highlight = c())
    
    # Store the info related to the country that is moused over
    observe({
      values$highlight <- input$map_geojson_mouseover
    })
    
    # Dynamically render the box in the lower-left
    output$countryInfo <- renderUI({
      # Check if null
      if (is.null(values$highlight)) {
        # HTML if no country has been moused over
        return("Hover mouse over a country")
      } else {
        # HTML for lower left display box
        return(tags$div(
          # Country Name
          tags$strong(values$highlight$properties$name),
          
          # Guns per capita
          tags$div(ifelse((length(values$highlight$properties$gun_own)==0),"NA",values$highlight$properties$gun_own),
                   "Guns per 100 Residents",
                   "(Rank : ",values$highlight$properties$gun_own_rank,")"),
          # Homicide Rate
          tags$div(ifelse((length(values$highlight$properties$homicide_rate)==0),"NA",values$highlight$properties$homicide_rate), "Homicides per 100,000 Residents",
                   "(",values$highlight$properties$homicide_rate_rank,")"),
          # Suicide Rate
          tags$div(ifelse((length(values$highlight$properties$suicide_rate)==0),"NA",values$highlight$properties$suicide_rate), "Suicides per 100,000 Residents",
                   "(",values$highlight$properties$suicide_rate_rank,")")
        ))
        
      }
    })
    
    # Add hghlighting for country on hover
    observe({
      values$highlight$properties$name
      # Check if null
      if(!is.null(values$highlight$properties$name)){
        if(values$highlight$properties$name %in% countrynames){ 
          # Find which state is hovered over
          hl <- which(values$highlight$properties$name == countrynames)
          # Add no fill and thick black outline for state hovered over
          # Group info added to allow easy removal later
          leafletProxy("map") %>% 
            addGeoJSON(countriesJson$features[[hl]], 
                       weight = 4, opacity =1,
                       fillOpacity = 0, group = values$highlight$properties$name, color = "black")
          
        }
      }
      
      
    })
    
    # Remove Highlighting
    observe({
      values$highlight$properties$name
      # Check if null
      if(!is.null(values$highlight$properties$name)){
        if(values$highlight$properties$name %in% countrynames){
          # Which countries aren't hovered over
          unhl <- which(!(values$highlight$properties$name == countrynames))
          
          # Remove hightlighting for all non-hovered countries
          leafletProxy("map") %>% 
            clearGroup(group = countrynames[unhl])
          
        }
      }
      
      
    })
  }
)
