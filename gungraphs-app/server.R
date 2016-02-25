# Server for Gun and Suicide/Crime info interactive graphs.
# 
# By Doug Duffy 2016

# Load Packages
library(shiny)
library(ggvis)

# Load prepared datasets
statesOut <- readRDS("data/statesOut.rds")
countryOut <- readRDS("data/countryOut.rds")

# Homogenize name for Gun columns
names(statesOut)[names(statesOut)=="Gun Ownership"] <- "Guns"
names(countryOut)[names(countryOut)=="Guns per 100 People"] <- "Guns"

# Various choices for Groups of Nations interactive subsetting
cgroups <- c("All Countries","G7","G8","G10","G20")

# Define members of the various Groups of Nations
g7 <- c("France","Germany","Italy","United Kingdom","Japan","United States","Canada")
g8 <- c("France","Germany","Italy","United Kingdom","Japan","United States","Canada","Russia")
g10 <- c("Belgium","Canada","France","Germany","Italy","Japan","Netherlands",
         "Sweden","Switzerland","United Kingdom","United States")
g20 <- c("Argentina","Australia","Brazil",
         "Canada","China","France","Germany","India",
         "Indonesia","Italy","Japan","South Korea",
         "Mexico","Russia","Saudi Arabia",
         "South Africa","Turkey","United Kingdom","United States")


#
# Server function
#


shinyServer(
  function(input, output) {
    
    # React to change in state/country dataset
    dataSource <- reactive({
      switch(input$dataset,
             "US State" = statesOut[complete.cases(statesOut),],
             "Country" = countryOut[complete.cases(countryOut),])
      })
    
    # Define x-axis label based on input dataset
    xaxis <- reactive({
      switch(input$dataset,
             "US State" = "Gun Ownership (% Residents)",
             "Country" = "Guns Per Capita (per 100 Residents)")
    })
    
    # Define y-axis label based on input dataset
    yaxis <- reactive({
      if(input$yvar %in% c("State","Country","Region","Subregion")){
        as.character(input$yvar)
      }else
        # for rates on y-axis add how rate is defined
        paste(as.character(input$yvar)," (per 100,000)
              ")
    })
    
    # Define y-axis offset based on y-axis variable
    yaxisoff <- reactive({
      if(input$yvar %in% c("Region")){
        100
      }else if(input$yvar %in% c("Subregion", "State")){
        150
      }else if(input$yvar %in% c("Violent Crime Rate")){
        75
      }else {
        50
      }
        
    })
    
    # Reactive for column name of place of interest based on changing dataset
    place <- reactive({
      switch(input$dataset,
             "US State" = "State",
             "Country" = "Country")
    })
    
    # Reactive for graph height based on dataset
    # required due to long hover list for state data not fitting on plot
    graphHeight <- reactive({
      switch(input$dataset,
             "US State" = 435,
             "Country" = 475)
    })
    
    # Reactive for point opacity based on plotted data
    # Plotting all countries has too many points -> higher opacity
    # still some opacity for other plots
    opac <- reactive({
      if(!(is.null(input$dataset)) & !(is.null(input$cgroup))){
        if(input$dataset=="Country" & input$cgroup=="All Countries"){
          return( 1/2.5)
        }else 
          return(1/1.5)
      }else
        return(1/1.5)
    })
    
    # Dynamically create the selectInput for y-axis
    output$yvar2 <- renderUI({selectInput("yvar", "Choose y-axis :",
                                          choices = names(dataSource())[
                                            !(names(dataSource())%in%
                                                c("Country","Guns")) & 
                                              !(grepl("Rank", names(dataSource())))], 
                                          selected = "Homicide Rate")})
    
    # Dynamically create the selectInput for point size
    output$size <- renderUI({selectInput("size", "Point Size :",
                                          choices = names(dataSource())[
                                            !(names(dataSource())%in%
                                                c("State","Country","Region","Subregion","Guns"))  & 
                                              !(grepl("Rank", names(dataSource())))], 
                                          selected = "Suicide Rate")})
    
    # If plotting countries, add selectInput for Groups of nations subsetting
    output$cgroups <- renderUI({
      if(input$dataset=="Country"){
        selectInput("cgroup", "Country Group :",
                    choices = cgroups,
                    selected = "All Countries")
      }
      })
    
    # Dynamic rendering of links to references for data sources
    output$refs <- renderUI({
      # Country Dataset References
      if(input$dataset=="Country"){
        absolutePanel(top=477, left = 765, width = 250,
                      
                      # Div for main styling
                      div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:7px", 
                          "Sources :", 
                          
                          # UN Homicide Rate data reference and link
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
                             "WHO")
                      ))
        
        # State dataset references
      }else if(input$dataset=="US State"){
        absolutePanel(top=477, left = 815, width = 200,
                      # Div for main styling
                      div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:7px", 
                          "Sources :", 
                          
                          # FBI Crime Stats data reference and link 
                          a( href = "http://www.ucrdatatool.gov/Search/Crime/Crime.cfm",
                             style ="text-decoration:none",target = "_blank",
                             "FBI"),
                          ", ",
                          
                          # CDC Suicide Rate data reference and link
                          a( href = "http://www.cdc.gov/mmwr/preview/mmwrhtml/mm6345a10.htm",
                             style ="text-decoration:none",target = "_blank",
                             "CDC"),
                          ", ",
                          
                          # British Medical Journal Gun ownership %'s data Reference and link
                          a( href = "http://injuryprevention.bmj.com/content/early/2015/06/09/injuryprev-2015-041586.full.pdf?keytype=ref&ijkey=doj6vx0laFZMsQ2",
                             style ="text-decoration:none",target = "_blank",
                             "BMJ")
                      )
                      )
      }
    })
    
    # Subset main dataset reactively
    my_subset_data <- reactive({        
      
      # State dataset
      # Here check if the column names correspond to the dataset
      if( any(input$yvar %in% names(dataSource())) & 
          any(input$size %in% names(dataSource())) &
          input$dataset == "US State" )
      {
        
        # Columns required to make the given plot
        reqd <- c("State", "Region","Guns",input$yvar, input$size)
        # ...those not "required" to plot, but we want to keep around for the hover popup
        notreqd <- names(dataSource())[!(names(dataSource()) %in% reqd)]
        df <- subset(dataSource(), select = c(reqd, notreqd))
        names(df) <- c("place","region","x","y","size",
                       "notreqd1","notreqd2","notreqd3","notreqd4")
        return(df)
        
        # Country dataset
        # Check column names
      }else if( any(input$yvar %in% names(dataSource())) & 
                any(input$size %in% names(dataSource())) &
                input$dataset == "Country" &
                !(is.null(input$cgroup)))
      {
        # Columns required for given plot
        reqd <- c("Country", "Region","Guns",input$yvar, input$size)
        # ...those not "required" to plot, but we want to keep around for the hover popup
        notreqd <- names(dataSource())[!(names(dataSource()) %in% reqd)]
        
        # Subsetting further for groups of nations based on input
        # G7 subset
        if(input$cgroup=="G7"){
          df <- subset(dataSource(), select = c(reqd, notreqd))
          df <- df[df$Country %in% g7, ]
          names(df) <- c("place","region","x","y","size")
          return(df)
          # G8 subset
        }else if(input$cgroup=="G8"){
          df <- subset(dataSource(), select = c(reqd, notreqd))
          df <- df[df$Country %in% g8, ]
          names(df) <- c("place","region","x","y","size")
          return(df)
          # G10 subset
        }else if(input$cgroup=="G10"){
          df <- subset(dataSource(), select = c(reqd, notreqd))
          df <- df[df$Country %in% g10, ]
          names(df) <- c("place","region","x","y","size")
          return(df)
          # G20 subset
        }else if(input$cgroup=="G20"){
          df <- subset(dataSource(), select = c(reqd, notreqd))
          df <- df[df$Country %in% g20, ]
          names(df) <- c("place","region","x","y","size")
          return(df)
          # All countries
        }else
          df <- subset(dataSource(), select = c(reqd, notreqd))
        names(df) <- c("place","region","x","y","size")
        return(df)
      }
      
    })
    
    # Observe function to plot graph based on various reactive inputs
    observe({
      test <- my_subset_data()
      data <- dataSource()
      pl <- place()
      op <- opac()
      
      # subset main data to our reqd and nonreqd data for hover popup
      if(any(input$yvar %in% names(dataSource())) & 
         any(input$size %in% names(dataSource())) &
         any(pl %in% names(dataSource()))){
        
        
        # columns required for plot
        if(input$yvar %in% c("Subregion")){
          if(input$yvar == input$size){
            reqd2 <- c(pl,"Region", input$yvar,"Guns")
          }else {
            reqd2 <- c(pl,"Region", input$yvar,"Guns", input$size)
          }
          
        }else if(input$yvar %in% c("Region")){
          if(input$dataset == "US State"){
            if(input$yvar == input$size){
              reqd2 <- c(pl, input$yvar,"Guns")
            }else {
              reqd2 <- c(pl, input$yvar,"Guns", input$size)
            }
            
          }else {
            if(input$yvar == input$size){
              reqd2 <- c(pl, input$yvar,"Subregion","Guns")
            }else {
              reqd2 <- c(pl, input$yvar,"Subregion","Guns", input$size)
            }
            
          }
          
        }else if(input$yvar %in% c("State")){
          if(input$yvar == input$size){
            reqd2 <- c(input$yvar, "Region","Guns")
          }else {
            reqd2 <- c(input$yvar, "Region","Guns", input$size)
          }
        }else {
          if(input$dataset == "US State"){
            if(input$yvar == input$size){
              reqd2 <- c(pl, "Region","Guns",input$yvar)
            }else {
              reqd2 <- c(pl, "Region","Guns",input$yvar, input$size)
            }
            
          }else {
            if(input$yvar == input$size){
              reqd2 <- c(pl, "Region","Subregion","Guns",input$yvar)
            }else {
              reqd2 <- c(pl, "Region","Subregion","Guns",input$yvar, input$size)
            }
            
          }
          
        }
        
        reqdRanks <- names(dataSource())[ (grepl("Gun", names(dataSource())) & 
                                             grepl("Rank", names(dataSource()))) |
                                            (grepl(input$yvar, names(dataSource())) & 
                                               grepl("Rank", names(dataSource()))) |
                                            (grepl(input$size, names(dataSource())) & 
                                               grepl("Rank", names(dataSource())))
                                            ]
        #...those not required
        notreqd2 <- names(dataSource())[!(names(dataSource()) %in% reqd2) &
                                          !(grepl("Rank", names(dataSource())))]
        notreqdRanks <- names(dataSource())[ !(names(dataSource()) %in% reqdRanks) &
                                               (grepl("Rank", names(dataSource()))) ]
        
        #subset data to these columns
        data <- data[, c(reqd2,notreqd2,reqdRanks, notreqdRanks)]
      }
      
      # Function to display HTML string for all values in hover popup 
      all_values <- function(x) {
        if(is.null(x)) return(NULL)
        if(is.null(x$place)) return(NULL)
        # use only data for the data point being hovered over
        row <- data[data[,pl] == x$place,  ]
        
        # HTML output
        if(input$dataset=="US State" & ncol(row) == 16){
          paste0("<p style='font-size:12px;margin:0'><strong >" ,
                 names(row)[1:9], " : </strong>", format(row[1:9]), 
                 c("","", paste(" ( Rank : ", row["Gun Ownership Rank"]," )", sep=""),
                   paste("    ( ", row[paste(names(row)[4:9], "Rank", sep = " " ) ] ," ) ",sep="")),
                 collapse = "<br />")
        }else if(input$dataset=="Country"& ncol(row) == 9){
          paste0("<p style='font-size:12px;margin:0'><strong >" ,
                 names(row)[c(1:6)], " : </strong>", 
                 format(row[c(1:6)]), 
                 c("","","",paste(" ( Rank : ", format(row["Guns per 100 People Rank"]), " ) ", sep=""),  
                   paste(" ( ", row[paste(names(row)[5:6], "Rank", sep = " " ) ], " ) ")),
                 collapse = "<br />")
        }
        
        
      } 
      
      # Check to make sure dataset and opacity aren't null
      if(!is.null(test) & !(is.null(op))){
        
        # Define p as the ggvis plot to build off of
        p <- test %>% 
          ggvis(~x, ~y)%>% 
          set_options(height = graphHeight(), width = 800, resizable=FALSE) %>%
          scale_numeric("size", range = c(20, 200), nice = FALSE) 
          
        # if trendline input and numeric y-axis then plot trendline here UNDER the points
        # otherwise shows strange hover on trendline and standard error
          if(input$trend & is.numeric(test$y)){
            p <- p %>%
              layer_model_predictions(model = "lm", formula = y ~ x, 
                                      se = TRUE) 
          }
          
        p <- p %>%  layer_points( size = ~size, fill = ~region, opacity := op, key := ~place) %>% 
          # Add Y-axis
          add_axis("y", title = yaxis(), title_offset = yaxisoff(),
                   properties = axis_props(
            labels = list(  fontSize = 15),
            title = list(  fontSize = 20)
          )) %>%
          # Add x-axis
          add_axis("x", title = xaxis(), title_offset = 50,
                   properties = axis_props(
            labels = list(  fontSize = 15),
            title = list(  fontSize = 20)
          ))   %>%
          # Add legend for point size
          add_legend("size", 
                     properties = legend_props(legend = list(y = 150),
                                               labels = list(  fontSize = 15),
                                               title = list(fontSize = 18),
                                               symbols = list(fill="black")),
                     title = as.character(input$size))%>%
          # Add legend for point color
          add_legend("fill", 
                     properties = legend_props(legend = list(y = 300),
                                               labels = list(  fontSize = 15),
                                               title = list(fontSize = 20)),
                     title = "Region")  %>%
          set_options(duration = 0) %>%
          add_tooltip(all_values, "hover")
         
        # Add label text by place name on input
        if(input$text){
         p <- p %>% layer_text( text := ~place,
                                fill = ~region,
                                opacity := 1/1.25) 
        }
        
        
        # Send p to shiny UI function
        p  %>%bind_shiny("ggvis")
      }
    })
    
    
    
    
  }
)


