# Practice Gun Violence Chloropleth in Leaflet
#
library(leaflet)
library(jsonlite)
library(rgdal)

viol <- readRDS("data/viol.rds")

# From http://leafletjs.com/examples/choropleth.html
# file : http://leafletjs.com/examples/us-states.js
statesJson <- readLines("data/us-states.js", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)



#  Default styles for all features
statesJson$style = list(
  weight = 1,
  color = "white",
  opacity = 1,
  fillOpacity = 0.5
)

# Gather names from all states
statenames <- sapply(statesJson$features, function(feat) {
  feat$properties$name
})

# Create ranks in states df
viol <- viol[order(viol$Gun., decreasing = T),]
viol$Gun.rank <- 1:nrow(viol)
viol <- viol[order(viol$HomicideRate, decreasing = T),]
viol$Homicide.Rate.rank <- 1:nrow(viol)
viol <- viol[order(viol$Suicide.Rate, decreasing = T),]
viol$Suicide.Rate.rank <- 1:nrow(viol)
viol <- viol[order(viol$Population, decreasing = T),]
viol$Population.rank <- 1:nrow(viol)


# Add a properties$gun_own and $homicide_rate and $suicide_rate list to each feature
statesJson$features <- lapply(statesJson$features, function(feat) {
  feat$properties$gun_own <- subset(viol, State == feat$properties$name)[,"Gun."]
  feat$properties$gun_own_rank <- subset(viol, State == feat$properties$name)[,"Gun.rank"]
  feat$properties$homicide_rate <- subset(viol, State == feat$properties$name)[,"HomicideRate"]
  feat$properties$homicide_rate_rank <- subset(viol, State == feat$properties$name)[,"Homicide.Rate.rank"]
  feat$properties$suicide_rate <- subset(viol, State == feat$properties$name)[,"Suicide.Rate"]
  feat$properties$suicide_rate_rank <- subset(viol, State == feat$properties$name)[,"Suicide.Rate.rank"]
  feat$properties$population <- subset(viol, State == feat$properties$name)[,"Population"]
  feat$properties$population_rank <- subset(viol, State == feat$properties$name)[,"Population.rank"]
  feat$properties$region <- subset(viol, State == feat$properties$name)[,"Region"]
  feat
})



# Gather gun_own from all states
gunown <- sapply(statesJson$features, function(feat) {
  feat$properties$gun_own
})
gunown[[52]] <- as.numeric(NA)
gunown <- t(as.data.frame(gunown))

# Gather homicide_rate from all states
homrate <- sapply(statesJson$features, function(feat) {
  feat$properties$homicide_rate
})
homrate[[52]] <- as.numeric(NA)
homrate <- t(as.data.frame(homrate))

# Gather suicide_rate from all states
suirate <- sapply(statesJson$features, function(feat) {
  feat$properties$suicide_rate
})
suirate[[52]] <- as.numeric(NA)
suirate <- t(as.data.frame(suirate))

# Gather region from all states
region <- sapply(statesJson$features, function(feat) {
  feat$properties$region
})
region[[52]] <- as.numeric(NA)
region <- t(as.data.frame(region))

paldef <- colorNumeric("OrRd", gunown)
# Add a properties$style list to each feature
statesJson$features <- lapply(statesJson$features, function(feat) {
  feat$properties$style <- list(
    fillColor = paldef(
      feat$properties$gun_own
    )
  )
  feat
})


saveRDS(statesJson, "states-app/data/statesJson.rds")
saveRDS(statenames, "states-app/data/statenames.rds")
saveRDS(gunown, "states-app/data/gunown.rds")
saveRDS(homrate, "states-app/data/homrate.rds")
saveRDS(suirate, "states-app/data/suirate.rds")




library(shiny)

leaflet() %>% addGeoJSON(statesJson) %>% addTiles() 



