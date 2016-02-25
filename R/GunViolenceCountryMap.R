# Gun Violence Chloropleth in Leaflet/Shiny
# Country Data
library(leaflet)
library(jsonlite)

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
countriesJson <- readLines("data/countryjson/countries.geojson", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

#  Default styles for all features
countriesJson$style = list(
  weight = 1,
  color = "white",
  opacity = 1,
  fillOpacity = 0.5
)


# Gather names from all countries
countrynames <- sapply(countriesJson$features, function(feat) {
  feat$properties$name
})

# See which country names don't match
#countrynames[!(countrynames %in% homguns$Country)]
# homguns$Country[!(homguns$Country %in% countrynames)]

tomodify <- c("Bosnia and Herz.", "Central African Rep.","Côte d'Ivoire",
              "Dem. Rep. Congo","Czech Rep.","Dominican Rep.","Eq. Guinea",
              "Greenland","Korea","Lao PDR","New Caledonia","Puerto Rico",
              "Dem. Rep. Korea","S. Sudan","Solomon Is.")
repl <- c("Bosnia and Herzegovina", "Central African Republic","Ivory Coast",
          "Democratic Republic of the Congo","Czech Republic","Dominican Republic","Equatorial Guinea",
          "Greenland (Denmark)","South Korea","Laos","New Caledonia (France)","Puerto Rico (US)",
          "North Korea","South Sudan","Solomon Islands")

# Replace/modify certain country names to match
countriesJson$features <- lapply(countriesJson$features, function(feat) {
  if(feat$properties$name %in% tomodify){
    feat$properties$name <- repl[which((feat$properties$name==tomodify))]
  }
  feat
})

# Gather names from all countries
countrynames <- sapply(countriesJson$features, function(feat) {
  feat$properties$name
})

# Create ranks in homguns df
homguns <- homguns[order(homguns$Guns.per.100, decreasing = T),]
homguns$Guns.per.100.rank <- 1:nrow(homguns)
homguns <- homguns[order(homguns$HomicideRate, decreasing = T),]
homguns$Homicide.Rate.rank <- 1:nrow(homguns)
homguns <- homguns[order(homguns$SuicideRate, decreasing = T),]
homguns$Suicide.Rate.rank <- 1:nrow(homguns)

# Add a properties$gun_own and $homicide_rate and $suicide_rate list to each feature
countriesJson$features <- lapply(countriesJson$features, function(feat) {
  feat$properties$gun_own <- subset(homguns, Country == feat$properties$name)[,"Guns.per.100"]
  feat$properties$gun_own_rank <- subset(homguns, Country == feat$properties$name)[,"Guns.per.100.rank"]
  feat$properties$homicide_rate <- subset(homguns, Country == feat$properties$name)[,"HomicideRate"]
  feat$properties$homicide_rate_rank <- subset(homguns, Country == feat$properties$name)[,"Homicide.Rate.rank"]
  feat$properties$suicide_rate <- subset(homguns, Country == feat$properties$name)[,"SuicideRate"]
  feat$properties$suicide_rate_rank <- subset(homguns, Country == feat$properties$name)[,"Suicide.Rate.rank"]
  feat
})


# Gather gun_own from all countries
gunown <- sapply(countriesJson$features, function(feat) {
  feat$properties$gun_own
})
for( i in 1:length(gunown)){
  if(length(gunown[i][[1]])==0) gunown[i] <- NA
}
gunown <- t(as.data.frame(gunown))

# Gather homicide_rate from all countries
homrate <- sapply(countriesJson$features, function(feat) {
  feat$properties$homicide_rate
})
for( i in 1:length(homrate)){
  if(length(homrate[i][[1]])==0) homrate[i] <- NA
}
homrate <- t(as.data.frame(homrate))

# Gather suicide_rate from all countries
suirate <- sapply(countriesJson$features, function(feat) {
  feat$properties$suicide_rate
})
for( i in 1:length(suirate)){
  if(length(suirate[i][[1]])==0) suirate[i] <- NA
}
suirate <- t(as.data.frame(suirate))

paldef <- colorNumeric("OrRd", gunown)
# Add a properties$style list to each feature
countriesJson$features <- lapply(countriesJson$features, function(feat) {
  feat$properties$style <- list(
    fillColor = paldef(
      feat$properties$gun_own
    )
  )
  feat
})

saveRDS(countriesJson, "countries-app/data/countriesJson.rds")
saveRDS(countrynames, "countries-app/data/countrynames.rds")
saveRDS(gunown, "countries-app/data/gunown.rds")
saveRDS(suirate, "countries-app/data/suirate.rds")
saveRDS(homrate, "countries-app/data/homrate.rds")




leaflet() %>% addGeoJSON(countriesJson) %>% addTiles() 


