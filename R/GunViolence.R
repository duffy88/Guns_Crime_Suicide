# Cleans and compiles homicide, suicide and gun statistics at both the 
# country and state level before plotting. In addition, violent crime rates
# by state are cleaned, compiled and plotted
#
#
# By Doug Duffy 2016

# Load required packages
library(xlsx)
library(ggplot2)

#
# Load Data
#

options(stringsAsFactors = FALSE)
homicide <- read.xlsx("data/GunViolence.xlsx", sheetIndex = "Homicide")
suicide <- read.xlsx("data/GunViolence.xlsx", sheetIndex = "Suicide")
guns <- read.xlsx("data/GunViolence.xlsx", sheetIndex = "Guns")
states <- read.xlsx("data/GunViolence.xlsx", sheetIndex = "States")
viol <- read.xlsx("data/GunViolence.xlsx", sheetIndex = "StatesViolCrime")

#
# Cleaning
#

# Homicide 
homicide$Notes <- NULL
homicide <- homicide[-1:-2,]
names(homicide)[1] <- "Country"

#Suicide
suicide <- suicide[-1,]
suicide$Country <- gsub("[^[:alnum:]///' ]","", suicide$Country)
suicide$Country <- gsub(" (more info)","", suicide$Country)
for(i in 1:nrow(suicide)){
  if(suicide$Country[i] == "GuineaBissau") suicide$Country[i] <- "Guinea-Bissau"
  if(suicide$Country[i] == "TimorLeste") suicide$Country[i] <- "Timor-Leste"
}

# Guns
guns$Notes <- NULL
guns <- guns[-1,]
guns$Country <- gsub("[^[:alnum:]///' ]","", guns$Country)
for(i in 1:nrow(guns)){
  if(guns$Country[i] == "GuineaBissau") guns$Country[i] <- "Guinea-Bissau"
  if(guns$Country[i] == "TimorLeste") guns$Country[i] <- "Timor-Leste"
}

# States 
states <- states[-1:-4,]
for(i in 2:(ncol(states)-1)) states[,i] <- as.numeric(states[,i])

# States Violent Crime
viol <- viol[9:nrow(viol), 1:12]
names(viol) <- c("State", "Population","ViolentCrime","Homicide","ForcibleRape",
                 "Robbery","AggravatedAssault","ViolentCrimeRate","HomicideRate",
                 "ForcibleRapeRate","RobberyRate","AggaravatedAssaultRate")

#
# Merging of data from different sources
#

# Violent Crime Rates w/ Gun Ownership
viol <- merge(viol, states[,c("State","Region","Gun.","Suicide.Rate")], by = "State", all = T)
for(i in 2:12) viol[,i] <- as.numeric(viol[,i])

# Homicide Rates w/ Guns per Capita w/ Suicide Rates
homguns <- merge(homicide, guns[,-3], by = "Country", all = T)
homguns <- merge(homguns, suicide[,2:3], by = "Country", all = T)
for(i in c(3,6:7)) homguns[,i] <- as.numeric(homguns[,i])
names(homguns) <- c("Country","HomicideRate", "HomicideCount",
                    "Region","Subregion", "Year", "Guns.per.100","SuicideRate")

# Create ranks in homguns df
homguns <- homguns[order(homguns$Guns.per.100, decreasing = T),]
homguns$Guns.per.100.rank <- 1:nrow(homguns)
homguns <- homguns[order(homguns$HomicideRate, decreasing = T),]
homguns$Homicide.Rate.rank <- 1:nrow(homguns)
homguns <- homguns[order(homguns$SuicideRate, decreasing = T),]
homguns$Suicide.Rate.rank <- 1:nrow(homguns)

# Create ranks in viol df
viol <- viol[order(viol$Gun., decreasing = T),]
viol$Gun.Own.rank <- 1:nrow(viol)
viol <- viol[order(viol$ViolentCrimeRate, decreasing = T),]
viol$Violent.Crime.rank <- 1:nrow(viol)
viol <- viol[order(viol$HomicideRate, decreasing = T),]
viol$Homicide.rank <- 1:nrow(viol)
viol <- viol[order(viol$ForcibleRapeRate, decreasing = T),]
viol$Forcible.Rape.rank <- 1:nrow(viol)
viol <- viol[order(viol$RobberyRate, decreasing = T),]
viol$Robbery.rank <- 1:nrow(viol)
viol <- viol[order(viol$AggaravatedAssaultRate, decreasing = T),]
viol$Aggravated.Assault.rank <- 1:nrow(viol)
viol <- viol[order(viol$Suicide.Rate, decreasing = T),]
viol$Suicide.Rate.rank <- 1:nrow(viol)



#
# Final Cleaning and Excel Output
#

statesOut <- states[,c(1,7,9:11)]
names(statesOut) <- c("State","Homicide Rate","Suicide Rate", 
                      "Gun Ownership","Region")
statesOut <- statesOut[,c(1,5,2:4)]

countryOut <- homguns[, c(1,4,5,2,8,7,10,11,9)]
names(countryOut) <- c("Country","Region","Subregion",
                       "Homicide Rate","Suicide Rate","Guns per 100 People",
                       "Homicide Rate Rank", "Suicide Rate Rank", "Guns per 100 People Rank")

countryOut$Region<- factor(countryOut$Region, levels = c("Africa","Americas","Asia","Europe","Oceania"))
countryOut$Subregion<- factor(countryOut$Subregion, levels = c("Eastern Africa","Middle Africa",
                                                              "Northern Africa","Southern Africa", "Western Africa",
                                                              "Caribbean","Central America","Northern America",
                                                              "South America",
                                                              "Central Asia", "Eastern Asia","Southern Asia","South-Eastern Asia",
                                                              "Western Asia",
                                                              "Eastern Europe","Northern Europe","Southern Europe", "Western Europe",
                                                              "Australasia","Melanesia"))

violOut <- viol[, c(1,8:22)]
names(violOut) <- c("State", "Violent Crime Rate", "Homicide Rate",
                    "Forcible Rape Rate", "Robbery Rate","Aggravated Assault Rate",
                    "Region", "Gun Ownership","Suicide Rate",
                    "Gun Ownership Rank", "Violent Crime Rate Rank", "Homicide Rate Rank",
                    "Forcible Rape Rate Rank","Robbery Rate Rank","Aggravated Assault Rate Rank",
                    "Suicide Rate Rank")
violOut <- violOut[,c(1,7,2:6,8:16)]

statesOut <- merge(statesOut[,c("State","Region","Suicide Rate","Gun Ownership")],
                   violOut[,c("State","Violent Crime Rate","Homicide Rate",
                              "Forcible Rape Rate","Robbery Rate","Aggravated Assault Rate",
                              "Gun Ownership Rank", "Violent Crime Rate Rank", "Homicide Rate Rank",
                              "Forcible Rape Rate Rank","Robbery Rate Rank","Aggravated Assault Rate Rank",
                              "Suicide Rate Rank")],
                   by = "State", all= T)

statesOut$Region <- factor(statesOut$Region, levels = c("Midwest", "Northeast","South","West"))

write.xlsx2(countryOut, "data/HomideSuicideGuns.xlsx", sheetName="Countries",row.names=F,showNA=F)
write.xlsx2(statesOut, "data/HomideSuicideGuns.xlsx", sheetName="States",row.names=F,showNA=F, append=T)
write.xlsx2(violOut, "data/HomideSuicideGuns.xlsx", sheetName="States Violent Crime",row.names=F,showNA=F, append=T)

saveRDS(countryOut, "gungraphs-app/data/countryOut.rds")
saveRDS(statesOut, "gungraphs-app/data/statesOut.rds")
saveRDS(countryOut, "data/countryOut.rds")
saveRDS(statesOut, "data/statesOut.rds")
saveRDS(viol, "data/viol.rds")
