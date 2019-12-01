### Clear workspace ###

rm(list = ls())

### Read base data files into R as descriptive variables ###

library(readr)



###Score Column Specification###
my_col_types<- cols(
  Date = col_date(format = "%m/%d/%Y"),
  Week = col_skip(),
  `Playoff Game?` = col_skip(),
  `Home Team` = col_factor(),
  `Away Team` = col_factor(),
  `Team ID` = col_skip(),
  Stadium = col_factor(),
  `Neutral Site?` = col_skip()
)
  
scores<- read_csv("NFL_scores.csv", col_types = my_col_types, na = c("NA", "N/A", " "))
teams <- read_csv("NFL_teams.csv", na = c("NA", "N/A", " "))
###I wasn't sure what columns we needed so I took my best guess,
###we could probably get rid of humidity column unless
###we need it for the wind chill calculation.


###Rename columns in scores table so they're easier to work with###
names(scores)[names(scores) == "Home Team"] <- "home_team"
names(scores)[names(scores) == "Away Team"] <- "away_team"
names(scores)[names(scores) == "Home Score"] <- "home_score"
names(scores)[names(scores) == "Away Score"] <- "away_score"
names(scores)[names(scores) == "Spread Favorite"] <- "spread_favorite"
names(scores)[names(scores) == "Over/Under Line"] <- "over_under_line"
names(scores)[names(scores) == "Wind MPH"] <- "wind_speed"
names(scores)[names(scores) == "Weather Detail"] <- "weather_detail"



### Subset data, betting data not complete until 1990, 2019 shedule included but no data ###
###We could use data as early as 1980 if we can figure out how to get over/ under issue worked out###
scores<- subset(scores, Season >= 1990 & Season <= 2018)

### Import data for stadium information.

### Set column specifications
my_col_types<- cols(
  `Stadium Type` = col_factor(),
  `Stadium Weather Type` = col_factor(),
  `Stadium Capacity` = col_skip(),
  `Stadium Surface` = col_skip(),
  STATION = col_skip(),
  NAME = col_skip())

### Read in data

stadiums <- read_csv("NFL_stadiums.csv", col_types = my_col_types, na = c("NA", "N/A", ""))

### Rename column types for accessability

names(stadiums)[names(stadiums) == "Stadium Name"]      <- "name"
names(stadiums)[names(stadiums) == "Stadium Location"]  <- "city_state"
names(stadiums)[names(stadiums) == "Stadium Open"]      <- "open"
names(stadiums)[names(stadiums) == "Stadium Close"]     <- "close"
names(stadiums)[names(stadiums) == "Stadium Type"]      <- "type"
names(stadiums)[names(stadiums) == "Stadium Weather Station Code"]      <- "state"
names(stadiums)[names(stadiums) == "Stadium Weather Type"] <- "weather_type"
names(stadiums)[names(stadiums) == "LATITUDE"]          <- "lat"
names(stadiums)[names(stadiums) == "LONGITUDE"]         <- "long"
names(stadiums)[names(stadiums) == "ELEVATION"]         <- "elevation"


### Create a function to pull the State code out of a city location

state.pull <- function(location = "") {                     
  substr(location, nchar(location)-1, nchar(location))     
}

### Testing function

print(state.pull("San Antonio, TX"))   

### Extract State code from each stadium, store in stadiums df as an old column 
### newly renamed 'state'.

for(i in 1:nrow(stadiums)) {
  stadiums[i, 7] <- state.pull(stadiums[i, 2])
}

### Creating a cloropleth map of all NFL stadiums ###


### Load necessary packages ###

suppressPackageStartupMessages(library(choroplethr))
library(choroplethrMaps)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(reshape2)

### Reclassify the 'state' column in stadiums as a factor ###

stadiums$state <- factor(stadiums$state)

### Check the levels in new 'state' factor ###

levels(stadiums$state)

### Some levels were listed twice because of a difference in case.  Change all state abbreviations
### to upper case, rereformat as a factor. 

stadiums$state <- toupper(stadiums$state)

stadiums$state <- factor(stadiums$state)

### Create new column called 'region' to separate states with NFL teams into northern and southern

stadiums$region <- stadiums$state

### 'if' function to sort states into their respective regions

region <- character(nrow(stadiums))

for(i in 1:nrow(stadiums)) {
  if(stadiums[i, 7] == "AZ" |
     stadiums[i, 7] == "CA" |
     stadiums[i, 7] == "NC" |
     stadiums[i, 7] == "SC" |
     stadiums[i, 7] == "GA" |
     stadiums[i, 7] == "FL" |
     stadiums[i, 7] == "TN" |
     stadiums[i, 7] == "AL" |
     stadiums[i, 7] == "MS" |
     stadiums[i, 7] == "LA" |
     stadiums[i, 7] == "AR" |
     stadiums[i, 7] == "OK" |
     stadiums[i, 7] == "TX" |
     stadiums[i, 7] == "NM"){
    region[i] <- paste(c("south"))
  } else {
    region[i] <- paste(c("north"))
  }
}

###  Turn regional data into a factor column in 'stadiums'

stadiums$region <- region
stadiums$region <- factor(stadiums$region)
stadiums$state  <- factor(stadiums$state)


stadiums <- group_by(stadiums, region, state)
count(stadiums)

reg_summ <- count(stadiums)


### Need to replace state abbreviations with lowercase state names


state_regions <- as_tibble(state.regions)


reg_summ$region <- as.character(reg_summ$region)

for(i in 1:nrow(reg_summ)){
   for(j in 1:nrow(state_regions)){
     if(reg_summ[i, 2] == state_regions[j, 2]) {
       reg_summ[i, 2] <- print(c(state_regions[j, 1]))
     }
   }
 }



### Need to rename some columns to fit with the state_choropleth function

names(reg_summ)[names(reg_summ) == "region"]  <- "NoS"
names(reg_summ)[names(reg_summ) == "state"]   <- "region"
names(reg_summ)[names(reg_summ) == "n"]       <- "value"


### Creat and format choropleth map

p <- state_choropleth(reg_summ, title = "NFL teams by Location", legend = "Number of Teams")



p2 <- qplot(value, data = reg_summ, geom = "histogram", facets = . ~ NoS)

###Creating column for over under hit###
scores$combined_score<- as.integer(scores$home_score +scores$away_score)

scores$over_under_hit <- NA ##initialize blank column##
for (i in 1:nrow(scores)) {
  if (scores$combined_score[i] > scores$over_under_line[i]) {
    scores$over_under_hit[i]<- c("over hit")
  }
  if (scores$combined_score[i] < scores$over_under_line[i]) {
    scores$over_under_hit[i]<- c("under hit")
  }
  if (scores$combined_score[i] == scores$over_under_line[i]) {
    scores$over_under_hit[i]<- c("push")
  }
}












