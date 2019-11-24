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
stadiums <- read_csv("NFL_stadiums.csv", na = c("NA", "N/A", " "))
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



### Subset data, betting data not complete until 1980 ###
scores<- subset(scores, Season >= 1980)

### Creating a cloropleth map of all NFL stadiums ###

library(choroplethr)
library(choroplethrMaps)

stadium_loc <- subset(stadiums, is.null(LATITUDE) == FALSE)  ##Need to fix this, should have 72 entries

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
  if(is.na(scores$over_under_line == TRUE)){
    scores$over_under_hit<- NA
  }
}












