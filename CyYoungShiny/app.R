library(caret)
library(class)
library(dplyr)
library(e1071)
library(EnvStats)
library(data.table)
library(ggplot2)
library(janitor)
library(lubridate)
library(mapproj)
library(maps)
library(partykit)
library(purrr)
library(randomForest)
library(readr)
library(rpart)
library(rpart.plot)
library(rvest)
library(stringr)
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyr)
library(tidyverse)
library(DT)



# CHUNK 2: READING IN DATA 
# Note: For the sake of speed, we downloaded our final dataset as a csv and worked with
#       that throughout the project, rather than reading in and wrangling the data
#       each time. This "shortcut" is shown in Chunk 4.
# Content: We include our data reading code here that we used to maka data.csv.

# # -------------------------------------------------- read in data from first website
# url <- "https://www.baseball-reference.com/awards/cya.shtml"
# page <- read_html(url)
# tables.html <- html_nodes(page, "table")
# table <- html_table(tables.html[[1]], fill = TRUE) 
# winners <- table %>%
#   drop_na()
# # -------------------------------------------------- reading in data from second website
# players <- read_csv("stats.csv")
# # -------------------------------------------------- reading in data from third website
# #temp_url for mlb.com data
# temp_url <- "https://www.mlb.com/stats/pitching/#playerPool=QUALIFIED"
# # there are 3 pages for each Year, and some with 4 pages
# pages <- data.table(firstpage = c(2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006,2005, 2004,2003,2002,2001,2000,1999,1998,1997,1996,1995,1993,1992,1991,1990,1989,1988,1987,1986,1985,1984,1983,1982,1980,1979,1978,1977,1976,1975,1974,1973,1972,1971,1970,1969,1968,1967), secondpage = c(2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006,2005,2004,2003,2002,2001,2000,1999,1998,1997,1996,1995,1993,1992,1991,1990,1989,1988,1987,1986,1985,1984,1983,1982,1980,1979,1978,1977,1976,1975,1974,1973,1972,1971,1970,1969,1968,1967), thirdpage = c(2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006,2005,2004,2003,2002,2001,2000,1999,1998,1997,1996,1995,1993,1992,1991,1990,1989,1988,1987,1986,1985,1984,1983,1982,1980,1979,1978,1977,1976,1975,1974,1973,1972,1971,1970,1969,1968,1967),
# fourthpage = c(2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005,2004,2003,2002,2001,2000,1999,1998,1997,1996,1993,1992,1991,1989,1988,1987,1986,1985,1984,1983,1982,1980,1979,1978,1977,1976,1975,1974,1973,1972,1971,1970,1969,1968))
# #(1981 and 1994 are outlier years, players went on strike)
# #make all the years into characters
# pages$firstpage <- as.character(pages$firstpage)
# pages$secondpage <- as.character(pages$secondpage)
# pages$thirdpage <- as.character(pages$thirdpage)
# pages$fourthpage <- as.character(pages$fourthpage)
# #add the defining parts of the url to each Year
# pages$firstpage <- paste0(pages$firstpage, "?")
# pages$secondpage <- paste0(pages$secondpage, "?page=2&")
# pages$thirdpage <- paste0(pages$thirdpage, "?page=3&")
# pages$fourthpage <- paste0(pages$fourthpage, "?page=4&")
# #put all pages into one column
# pages <- data.frame(pages = c(t(pages)), stringsAsFactors=FALSE)
# #get it in vector form as characters
# pages <- as.character(pages[, 1])
# #replace the # in the temp_url with the defining charactersitics of each site
# urls <- str_replace(temp_url, "#", as.character(pages))
# #function to get table
# pull_table <- function(url){
# Htmlfun <- read_html(url)
# HtmlTablesfun <- html_nodes(Htmlfun, "table")
# Tablefun <- html_table(HtmlTablesfun[[1]]) 
# #SVO started getting calculated in 1969, so there are some years with 
# #-- instead of a number, make them all characters
# Tablefun$SVO <- as.character(Tablefun$SVO)
# Tablefun %>%
#   as_tibble()
# }
# #use map_df to get all of this data in dataframe form
# pages_df <- map_df(urls, pull_table)



# # CHUNK 3: DATA WRANGLING
# # Note: For the sake of speed, we downloaded our final dataset as a csv and worked with
# #       that throughout the project, rather than reading in and wrangling the data
# #       each time. This "shortcut" is shown in Chunk 4.
# # Content: We include our data wrangling code here that we used to make data.csv.
# 
# # ---------------------------------------------------- data wrangling first website
# winners_new <- winners %>%
#   select(Name, Year, Voting, Lg, SV, Tm, WAR, SV) %>% 
#   rename(Name = Name, Year = Year, voting = Voting) %>%
#   filter(Year >= 1967, Year < 2020)
# # unique(winners_new$Name) tells us there were 107 wins 
# # but only 70 winners (repeat winners)
# 
# # --------------------------------------------------- data wrangling second website
# players_new <- players %>%
# mutate(Name = str_c(first_Name, " ", last_Name)) %>% # replicate the Name format
# relocate(Name) %>%
# select(-last_Name, -first_Name)
# # ----------------------------------------------------- data wrangling third website
# #have been Name changes before, adjusted for those
# pages_df$Team <- recode_factor(pages_df$Team, "WAS" = "WSH", "CAL" = "LAA",
#     "ANA" = "LAA", "LA" = "LAD", "KCA" = "OAK", "MON" = "WSH", "FLA" = "MIA")
# #if they are a NL Team, return true
# pages_df <- pages_df %>%
#   mutate(league = 
# Team == "ARI"|Team =="ATL"|Team=="CHC"|Team=="CIN"|Team=="COL"|Team=="MIA"|
#   Team=="LAD"|Team=="MIL"|Team=="NYM"|Team=="PHI"|Team=="PIT"|Team=="SD"|
#   Team=="SF"|Team=="STL"|Team=="WSH")
# #if it's true, it is NL Team
# pages_df <- pages_df %>%
#   mutate(League = ifelse(league == TRUE, "NL", "AL"))
# pages_df <- pages_df %>%
#   rename(ERA = `caret-upcaret-downERA`)
# # --------------------------------------------------------------- FINAL DATA
# # for some of these variables, we will only have values for players 
# # who won the award (WAR, SV, etc.)
# data <- left_join(players_new, winners_new, by = c("Name", "Year")) %>% 
#   mutate(winner = ifelse(is.na(voting), "no", "yes")) %>% 
#   mutate(winner = factor(winner, levels = c("yes", "no"))) %>%
#   rename(W = p_win, L = p_loss, ERA = p_era, IP = p_formatted_ip, age = player_age, 
#          SO = p_strikeout, Team = Tm, league = Lg)
# data <- left_join(pages_df, data, by = c("W", "L", "SO", "ERA", "IP"))
# data <- data %>%
#   select(Name, Team, League, winner, Year, age, W, L, SO, ERA, IP, H, R, 
#          HR, BB, WHIP, AVG, SHO) %>%
#   filter(Name != "NA")
# 
# data <-  data %>%
#   mutate(region = Team ==
# "ARI"|Team=="COL"|Team=="LAD"|Team=="SD"|Team=="SF"|
# Team== "SEA"|Team == "TEX"| Team== "HOU"| Team=="LAA"|Team=="OAK")
#  data <-  data %>%
#   mutate(West = ifelse(region == TRUE, "West", "Central or East"))
#  data <-  data %>%
#   mutate(region =
# Team == "ATL"|Team=="BAL"|Team=="BOS"|Team=="WSH"|Team=="NYY"|
# Team== "NYM"|Team == "PHI"| Team== "TOR"| Team=="TB"|Team=="MIA")
#  data <-  data %>%
#   mutate(East = ifelse(region == TRUE, "East", "Central or West"))
#  data <-  data %>%
#  mutate(region =
# Team == "CHC"|Team=="CWS"|Team=="CIN"|Team=="PIT"|Team=="STL"|
# Team== "CLE"|Team == "KC"| Team== "MIN"| Team=="MIL"|Team=="DET")
#  data <-  data %>%
#  mutate(Central = ifelse(region == TRUE, "Central", "East or West"))
#  data <-  data %>%
#   mutate(Region = ifelse(West == "West", "West", ifelse(East ==
# "East", "East", ifelse(Central == "Central", "Central",
#  "hopefully nothing"))))
#  data <- data %>%
#    mutate(Division = ifelse(Region == "West" & League == "AL",
#    "AL West", ifelse(Region == "West" & League == "NL",
#    "NL West", ifelse(Region == "East" & League == "AL",
#    "AL East", ifelse(Region == "East" & League == "NL",
#    "NL East", ifelse(Region == "Central" & League == "AL",
#    "AL Central", ifelse(Region == "Central" & League == "NL",
#    "NL Central", "hopefully nothing"))))))) %>%
#    select(-region, -West, -East, -Central)
# 
# levels(data$winner)
# table(data$winner) 


### CHUNK 4: READING IN DATA SHORTCUT
# Content: Our shortcut for reading in our data. 
# We also include a few commands which tidy it up.

data <- read_csv('data.csv') %>%
  select(-1)



data <- data %>%
  rename(Team = TEAM, Name = name, Year = year)

#4272 x 20



## CHUNK 5: TRAIN/TEST DATA
# Content: We randomly choose 80% of our cases to be training, and 20% to be test.

data <- data %>% mutate(winner = factor(winner, levels = c("yes", "no")), 
                        Team = factor(Team), League = factor(League))

set.seed(71903948)
n <- nrow(data)
train_index <- sample(nrow(data), size=round(.8*n)) # 80% training

#train data
data_train <- data %>%
  slice(train_index)

#test data
data_test <- data %>%
  slice(-train_index)



# CHUNK 6: KNN
# Content: We use k-nearest-neighbor classification and cross-validation
# to predict whether a test case is a Cy Young Award winner or not.

# k_df <- data.frame(k = seq(1,25, by = 2))
# train_control <- trainControl(method ="cv", # cross-validation
#                               number = 10) # 10-folds
#                               
# set.seed(30498492)
# data_knn_cv <- train(winner ~ .-Name -Team -League -Region -Division, # predict using numeric vars
#                       data = data_train, # training data
#                       method ="knn", # classification method
#                       preProc = c("center","scale"), # standardize predictors
#                       tuneGrid = k_df, # knn parameters
#                       trControl = train_control) # validation method                          
#                               
# data_knn_cv$finalModel   ## so we will use a k-value of 3, tuned for accuracy
# 
# standard_fun <- function(x) { (x - mean(x, na.rm=TRUE))/sd(x, na.rm = TRUE)}
# 
# train_pred_stand <- data_train %>%
#   select(-Name, -Team, -League, -winner, -Region, -Division) %>%
#   mutate_all(standard_fun)
# 
# # get training means
# train_mns <- data_train %>%
#   select(-Name, -Team, -League, -winner, -Region, -Division) %>%
#   summarize_all(mean, na.rm = TRUE)
# 
# # get training sd's
# train_sds <- data_train %>%
#   select(-Name, -Team, -League, -winner, -Region, -Division) %>%
#   summarize_all(sd, na.rm = TRUE)
# 
# # standardize TEST predictors using TRAINING means and sd's
# test_pred_stand <- data_test %>%
#   select(-Name, -Team, -League, -winner, -Region, -Division)
# 
# for (i in colnames(train_mns)) {
#   test_pred_stand[[i]] <- ((test_pred_stand[[i]] - train_mns[[i]])/train_sds[[i]])
# }
# 
# set.seed(4869850) # make reproducible in case of ties
# data_knn <- knn(train = train_pred_stand, # stand. train predictors
#                  test = test_pred_stand, # stand. test predictors
#                  cl= data_train$winner, # train classes
#                  k=3) # neighborhood size
# 
# # add these predictions to the test set
# data_test <- data_test %>%
#   mutate(prediction = data_knn)
# 
# # compute stats
# data_test %>%
#   summarize(accuracy = mean(prediction == winner), sensitivity = 
#               sum(prediction == "yes" & winner == "yes")/sum(winner =="yes"), 
#             specificity = sum(prediction == "no" & winner == "no")/
#               sum(winner == "no"), precision = sum(prediction == 
#               "yes" & winner == "yes")/sum(prediction == "yes"))
# 
# # accuracy 0.984, sensitivity 0.267, specificity 0.996, precision 0.571



## CHUNK 7: RANDOM FOREST (full)
# Content: We use a random forest to average the results of 500 decision
# trees which predict whether or not a test case is a Cy Young Award winner.
# We consider all predictors and get an optimal mtry value is 3, but in
# the next chunk we make another forest narrowing our scope to just the 5
# most important predictors.

# set.seed(71903948)
# 
# train_control_rf <- trainControl(
#                       method = "cv", # cross-validation
#                       number = 10 # 10-folds
#                       )
# 
# # try mtry values from 1 to 8
# mtry_grid <- data.frame(mtry = seq(1, 8, by = 1))
# rf_cv <- train(
#             winner ~ .-Name,
#             data = data_train, # training data
#             method = "rf", # classification method
#             tuneGrid = mtry_grid, # rf parameters
#             trControl = train_control_rf # validation method
#               )
# plot(rf_cv)
# 
# # 3 is the best mtry. Thus train again using mtry = 3
# 
# rf_best <- randomForest(
#                 winner ~ .-Name,
#                 data = data_train,
#                  ntree = 500,
#                  mtry = 3)
# 
# varImpPlot(rf_best)



## CHUNK 8: RANDOM FOREST (reduced) 
# Content: We use a random forest to average the results of 500 decision
# trees which predict whether or not a test case is a Cy Young Award winner.
# Rather than considering all predictors, we narrow
# our scope to the 5 most important predictors (W, TEAM, SO, ERA, and WHIP)
# and find that our predictions are more accurate. Our optimal mtry value is 3.

# most important predictors: W, Team, SO, ERA, WHIP
# we will train again using only these 5 vars.
# mtry_grid <- data.frame(mtry = seq(1, 5, by = 1))
# rf_reduced <- train(
#                 winner ~ W + Team + SO + ERA + WHIP,
#                 data = data_train, # training data
#                 method = "rf", # classification method
#                 tuneGrid = mtry_grid, # rf parameters
#                 trControl = train_control_rf # validation method
#               )
# plot(rf_reduced)

# 3 is still our optimal mtry value
# we train using mtry = 3

rf_best_reduced <- randomForest(
  winner ~ W + Team + SO + ERA + WHIP,
  data = data_train,
  ntree = 500,
  mtry = 3)

# We compare the two random forests.

# conf_mat_rf_best <- confusionMatrix(
#                       data = predict(rf_best, newdata = data_test),
#                       reference = data_test$winner,
#                       positive = "yes" )

conf_mat_rf_best_reduced <- confusionMatrix(
  data = predict(rf_best_reduced, newdata = data_test),
  reference = data_test$winner,
  positive = "yes" )


# conf_mat_rf_best$byClass
# conf_mat_rf_best$overall
# conf_mat_rf_best_reduced$byClass
# conf_mat_rf_best_reduced$overall

# The reduced model, which only considered the five most
# important variables, is the better model due to it's higher precision.

# This model predicts with 98.2% accuracy, 50% precision,
# 20% sensitivity, and 99.6% specificity.



# CHUNK 9: DATA BOUNDS
# Content: We calculate the min and max values of each
# predictor for use in the sliderInput.

data_bounds <- data %>% summarise(min_Year = min(Year, na.rm = TRUE),
                                  max_Year = max(Year, na.rm = TRUE),
                                  min_age = min(age, na.rm = TRUE),
                                  max_age = max(age, na.rm = TRUE),
                                  min_W = min(W, na.rm = TRUE),
                                  max_W = max(W, na.rm = TRUE),
                                  min_L = min(L, na.rm = TRUE),
                                  max_L = max(L, na.rm = TRUE),
                                  min_SO = min(SO, na.rm = TRUE),
                                  max_SO = max(SO, na.rm = TRUE),
                                  min_ERA = min(ERA, na.rm = TRUE),
                                  max_ERA = max(ERA, na.rm = TRUE),
                                  min_IP = min(IP, na.rm = TRUE),
                                  max_IP = max(IP, na.rm = TRUE),
                                  min_H = min(H, na.rm = TRUE),
                                  max_H = max(H, na.rm = TRUE),
                                  min_R = min(R, na.rm = TRUE),
                                  max_R = max(R, na.rm = TRUE),
                                  min_HR = min(HR, na.rm = TRUE),
                                  max_HR = max(HR, na.rm = TRUE),
                                  min_BB = min(BB, na.rm = TRUE),
                                  max_BB = max(BB, na.rm = TRUE),
                                  min_WHIP = min(WHIP, na.rm = TRUE),
                                  max_WHIP = max(WHIP, na.rm = TRUE),
                                  min_AVG = min(AVG, na.rm = TRUE),
                                  max_AVG = max(AVG, na.rm = TRUE),
                                  min_SHO = min(SHO, na.rm = TRUE),
                                  max_SHO = max(SHO, na.rm = TRUE))




## CHUNK 10: MAP CODE
# Content: We data wrangle and perform analyses on regions and teams
# in order to create the map on our Shiny site.

# read in parks.csv file
Parks <- read_csv("Parks.csv")

# na in end represents current stadiums, texas did not have 
# a stadium in this data so their end is 2019. london and st paul
# are not mlb stadiums. str_sub PARKID to get abbrevations for teams
Parks <- Parks %>%
  filter(is.na(END) | END == 20190929) %>%
  filter(CITY != "London", CITY != "St. Paul") %>% 
  mutate(Team = str_sub(PARKID, 1, 5))

# some cities have two teams, so recode those right now
Parks$Team <- recode_factor(Parks$Team, "CHI11" = "CHC", "CHI12" ="CWS", "NYC20" = "NYM", "NYC21" = "NYY")

# get rid of digits now that there is no duplicates
Parks <- Parks %>%
  mutate(Team = str_replace_all(Team, "\\d", ""))

# change some abbreveations for parks to match with data abbreveations
Parks$Team <- recode_factor(Parks$Team, "ANA" = "LAA",
                            "LOS" ="LAD", "DEN" = "COL", "KAN" = "KC", "PHO" = "ARI", 
                            "SAN" = "SD", "ARL" = "TEX", "STP" = "TB", "WAS" = "WSH", "SFO" = "SF")

# recode states to match with world map names
Parks$STATE <- recode_factor(Parks$STATE, "AZ" = "arizona", 
                             "CA" ="california", "CO" = "colorado", 
                             "DC" = "district of columbia", "FL" = "florida", 
                             "GA" = "georgia", "IL" = "illinois", "MA" = "massachusetts", 
                             "MD" = "maryland", "MI" = "michigan", "MN" = "minnesota", 
                             "MO" = "missouri", "NY" = "new york", "OH" = "ohio", 
                             "ONT" = "Canada", "PA" = "pennsylvania", "TX" = "texas", 
                             "WA" = "washington", "WI" = "wisconsin" )

# read in world map for canada and us
canada <- map_data("world", region = "canada")

# paste 1111 so order and group is not same for canada and us
canada$group <- paste0(canada$group, 1111)
canada$order <- paste0(canada$order, 1111)
states_map <-map_data("state")
canadaus <- rbind(canada, states_map)

# get only the west states
West <- canadaus %>%
  filter(region == "washington" | region == "oregon"| 
           region == "california"| region == "nevada"| region == "idaho"|
           region == "montana"| region == "wyoming"| region == "utah"|
           region == "arizona"| region == "new mexico"| 
           region == "colorado"| region == "wyoming"|
           region == "north dakota"| region == "south dakota"| 
           region == "nebraska"| region == "kansas"| region == "oklahoma"| region == "texas")

# get only the central states
Central <- canadaus %>%
  filter(region == "north dakota"| region == "south dakota"|
           region == "nebraska"| region == "kansas"| region == "oklahoma"|
           region == "texas"| region == "minnesota"| region == "iowa"|
           region == "missouri"| region == "arkansas"| 
           region == "lousiana"| region == "mississippi"| 
           region == "tennessee"| region == "alabama"| 
           region == "kentucky"| region == "ohio"| 
           region == "indiana"| region == "michigan"| 
           region == "wisconsin" | region == "illinois")

# get only the eastern states
East <- canadaus %>%
  filter(region == "florida"| region == "georgia"| region == "north carolina"| region == "south carolina"| region == "virginia"| region == "maryland"| region == "west virginia"| region == "pennsylvania"| region == "maryland"| region == "delaware"| region == "new jersey"| region == "new york"| region == "massachusetts" | region == "Canada")

# join parks and data by team
data <- left_join(data, Parks, by = c("Team" = "Team"))

#mutate color for team based on abbreveation
data <- data %>% 
  mutate(color = ifelse(Team == "ARI", "#E3D4AD", 
                        ifelse(Team == "ATL", "#CE1141", 
                               ifelse(Team == "BAL", "#DF4601", 
                                      ifelse(Team == "BOS", "#BD3039", 
                                             ifelse(Team =="CHC", "#0E3386",
                                                    ifelse(Team == "CWS", "#27251F", 
                                                           ifelse(Team == "CIN", "#C6011F",
                                                                  ifelse(Team == "CLE", "#0C2340",
                                                                         ifelse(Team == "COL", "#33006F",
                                                                                ifelse(Team == "DET", "#FA4616",
                                                                                       ifelse(Team == "HOU", "#9A3324",
                                                                                              ifelse(Team == "KC", "#004687",
                                                                                                     ifelse(Team == "LAA", "#BA0021",
                                                                                                            ifelse(Team == "LAD", "#005A9C",
                                                                                                                   ifelse(Team == "MIA", "#FF6600",
                                                                                                                          ifelse(Team == "MIL", "#FFC52F",
                                                                                                                                 ifelse(Team == "MIN", "#002B5C",
                                                                                                                                        ifelse(Team == "NYM", "#FF5910", 
                                                                                                                                               ifelse(Team == "NYY", "#003087",
                                                                                                                                                      ifelse(Team == "OAK", "#115740",
                                                                                                                                                             ifelse(Team == "PHI", "#E81828",
                                                                                                                                                                    ifelse(Team == "PIT", "#FDB827",
                                                                                                                                                                           ifelse(Team == "STL", "#C41E3A",
                                                                                                                                                                                  ifelse(Team == "SD", "#FFC425",
                                                                                                                                                                                         ifelse(Team == "SF", "#FD5A1E",
                                                                                                                                                                                                ifelse(Team == "SEA", "#005C5C",
                                                                                                                                                                                                       ifelse(Team == "TB", "#8FBCE6",
                                                                                                                                                                                                              ifelse(Team == "TEX", "#003278",
                                                                                                                                                                                                                     ifelse(Team == "TOR", "#006BA6",
                                                                                                                                                                                                                            ifelse(Team == "WSH", "#AB0003",
                                                                                                                                                                                                                                   "nothing")))))))))))))))))))))))))))))))

# select variables we want
data <- data %>%
  select(Name, Team, League, winner, Year, age, W, L, SO, ERA,
         IP, H, R, HR, BB, WHIP, AVG, SHO, Region, Division, Latitude,
         Longitude, STATE, color)

# rename 
data <- data %>%
  rename(State = STATE)

# make years for choices in selectinput
years <- data %>%
  select(Year) %>%
  distinct() %>%
  drop_na() 
years$Year <- as.character(years$Year)

# add all years
years <- years %>%
  add_row(Year = "All Years")

# desc by year
years <- years %>%
  arrange(desc(Year))

# get leagues for choices in selectinput
Leagues <- data.frame(leauges = c("AL", "NL"))

# get regions for choices in selectinput
Regions <- data.frame(regions = c("West", "Central", "East"))

# get divisions for choices in selectinput
Divisions <- data.frame(divisions = c("AL West", "AL Central", "AL East", "NL West", "NL Central", "NL East"))


# function to add strings together
`+` <- function(e1, e2) {
  if (is.character(e1) | is.character(e2)) {
    paste0(e1, e2)
  } else {
    base::`+`(e1, e2)
  }
}

# get franchise name for shiny map output
data <- data %>% 
  mutate(Franchise = ifelse(Team == "ARI", "Arizona Diamondbacks",
                            ifelse(Team == "ATL", "Atlanta Braves", 
                                   ifelse(Team == "BAL", "Baltimore Orioles", 
                                          ifelse(Team == "BOS", "Boston Red Sox", 
                                                 ifelse(Team =="CHC", "Chicago Cubs",
                                                        ifelse(Team == "CWS", "Chicago White Sox", 
                                                               ifelse(Team == "CIN", "Cincinnati Reds",
                                                                      ifelse(Team == "CLE", "Cleveland Indians",
                                                                             ifelse(Team == "COL", "Colorado Rockies",
                                                                                    ifelse(Team == "DET", "Detroit Tigers",
                                                                                           ifelse(Team == "HOU", "Houston Astros",
                                                                                                  ifelse(Team == "KC", "Kansas City Royals",
                                                                                                         ifelse(Team == "LAA", "Los Angeles Angels",
                                                                                                                ifelse(Team == "LAD", "Los Angeles Dodgers",
                                                                                                                       ifelse(Team == "MIA", "Miami Marlins",
                                                                                                                              ifelse(Team == "MIL", "Milwaukee Brewers",
                                                                                                                                     ifelse(Team == "MIN", "Minnesota Twins",
                                                                                                                                            ifelse(Team == "NYM", "New York Mets", 
                                                                                                                                                   ifelse(Team == "NYY", "New York Yankees",
                                                                                                                                                          ifelse(Team == "OAK", "Oakland Athletics",
                                                                                                                                                                 ifelse(Team == "PHI", "Philladelphia Phillies",
                                                                                                                                                                        ifelse(Team == "PIT", "Pittsburgh Pirates",
                                                                                                                                                                               ifelse(Team == "STL", "St. Louis Cardinals",
                                                                                                                                                                                      ifelse(Team == "SD", "San Diego Padres",
                                                                                                                                                                                             ifelse(Team == "SF", "San Francisco Giants",
                                                                                                                                                                                                    ifelse(Team == "SEA", "Seattle Mariners",
                                                                                                                                                                                                           ifelse(Team == "TB", "Tampa Bay Rays",
                                                                                                                                                                                                                  ifelse(Team == "TEX", "Texas Rangers",
                                                                                                                                                                                                                         ifelse(Team == "TOR", "Toronto Blue Jays",
                                                                                                                                                                                                                                ifelse(Team == "WSH", "Washington Nationals", "nothing")))))))))))))))))))))))))))))))

# get teams data frame for choices in selectinput
Teams <- data.frame(Teams = c("Arizona Diamondbacks", "Atlanta Braves", 
                              "Baltimore Orioles", "Boston Red Sox", "Chicago Cubs",
                              "Chicago White Sox", "Cincinnati Reds", "Cleveland Indians",
                              "Colorado Rockies", "Detroit Tigers", "Houston Astros",
                              "Kansas City Royals", "Los Angeles Angels",
                              "Los Angeles Dodgers", "Miami Marlins","Milwaukee Brewers",
                              "Minnesota Twins", "New York Mets", "New York Yankees", 
                              "Oakland Athletics", "Philladelphia Phillies",
                              "Pittsburgh Pirates", "St. Louis Cardinals", "San Diego Padres",
                              "San Francisco Giants", "Seattle Mariners", "Tampa Bay Rays",
                              "Texas Rangers", "Toronto Blue Jays", "Washington Nationals"))

# select new data we want
data <- data %>%
  select(Name, Team, League, winner, Year, age, W, L, SO, ERA,
         IP, H, R, HR, BB, WHIP, AVG, SHO, Latitude, Longitude, State,
         color, Region, Division, Franchise)

# rename
data <- data %>%
  rename(long = Longitude, lat = Latitude, Name = Name, Year = Year)

# make new longs for some teams so that it doesn't interfere
data <- data %>%
  mutate(long = ifelse(Team == "SEA" | Team == "OAK", -120, 
                       ifelse(Team == "NYY", -75, data$long)))

data <- data %>%
  mutate(lat = ifelse(Team == "NYY", 43, data$lat))

# make data_coloured to get colored columns for data table
data_coloured <- data
colnames(data_coloured)[c(1, 2, 3, 5, 6:17)] <- paste0('<span style="color:',c("red"),'">',colnames(data)[c(1, 2, 3, 5, 6:17)],'</span>')

# function for fill of ggplots
functionwinnersper <- function(x){
  data <- x %>%
    # group by team
    group_by(`<span style=\"color:red\">Team</span>`) %>%
    # winners per team
    mutate(numberofwinnersperTeam = length(which(winner == "yes")))
  data <- data %>%
    # group by state
    group_by(State) %>%
    # get winners per state
    mutate(numberofwinnersperstate = length(which(winner == "yes"))) %>%
    ungroup()
  data
}

# function for adding number of winners to canadaus data frame
functioncanadaus <- function(x){
  # make data frame of winners per state
  numberofwinnersperstate <- data.frame(numberofwinnersperstate =
                                          x$numberofwinnersperstate, state = x$State)
  # join these by state/region
  newdata <- left_join(canadaus, numberofwinnersperstate, by = c("region" = "state"))
  # get rid of duplicated points
  canadaus <- newdata[!duplicated(newdata$order), ]
  # make NA 0
  canadaus[is.na(canadaus)] <- 0
  canadaus
}

# same process but for west map
functionWest <- function(x){
  numberofwinnersperstate <- data.frame(numberofwinnersperstate =
                                          x$numberofwinnersperstate, state = x$State)
  newdata <- left_join(West, numberofwinnersperstate, by = c("region" = "state"))
  West <- newdata[!duplicated(newdata$order), ]
  West[is.na(West)] <- 0
  West
}

# same process but for east map
functionEast <- function(x){
  numberofwinnersperstate <- data.frame(numberofwinnersperstate =
                                          x$numberofwinnersperstate, state = x$State)
  newdata <- left_join(East, numberofwinnersperstate, by = c("region" = "state"))
  East <- newdata[!duplicated(newdata$order), ]
  East[is.na(East)] <- 0
  East
}

# same process but for central map
functionCentral <- function(x){
  numberofwinnersperstate <- data.frame(numberofwinnersperstate =
                                          x$numberofwinnersperstate, state = x$State)
  newdata <- left_join(Central, numberofwinnersperstate, by = c("region" = "state"))
  Central <- newdata[!duplicated(newdata$order), ]
  Central[is.na(Central)] <- 0
  Central
}

# function to return data table for all years
functiontableallyears <- function(x){
  # render the table
  table <- DT::datatable(x,options=list(columnDefs = list(list(visible=FALSE, 
                                                               # make these columns invisible, not relevant             
                                                               targets=c(4, 12:15, 18:27)))),  escape=F) %>%
    # make columns blue
    formatStyle(columns = c(1:25), color = "blue")
  table
}

# function to return map for all years
functionallyearsplot <- function(a, b, c, d){
  # make ggplot
  p <- ggplot(data = a, 
              # long and lat of dataset that contains players
              mapping = aes(x= long, y = lat)) +
    # long and lat of data set that includes map, how dark the 
    # shade is number of winners per state
    geom_polygon(data = b, mapping = 
                   aes(x=long, y=lat, group=group, fill = numberofwinnersperstate),
                 color = "black") +
    # add geom_point, size is number of winners per team
    geom_point(mapping = aes(size = numberofwinnersperTeam), 
               # this is team color
               color = a$color) +
    scale_fill_continuous(low = "white", high = "green") +
    scale_size_binned() +
    # add labels
    labs(title = "MLB Cy Young Winners",
         subtitle = c + " " + d,
         size = "Number of Winners for Franchise",
         fill = "Number of Winners for that State",
         x = "Longitude",
         y = "Latitude") +
    # legend size and title size
    theme(legend.position = "right", plot.title = element_text(size=10))  
  p
}

# function to return table for only one year
functionparticularYeartable <- function(x){
  table <- DT::datatable(x, options=list(columnDefs = list(list(visible=FALSE, 
                                                                # make length change false since there will never be more than 10
                                                                targets=c(4, 12:15, 18:25))), pageLength = 15, lengthChange = FALSE),  escape=F) %>%
    formatStyle(columns = c(1:25), color = "blue")
  table
}

# function to return plot for only one year, same as allyearsplot
# but the fill is white for all states and the size is 10
functionparticularYearplot <- function(a, b, c, d) {
  p <-  ggplot(data = a) +
    geom_polygon(data = b, mapping = 
                   aes(x=long, y=lat, group=group), fill = "white", color = "black")+
    geom_point(mapping = aes(x = long, y = lat), size = 10, color = a$color) +
    scale_fill_continuous(low = "white", high = "green") +
    labs(title = "MLB Cy Young Winners",
         subtitle = c + " " + d,
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right", plot.title = element_text(size=10))  
  p
}







## CHUNK 11: SHINY
# Content: We create our shiny site.
b1 <- base64enc::dataURI(file="image.png", mime="image/png")
ui <- navbarPage(title = "MLB Pitchers", 
                 theme = shinytheme("darkly"),
                 # App title ----
                 titlePanel("MLB and the Cy Young Award"),
                 tabsetPanel(
                   # TAB 0
                   tabPanel("Map of Cy Young Winners", fluid = TRUE,
                            sidebarLayout(
                              # sidebarPanel for inputs
                              sidebarPanel(selectInput(inputId = "Year", 
                                                       label = "What Year Do You Choose?",
                                                       # use the Names made earlier as choices
                                                       choices = years),
                                           # checkbox input
                                           checkboxInput(inputId = "Narrow",
                                                         label = "Check this box if you want winners from a particular
League, Region, Division, or Team", value = FALSE),
                                           # uiOutputs for later use
                                           uiOutput(outputId = "particular"),
                                           uiOutput(outputId = "newvar")
                              ),
                              # code to make tables look better
                              mainPanel(tags$style(HTML(".dataTables_wrapper
.dataTables_length, .dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_info, .dataTables_wrapper
.dataTables_processing,.dataTables_wrapper .dataTables_paginate
.paginate_button, .dataTables_wrapper .dataTables_paginate
.paginate_button.disabled {
            color: red !important;}"), 
                                                   # gets rid of uneccessary error code
                                                   type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"),
                                        # text output
                                        textOutput("Text"),
                                        # plot output
                                        plotOutput("plot15"),
                                        # datatable output
                                        dataTableOutput("table"))
                            )
                            
                   ),
                   # TAB 1
                   tabPanel("Cy Young Winners by League", fluid = TRUE, 
                            sidebarLayout(                   
                              # sidebar panel for inputs
                              sidebarPanel(
                                # 1 examine how the stats of winners varies by league
                                varSelectInput(inputId = "winnerstatsbyleague", 
                                               label = "Choose a stat and compare Cy Young winners in each
        league. Examine their wins (W), 
        losses (L), earned run averages (ERA), innings pitched (IP), 
        strikeouts (SO), walks plus hits per inning pitched (WHIP),
        base on balls (BB), or homeruns given up (HR)
        between 1967 and 2019.", 
                                               data = select(data, W, L, ERA, IP, SO, WHIP, L , BB , HR))
                              ),
                              
                              # main panel for displaying outputs
                              mainPanel(
                                tabsetPanel(type = "tabs", tabPanel("Plot 1: Stats of Cy Young 
        Winners, comparing Leagues", plotOutput("plot1")))
                              )
                            )
                   ),
                   
                   # TAB 2
                   tabPanel("All Players, by Cy Young win/loss", fluid = TRUE, 
                            sidebarLayout(                   
                              # sidebar panel for inputs
                              sidebarPanel(
                                # 2 compare the stats of winners to those of non-winners
                                varSelectInput(inputId = "statswinnersvslosers", 
                                               label = "Choose a stat and compare Cy Young winners and non-winners.
        Compare their wins (W), losses (L), earned run averages (ERA), 
        innings pitched (IP), strikeouts (SO), walks plus hits per inning 
        pitched (WHIP), base on balls (BB), or homeruns given up (HR)
        between 1967 and 2019.", 
                                               data = select(data, W, L, ERA, IP, SO, WHIP, BB , HR))
                              ),
                              
                              # main panel for displaying outputs
                              mainPanel(
                                tabsetPanel(type = "tabs", tabPanel("Plot 2: Stats of Cy Young Winners and   
        Losers", plotOutput("plot2")))
                              )
                            )
                   ),
                   
                   
                   # TAB 3
                   tabPanel("American League Pitchers, by Division", fluid = TRUE,
                            sidebarLayout(
                              sidebarPanel(varSelectInput(inputId = "AL",
                                                          label = "Choose a stat and compare all American League 
                 pitchers (whether they won
                 the Cy Young or not) from a particular division. Examine their wins (W), 
                 losses (L), earned run averages (ERA), innings pitched (IP), 
                 strikeouts (SO), walks plus hits per inning pitched (WHIP),
                 base on balls (BB), or homeruns given up (HR)
                 between 1967 and 2019.",                             
                                                          data = select(data, W, L, ERA, IP, SO, WHIP, L, BB , HR)),
                                           radioButtons(inputId = "DivisionAL",label = "Choose a Division",
                                                        choices= c("AL West", "AL East", "AL Central"), selected = "AL West"),
                                           numericInput(inputId = "YearAL", label = "Choose a Year", value = 1967,min = 1967, max = 2019, step =1)
                              ),
                              
                              # main panel for displaying outputs
                              mainPanel(
                                tabsetPanel(type = "tabs", tabPanel("Plot 3: American League Trends ",
                                                                    plotOutput("plot3")))
                              )
                            )
                   ),
                   
                   # TAB 4
                   tabPanel("National League Pitchers, by Division", fluid = TRUE,
                            sidebarLayout(
                              sidebarPanel(varSelectInput(inputId = "NL",
                                                          label = "Choose a stat and compare all National League 
                 pitchers (whether they won
                 the Cy Young or not) from a particular division. Examine their wins (W), 
                 losses (L), earned run averages (ERA), innings pitched (IP), 
                 strikeouts (SO), walks plus hits per inning pitched (WHIP),
                 base on balls (BB), or homeruns given up (HR)
                 between 1967 and 2019.",         
                                                          data = select(data, W, L, ERA, IP, SO, WHIP, L, BB , HR)),
                                           radioButtons(inputId = "DivisionNL",label = "Choose a Division",
                                                        choices= c("NL West", "NL East", "NL Central"), selected = "NL West"),
                                           numericInput(inputId = "YearNL", label = "Choose a Year", 
                                                        value = 1967,min = 1967, max = 2019, step =1)
                              ),
                              
                              # main panel for displaying outputs
                              mainPanel(
                                tabsetPanel(type = "tabs", tabPanel("Plot 4: National League Trends ", plotOutput("plot4")))
                              )
                            )
                   ),
                   
                   
                   # TAB 5
                   tabPanel("Predict", fluid = TRUE,
                            sidebarLayout(
                              sidebarPanel(textInput("playerName", label = h3("Enter Player's Name"),
                                                     value = ""),
                                           selectInput("Team",
                                                       "Team:",c("LAD","NYM","HOU","ATL","STL","CIN","WSH","TB","CLE","CWS","CHC",
                                                                 "SF","TEX","MIN","BOS",
                                                                 "PHI","MIA","OAK","SEA","SD","KC","ARI","PIT","NYY","DET","COL","BAL","MIL","LAA","TOR")),
                                           sliderInput("Year", "Year:", min = data_bounds$min_Year, max = data_bounds$max_Year, value = 2000, step = 1, sep = ""),
                                           sliderInput("age", "age:", min = data_bounds$min_age, max = data_bounds$max_age, value = 30, step = 1),
                                           sliderInput("W", "W:", min = data_bounds$min_W, max = data_bounds$max_W, value = 10, step = 1),
                                           sliderInput("L", "L:", min = data_bounds$min_L, max = data_bounds$max_L, value = 10, step = 1),
                                           sliderInput("SO", "SO:", min = data_bounds$min_SO, max = data_bounds$max_SO, value = 200, step = 1),
                                           sliderInput("ERA", "ERA:", min = data_bounds$min_ERA, max = data_bounds$max_ERA, value = 5.0, step = 0.01),
                                           sliderInput("IP", "IP:", min = data_bounds$min_IP, max = data_bounds$max_IP, value = 200.0, step = 0.1),
                                           sliderInput("H", "H:", min = data_bounds$min_H, max = data_bounds$max_H, value = 200, step = 1),
                                           sliderInput("R", "R:", min = data_bounds$min_R, max = data_bounds$max_R, value = 100, step = 1),
                                           sliderInput("HR", "HR:", min = data_bounds$min_HR, max = data_bounds$max_HR, value = 30, step = 1),
                                           sliderInput("BB", "BB:", min = data_bounds$min_BB, max = data_bounds$max_BB, value = 100, step = 1),
                                           sliderInput("WHIP", "WHIP:", min = data_bounds$min_WHIP, max = data_bounds$max_WHIP, value = 1.00, step = 0.01),
                                           sliderInput("AVG", "AVG:", min = data_bounds$min_AVG, max = data_bounds$max_AVG, value = 0.2, step = 0.001),
                                           sliderInput("SHO", "SHO:", min = data_bounds$min_SHO, max = data_bounds$max_SHO, value = 10, step = 1)),
                              mainPanel(fluidRow(
                                
                                tags$style(type='text/css', '#myoutput1 {background-color: rgba(0,0,255,0.10); color: white; font-size: 32px}'),
                                tabPanel("Output 1", verbatimTextOutput("myoutput1")),
                                uiOutput(outputId = "image")
                              )
                              )
                            )
                   )
                   
                 )
)
server <- function(input, output) {
  
  # observe the checkmark
  observeEvent(input$Narrow, ignoreNULL = TRUE, {
    # if the checkmark is checked
    if(input$Narrow){
      # then render a UI for variable selection
      output$particular <- renderUI(
        # selectinput for variable
        varSelectInput(inputId = "LeagueRegionDivisionTeam",
                       # label
                       label = "Do you want Cy Young winners from a particular League,
        Region, Division, or Franchise?",
                       # get columns from data
                       data = select(data, c(3, 23, 24, 25)), selected = "League"))
      
      # now, observe the new rendered ui
      observeEvent(input$LeagueRegionDivisionTeam, ignoreNULL = TRUE,{
        # if it is league, then render a ui for league
        if(input$LeagueRegionDivisionTeam == "League"){
          output$newvar <- renderUI(
            selectInput(inputId = "League",
                        label = "What League Would You Like?",
                        # choices are leagues data frame we made earlier
                        choices = Leagues))
        }else{
          # if it is region, then render a ui for region
          if(input$LeagueRegionDivisionTeam == "Region"){
            output$newvar <- renderUI(
              selectInput(inputId = "Region",
                          label = "What Region Would You Like?",
                          # choices are regions data frame we made earlier
                          choices = Regions))
          }else{
            # if it is division, then render a ui for division
            if(input$LeagueRegionDivisionTeam == "Division"){
              output$newvar <- renderUI(
                selectInput(inputId = "Division",
                            label = "What Division Would You Like",
                            # choices are divisions data frame we made earlier
                            choices = Divisions))
            }else{
              # if it is none of them, it is franchise
              output$newvar <- renderUI(
                selectInput(inputId = "Franchise",
                            label = "What Franchise Would You Like",
                            # choices are teams data frame we made earlier
                            choices = Teams))
            }
          }
        }
      })
      # if it is not checked, then dont return ui
    }else{
      if(input$Narrow == FALSE){
        output$particular <- renderUI(NULL)
        observeEvent(input$LeagueRegionDivisionTeam,
                     ignoreNULL=FALSE, {
                       output$newvar <- renderUI(NULL)
                     })
      }
    }
  })
  # render the plot
  output$plot15 <- renderPlot({
    # if the check is checked
    if(input$Narrow){
      # and it is league
      if(input$LeagueRegionDivisionTeam == "League"){
        # and the years are all years
        if(input$Year == "All Years"){
          # filter data_coloured by only winners and league
          data_coloured <- data_coloured %>%
            filter(winner == "yes",
                   # column name for league in data_coloured
                   `<span style=\"color:red\">League</span>` == input$League)
          # run data_coloured in function to get winnersper for state and team
          data_coloured <- functionwinnersper(data_coloured)
          # add those to canadaus using function
          canadaus <- functioncanadaus(data_coloured)
          # output the text
          output$Text <- renderText({ 
            "This is the map for all the years for Cy Young winners in the " +
              input$League + ". Check out their stats below!"
          })
          # output the table for data using function made earlier
          output$table = DT::renderDataTable({
            functiontableallyears(data_coloured)
          })
          # get plot for data using function made earlier
          p <- functionallyearsplot(data_coloured, canadaus, input$Year, input$League)
          # xlim and ylim for canadaus map
          p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
        }else{
          # if it isn't all years, then filter for league, year, and winner
          data_coloured <- data_coloured %>%
            filter(`<span style=\"color:red\">Year</span>` == input$Year,
                   winner == "yes", `<span style=\"color:red\">League</span>` == input$League)
          # there were two winners in the american league for 1969
          if(input$Year == "1969" & input$League == "AL"){
            # output text explaining, use + function from earlier to add strings
            output$Text <- renderText({ 
              "This is the map for the 1969 AL Winners. This
     is the only Year 2 people from one Year won the Cy Young as
     there was a tie in the American League Cy Young voting that
     Year" + ". The Cy Young winners for that Year were " +
                data_coloured[1,1] + " who played for the " +
                data_coloured[1, 25] + " and " + data_coloured[2,1] + " who
     played for the " + data_coloured[2, 25] + ". Check out their stats below!"
            })
            # get table for data using function made earlier
            output$table = DT::renderDataTable({
              functionparticularYeartable(data_coloured)
            })
            # get plot for data using function made earlier
            p <- functionparticularYearplot(data_coloured, canadaus, input$Year, input$League)
            # xlim and ylim of canadaus map
            p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
            # if it is not 1969 and american, then it is normal
          }else{
            # if the dimensions are 0, then return this text
            if (dim(data_coloured)[1] == 0) {
              output$Text <- renderText({
                "There were no Starting Pitchers who won under the
      qualifications of your choices, try a new qualifcation."
              })
              # if dimensions are not 0, then it could be 2, since teams
              # have switched leagues, explain this in output text
            }else{
              if(dim(data_coloured)[1] == 2){
                output$Text <- renderText({ 
                  "This is the map for " + input$Year + " for the Cy Young winners that
  would be in the " + input$League + " today. There have been
  teams that have switched leagues since 1967, but our analysis
  was done with the assumption that the divisions and leagues are
  under today's MLB format. The Cy Young winners for that year
    were " + data_coloured[1,1] + " who played for the " +
                    data_coloured[1, 25] + " and " + data_coloured[2, 1] + " who
    played for the " + data_coloured[2, 25] + ". Check out their great stats below!"
                })
                # output table for data using function from earlier
                output$table = DT::renderDataTable({
                  functionparticularYeartable(data_coloured)
                })
                # output plot for data using function from earlier
                p <- functionparticularYearplot(data_coloured, canadaus, input$Year, input$League)
                p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
                # if it is not 0 or 2, it is normal
              }else{
                # output text explaining
                output$Text <- renderText({ 
                  "This is the map for " + input$Year + " for the player in the     " + 
                    input$League + " The Cy Young winner for that Year
    was " + data_coloured[1,1] + " who played for the " +
                    data_coloured[1, 25] + ". Check out his great stats below!"
                })
                # output table for data using function from earlier
                output$table = DT::renderDataTable({
                  functionparticularYeartable(data_coloured)
                })
                # output plot for data using function from earlier
                p <- functionparticularYearplot(data_coloured, canadaus, input$Year, input$League)
                p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
              }
            }
          }
        }
        
        # if it is region instead of league
      }else{
        if(input$LeagueRegionDivisionTeam == "Region"){
          # if the region is west
          if(input$Region == "West"){
            # and the year is all years
            if(input$Year == "All Years"){
              # filter by only region and winners
              data_coloured <- data_coloured %>%
                filter(winner == "yes",
                       Region == input$Region)
              # get winners per state and team using function
              data_coloured <- functionwinnersper(data_coloured)
              # add it to west using functionwest
              West <- functionWest(data_coloured)
              # output text explaining map
              output$Text <- renderText({ 
                "This is the map for all the years for Cy Young winners in the " +
                  input$Region + " Region." + ". Check out their stats below!"
              })
              # output table using function from earlier
              output$table = DT::renderDataTable({
                functiontableallyears(data_coloured)
              })
              # output plot using function from earlier
              p <- functionallyearsplot(data_coloured, West, input$Year, input$Region)
              # xlim and ylim for west map
              p + coord_map(xlim = c(-126, -91), ylim = c(23, 51))
            }else{
              # if it is not all years, then filter with year added in
              data_coloured <- data_coloured %>%
                filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes", Region == input$Region)
              # if dimensions are 0, then
              if (dim(data_coloured)[1] == 0) {
                #output text explaining
                output$Text <- renderText({
                  "There were no Starting Pitchers who won under the qualifications of your choices, try a new qualifcation."
                })
              }else{
                # if dimensions are one, only one winner in that region
                # that year
                if(dim(data_coloured)[1] == 1){
                  # output text explaining
                  output$Text <- renderText({ 
                    "This is the map for " + input$Year + " for Cy Young winners in the " +  
                      input$Region + " Region. The Cy Young winner for that Year
    was " + data_coloured[1,1] + " who played for the " +
                      data_coloured[1, 25] + ". Check out his great stats below!"
                  })
                  # output table using function from earlier
                  output$table = DT::renderDataTable({
                    functionparticularYeartable(data_coloured)
                  })
                  # get plot using function from earlier
                  p <- functionparticularYearplot(data_coloured, West, input$Year, input$Region)
                  p + coord_map(xlim = c(-126, -91), ylim = c(23, 51))
                  # if the dimensions are two, two winners in that region
                }else{
                  # output text explaining
                  output$Text <- renderText({ 
                    "This is the map for " + input$Year + " for Cy Young winners in the " +  
                      input$Region + " Region. The Cy Young winners for that Year
    were " + data_coloured[1,1] + " who played for the " +
                      data_coloured[1, 25] + " and " + data_coloured[2, 1] + 
                      " who played for the " + data_coloured[2, 25] + ". Check out their great stats below!"
                  })
                  # output table using function from earlier
                  output$table = DT::renderDataTable({
                    functionparticularYeartable(data_coloured)
                  })
                  # get plot using function from earlier
                  p <- functionparticularYearplot(data_coloured, West, input$Year, input$Region)
                  p + coord_map(xlim = c(-126, -91), ylim = c(23, 51))
                }
              }
            }
            # if the region is east, the process is the same as west
            # excpet the xlim and ylim changes on plots and 
            #functioneast is used instead of functionwest
          }else{
            if(input$Region == "East"){
              if(input$Year == "All Years"){
                data_coloured <- data_coloured %>%
                  filter(winner == "yes",
                         Region == input$Region)
                data_coloured <- functionwinnersper(data_coloured)
                East <- functionEast(data_coloured)
                output$Text <- renderText({ 
                  "This is the map for all the years for Cy Young winners in the " +
                    input$Region + " Region." + ". Check out their stats below!"
                })
                output$table = DT::renderDataTable({
                  functiontableallyears(data_coloured)
                })
                p <- functionallyearsplot(data_coloured, East, input$Year, input$Region)
                p + coord_map(xlim = c(-89, -67), ylim = c(23, 47))
              }else{
                data_coloured <- data_coloured %>%
                  filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes", Region == input$Region)
                if (dim(data_coloured)[1] == 0) {
                  output$Text <- renderText({
                    "There were no Starting Pitchers who won under the
      qualifications of your choices, try a new qualifcation."
                  })
                }else{
                  if(dim(data_coloured)[1] == 1){
                    output$Text <- renderText({ 
                      "This is the map for " + input$Year + " for Cy Young winners in the " +  
                        input$Region + " Region. The Cy Young winner for that Year
    was " + data_coloured[1,1] + " who played for the " +
                        data_coloured[1, 25] + ". Check out his great stats below!"
                    })
                    output$table = DT::renderDataTable({
                      functionparticularYeartable(data_coloured)
                    })
                    p <- functionparticularYearplot(data_coloured, East, input$Year, input$Region)
                    p + coord_map(xlim = c(-89, -67), ylim = c(23, 47))
                  }else{
                    output$Text <- renderText({ 
                      "This is the map for " + input$Year + " for Cy Young winners in the " +  
                        input$Region + " Region. The Cy Young winners for that Year
    were " + data_coloured[1,1] + " who played for the " +
                        data_coloured[1, 25] + " and " + data_coloured[2, 1] + 
                        " who played for the " + data_coloured[2, 25] + ". Check out their great stats below!"
                    })
                    output$table = DT::renderDataTable({
                      functionparticularYeartable(data_coloured)
                    })
                    p <- functionparticularYearplot(data_coloured, East, input$Year, input$Region)
                    p + coord_map(xlim = c(-89, -67), ylim = c(23, 47))
                  }
                }
              }
              # same for if region is central, same process as east and
              # west but the xlim and ylim are changed on plots and
              # functioncentral is used
            }else{
              if(input$Year == "All Years"){
                data_coloured <- data_coloured %>%
                  filter(winner == "yes",
                         Region == input$Region)
                data_coloured <- functionwinnersper(data_coloured)
                Central <- functionCentral(data_coloured)
                output$Text <- renderText({ 
                  "This is the map for all the years for Cy Young winners in the " +
                    input$Region + " Region." + ". Check out their stats below!"
                })
                output$table = DT::renderDataTable({
                  functiontableallyears(data_coloured)
                })
                p <- functionallyearsplot(data_coloured, Central, input$Year, input$Region)
                p + coord_map(xlim = c(-108, -78), ylim = c(23, 51))
              }else{
                data_coloured <- data_coloured %>%
                  filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes", Region == input$Region)
                if (dim(data_coloured)[1] == 0) {
                  output$Text <- renderText({
                    "There were no Starting Pitchers who won under the qualifications of your choices, try a new qualifcation."
                  })
                }else{
                  if(dim(data_coloured)[1] == 1){
                    output$Text <- renderText({ 
                      "This is the map for " + input$Year + " for Cy Young winners in the " +  
                        input$Region + " Region. The Cy Young winner for that Year
    was " + data_coloured[1,1] + " who played for the " +
                        data_coloured[1, 25] + ". Check out his great stats below!"
                    })
                    output$table = DT::renderDataTable({
                      functionparticularYeartable(data_coloured)
                    })
                    p <- functionparticularYearplot(data_coloured, Central, input$Year, input$Region)
                    p + coord_map(xlim = c(-108, -78), ylim = c(23, 51))
                  }else{
                    output$Text <- renderText({ 
                      "This is the map for " + input$Year + " for Cy Young winners in the " +  
                        input$Region + " Region. The Cy Young winners for that Year
    were " + data_coloured[1,1] + " who played for the " +
                        data_coloured[1, 25] + " and " + data_coloured[2, 1] + 
                        " who played for the " + data_coloured[2, 25] + ". Check out their great stats below!"
                    })
                    output$table = DT::renderDataTable({
                      functionparticularYeartable(data_coloured)
                    })
                    p <- functionparticularYearplot(data_coloured, East, input$Year, input$Region)
                    p + coord_map(xlim = c(-108, -78), ylim = c(23, 51))
                  }
                }
              }
            }
          }
          # if the wanted new variable is division
        }else{
          if(input$LeagueRegionDivisionTeam == "Division"){
            # if the division is AL West or NL West
            if(input$Division == "AL West" | input$Division == "NL West"){
              # if the inputyear is All Years
              if(input$Year == "All Years"){
                # then only filter by winner and division
                data_coloured <- data_coloured %>%
                  filter(winner == "yes",
                         Division == input$Division)
                data_coloured <- functionwinnersper(data_coloured)
                # still using function west
                West <- functionWest(data_coloured)
                # output text explaining
                output$Text <- renderText({ 
                  "This is the map for all the years for Cy Young winners in the " +
                    input$Division + " Division." + ". Check out their stats below!"
                })
                # output table using function
                output$table = DT::renderDataTable({
                  functiontableallyears(data_coloured)
                })
                #plot using function
                p <- functionallyearsplot(data_coloured, West, input$Year, input$Division)
                # xlim and ylim different
                p + coord_map(xlim = c(-126, -91), ylim = c(23, 51))
                # if the years are not all years
              }else{
                #filter for year too
                data_coloured <- data_coloured %>%
                  filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes", Division == input$Division)
                # if dimensions are 0
                if (dim(data_coloured)[1] == 0) {
                  # text explaining
                  output$Text <- renderText({
                    "There were no Starting Pitchers who won under the qualifications of your choices, try a new qualifcation."
                  })
                  # there will never be 2 dimensions, so only else
                }else{
                  # text explaining
                  output$Text <- renderText({ 
                    "This is the map for " + input$Year + " for Cy Young winners in the " +  
                      input$Division +  " Division. The Cy Young winner for that
    Year was " + data_coloured[1,1] + " who played for the " + 
                      data_coloured[1, 25] + ". Check out his great stats below!"
                  })
                  # table using function
                  output$table = DT::renderDataTable({
                    functionparticularYeartable(data_coloured)
                  })
                  # plot using function
                  p <- functionparticularYearplot(data_coloured, West, input$Year, input$Division)
                  # xlim and ylim for west
                  p + coord_map(xlim = c(-126, -91), ylim = c(23, 51))
                }
              }
              # if division is AL East or NL East, same process as AL
              # West or NL West, but change to functioneast, and xlim 
              # and ylim are different
            }else{
              if(input$Division == "AL East" | input$Division == "NL East"){
                if(input$Year == "All Years"){
                  data_coloured <- data_coloured %>%
                    filter(winner == "yes",
                           Division == input$Division)
                  data_coloured <- functionwinnersper(data_coloured)
                  East <- functionEast(data_coloured)
                  output$Text <- renderText({ 
                    "This is the map for all the years for Cy Young winners in the " +
                      input$Division + " Division." + ". Check out their stats below!"
                  })
                  output$table = DT::renderDataTable({
                    functiontableallyears(data_coloured)
                  })
                  p <- functionallyearsplot(data_coloured, East, input$Year, input$Division)
                  p + coord_map(xlim = c(-89, -67), ylim = c(23, 47))
                }else{
                  data_coloured <- data_coloured %>%
                    filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes", Division == input$Division)
                  if (dim(data_coloured)[1] == 0) {
                    output$Text <- renderText({
                      "There were no Starting Pitchers who won under the qualifications of your choices, try a new qualifcation."
                    })
                  }else{
                    output$Text <- renderText({ 
                      "This is the map for " + input$Year + " for Cy Young winners in the "+
                        input$Division + " Division. The Cy Young winner for that
      Year was " + data_coloured[1,1] + " who played for the " +
                        data_coloured[1, 25] + ". Check out his great stats below!"
                    })
                    output$table = DT::renderDataTable({
                      functionparticularYeartable(data_coloured)
                    })
                    p <- functionparticularYearplot(data_coloured, East, input$Year, input$Division)
                    p + coord_map(xlim = c(-89, -67), ylim = c(23, 47))
                  }
                }
                # same for NL Central or AL Central, change to 
                # function central and change xlim and ylim
              }else{
                if(input$Year == "All Years"){
                  data_coloured <- data_coloured %>%
                    filter(winner == "yes",
                           Division == input$Division)
                  data_coloured <- functionwinnersper(data_coloured)
                  Central <- functionCentral(data_coloured)
                  output$Text <- renderText({ 
                    "This is the map for all the years for Cy Young winners in the " +
                      input$Division + " Division." + ". Check out their stats below!"
                  })
                  output$table = DT::renderDataTable({
                    functiontableallyears(data_coloured)
                  })
                  p <- functionallyearsplot(data_coloured, Central, input$Year, input$Division)
                  p + coord_map(xlim = c(-108, -78), ylim = c(23, 51))
                }else{
                  data_coloured <- data_coloured %>%
                    filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes", Division == input$Division)
                  if (dim(data_coloured)[1] == 0) {
                    output$Text <- renderText({
                      "There were no Starting Pitchers who won under the qualifications of your choices, try a new qualifcation."
                    })
                  }else{
                    output$Text <- renderText({ 
                      "This is the map for " + input$Year + " for Cy Young winners in the " +  
                        input$Division + " Division. The Cy Young winner for that
    Year was " + data_coloured[1,1] + " who played for the " +
                        data_coloured[1, 25] + ". Check out his great stats below!"
                    })
                    output$table = DT::renderDataTable({
                      functionparticularYeartable(data_coloured)
                    })
                    p <- functionparticularYearplot(data_coloured, Central, input$Year, input$Division)
                    p + coord_map(xlim = c(-108, -78), ylim = c(23, 51))
                  }
                }
              }
            }
            # if the new variable is franchise
          }else{
            data_coloured <- data_coloured %>%
              # filter by winners and franchise
              filter(winner == "yes",
                     Franchise == input$Franchise)
            # if the years are all years
            if(input$Year == "All Years"){
              # if the dimensions are 0, they have never won a cy young
              if(dim(data_coloured)[1] == 0){
                # explain with outputtext
                output$Text <- renderText({
                  "The " + input$Franchise + " have never won a Cy Young
          Award!"
                })
              }else{
                # if there are more than 0 dimensions, print how many they
                # have won
                output$Text <- renderText({ 
                  "This is the map for all the years for Cy Young winners who
     played for the " + input$Franchise + " when they won the Cy
    Young. The " + input$Franchise + " have had " +
                    as.character(dim(data_coloured)[1]) + " winners in their
    franchise history since 1967"
                })
                # print table of all winners for that franchise
                output$table = DT::renderDataTable({
                  functionparticularYeartable(data_coloured)
                })
                # print plot for that franchise's point
                p <- functionparticularYearplot(data_coloured, canadaus, input$Year, input$Franchise)
                p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
              }
              # if it is a specific year
            }else{
              data_coloured <- data_coloured %>%
                # filter by winners and franchise and year
                filter(winner == "yes",
                       Franchise == input$Franchise, 
                       `<span style=\"color:red\">Year</span>` == input$Year)
              # if dimensions are 0, they did not win the cy young that year
              if(dim(data_coloured)[1] == 0){
                output$Text <- renderText({ 
                  "The " + input$Franchise + " did not win a Cy Young Award that year"
                })
              }else{
                # if it is not 0, franchise player won the cy young that year
                output$Text <- renderText({ 
                  "The " + input$Franchise + " did win a Cy Young Award that year. "+
                    data_coloured[1, 1] + " won the Cy Young in " + input$Year
                })
                output$table = DT::renderDataTable({
                  functionparticularYeartable(data_coloured)
                })
                p <- functionparticularYearplot(data_coloured, canadaus, input$Year, input$Franchise)
                p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
              }
            }
          }
        }
      }
      # if the checkmark is not checked
    }else{
      # and the input year is all years
      if(input$Year == "All Years"){ 
        # filter by winner only
        data_coloured <- data_coloured %>% 
          filter(winner == "yes")
        data_coloured <- functionwinnersper(data_coloured)
        canadaus <- functioncanadaus(data_coloured)
        # output text to explain
        output$Text <- renderText({ 
          "This is the map for all Cy young winners through all the
     years, some teams are hard to see because of their lack 
     of winners, try looking for a particular Franchise by
     using the checkmark on the left." + " Check out their stats below!"
        })
        output$table = DT::renderDataTable({
          functiontableallyears(data_coloured)
        })
        p <- functionallyearsplot(data_coloured, canadaus, input$Year, " ")
        p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
        
      }else{
        # if the year is 1969, there were 3 winners
        if(input$Year == "1969"){
          data_coloured <- data_coloured %>%
            filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes")
          # explain using text output
          output$Text <- renderText({ 
            "This is the map for " + input$Year + ". This is the only
    Year 3 people won the award in one Year, as there was a tie
    for first in the American League that Year" + ". The Cy Young
    winners for that Year were " + data_coloured[1,1] + " who
    played for the " + data_coloured[1, 25] + " and " +
              data_coloured[2,1] + " who played for the " +
              data_coloured[2, 25] + " and " + data_coloured[3,1] + "who
    played for the " + data_coloured[3,25] + ". Check out their 
    great stats below!"
          })
          output$table = DT::renderDataTable({
            functionparticularYeartable(data_coloured)
          })
          p <- functionparticularYearplot(data_coloured, canadaus, input$Year, " ")
          p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
        }else{
          # if not 1969, reliever could have won too
          data_coloured <- data_coloured %>%
            filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes")
          # so if the dimension is only 1
          if (dim(data_coloured)[1] == 1) {
            # explain that we left reliever out of analysis
            output$Text <- renderText({ 
              "This is the map for " + input$Year + ". The Cy Young winner
    for that year was " + data_coloured[1,1] + " who played for
    the " + data_coloured[1, 25] + " and a reliever that we left
    out of the analysis. Check out his great stats below!"
            })
            output$table = DT::renderDataTable({
              functionparticularYeartable(data_coloured)
            })
            p <- functionparticularYearplot(data_coloured, canadaus, input$Year, " ")
            p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
            # if not any those special cases
          }else{
            # normal year, print out text for 2 dimensions and their
            # table and plot using the functions
            data_coloured <- data_coloured %>%
              filter(`<span style=\"color:red\">Year</span>` == input$Year, winner == "yes")
            output$Text <- renderText({ 
              "This is the map for " + input$Year + ". The Cy Young winners
    for that Year were " + data_coloured[1,1] + " who played for
    the " + data_coloured[1, 25] + " and " + data_coloured[2,1] +
                " who played for the " + data_coloured[2, 25] + ". Check out 
    their great stats below!"
            })
            output$table = DT::renderDataTable({
              functionparticularYeartable(data_coloured)
            })
            p <- functionparticularYearplot(data_coloured, canadaus, input$Year, " ")
            p + coord_map(xlim = c(-124, -69), ylim = c(23, 50))
            
          }
        }
        
      }
    }
  })
  
  # winners stats, comparing the leagues
  output$plot1 <- renderPlot({
    data %>%
      filter(winner == "yes") %>%
      ggplot(aes(x = Year, y = !!input$winnerstatsbyleague, color = League)) +
      geom_point() + 
      geom_jitter(width = 0.2) +
      geom_line(se = FALSE, fill="black") + # use geom_line!
      labs(x = "Year (1967-2019)")
  })
  # all player's stats, comparing Cy Young winners and losers
  output$plot2 <- renderPlot({
    ggplot(data, aes(x = Year, y = !!input$statswinnersvslosers, color = winner)) +
      geom_point() + 
      geom_jitter(width = 0.2) +
      geom_line(se = FALSE, fill="black") +
      labs(x = "Year (1967-2019)")
  })
  
  output$plot3 <- renderPlot({
    data%>%filter(Division == input$DivisionAL)%>% filter(Year == input$YearAL)%>% group_by(Team)%>% ggplot(aes(Name,!!input$AL))+geom_col(aes(fill =Team) )+coord_flip() + xlab("") +theme_bw()
  })
  output$plot4 <- renderPlot({
    data%>%filter(Division == input$DivisionNL)%>% filter(Year == input$YearNL)%>%  group_by(Team)%>% ggplot(aes(Name,!!input$NL))+geom_col(aes(fill =Team) )+coord_flip() + xlab("") +theme_bw()
  })
  
  output$myoutput1 <- renderText({
    test_df <- data_test %>% slice(1)
    test_df[1] <- as.character(c('player'))
    test_df[2] <- as.character(c(input$Team))
    test_df[3] <- as.factor(c('NL'))
    test_df[4] <- as.factor(c('no'))
    test_df[5] <- as.numeric(c(input$Year))
    test_df[6] <- as.numeric(c(input$age))
    test_df[7] <- as.numeric(c(input$W))
    test_df[8] <- as.numeric(c(input$L))
    test_df[9] <- as.numeric(c(input$SO))
    test_df[10] <- as.numeric(c(input$ERA))
    test_df[11] <- as.numeric(c(input$IP))
    test_df[12] <- as.numeric(c(input$H))
    test_df[13] <- as.numeric(c(input$R))
    test_df[14] <- as.numeric(c(input$HR))
    test_df[15] <- as.numeric(c(input$BB))
    test_df[16] <- as.numeric(c(input$WHIP))
    test_df[17] <- as.numeric(c(input$AVG))
    test_df[18] <- as.numeric(c(input$SHO))
    
    test_df <- test_df %>% mutate(Team = factor(Team, levels = c("ARI", "ATL", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU", "KC", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")))
    
    results <- as_tibble(predict(rf_best_reduced, type="prob", newdata = test_df)) %>%
      bind_cols(test_df) %>%
      mutate(prediction = predict(rf_best_reduced, newdata=test_df))
    prediction_g <<- results %>% select(prediction)
    if (prediction_g == "yes") {
      paste("Congrats!", input$playerName, "you are bound to win!") 
    } else {
      paste("Sorry,", input$playerName, "you won't win the Cy Young.") 
    }
  })
  
  output$image <- renderUI({
    tags$img(src=b1)
  })
  
}
shinyApp(ui, server, options = list(height = 600))
