
library(baseballr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)
library(sportyR)


# function 1
annual_statcast_query <- function(season) {
  
  data_base_column_types <- read.csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  dates <- seq.Date(as.Date(paste0(season, '-04-07')),
                    as.Date(paste0(season, '-10-06')), by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates, 
                              end_date = dates + 3)
  
  safe_savant <- purrr::safely(scrape_statcast_savant)
  
  payload <- purrr::map(.x = seq_along(date_grid$start_date), 
                        ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                          
                          payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                                 end_date = date_grid$end_date[.x], type = 'pitcher')
                          
                          return(payload)
                        })
  
  payload_df <- purrr::map(payload, 'result')
  
  number_rows <- purrr::map_df(.x = seq_along(payload_df), 
                               ~{number_rows <- tibble::tibble(week = .x, 
                                                               number_rows = length(payload_df[[.x]]$game_date))}) %>%
    dplyr::filter(number_rows > 0) %>%
    dplyr::pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  payload_df_reduced_formatted <- purrr::map(.x = seq_along(payload_df_reduced), 
                                             ~{cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                                                                      "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                                                                      "fielder_8", "fielder_9")
                                             
                                             df <- purrr::pluck(payload_df_reduced, .x) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, as.numeric) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, function(x) {
                                                 ifelse(is.na(x), 999999999, x)
                                               })
                                             
                                             character_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "character") %>%
                                               dplyr::pull(variable)
                                             
                                             numeric_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "numeric") %>%
                                               dplyr::pull(variable)
                                             
                                             integer_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "integer") %>%
                                               dplyr::pull(variable)
                                             
                                             df <- df %>%
                                               dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
                                               dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
                                               dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
                                             
                                             return(df)
                                             })
  
  combined <- payload_df_reduced_formatted %>%
    dplyr::bind_rows()
  
  combined
}

#function 2
format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set
  
  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3, 
                                                 ifelse(type == "X" & events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1, 
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    
    df <- df %>%
      dplyr::mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      dplyr::mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      dplyr::filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    dplyr::arrange(game_date)
  
  df <- df %>%
    dplyr::filter(!is.na(game_date))
  
  df <- df %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::select(setdiff(names(.), c("error")))
  
  return(df)
}


#function 3

delete_and_upload <- function(df, 
                              year, 
                              db_driver = "PostgreSQL", 
                              dbname, 
                              user, 
                              password, 
                              host = 'local_host', 
                              port = 5432) {
  
  pg <- dbDriver(db_driver)
  
  statcast_db <- dbConnect(pg, 
                           dbname = dbname, 
                           user = user, 
                           password = password,
                           host = host, 
                           port = posrt)
  
  query <- paste0('DELETE from statcast where game_year = ', year)
  
  dbGetQuery(statcast_db, query)
  
  dbWriteTable(statcast_db, "statcast", df, append = TRUE)
  
  dbDisconnect(statcast_db)
  rm(statcast_db)
}

payload_statcast <- annual_statcast_query(2022)

df <- format_append_statcast(df = payload_statcast)



baseballdata <- Rawdata %>%
  filter(description == "hit_into_play")

baseballdata <- baseballdata %>%
  mutate(hit = ifelse(is.na(hit), 0, 1)) %>%
  mutate(hit_type = ifelse(is.na(hit_type), 0, hit_type)) %>%
  mutate(losing = ifelse(bat_score < fld_score, 1, 0)) %>%
  mutate(hit = as.factor(hit))

library(sportyR)
baseballdata <- baseballdata %>%
  mutate(location_x = 1.86 * (hc_x - 125.42),
         location_y = 2 * (215.27 - hc_y))

baseballgraph <- geom_baseball(league = "MLB") +
  geom_point(data = baseballdataover91,
             aes(location_x, location_y,
                 color = hit)) +
  scale_colour_manual(values =
                        c("red", "green")) +
  ggtitle("All Fly Balls Over 100 MPH Exit Velocity in 2022")

class(baseballgraph)

ggspraychart(baseballdataover90, x_value = "hc_x", y_value = "-hc_y", fill_value = "hit",
          fill_palette = c("red", "green"),   fill_legend_title = "All Batted Balls in 2022") + geom_point()
geom_baseball(league = "MLB") +
  geom_point(data = baseballdataover90,
             aes(location_x, location_y,
                 color = hit)) +
  scale_color_manual(values=c("red","green"), name = "Hit?", labels = c("No", "Yes")) +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom",) +
  labs(title = "All Line Drives Over 100 MPH Exit Velocity in 2022")

geom_baseball(league = "MLB") +
  geom_point(data = baseballdataover92,
             aes(location_x, location_y,
                 color = hit)) +
  scale_colour_manual(values =
                        c("red", "green")) +
  ggtitle("All Fly Balls Over 100 MPH Exit Velocity in 2022")

baseballdataover90 <- baseballdata %>%
  filter(launch_speed > 100 & launch_angle > 12 & launch_angle < 25)

baseballdataover91 <- baseballdata %>%
  filter(launch_speed > 100 & launch_angle > 25 & launch_angle < 50)

baseballdataover92 <- baseballdata %>%
  filter(launch_speed > 100 & launch_angle < 10)

baseballdatahits <- baseballdata %>%
  filter(hit == 1) %>%
  arrange(estimated_ba_using_speedangle) 

baseballdataover90 <- baseballdatanohits %>%
  filter(estimated_ba_using_speedangle > 0.50)



baseballdatanohits <- baseballdata %>%
  filter(hit == 0) %>%
  arrange(desc(estimated_ba_using_speedangle))


  

summary(baseballdata$launch_speed)
summary(baseballdata$launch_angle)
summary(baseballdata$hit_distance_sc)





write.csv(baseballdata, "COMPSFINALDATA.csv")



baseballdata <- baseballdata %>%
  select(stand, p_throws, hit_distance_sc, launch_speed, launch_angle,
  if_fielding_alignment, of_fielding_alignment, des, hit_type, barrel,
  hit, hit_type,  hc_x , hc_y, fielding_team, home_team, losing)

baseballdatamodel <- baseballdata %>%
  select(stand, p_throws, hit_distance_sc, launch_speed, launch_angle,
         if_fielding_alignment, of_fielding_alignment,
         hit, fielding_team, home_team, losing)

baseballdatamodel <- na.omit(baseballdatamodel)


baseballdatamodel <- baseballdatamodel %>%
  mutate(of_fielding_alignment = ifelse(of_fielding_alignment == "4th outfielder"
  | of_fielding_alignment =="Strategic", 
         "Strategic", "Standard"))

baseballdatamodel <- baseballdatamodel %>%
  mutate(if_fielding_alignment = ifelse(if_fielding_alignment == "Infield Shift"
                                        | if_fielding_alignment =="Strategic", 
                                        "Strategic", "Standard"))

baseballdata <- baseballdatamodel

levels(baseballdata$hit)
baseballdata$hit <- as.factor(baseballdata$hit)
levels(baseballdata$hit)
levels(baseballdata$stand)
baseballdata$stand <- as.factor(baseballdata$stand)
baseballdata$p_throws <- as.factor(baseballdata$p_throws)
baseballdata$losing <- as.factor(baseballdata$losing)
baseballdata$home_team <- as.factor(baseballdata$home_team)
baseballdata$fielding_team <- as.factor(baseballdata$fielding_team)
baseballdata$if_fielding_alignment <- as.factor(baseballdata$if_fielding_alignment)
baseballdata$of_fielding_alignment <- as.factor(baseballdata$of_fielding_alignment)
levels(baseballdata$if_fielding_alignment)
table(baseballdata$if_fielding_alignment)
table(baseballdata$of_fielding_alignment)
table(baseballdata$stand)
table(baseballdata$p_throws)
table(baseballdata$hit)
table(baseballdata$fielding_team)
table(baseballdata$home_team)
table(baseballdata$losing)




#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= hit_distance_sc, y = hit)) +
  geom_point() +
  facet_wrap(~stand) +
  stat_smooth(method="lm", se = FALSE)
#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= hit_distance_sc, y = hit)) +
  geom_point() +
  facet_wrap(~p_throws) +
  stat_smooth(method="lm", se = FALSE)
#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= hit_distance_sc, y = hit)) +
  geom_point() +
  facet_wrap(~if_fielding_alignment) +
  stat_smooth(method="lm", se = FALSE)
#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= hit_distance_sc, y = hit)) +
  geom_point() +
  facet_wrap(~of_fielding_alignment) +
  stat_smooth(method="lm", se = FALSE)
#lines appear to all be parallel, likely no interaction
ggplot(baseballdata, aes(x= hit_distance_sc, y = hit)) +
  geom_point() +
  facet_wrap(~fielding_team) +
  stat_smooth(method="lm", se = FALSE)
#lines appear to all be parallel, likely no interaction
ggplot(baseballdata, aes(x= hit_distance_sc, y = hit)) +
  geom_point() +
  facet_wrap(~home_team) +
  stat_smooth(method="lm", se = FALSE)
#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= hit_distance_sc, y = hit)) +
  geom_point() +
  facet_wrap(~losing) +
  stat_smooth(method="lm", se = FALSE)

#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= launch_speed, y = hit)) +
  geom_point() +
  facet_wrap(~stand) +
  stat_smooth(method="lm", se = FALSE)
#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= launch_speed, y = hit)) +
  geom_point() +
  facet_wrap(~p_throws) +
  stat_smooth(method="lm", se = FALSE)
#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= launch_speed, y = hit)) +
  geom_point() +
  facet_wrap(~if_fielding_alignment) +
  stat_smooth(method="lm", se = FALSE)
#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= launch_speed, y = hit)) +
  geom_point() +
  facet_wrap(~of_fielding_alignment) +
  stat_smooth(method="lm", se = FALSE)
#lines appear to all be parallel, likely no interaction
ggplot(baseballdata, aes(x= launch_speed, y = hit)) +
  geom_point() +
  facet_wrap(~fielding_team) +
  stat_smooth(method="lm", se = FALSE)
#lines appear to all be parallel, likely no interaction
ggplot(baseballdata, aes(x= launch_speed, y = hit)) +
  geom_point() +
  facet_wrap(~home_team) +
  stat_smooth(method="lm", se = FALSE)
#lines parallel, likely no interaction
ggplot(baseballdata, aes(x= launch_speed, y = hit)) +
  geom_point() +
  facet_wrap(~losing) +
  stat_smooth(method="lm", se = FALSE)

library(caret)
baseballdata <- baseballdata %>%
  mutate(hit = ifelse(hit == 1, "hit", "nothit")) %>%
  mutate(hit = fct_relevel(hit, "nothit"))

levels(baseballdata$hit)
baseballdata$stand <- as.factor(baseballdata$stand)
baseballdata$p_throws <- as.factor(baseballdata$p_throws)
baseballdata$losing <- as.factor(baseballdata$losing)
baseballdata$home_team <- as.factor(baseballdata$home_team)
baseballdata$fielding_team <- as.factor(baseballdata$fielding_team)
baseballdata$if_fielding_alignment <- as.factor(baseballdata$if_fielding_alignment)
baseballdata$of_fielding_alignment <- as.factor(baseballdata$of_fielding_alignment)

levels(baseballdata$hit)
set.seed(999) # make reproducible
n <- nrow(baseballdata)
train_index <- sample(nrow(baseballdata), size=round(.7*n)) # 70% training
baseballdata_train <- baseballdata %>%
  slice(train_index) 
baseballdata_test <- baseballdata %>%
  slice(-train_index)



train_control <- trainControl(
  method =
    "cv"
  , # cross-validation
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)



baseballglm <- glm(hit ~launch_speed + launch_angle + hit_distance_sc + stand + 
              if_fielding_alignment + of_fielding_alignment + p_throws + fielding_team +
              home_team + losing, data = baseballdata_train, family = "binomial" )


baseballdata_test <- baseballdata_test %>%
  mutate(prob = predict(baseballglm, newdata = baseballdata_test, type =
                          "response"))

baseballroctest <- roc(baseballdata_test$hit, baseballdata_test$prob)

bestthresh <- as.numeric(coords(baseballroctest, "best", ret = "threshold"))










baseballglm2 <- glm(hit ~launch_speed + launch_angle + hit_distance_sc + stand + 
                      if_fielding_alignment + of_fielding_alignment + p_throws, 
                    data = baseballdata_train, family = "binomial" )
baseballdata_test2 <- baseballdata %>%
  slice(-train_index)

baseballdata_test2 <- baseballdata_test2 %>%
  mutate(prob = predict(baseballglm2, newdata = baseballdata_test2, type =
                          "response"))
levels(baseballdata_test2$prediction)
plot(baseballroctest ,main ="ROC curve -- Logistic Classification ")
baseballroctest <- roc(baseballdata_test2$hit, baseballdata_test2$prob)
plot
bestthresh <- as.numeric(coords(baseballroctest, "best", ret = "threshold"))
baseballroctest$auc

baseballdata_traincolcheck <- baseballdata_train %>%
  mutate(hit = ifelse(hit == "hit", 1, 0))

baseballlm2 <- lm(hit ~launch_speed + launch_angle + hit_distance_sc + stand + 
                      if_fielding_alignment + of_fielding_alignment + p_throws, 
                    data = baseballdata_traincolcheck)


#no issues with colinearity
coefficients(baseballlm2)




baseballglm3 <- glm(hit~launch_speed + launch_angle + hit_distance_sc+
                      fielding_team + home_team + stand, 
                    data = baseballdata_train, family = "binomial" )
baseballdata_test3 <- baseballdata %>%
  slice(-train_index)

baseballdata_test3 <- baseballdata_test3 %>%
  mutate(prob = predict(baseballglm3, newdata = baseballdata_test3, type =
                          "response"))






baseballglm4 <- glm(hit ~launch_speed + launch_angle + hit_distance_sc, 
                    data = baseballdata_train, family = "binomial" )
baseballdata_test4 <- baseballdata %>%
  slice(-train_index)

baseballdata_test4 <- baseballdata_test4 %>%
  mutate(prob = predict(baseballglm4, newdata = baseballdata_test4, type =
                          "response"))



baseballglm5 <- glm(hit ~launch_speed + launch_angle, 
                    data = baseballdata_train, family = "binomial" )
baseballdata_test5 <- baseballdata %>%
  slice(-train_index)




baseballdata_test5 <- baseballdata_test5 %>%
  mutate(prob = predict(baseballglm5, newdata = baseballdata_test5, type =
                          "response"))




baseballglm6 <- glm(hit ~hit_distance_sc, 
                    data = baseballdata_train, family = "binomial" )
baseballdata_test6 <- baseballdata %>%
  slice(-train_index)

baseballdata_test6 <- baseballdata_test6 %>%
  mutate(prob = predict(baseballglm6, newdata = baseballdata_test6, type =
                          "response"))



levels(baseballdata_test$hit)

library(pROC)
eval_fun <- function(test) {
  
  baseballroctest <- roc(test$hit, test$prob)
  
  bestthresh <- as.numeric(coords(baseballroctest, "best", ret = "threshold"))
  
  
  
  test <- test %>%
    mutate(prediction = ifelse( prob > bestthresh,
                                "hit",
                                "nothit"))
   test %>%
    summarize(
              accuracy = mean(hit == prediction),
              precision = sum(hit ==
                                "hit" &
                                prediction ==
                                "hit")/sum(prediction ==
                                             "hit"),
              sensitivity = sum(hit ==
                                  "hit" &
                                  prediction ==
                                  "hit")/sum(hit ==
                                               "hit"),
              specificity = sum(hit ==
                                  "nothit" &
                                  prediction ==
                                  "nothit")/sum(hit ==
                                                  "nothit"),
              F1Score = 2*((precision*sensitivity)/(precision + sensitivity)), 
              sum = accuracy + precision + sensitivity + specificity + F1Score,
              ROC = as.numeric(baseballroctest$auc),
              bestThresh = coords(baseballroctest, "best", ret = "threshold"))
}



library(purrr)
t_vals <- seq(0.1, 0.9, by=0.1)
baseballtesteval <- eval_fun(baseballdata_test)
baseballtesteval2 <- eval_fun(baseballdata_test2)
baseballtesteval3 <- eval_fun(baseballdata_test3)
baseballtesteval4 <- eval_fun(baseballdata_test4)
baseballtesteval5 <- eval_fun(baseballdata_test5)
baseballtesteval
baseballtesteval2
baseballtesteval3
baseballtesteval4
baseballtesteval5
logit_P <- predict(baseballglm2 , newdata = baseballdata_test2 ,type = 'response' )
logit_P <- ifelse(logit_P > .5,1,0)

roc_score <- roc(baseballdata_test2$hit, logit_P)
plot(roc_score ,main ="ROC curve -- Logistic Regression ")
roc_score$auc
model 2, threshold .4

install.packages("ranger")
library(Rcpp)
library(ranger)
library(e1071)
set.seed(999) # make reproducible
tgrid <- expand.grid(
  .mtry = c(7),
  .splitrule = c("hellinger"),
  .min.node.size = c(1000)
)
baseballrftrain <- train(x = baseballdata_train[, -c(1, 9)],
                         y = baseballdata_train$hit, # training data
  method ="ranger", # classification method 
  trControl = train_control,
  tuneGrid = tgrid,
  num.trees = 501,
  metric = "ROC",
  respect.unordered.factors = "order",
  importance = "impurity")# validation method

baseballrftrain$finalModel


Fitting mtry = 10, splitrule = hellinger, min.node.size = 1 on full training set
Growing trees.. Progress: 69%. Estimated remaining time: 13 seconds.

resultstrain1 <- baseballrftrain$results
resultstrain1

baseballrf <- ranger(hit ~launch_speed + launch_angle + hit_distance_sc + stand + 
                      if_fielding_alignment + of_fielding_alignment + p_throws +
                       fielding_team + home_team + losing,
                     data = baseballdata,
                     num.trees = 501,
                     mtry = 7,
                     splitrule = "hellinger",
                     importance = "impurity",
                     respect.unordered.factors = "order",
                     min.node.size = 1000,
                     probability = TRUE)

varImp(baseballrftrain)

v<-as.vector(varImp(baseballrftrain))
v <- as.vector(v$importance)
w <- as.vector(c(""))
df <- tibble::rownames_to_column(v, "VALUE")
ggplot(df, aes(x=reorder(VALUE,Overall), y=Overall,fill=Overall))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
ylab("Variable Importance")+
  xlab(c("Variables"))+
 ggtitle("Importance of Variables in Random Forest")+
scale_fill_gradient(low="red", high="blue") +
scale_x_discrete(labels = c("IF Strategy", "Losing", "Pitcher Handedness", "OF Strategy", "Batter Handedness",
                              "Fielding Team", "Home Team", "Launch Angle", "Exit Velocity", "Distance")) 

baseballrftest <- baseballdata %>%
  slice(-train_index)

baseballrftest <- baseballrftest %>%
  mutate(prediction = predict(baseballrf, 
                              data=baseballdata_test, type = "response")$prediction,
         prob = prediction[,"hit"])

plot(baseballroctestrf ,main ="ROC curve -- Random Forest Classification ")
baseballroctestrf <- roc(baseballrftest$hit, baseballrftest$prob)

bestthresh <- as.numeric(coords(baseballroctestrf, "best", ret = "threshold"))
baseballroctestrf$auc




set.seed(999) # make reproducible
tgrid2 <- expand.grid(
  .mtry = c(5),
  .splitrule = c("hellinger"),
  .min.node.size = c(1000)
)
baseballrftrain2 <- train(hit ~launch_speed + launch_angle + hit_distance_sc + stand  
    + if_fielding_alignment + of_fielding_alignment + p_throws,
     data = baseballdata_train, # training data
     method ="ranger", # classification method 
     trControl = train_control,
     tuneGrid = tgrid2,
     num.trees = 501,
     metric = "ROC",
    importance = "impurity",
    respect.unordered.factors = "order")

baseballrftrain2$finalModel
levels(baseballdata_train$home_team)
varImp(baseballrftrain2)

baseballrf2 <- ranger(hit ~launch_speed + launch_angle + hit_distance_sc + stand + 
                        if_fielding_alignment + of_fielding_alignment + p_throws +
                        fielding_team + home_team + losing,
                      data = baseballdata,
                      num.trees = 501,
                      mtry = 5,
                      splitrule = "hellinger",
                      importance = "impurity",
                      respect.unordered.factors = "order",
                      min.node.size = 1000,
                      probability = TRUE)

baseballrftest2 <- baseballdata %>%
  slice(-train_index)

baseballrftest2 <- baseballrftest2 %>%
  mutate(prediction = predict(baseballrf2, 
                              data=baseballrftest, type = "response")$prediction,
         prob = prediction[,"hit"])



Fitting mtry = 5, splitrule = hellinger, min.node.size = 1000 on full training set

set.seed(999) # make reproducible
tgrid3 <- expand.grid(
  .mtry = c(1, 3, 5, 6),
  .splitrule = c("gini", "hellinger"),
  .min.node.size = c(1, 1000, 10000)
)
baseballrftrain3 <- train(x = baseballdata_train[, -c(1,3,7,8, 9, 12)],
                          y = baseballdata_train$hit, # training data
                          method ="ranger", # classification method 
                          trControl = train_control,
                          tuneGrid = tgrid3,
                          respect.unordered.factors = "order",
                          num.trees = 501,
                          metric = "ROC",
                          importance = "impurity")

baseballrftrain3$finalModel
varImp(baseballrftrain3)

baseballrf3 <- ranger(hit ~launch_speed + launch_angle + hit_distance_sc + stand 
                       + fielding_team + home_team,
                      data = baseballdata_train,
                      num.trees = 501,
                      mtry =5 ,
                      splitrule = "hellinger",
                      importance = "impurity",
                      respect.unordered.factors = "order",
                      min.node.size = 1000,
                      probability = TRUE)

baseballrftest3 <- baseballdata %>%
  slice(-train_index)

baseballrftest3 <- baseballrftest3 %>%
  mutate(prediction = predict(baseballrf3, 
                              data=baseballdata_test, type = "response")$prediction,
         prob = prediction[,"hit"])








set.seed(999) # make reproducible
tgrid4 <- expand.grid(
  .mtry = c(1,2,3),
  .splitrule = c("gini", "hellinger"),
  .min.node.size = c(1, 1000, 10000)
)
baseballrftrain4 <- train(hit ~launch_speed + launch_angle + hit_distance_sc,
                          data = baseballdata_train, # training data
                          method ="ranger", # classification method 
                          trControl = train_control,
                          tuneGrid = tgrid4,
                          num.trees = 501,
                          metric = "ROC",
                          respect.unordered.factors = "order",
                          importance = "impurity")

baseballrf4 <- ranger(hit ~launch_speed + launch_angle + hit_distance_sc,
                      data = baseballdata_train,
                      num.trees = 501,
                      mtry =2 ,
                      splitrule = "hellinger",
                      importance = "impurity",
                      respect.unordered.factors = "order",
                      min.node.size = 1000,
                      probability = TRUE)

baseballrftest4 <- baseballdata %>%
  slice(-train_index)

baseballrftest4 <- baseballrftest4 %>%
  mutate(prediction = predict(baseballrf4, 
                              data=baseballdata_test, type = "response")$prediction,
         prob = prediction[,"hit"])





baseballrf5 <- ranger(hit ~launch_speed + launch_angle,
                      data = baseballdata_train,
                      num.trees = 501,
                      mtry =1,
                      splitrule = "hellinger",
                      importance = "impurity",
                      respect.unordered.factors = "order",
                      min.node.size = 1000,
                      probability = TRUE)



baseballrftest5 <- baseballdata %>%
  slice(-train_index)

baseballrftest5 <- baseballrftest5 %>%
  mutate(prediction = predict(baseballrf5, 
                              data=baseballdata_test, type = "response")$prediction,
         prob = prediction[,"hit"])





confusionmatrixbaseballrf5 <- caret::confusionMatrix(
  data = predict(baseballrf5, newdata=baseballdata_test),
  reference = baseballdata_test$hit,
  positive =
    "hit" )

set.seed(999) # make reproducible
tgrid6 <- expand.grid(
  .mtry = c(1),
  .splitrule = c("gini", "hellinger"),
  .min.node.size = c(1, 1000, 10000)
)
baseballrftrain5 <- train(hit ~hit_distance_sc,
                          data = baseballdata_train, # training data
                          method ="ranger", # classification method 
                          trControl = train_control,
                          tuneGrid = tgrid6,
                          num.trees = 501,
                          metric = "ROC")

baseballrf6 <- ranger(hit ~hit_distance_sc,
                      data = baseballdata_train,
                      num.trees = 501,
                      mtry =6 ,
                      splitrule = "hellinger",
                      importance = "impurity",
                      respect.unordered.factors = "order",
                      min.node.size = 10000)

baseballrftest6 <- baseballdata_test %>%
  mutate(prediction = predict(baseballrftrain6, 
                              newdata=baseballdata_test, type = "prob"),
         prob = prob$hit)

confusionmatrixbaseballrf6 <- caret::confusionMatrix(
  data = predict(baseballrf6, newdata=baseballdata_test),
  reference = baseballdata_test$hit,
  positive =
    "hit" )


library(purrr)
t_vals <- seq(0.1, 0.9, by=0.1)
baseballtestrfeval<- eval_fun(baseballrftest)
baseballtestrfeval2<- eval_fun(baseballrftest2)
baseballtestrfeval3<- eval_fun(baseballrftest3)
baseballtestrfeval4<- eval_fun(baseballrftest4)
baseballtestrfeval5<- eval_fun(baseballrftest5)
baseballtestrfeval
baseballtestrfeval2
baseballtestrfeval3
baseballtestrfeval4
baseballtestrfeval5
baseballdata$hit <- as.numeric(baseballdata$hit)
baseballlmex <- glm(hit~ hit_distance_sc, data = baseballdata, family = "binomial")
plotting_dfm <- expand.grid(hit_distance_sc = seq(from=10, to = 500, by=10))
plotting_dfm$preds <- plogis( predict(baseballlmex , newdata=plotting_dfm))

library(jtools)
g <- effect_plot(baseballlmex, pred = hit_distance_sc, x.label = "Distance", y.label = "Hit Probability")
g+
  xlim(0, 300) + ylim(0, 1)

pl <- ggplot(plotting_dfm, aes(x=hit_distance_sc, y =preds))
pl + 
  geom_point( ) +
  ggtitle("Predicted hits by Distance") + 
  ggplot2::ylab("Predicted Hits")




numericalvariablesdata <- baseballdatamodel %>%
  select(hit_distance_sc, launch_speed, launch_angle)
library(GGally)
library(ggplot2)
ggpairs(numericalvariablesdata)+theme_bw()
ggpairs(numericalvariablesdata)

library(gam)







#testing the different models
baseballgamtest <- baseballdata %>% slice(-train_index)
tune_gridgam <- data.frame(df= c(1, 2, 3, 4, 5, 6, 7, 8 ))


baseballgamtrainlaunchspeed <- train(
  hit ~launch_speed,
  data = baseballdata_train, # training data
  method ="gamSpline", # classification method 
  trControl = train_control,
  tuneGrid = tune_gridgam,
  metric = "ROC")# validation method


baseballgamtrainlaunchspeed$results
df = 5
plot(baseballgamlaunchspeed)

baseballgamtrainhitdistance <- train(
  hit ~ hit_distance_sc,
  data = baseballdata_train, # training data
  method ="gamSpline", # classification method 
  trControl = train_control,
  tuneGrid = tune_gridgam,
  metric = "ROC")# validation method

baseballgamtrainhitdistance$results

df = 6

baseballgamtrainlaunchangle <- train(
  hit ~ launch_angle,
  data = baseballdata_train, # training data
  method ="gamSpline", # classification method 
  trControl = train_control,
  tuneGrid = tune_gridgam,
  metric = "ROC")# validation method

baseballgamtrainlaunchangle$results

df = 5

baseballgam <- gam(
  hit ~ s(launch_speed, df =5) + s(launch_angle, df =5 ) + s(hit_distance_sc, df =6) 
  + stand + if_fielding_alignment + of_fielding_alignment + p_throws + fielding_team +
    home_team + losing,
  data = baseballdata_train, # training data
  family = "binomial")# validation method





baseballgamtest <- baseballgamtest %>%
  mutate(prob = predict(baseballgam, newdata = baseballgamtest, type =
                          "response"))


baseballgamtest2 <- baseballdata %>% slice(-train_index)


baseballgam2 <- gam(
  hit ~s(launch_speed, df =5) + s(launch_angle, df =5 ) + s(hit_distance_sc, df =6) + 
    stand +  if_fielding_alignment + of_fielding_alignment 
  + p_throws,
  data = baseballdata_train, # training data
  family = "binomial")# validation method

plot(baseballroctestgam,main ="ROC curve -- GAM Classification ")
baseballroctestgam <- roc(baseballgamtest2$hit, baseballgamtest2$prob)
plot
bestthresh <- as.numeric(coords(baseballroctestgam, "best", ret = "threshold"))
baseballroctestgam$auc

par (mfrow = c(1, 3))
plot (baseballgam2, main = "Smoothing Spline For Distance",
      xlab = "Distance", ylab = "f(Distance)",se = TRUE , col = " blue ", cex.main = 1)



baseballgamtest2 <- baseballgamtest2 %>%
  mutate(prob = predict(baseballgam2, newdata = baseballgamtest2, type =
                          "response"))



baseballgamtest3 <- baseballdata %>% slice(-train_index)
baseballgam3 <- gam(
  hit~s(launch_speed, df =5) + s(launch_angle, df =5 ) + s(hit_distance_sc, df =6)
  +fielding_team + home_team + stand,
  data = baseballdata_train, # training data
  family = "binomial")# validation method


baseballgamtest3 <- baseballgamtest3 %>%
  mutate(prob = predict(baseballgam3, newdata = baseballgamtest3, type =
                          "response"))



baseballgamtest4 <- baseballdata %>% slice(-train_index)
baseballgam4 <- gam(
  hit ~s(launch_speed, df =5) + s(launch_angle, df =5 ) + s(hit_distance_sc, df =6),
  data = baseballdata_train, # training data
  family = "binomial")# validation method

plot(baseballgam4)

baseballgamtest4 <- baseballgamtest4 %>%
  mutate(prob = predict(baseballgam4, newdata = baseballgamtest4, type =
                          "response"))


baseballgamtest5 <- baseballdata %>% slice(-train_index)
baseballgam5 <- gam(
  hit ~s(launch_speed, df =5) + s(launch_angle, df =5 ),
  data = baseballdata_train, # training data
  family = "binomial")# validation method



baseballgamtest5 <- baseballgamtest5 %>%
  mutate(prob = predict(baseballgam5, newdata = baseballgamtest5, type =
                          "response"))





baseballgamtest6 <- baseballdata %>% slice(-train_index)
baseballgam6 <- gam(
  hit ~ s(hit_distance_sc, df =6),
  data = baseballdata_train, # training data
  family = "binomial")# validation method



baseballgamtest6 <- baseballgamtest6 %>%
  mutate(prob = predict(baseballgam6, newdata = baseballgamtest6, type =
                          "response"))


library(purrr)

t_vals <- seq(0.1, 0.9, by=0.1)

baseballtestgameval <- eval_fun(baseballgamtest)
baseballtestgameval2 <- eval_fun(baseballgamtest2)
baseballtestgameval3 <- eval_fun(baseballgamtest3)
baseballtestgameval4 <- eval_fun(baseballgamtest4)
baseballtestgameval5 <- eval_fun(baseballgamtest5)
baseballtestgameval
baseballtestgameval2
baseballtestgameval3
baseballtestgameval4
baseballtestgameval5


levels(baseballdata_train$hit)

model 4, threshold .3
