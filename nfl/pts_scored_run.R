#Libraries
library(xgboost)
library(dplyr)
library(sqldf)
library(devtools) 
#install_github("AppliedDataSciencePartners/xgboostExplainer")
library(xgboostExplainer)
library(gmodels)


#Team list
teams = c('BAL',
          'BUF',
          'DEN',
          'GB',
          'HOU',
          'KC',
          'MIN',
          'NE',
          'NO',
          'NYG',
          'PIT',
          'SEA',
          'TB',
          'TEN',
          'WAS',
          'LV',
          'ARI',
          'CAR',
          'CLE',
          'DAL',
          'DET',
          'IND',
          'MIA',
          'NYJ',
          'PHI',
          'SF',
          'LA',
          'LAC',
          'ATL',
          'CHI',
          'CIN',
          'JAX'
)

wk = 17
base = data.frame(Week = as.numeric(), Team = as.character(), Prediction = as.numeric(),stringsAsFactors = FALSE)

#Add week so that we can exclude games that already have happened
clean = clean %>% mutate(week = substr(game_id,6,7))

#Model Testing
i=2
for (team in teams)
{
  
  print(team)
  target_team = team

#Adjsut for Dak injury

team = clean%>%filter(season =="2021" & posteam ==target_team & week != '17')%>%
select(-season,-posteam,-week,-game_id)
team = as.data.frame(apply(team,2,mean))
team = as.data.frame(t(team))
rownames(team) = c()

#Save raw data for each team for comparison
assign(paste(wk),team)

train = clean %>% select(-season,-posteam,-week,-game_id)
test = team

#Convert to matrix
xtest <- xgb.DMatrix(data = as.matrix(test[, names(test) != "pts_scored"]))
xtrain = xgb.DMatrix(data = as.matrix.data.frame(sapply(train[, names(train) != "pts_scored"],as.numeric)), 
                     label = as.vector(train$pts_scored))

#Model
xmod = xgboost(data = xtrain, 
               label = as.vector(train$pts_scored), 
               max.depth = 6, 
               nthread = 2, 
               nrounds = 6, 
               objective = "reg:linear",
               verbose=1)

#Predictions
pred = predict(xmod, xtest)
#Always round up
pred = ceiling(pred)
#Change funky scores
pred = ifelse(pred<3,0,pred)
pred = ifelse(pred>3 & pred<6,6,pred)
pred = ifelse(pred==11,12,pred)

#Add data to dataframe
base = base %>% add_row(Week=wk,Team=target_team,Prediction=pred)

}

#Select game schedule
games = read_csv("http://www.habitatring.com/games.csv")
games =  games %>% filter(season==2021 & week==wk) %>%
  select(game_id,season,game_type,week,weekday,away_team,away_score,home_team,home_score,result,total,spread_line,total_line)

#Look for away teams
away = sqldf("SELECT a.*, b.Prediction as away_prediction
             FROM games a LEFT JOIN base b on a.away_team=b.Team")
#Look for home teams
final = sqldf("SELECT a.*, b.Prediction as home_prediction
             FROM away a LEFT JOIN base b on a.home_team=b.Team")
#Add additional fields
final = final %>%
  mutate(home_win = ifelse(home_score>away_score,1,0))%>%
  mutate(home_cover = ifelse(result>=spread_line,1,0))%>%
  mutate(over = ifelse(total>=total_line,1,0))%>%
  mutate(pred_result = home_prediction-away_prediction)%>%
  mutate(pred_total = home_prediction+away_prediction)%>%
  mutate(pred_home_win = ifelse(home_prediction>away_prediction,1,0))%>%
  mutate(pred_home_cover = ifelse(pred_result>=spread_line,1,0))%>%
  mutate(pred_over = ifelse(pred_total>=total_line,1,0))
  
#Write out final predictions to weekly csv
assign(paste(wk),final)
filePath = paste("C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\output\\predictions\\2021\\weekly\\",wk,".csv",sep="") 
write.csv(final,file = filePath)
  
#Evaluation

#How often did we pick the winner?
CrossTable(final$home_win,final$pred_home_win)

#How often did we correctly pick a home cover?
CrossTable(final$home_cover,final$pred_home_cover)

#How often did we correctly pick an over?
CrossTable(final$over,final$pred_over)