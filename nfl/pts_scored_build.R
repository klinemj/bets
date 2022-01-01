#Libraries
library(sqldf)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(nflscrapR)
library(nflfastR)
library(RCurl)
#Data sources
#pbp
#https://github.com/guga31bb/nflfastR-data/tree/master/data
#elo
#https://github.com/fivethirtyeight/data/tree/master/nfl-elo

#Load data
x = getURL("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv")
elo = read.csv(text = x)


seasons <- 2010:2021
pbp_fast <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

#Clean data
elo = elo %>% filter(season %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))
elo$date = as.Date(elo$date)
elo$team1 = as.character(elo$team1)

#Additional cleaning, filtering, new attributes

#Prep ELO data
elohome = elo %>%
  select(date,season,team1,elo_prob1,qbelo1_pre,qb1_value_pre,qbelo_prob1)%>%
  mutate(home = 'Y')
eloaway = elo %>% 
  select(date,season,team2,elo_prob2,qbelo2_pre,qb2_value_pre,qbelo_prob2)%>%
  rename(team1 = team2,elo_prob1 = elo_prob2,qbelo1_pre=qbelo2_pre,qb1_value_pre=qb2_value_pre,qbelo_prob1=qbelo_prob2)%>%
  mutate(home = 'N')
elobase = rbind(elohome,eloaway)
rm(elohome,eloaway)
#Change some team names to match pbp
elobase = elobase %>% mutate(posteam = case_when(team1=='WSH'~'WAS',
                                               team1=='LAR'~'LA',
                                               team1=='OAK'~'LV',
                                               TRUE~team1))

#Prep PBP data
pbp = pbp_fast %>%
  filter(!is.na(posteam)) %>%
  group_by(game_id,posteam) %>%
  mutate(season = substr(game_id,1,4))%>%
  mutate(year = paste0(substr(old_game_id,1,4),"-"))%>%
  mutate(month = paste0(substr(old_game_id,5,6),"-"))%>% 
  mutate(day = substr(old_game_id,7,8))%>% 
  mutate(date = as.Date(paste0(year,month,day)))%>%
  #target variable
  mutate(pts_scored = ifelse(posteam_type=="home",home_score,away_score))%>%
  mutate(total_pass = sum(ifelse(play_type == "pass",1,0),na.rm = TRUE)) %>%
  mutate(total_yds_gained = sum(yards_gained,na.rm=TRUE)) %>%
  mutate(total_shotguns = sum(shotgun,na.rm=TRUE)) %>%
  mutate(total_non_shotguns = sum(ifelse(shotgun == 0,1,0),na.rm = TRUE)) %>%
  mutate(total_no_huddles = sum(no_huddle,na.rm=TRUE)) %>%
  mutate(total_run_right = sum(ifelse(run_location == "right",1,0),na.rm = TRUE)) %>%
  mutate(total_run_left = sum(ifelse(run_location == "left",1,0),na.rm = TRUE)) %>%
  mutate(total_kick_dist = sum(kick_distance,na.rm=TRUE)) %>%
  mutate(total_ep = round(mean(ep,na.rm=TRUE),2)) %>%
  mutate(total_epa = round(mean(epa,na.rm=TRUE),2)) %>%
  mutate(total_wp = round(mean(wp,na.rm=TRUE),2)) %>%
  mutate(total_wpa = round(mean(wpa,na.rm=TRUE),2)) %>%
  mutate(total_1st_down_rush = sum(first_down_rush,na.rm=TRUE)) %>%
  mutate(total_3rd_down_fail = sum(third_down_failed,na.rm=TRUE)) %>%
  mutate(total_4th_down_fail = sum(fourth_down_failed,na.rm=TRUE)) %>%
  mutate(total_avg_no_score = round(mean(no_score_prob,na.rm=TRUE),2)) %>%
  add_tally() %>% rename(total_plays=n) %>%
  mutate(total_redzone = sum(ifelse(as.character(side_of_field) != as.character(posteam) & yardline_100 <=20,1,0))) %>%
  mutate(total_redzone_td_pct = round(sum(ifelse(as.character(side_of_field) != as.character(posteam) & yardline_100 <=20 & touchdown==1,1,0))/sum(ifelse(as.character(side_of_field) != as.character(posteam) & yardline_100 <=20,1,0)),2)) %>%
  mutate(total_td_per = round(sum(touchdown)/total_plays,2)) %>%
  mutate(total_completion_pct = round(sum(complete_pass,na.rm=TRUE)/sum(pass_attempt,na.rm=TRUE),2))%>%
  mutate(total_yds_per = round(sum(ifelse(play_type %in% c("pass","run"),yards_gained,0),na.rm = TRUE)/sum(pass_attempt,na.rm=TRUE),2))%>%
  mutate(total_td_pct = round(sum(pass_touchdown,na.rm=TRUE)/sum(pass_attempt,na.rm=TRUE),2))%>%
  mutate(total_rush_att = sum(rush_attempt,na.rm=TRUE)) %>%
  mutate(total_half1_rush_att = sum(ifelse(game_half == "Half1",rush_attempt,0),na.rm = TRUE)) %>%
  mutate(total_half2_rush_att = sum(ifelse(game_half == "Half2",rush_attempt,0),na.rm = TRUE)) %>%
  mutate(total_sacks = sum(sack,na.rm=TRUE)) %>%
  mutate(total_tds = sum(touchdown,na.rm=TRUE)) %>%
  mutate(total_pass_tds = sum(pass_touchdown,na.rm=TRUE)) %>%
  mutate(total_rush_tds = sum(rush_touchdown,na.rm=TRUE)) %>%
  mutate(total_punt_att = sum(punt_attempt,na.rm=TRUE)) %>%
  mutate(total_rush_epa = case_when(posteam_type=='home'~round(mean(total_home_rush_epa,na.rm=TRUE),2),
                                    posteam_type=='away'~round(mean(total_away_rush_epa,na.rm=TRUE),2),
                                    TRUE~0))%>%
  mutate(total_pass_epa = case_when(posteam_type=='home'~round(mean(total_home_pass_epa,na.rm=TRUE),2),
                                    posteam_type=='away'~round(mean(total_away_pass_epa,na.rm=TRUE),2),
                                    TRUE~0))%>%
  mutate(total_comp_air_epa = case_when(posteam_type=='home'~round(mean(total_home_comp_air_epa,na.rm=TRUE),2),
                                        posteam_type=='away'~round(mean(total_away_comp_air_epa,na.rm=TRUE),2),
                                        TRUE~0))%>%
  mutate(total_cpoe = round(mean(cpoe,na.rm=TRUE),2))%>%
  mutate(total_series_success =sum(series_success,na.rm = TRUE))%>%
  mutate(total_qb_epa=round(mean(qb_epa,na.rm=TRUE),2))%>%
  mutate(total_qb_hits = sum(qb_hit,na.rm=TRUE))%>%
  mutate(total_explosive_run = sum(ifelse(play_type=="run" & yards_gained >=12,1,0),na.rm = TRUE))%>%
  mutate(total_explosive_pass = sum(ifelse(play_type=="pass" & yards_gained >=20,1,0),na.rm = TRUE))%>%
  mutate(total_explosive_plays = total_explosive_pass+total_explosive_run)%>%
  select(season,
         date,
         game_id,
         posteam,
         total_pass ,
         total_yds_gained ,
         total_shotguns ,
         total_non_shotguns ,
         total_no_huddles ,
         total_run_right ,
         total_run_left ,
         total_kick_dist ,
         total_ep ,
         total_epa ,
         total_wp ,
         total_wpa ,
         total_1st_down_rush ,
         total_3rd_down_fail ,
         total_4th_down_fail ,
         total_avg_no_score ,
         total_redzone ,
         total_redzone_td_pct ,
         total_td_per ,
         total_yds_per,
         total_td_pct ,
         total_rush_att ,
         total_half1_rush_att ,
         total_half2_rush_att ,
         total_sacks ,
         total_tds ,
         total_pass_tds ,
         total_rush_tds ,
         total_punt_att ,
         total_plays,
         total_rush_epa,
         total_pass_epa,
         total_comp_air_epa,
         total_cpoe,
         total_series_success,
         total_qb_epa,
         total_qb_hits,
         total_explosive_plays,
         total_line,
         pts_scored) %>%
  unique()

#Success rate
pbp_success = pbp_fast %>%
  filter(!is.na(posteam)) %>%
  filter(play_type %in% c('run','pass')& down %in% c(1,2,3,4))%>%
  group_by(game_id,posteam)%>%
  add_tally() %>% rename(total_plays=n) %>%
  mutate(off_success_rate = round(sum(case_when(down ==1 & (0.4*ydstogo<=yards_gained)~1,
                                     down ==2 & (0.6*ydstogo<=yards_gained)~1,
                                     down ==3 & (1.0*ydstogo<=yards_gained)~1,
                                     down ==4 & (1.0*ydstogo<=yards_gained)~1,
                                     TRUE ~ 0))/total_plays,2)) %>%
  select(game_id,posteam,off_success_rate)%>%
  unique()

#Half time lead
pbp_halftime = pbp_fast %>% filter(!is.na(posteam)& game_half=="Half1") %>%
  group_by(game_id,posteam)%>%
  filter(play_id == max(play_id))%>%
  mutate(total_halftime_lead = posteam_score_post-defteam_score_post)%>%
  select(game_id,posteam,total_halftime_lead)

#Average points scored
home_avg = pbp_fast %>%
  filter(!is.na(posteam) & posteam_type =="home") %>%
  mutate(season = substr(game_id,1,4))%>%
  group_by(season,posteam) %>%
  mutate(avg_home_score = round(mean(home_score),2))%>%
  select(season,posteam,avg_home_score)%>%
  unique()
  
away_avg = pbp_fast %>%
  filter(!is.na(posteam) & posteam_type =="away") %>%
  mutate(season = substr(game_id,1,4))%>%
  group_by(season,posteam) %>%
  mutate(avg_away_score = round(mean(away_score),2))%>%
  select(season,posteam,avg_away_score)%>%
  unique()

#Defense PBP data
pbp_def = pbp_fast %>%
  filter(!is.na(defteam)) %>%
  group_by(game_id,defteam) %>%
  mutate(total_yds_allowed = sum(yards_gained,na.rm=TRUE)) %>%
  mutate(total_turnovers = sum(interception,fumble_lost,na.rm=TRUE)) %>%
  mutate(total_tds_allowed = sum(touchdown,na.rm=TRUE)) %>%
  mutate(total_explosive_run = sum(ifelse(play_type=="run" & yards_gained >=12,1,0),na.rm = TRUE))%>%
  mutate(total_explosive_pass = sum(ifelse(play_type=="pass" & yards_gained >=20,1,0),na.rm = TRUE))%>%
  mutate(total_explosive_allowed = total_explosive_pass+total_explosive_run)%>%
  select(game_id,defteam,total_yds_allowed,total_turnovers,total_explosive_allowed,total_tds_allowed) %>%
  unique()

#Defense success rate
pbp_def_success = pbp_fast %>%
  filter(!is.na(defteam)) %>%
  filter(play_type %in% c('run','pass')& down %in% c(1,2,3,4))%>%
  group_by(game_id,defteam) %>%
  add_tally() %>% rename(total_plays=n) %>%
  mutate(def_success_rate = round(sum(case_when(down ==1 & (0.4*ydstogo<=yards_gained)~0,
                                                down ==2 & (0.6*ydstogo<=yards_gained)~0,
                                                down ==3 & (1.0*ydstogo<=yards_gained)~0,
                                                down ==4 & (1.0*ydstogo<=yards_gained)~0,
                                                TRUE ~ 1))/total_plays,2)) %>%
  select(game_id,defteam,def_success_rate)%>%
  unique()
  


pbp_base = sqldf("SELECT a.*, b.total_yds_allowed, b.total_turnovers, 
b. total_tds_allowed, b.total_explosive_allowed,
c.off_success_rate,d.def_success_rate, e.total_halftime_lead
FROM 
      pbp a INNER JOIN pbp_def b 
      ON a.game_id=b.game_id AND a.posteam=b.defteam
      INNER JOIN pbp_success c ON a.game_id=c.game_id AND a.posteam=c.posteam
      INNER JOIN pbp_def_success d on a.game_id=d.game_id AND a.posteam=d.defteam
      INNER JOIN pbp_halftime e ON a.game_id=e.game_id AND a.posteam=e.posteam")
rm(pbp,pbp_def,pbp_success,pbp_def_success)


#Merge all data together
clean = sqldf("SELECT a.*, b.avg_home_score, c.avg_away_score,
        d.elo_prob1, d.qb1_value_pre, d.qbelo_prob1, d.home
              FROM pbp_base a INNER JOIN
              home_avg b ON a.season = b.season AND a.posteam = b.posteam INNER JOIN
              away_avg c ON a.season = c.season AND a.posteam = c.posteam LEFT JOIN
              elobase d ON a.season = d.season AND a.date = d.date AND a.posteam = d.posteam")

final = clean

#Write out data to Tableau file
#write.csv(final,"tableau/pbp_2010_2019.csv")

#Drop descriptor variables

#Remove td related variables
#clean = clean %>% select(-total_tds,-total_td_pct,-total_tds_allowed,-total_td_per,-total_redzone_td_pct,-total_pass_tds,-total_rush_tds,-total_no_huddles,-schedule_date,-schedule_week)
#clean = clean %>% select(-total_tds,-total_td_pct,-total_tds_allowed,-total_td_per,-total_redzone_td_pct,-total_pass_tds,-total_rush_tds,-total_no_huddles,-schedule_date,-schedule_season,-schedule_week,-week,-team_id,-posteam)
clean = clean %>% select(-date,-home)
