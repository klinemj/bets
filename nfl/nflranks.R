#Purpose of this code is to rank NFL teams
#Goal: Pass offense rank, run offense rank, pass defense rank, run offense rank, overal offense rank, overal defense rank
#Pass offense schedule rank, run offense schedule rank, pass defense schedule rank, run defense schedule rank
#Theory: Use combination of EPA and Success Rate for ranking metrics.  For QB's, use EPA and ELO ratings.  Overall
#Ratings based off offensive rank, QB rank, defensive rank

#NOTE: QB's change, make sure to updated code to get up to date roster

library(nflfastR)
library(dplyr)
library(ggplot2)
library(RCurl)
library(stringr)
library(sqldf)
library(openxlsx)
library(rvest)

#Load data
seasons <- 2021
pbp = purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

# #Remove crappy week 17 games
# '%notin%' = Negate('%in%')
# pbp = pbp %>% filter(game_id %notin% c('2020_17_LAC_KC','2020_17_NO_CAR','2020_17_PIT_CLE','	
# 2020_17_WAS_PHI'))
# #Load elo data for QB rank
x = getURL("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv")
elo = read.csv(text = x)

#Pass offense rank
hpass = pbp %>% 
  filter(!is.na(posteam) & pass==1 & home_team==posteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = posteam) %>%
  select(team,epa,success)

apass = pbp %>% 
  filter(!is.na(posteam) & pass==1 & away_team==posteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = posteam) %>%
  select(team,epa,success)

passo = bind_rows(hpass, apass) %>%
  group_by(team) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success)) %>%
  mutate(pass_o_eff = round(mean_epa*success_rate,5)) %>%
  mutate(pass_o_rank = rank(-pass_o_eff)) %>%
  select(team,pass_o_eff,pass_o_rank)%>%
  unique()

#Run offense rank
hrun = pbp %>% 
  filter(!is.na(posteam) & rush==1 & home_team==posteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = posteam) %>%
  select(team,epa,success)

arun = pbp %>% 
  filter(!is.na(posteam) & rush==1 & away_team==posteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = posteam) %>%
  select(team,epa,success)

runo = bind_rows(hrun, arun) %>%
  group_by(team) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success)) %>%
  mutate(run_o_eff = round(mean_epa*success_rate,5)) %>%
  mutate(run_o_rank = rank(-run_o_eff)) %>%
  select(team,run_o_eff,run_o_rank)%>%
  unique()

#total offense rank
ho = pbp %>% 
  filter(!is.na(posteam) & (rush==1 | pass==1) & home_team==posteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = posteam) %>%
  select(team,epa,success)

ao = pbp %>% 
  filter(!is.na(posteam) & (rush==1 | pass==1) & away_team==posteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = posteam) %>%
  select(team,epa,success)

o = bind_rows(ho, ao) %>%
  group_by(team) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success)) %>%
  mutate(o_eff = round(mean_epa*success_rate,5)) %>%
  mutate(o_rank = rank(-o_eff)) %>%
  select(team,o_eff,o_rank)%>%
  unique()

#Pass defense rank
hpassd = pbp %>% 
  filter(!is.na(defteam) & pass==1 & home_team==defteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = defteam) %>%
  select(team,epa,success)

apassd = pbp %>% 
  filter(!is.na(defteam) & pass==1 & away_team==defteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = defteam) %>%
  select(team,epa,success)

passd = bind_rows(hpassd, apassd) %>%
  group_by(team) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success)) %>%
  mutate(pass_d_eff = round(mean_epa*success_rate,5)) %>%
  mutate(pass_d_rank = rank(pass_d_eff)) %>%
  select(team,pass_d_eff,pass_d_rank)%>%
  unique()

#Run defense rank
hrund = pbp %>% 
  filter(!is.na(defteam) & rush==1 & home_team==defteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = defteam) %>%
  select(team,epa,success)

arund = pbp %>% 
  filter(!is.na(defteam) & rush==1 & away_team==defteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = defteam) %>%
  select(team,epa,success)

rund = bind_rows(hrund, arund) %>%
  group_by(team) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success)) %>%
  mutate(run_d_eff = round(mean_epa*success_rate,5)) %>%
  mutate(run_d_rank = rank(run_d_eff)) %>%
  select(team,run_d_eff,run_d_rank)%>%
  unique()

#total defense rank
hd = pbp %>% 
  filter(!is.na(defteam) & (rush==1 | pass==1) & home_team==defteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = defteam) %>%
  select(team,epa,success)

ad = pbp %>% 
  filter(!is.na(defteam) & (rush==1 | pass==1) & away_team==defteam & !is.na(epa) & wp > 0.10) %>%
  mutate(team = defteam) %>%
  select(team,epa,success)

d = bind_rows(hd, ad) %>%
  group_by(team) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success)) %>%
  mutate(d_eff = round(mean_epa*success_rate,5)) %>%
  mutate(d_rank = rank(d_eff)) %>%
  select(team,d_eff,d_rank)%>%
  unique()

#QB rank
#Assumption is that the QB is very important to winning itself
#Using it as a third factor in overall ranking

elo1 = elo %>% 
  filter(!is.na(score1) & season==2021) %>% 
  select(team1, qb1, qb1_value_post) %>%
  rename(team=team1, qb=qb1, qb_elo = qb1_value_post)

elo2 = elo %>% 
  filter(!is.na(score2) & season==2021) %>% 
  select(team2, qb2, qb2_value_post) %>%
  rename(team=team2, qb=qb2, qb_elo = qb2_value_post)

qbelo = bind_rows(elo1, elo2) %>%
  group_by(team,qb) %>%
  summarize(qb_elo = mean(qb_elo)) %>%
  unique()

#Make some changes to get elo data comparable with pbp
qbelo = qbelo %>% mutate(posteam = case_when(as.character(team)=='WSH'~'WAS',
                                             as.character(team)=='LAR'~'LA',
                                             as.character(team)=='OAK'~'LV',
          TRUE~as.character(team)))

#Create new QB name to join with pbp
firstname = sapply(strsplit(as.character(qbelo$qb), ' '), function(x) x[1])
lastname = sapply(strsplit(as.character(qbelo$qb), ' '), function(x) x[length(x)])
qbelo$qbpbp=paste(substring(firstname,1,1),".",lastname,sep="")

#Fix names
qbelo = qbelo %>% mutate(qbpbp = case_when(qbpbp == 'A.Rodgers'~'Aa.Rodgers', TRUE~qbpbp))
#qbelo = qbelo %>% mutate(qbpbp = case_when(qbpbp == 'T.Taylor'~'Ty.Taylor', TRUE~qbpbp))

#Looking for qbs with at least 15 passes
pbpqb = pbp %>% filter(pass==1 & !is.na(epa) & wp > 0.10) %>% 
  select(posteam,passer, qb_epa) %>% 
  group_by(posteam,passer) %>% 
  summarize(qb_epa=mean(qb_epa),passes=n()) %>% 
  filter(passes>15)

#Join pbp and elo qb data
qbs = sqldf("SELECT a.*, b.qb_elo
              FROM pbpqb a LEFT JOIN
              qbelo b ON a.passer = b.qbpbp AND a.posteam = b.posteam
              ")
#Qb metric is epa * elo
qbs = qbs %>%
  mutate(qb_metric = qb_epa*qb_elo)

#Remove fallen soldiers
'%notin%' = Negate('%in%')
qbs = qbs %>% 
  filter(passer %notin% c('A.Dalton','J.Brissett','T.Taylor', 'T.Lance','G.Smith','N.Mullens','C.Rush','J.Winston', 'T.Siemian',
                          'M.White','J.Flacco','C.McCoy','J.Love', 'M.Rudolph','P.Walker','S.Darnold','J.Johnson','T.Boyle', 'D.Lock', 'G.Minshew II',
                          'D.Jones', 'C.Keenum','G.Gilbert','J.Fields', 'L.Jackson','C.Newton', 'M.Glennon')) %>%
  mutate(qb_rank = rank(-qb_metric)) %>%
  rename(team=posteam) %>%
  select(team,qb_metric,qb_rank)


#Overall rank
overall = left_join(o,d, by="team") %>%
  left_join(qbs, by="team") %>% 
  left_join(passo, by="team") %>%
  left_join(passd, by="team") %>%
  left_join(runo, by="team") %>%
  left_join(rund, by="team") %>%
  group_by(team)%>%
  mutate(all_metric=round(sum(o_rank,d_rank,qb_rank)/3,3)) %>%
  arrange(all_metric)

power = as.integer(rank(overall$all_metric))

overall$power_rank = power

#Get average point differential
hpts = pbp %>% filter(!is.na(posteam)& home_team==posteam) %>%
  group_by(game_id,posteam)%>%
  filter(play_id == max(play_id))%>%
  mutate(pt_result = posteam_score_post-defteam_score_post)%>%
  rename(team=posteam)%>%
  select(team,pt_result)%>%
  unique()

apts = pbp %>% filter(!is.na(posteam)& away_team==posteam) %>%
  group_by(game_id,posteam)%>%
  filter(play_id == max(play_id))%>%
  mutate(pt_result = posteam_score_post-defteam_score_post)%>%
  rename(team=posteam)%>%
  select(team,pt_result)%>%
  unique()

pts = bind_rows(hpts, apts) %>%
  group_by(team) %>%
  summarize(avg_pt_result = round(mean(pt_result),2)) %>%
  unique()

overall=sqldf("select a.*, b.avg_pt_result
      from overall a inner join pts b on
      a.team=b.team")

#Add todays date
overall$updated_on = Sys.Date()

###############################

#Strength of schedule

#Loop through each teams schedule and lookup their opponents rank
#Get an average of those ranks and then rank the ranks

teams = teams = c('BAL',
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

base = data.frame(team = as.character(), avg_pwr_rank = as.numeric(), 
                  avg_o_rank = as.numeric(),avg_qb_rank = as.numeric(),
                  avg_pass_o_rank = as.numeric(),avg_run_o_rank= as.numeric(),
                  avg_d_rank = as.numeric(), avg_pass_d_rank = as.numeric(), avg_run_d_rank = as.numeric(),
                  avg_score_diff = as.numeric(),stringsAsFactors = FALSE)

for (team in teams) 
{
  
  print(team)
hs = pbp %>% filter(home_team==team) %>%
  select(home_team,away_team) %>%
  rename(team=home_team,opponent=away_team) %>%
  unique()

as = pbp %>% filter(away_team==team) %>%
  select(home_team,away_team) %>%
  rename(team=away_team,opponent=home_team) %>%
  unique()

ts = bind_rows(hs,as)

#Look up opponent ranks
tr = sqldf("select a.*,b.power_rank,b.o_rank,b.qb_rank,b.pass_o_rank,b.run_o_rank,b.d_rank,
            b.pass_d_rank,b.run_d_rank,b.avg_pt_result
           from ts a inner join overall b on
           a.opponent=b.team")

#Get average ranks
avgr = tr %>% mutate(avg_pwr_rank = mean(power_rank),avg_o_rank=mean(o_rank),avg_qb_rank=mean(qb_rank),
                        avg_pass_o_rank=mean(pass_o_rank),avg_run_o_rank=mean(run_o_rank),avg_d_rank=mean(d_rank),
                        avg_pass_d_rank=mean(pass_d_rank),avg_run_d_rank=mean(run_d_rank),avg_score_diff=mean(avg_pt_result)) %>%
  select(team,avg_pwr_rank,avg_o_rank,avg_qb_rank,avg_pass_o_rank,avg_run_o_rank,avg_d_rank,avg_pass_d_rank,avg_run_d_rank,avg_score_diff)%>%
  unique()

#Append to base dataset
base = base %>% add_row(team=avgr$team,avg_pwr_rank=avgr$avg_pwr_rank,avg_o_rank=avgr$avg_o_rank,avg_qb_rank=avgr$avg_qb_rank,
               avg_pass_o_rank=avgr$avg_pass_o_rank,avg_run_o_rank=avgr$avg_run_o_rank,avg_d_rank=avgr$avg_d_rank,
               avg_pass_d_rank=avgr$avg_pass_d_rank,avg_run_d_rank=avgr$avg_run_d_rank,avg_score_diff=avgr$avg_score_diff)

}


#Rank opponents strength
opppwr = rank(base$avg_pwr_rank)
oppo = rank(base$avg_o_rank)
oppqb = rank(base$avg_qb_rank)
opppasso = rank(base$avg_pass_o_rank)
oppruno = rank(base$avg_run_o_rank)
oppd = rank(base$avg_d_rank)
opppassd = rank(base$avg_pass_d_rank)
opprund = rank(base$avg_run_d_rank)
oppscore = rank(-base$avg_score_diff)

base = base %>%
  mutate(
    opp_pwr_rank = opppwr,
    opp_o_rank = oppo,
    opp_qb_rank = oppqb,
    opp_pass_o_rank = opppasso,
    opp_run_o_rank = oppruno,
    opp_d_rank = oppd,
    opp_pass_d_rank = opppassd,
    opp_run_d_rank = opprund,
    opp_score_diff_rank = oppscore
  ) %>%
  select(team,opp_pwr_rank,opp_o_rank,opp_qb_rank,opp_pass_o_rank,opp_run_o_rank,opp_d_rank,opp_pass_d_rank,opp_run_d_rank,opp_score_diff_rank)

#Add opponent ranks and clean up data
overall = sqldf("select a.*,b.opp_pwr_rank, b.opp_o_rank, b.opp_qb_rank, b.opp_pass_o_rank, b.opp_run_o_rank, 
                b.opp_d_rank, b.opp_pass_d_rank, b.opp_run_d_rank, b.opp_score_diff_rank
                from overall a left join base b on a.team=b.team")

overall = overall %>%
  select(-c('o_eff','d_eff','qb_metric','pass_o_eff','pass_d_eff','run_o_eff','run_d_eff','all_metric'))

#Get turnover differential from FootballDB
# tables =  read_html("https://www.footballdb.com/stats/turnovers.html") %>%
#   html_table(fill=TRUE)
# 
# turnovers = tables[[1]]
# to = data.frame(team=turnovers[1], diff=turnovers[3])
# 
# tod = to %>% filter(Var.1 !='Team' & Var.2 !='Diff') %>%
#   mutate(team=case_when(Var.1=='Indianapolis ColtsIndianapolis'~'IND',
#                         Var.1=='New Orleans SaintsNew Orleans'~'NO',
#                         Var.1=='Kansas City ChiefsKansas City'~'KC',
#                         Var.1=='Baltimore RavensBaltimore'~'BAL',
#                         Var.1=='Buffalo BillsBuffalo'~'BUF',
#                         Var.1=='Arizona CardinalsArizona'~'ARI',
#                         Var.1=='New York JetsNY Jets'~'NYJ',
#                         Var.1=='Chicago BearsChicago'~'CHI',
#                         Var.1=='Dallas CowboysDallas'~'DAL',
#                         Var.1=='Jacksonville JaguarsJacksonville'~'JAX',
#                         Var.1=='San Francisco 49ersSan Francisco'~'SF',
#                         Var.1=='Tennessee TitansTennessee'~'TEN',
#                         Var.1=='Pittsburgh SteelersPittsburgh'~'PIT',
#                         Var.1=='Green Bay PackersGreen Bay'~'GB',
#                         Var.1=='Green Bay PackersGreen Bay'~'GB',
#                         Var.1=='Carolina PanthersCarolina'~'CAR',
#                         Var.1=='Seattle SeahawksSeattle'~'SEA',
#                         Var.1=='Los Angeles ChargersLA Chargers'~'LAC',
#                         Var.1=='Los Angeles RamsLA Rams'~'LA',
#                         Var.1=='Detroit LionsDetroit'~'DET',
#                         Var.1=='Las Vegas RaidersLas Vegas'~'LV',
#                         Var.1=='Philadelphia EaglesPhiladelphia'~'PHI',
#                         Var.1=='Denver BroncosDenver'~'DEN',
#                         Var.1=='Miami DolphinsMiami'~'MIA',
#                         Var.1=='Cleveland BrownsCleveland'~'CLE',
#                         Var.1=='Tampa Bay BuccaneersTampa Bay'~'TB',
#                         Var.1=='Atlanta FalconsAtlanta'~'ATL',
#                         Var.1=='New England PatriotsNew England'~'NE',
#                         Var.1=='New York GiantsNY Giants'~'NYG',
#                         Var.1=='Washington Football TeamWashington'~'WAS',
#                         Var.1=='Minnesota VikingsMinnesota'~'MIN',
#                         Var.1=='Cincinnati BengalsCincinnati'~'CIN',
#                         Var.1=='Houston TexansHouston'~'HOU',
#                         TRUE~Var.1)) %>%
#   mutate(to_diff=as.numeric(Var.2)) %>%
#   select(team,to_diff)


#overall = sqldf("select a.*,b.to_diff from overall as a left join tod as b on a.team=b.team")
#Output rankings

write.xlsx(
  overall,
  file="C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\output\\powerrank\\powerrank.xlsx",
  sheetName = "RAW",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE
)