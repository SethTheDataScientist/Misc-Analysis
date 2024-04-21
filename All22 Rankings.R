
setwd("C:/Users/sethl/OneDrive/Excel Files/Football/Data Csv")


pff_api <- read_csv("pff_api.csv")

setwd("C:/Users/sethl/OneDrive/Important Stuff/R/R files/NFL")


All22Rankings <- IndivWARData %>% 
    group_by(player_id) %>% 
    arrange(desc(season)) %>% 
  mutate(CareerTrend = map_dbl(split(WAR, season), 
                         ~coef(lm(WAR ~ season))[[2]])) %>% 
    mutate(MostRecentSeason = max(season),
           YearsInNFL = n(),
           careerAvg = mean(WAR, na.rm = T),
           RecentAvg = mean(WAR[MostRecentSeason - season < 3], na.rm = T)) %>%
    filter(!is.na(position), season == 2022) %>% 
  left_join(pff_api, by = c("player_id" = "id")) %>% 
  group_by(position.x) %>% 
  mutate(AgePosRank = 1-percent_rank(age),
         RecentWARRank = percent_rank(RecentAvg)) %>% 
  group_by() %>% 
  mutate(CareerTrend = percent_rank(CareerTrend),
          Value = (WAR * 3 + RecentWARRank * 3 + CareerTrend * 2 + AgePosRank)/9) %>% 
  select(player_id, name, position.x, WAR, YearsInNFL, careerAvg, RecentAvg, age, AgePosRank, RecentWARRank, CareerTrend, Value) %>% 
  arrange(desc(Value)) %>% 
  mutate(Over30 = if_else(age >= 30, 1, 0),
         position.x = as.factor(position.x))


write_xlsx(All22Rankings, "All22Rankings.xlsx")




