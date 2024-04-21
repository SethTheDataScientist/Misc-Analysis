TeamReceivingSupport <- WRWARJoin %>% 
  select(!RecTeamWAR) %>% 
  filter(position == "WR" | position == "TE", Snaps >= 75) %>% 
  mutate(HighValue = case_when(WAR >= 0.65 & season >= 2018 ~ 1,
                               T ~ 0)) %>% 
  group_by(player) %>% 
  mutate(HighValue = case_when(sum(HighValue) >= 1 ~ 1,
                                   T ~ 0)) %>% 
  group_by(season, team_name) %>% 
  summarise(Support = sum(WAR[WAR<0.65 & HighValue == 0]),
            SupportCount = sum(if_else(WAR < 0.65 & HighValue == 0, 1, 0)),
         HighEnd = sum(WAR[WAR >= 0.65 | HighValue == 1]),
         HighEndCount = sum(if_else(WAR >= 0.65 | HighValue == 1, 1, 0)),
         TotalWAR = sum(Support + HighEnd))


AlphaWRMetric <- pbpCurrent %>% 
  filter(epa != 0, down != 0, !is.na(epa), !is.na(down),
         !is.na(posteam), !is.na(drive), 
         penalty == 0, special_teams_play == 0,
         season >= 2011) %>% 
  group_by(season, posteam, game_id, drive) %>% 
  mutate(TotalPlays = n(),
         Passes = sum(pass, na.rm  =T),
         Rushes = sum(rush, na.rm = T),
         Yards = sum(yards_gained, na.rm = T)) %>% 
  group_by(season, posteam, receiver, receiver_player_id, game_id, drive) %>% 
  summarise(Count = n(),
            Targets = sum(pass, na.rm = T),
            Catches = sum(complete_pass, na.rm = T),
            RECYards = sum(yards_gained, na.rm = T),
            TotalPlays = head(TotalPlays, 1),
            Passes = head(Passes, 1),
            Yards = head(Yards, 1),
            TargetShare = Targets/TotalPlays,
            PassTargetShare = Targets/Passes,
            YardsShare = RECYards/Yards) %>% 
  distinct() %>% 
  group_by() %>% 
  filter(TotalPlays >= 4, !is.na(receiver), !is.na(receiver_player_id)) %>% 
  group_by(season, posteam, receiver, receiver_player_id) %>% 
  mutate(Count = n(),
            AlphaDrive = (case_when(TargetShare >= 0.4 & 
                                          YardsShare >= 0.4 ~ 1,
                                        T ~ 0))) %>% 
  group_by(season, posteam, receiver, receiver_player_id) %>% 
  summarise(Count = n(),
            AlphaDrives = sum(case_when(TargetShare >= 0.4 & 
                            YardsShare >= 0.4 ~ 1,
                          T ~ 0)),
            RECYards = sum(RECYards, na.rm = T),
            AlphaPercent = AlphaDrives/Count,
            AlphaTargetShare = mean(TargetShare[AlphaDrive >= 1], na.rm = T),
            AlphaYardsShare = mean(YardsShare[AlphaDrive >= 1], na.rm = T)) %>%  
  filter(!is.nan(AlphaTargetShare), Count >= 20, RECYards >= 500) %>% 
  group_by() %>% 
  mutate(ADPR = percent_rank(AlphaDrives),
         APPR = percent_rank(AlphaPercent),
         ATSPR = percent_rank(AlphaTargetShare),
         AYSPR = percent_rank(AlphaYardsShare),
         Value = (ADPR + ATSPR + AYSPR)/3,
         AlphaValue = (ADPR *2 + APPR)/3
         ) %>% 
  group_by(receiver_player_id) %>% 
  mutate(CareerAlphaValue = mean(AlphaValue),
         Seasons = n(),
         LastSeason = max(season)) %>% 
  group_by(posteam, receiver_player_id) %>% 
  mutate(TeamAlphaValue = mean(AlphaValue)) %>% 
  filter(Seasons >= 3 | (Seasons <3 & LastSeason >= 2020)) %>% 
  select(season, posteam, receiver,
         AlphaValue, TeamAlphaValue, CareerAlphaValue,
         Count, AlphaDrives, AlphaPercent, everything())


# college -----------------------------------------------------------------

TeamReceivingSupport <- WRWARJoin %>% 
  select(!RecTeamWAR) %>% 
  filter(position == "WR" | position == "TE", Snaps >= 75) %>% 
  mutate(HighValue = case_when(WAR >= 0.65 & season >= 2018 ~ 1,
                               T ~ 0)) %>% 
  group_by(player) %>% 
  mutate(HighValue = case_when(sum(HighValue) >= 1 ~ 1,
                               T ~ 0)) %>% 
  group_by(season, team_name) %>% 
  summarise(Support = sum(WAR[WAR<0.65 & HighValue == 0]),
            SupportCount = sum(if_else(WAR < 0.65 & HighValue == 0, 1, 0)),
            HighEnd = sum(WAR[WAR >= 0.65 | HighValue == 1]),
            HighEndCount = sum(if_else(WAR >= 0.65 | HighValue == 1, 1, 0)),
            TotalWAR = sum(Support + HighEnd))






AlphaCollegeWRMetric <- pbpCollege %>% 
  filter(EPA != 0, down != 0, !is.na(EPA), !is.na(down),
         !is.na(pos_team), !is.na(drive_number), 
         is.na(yds_penalty) == 1, kick_play == 0,
         season >= 2011) %>%
  left_join(PFF_College_conference_addition, by = c("pos_team")) %>% 
  filter(Conference == "ACC" | Conference == "SEC" | Conference == "Big 12" |
           Conference == "Big Ten" | Conference == "Pac-12" |
           pos_team == "Notre Dame") %>% 
  group_by(season, pos_team, game_id, drive_number) %>% 
  mutate(TotalPlays = n(),
         Passes = sum(pass, na.rm  =T),
         Rushes = sum(rush, na.rm = T),
         Yards = sum(yards_gained, na.rm = T)) %>% 
  group_by(season, pos_team, receiver_player_name) %>% 
  mutate(RECYards = sum(yds_receiving, na.rm = T)) %>% 
  group_by(season, pos_team, receiver_player_name,  game_id, drive_number) %>% 
  summarise(Count = n(),
            Targets = sum(pass, na.rm = T),
            Catches = sum(completion, na.rm = T),
            RECYards = head(RECYards, 1),
            TotalPlays = head(TotalPlays, 1),
            Passes = head(Passes, 1),
            Yards = head(Yards, 1),
            TargetShare = Targets/TotalPlays,
            PassTargetShare = Targets/Passes,
            YardsShare = RECYards/Yards) %>% 
  distinct() %>% 
  group_by() %>% 
  filter(TotalPlays >= 4, !is.na(receiver_player_name)) %>% 
  group_by(season, pos_team, receiver_player_name) %>% 
  mutate(Count = n(),
         Alphadrive_number = (case_when(TargetShare >= 0.4 & 
                                   YardsShare >= 0.4 ~ 1,
                                 T ~ 0))) %>% 
  group_by(season, pos_team, receiver_player_name) %>% 
  summarise(Count = n(),
            Alphadrive_numbers = sum(case_when(TargetShare >= 0.4 & 
                                          YardsShare >= 0.4 ~ 1,
                                        T ~ 0)),
            RECYards = head(RECYards, 1),
            AlphaPercent = Alphadrive_numbers/Count,
            AlphaTargetShare = mean(TargetShare[Alphadrive_number >= 1], na.rm = T),
            AlphaYardsShare = mean(YardsShare[Alphadrive_number >= 1], na.rm = T)) %>%  
  filter(Count >= 15, !is.nan(AlphaTargetShare)) %>% 
  group_by() %>% 
  mutate(ADPR = percent_rank(Alphadrive_numbers),
         APPR = percent_rank(AlphaPercent),
         ATSPR = percent_rank(AlphaTargetShare),
         AYSPR = percent_rank(AlphaYardsShare),
         Value = (ADPR + ATSPR + AYSPR)/3,
         AlphaValue = (ADPR *2 + APPR)/3
  ) %>% 
  group_by(receiver_player_name) %>% 
  mutate(CareerAlphaValue = mean(AlphaValue),
         Seasons = n(),
         LastSeason = max(season)) %>% 
  group_by(pos_team, receiver_player_name) %>% 
  mutate(TeamAlphaValue = mean(AlphaValue)) %>% 
  filter(Seasons >= 2 | (Seasons <2 & LastSeason >= 2018)) %>% 
  select(season, pos_team, receiver_player_name,
         AlphaValue, TeamAlphaValue, CareerAlphaValue,
         Count, Alphadrive_numbers, AlphaPercent, everything())


