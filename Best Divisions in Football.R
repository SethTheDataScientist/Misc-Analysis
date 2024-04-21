  NFLDivisions <- EPA_data %>% 
    group_by() %>% 
    mutate(HomeDivision = case_when(
      posteam == "ARI" ~ "NFCW",
      posteam == "SEA" ~ "NFCW",
      posteam == "LA" ~ "NFCW",
      posteam == "SF" ~ "NFCW",
      posteam == "MIN" ~ "NFCN",
      posteam == "GB" ~ "NFCN",
      posteam == "CHI" ~ "NFCN",
      posteam == "DET" ~ "NFCN",
      posteam == "DAL" ~ "NFCE",
      posteam == "NYG" ~ "NFCE",
      posteam == "PHI" ~ "NFCE",
      posteam == "WAS" ~ "NFCE",
      posteam == "NO" ~ "NFCS",
      posteam == "TB" ~ "NFCS",
      posteam == "ATL" ~ "NFCS",
      posteam == "CAR" ~ "NFCS",
      posteam == "DEN" ~ "AFCW",
      posteam == "KC" ~ "AFCW",
      posteam == "LV" ~ "AFCW",
      posteam == "LAC" ~ "AFCW",
      posteam == "BAL" ~ "AFCN",
      posteam == "CLE" ~ "AFCN",
      posteam == "PIT" ~ "AFCN",
      posteam == "CIN" ~ "AFCN",
      posteam == "NYJ" ~ "AFCE",
      posteam == "NE" ~ "AFCE",
      posteam == "BUF" ~ "AFCE",
      posteam == "MIA" ~ "AFCE",
      posteam == "TEN" ~ "AFCS",
      posteam == "IND" ~ "AFCS",
      posteam == "HOU" ~ "AFCS",
      posteam == "JAX" ~ "AFCS")) %>%  
    group_by(season, posteam) %>% 
    mutate(FinPassEPA.x = mean(FinPassEPA.x),
           FinRushEPA.x = mean(FinRushEPA.x),
           FinPassEPA.y = mean(FinPassEPA.y),
           FinRushEPA.y = mean(FinRushEPA.y)) %>% 
    slice_tail(n = 1) %>% 
    group_by(season) %>% 
    mutate(OffPass = percent_rank(FinPassEPA.x),
           OffRush = percent_rank(FinRushEPA.x),
           DefPass = 1-percent_rank(FinPassEPA.y),
           DefRush = 1-percent_rank(FinRushEPA.y)) %>% 
    group_by(season, HomeDivision) %>% 
    summarise(OffPass = mean(OffPass),
              OffRush = mean(OffRush),
              DefPass = mean(DefPass),
              DefRush = mean(DefRush),
                
              DivisionRank = (OffPass * 6 + DefPass * 3 + OffRush * 2 + DefRush)/12)
  
  NFLFinalDivisions <- NFLDivisions %>% 
    group_by(HomeDivision) %>% 
    summarise(OffPass = mean(OffPass),
              OffRush = mean(OffRush),
              DefPass = mean(DefPass),
              DefRush = mean(DefRush),
              
              DivisionRank = mean(DivisionRank))
  
  
  NFLDivisionsTeams <- EPA_data %>% 
    group_by() %>% 
    mutate(HomeDivision = case_when(
      posteam == "ARI" ~ "NFCW",
      posteam == "SEA" ~ "NFCW",
      posteam == "LA" ~ "NFCW",
      posteam == "SF" ~ "NFCW",
      posteam == "MIN" ~ "NFCN",
      posteam == "GB" ~ "NFCN",
      posteam == "CHI" ~ "NFCN",
      posteam == "DET" ~ "NFCN",
      posteam == "DAL" ~ "NFCE",
      posteam == "NYG" ~ "NFCE",
      posteam == "PHI" ~ "NFCE",
      posteam == "WAS" ~ "NFCE",
      posteam == "NO" ~ "NFCS",
      posteam == "TB" ~ "NFCS",
      posteam == "ATL" ~ "NFCS",
      posteam == "CAR" ~ "NFCS",
      posteam == "DEN" ~ "AFCW",
      posteam == "KC" ~ "AFCW",
      posteam == "LV" ~ "AFCW",
      posteam == "LAC" ~ "AFCW",
      posteam == "BAL" ~ "AFCN",
      posteam == "CLE" ~ "AFCN",
      posteam == "PIT" ~ "AFCN",
      posteam == "CIN" ~ "AFCN",
      posteam == "NYJ" ~ "AFCE",
      posteam == "NE" ~ "AFCE",
      posteam == "BUF" ~ "AFCE",
      posteam == "MIA" ~ "AFCE",
      posteam == "TEN" ~ "AFCS",
      posteam == "IND" ~ "AFCS",
      posteam == "HOU" ~ "AFCS",
      posteam == "JAX" ~ "AFCS")) %>%  
    group_by(season, posteam) %>% 
    mutate(FinPassEPA.x = mean(FinPassEPA.x),
          FinRushEPA.x = mean(FinRushEPA.x),
          FinPassEPA.y = mean(FinPassEPA.y),
          FinRushEPA.y = mean(FinRushEPA.y)) %>% 
    slice_tail(n = 1) %>% 
    group_by(season) %>% 
    mutate(OffPass = percent_rank(FinPassEPA.x),
           OffRush = percent_rank(FinRushEPA.x),
           DefPass = 1-percent_rank(FinPassEPA.y),
           DefRush = 1-percent_rank(FinRushEPA.y)) %>% 
    group_by(season, posteam, HomeDivision) %>% 
    summarise(OffPass = mean(OffPass),
              OffRush = mean(OffRush),
              DefPass = mean(DefPass),
              DefRush = mean(DefRush),
              
              DivisionRank = (OffPass * 6 + DefPass * 3 + OffRush * 2 + DefRush)/12)
  