PlayoffKryptoniteTot <- Playoff_Wins_and_Losses %>% 
  select(Column1, Column2) %>% 
  rename(Winner = Column1,
         Loser = Column2) %>% 
  group_by(Winner, Loser) %>% 
  summarise(Count = n()) %>% 
  group_by(Loser) %>% 
  left_join(PlayoffKryptoniteLoss, by = c("Loser" = "Winner"))  
  filter(Winner == Loser.y)


write_xlsx(PlayoffKryptoniteTot, path = "PlayoffKryptoniteTotal.xlsx")
