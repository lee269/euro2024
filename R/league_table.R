league_table <- function(groupname, data){
  
table <- data %>% 
  na.omit() %>% 
  filter(group == groupname) %>%
  group_by(team) %>% 
  summarise(played = n(),
            goals_for = sum(goals_for),
            goals_against = sum(goals_against),
            goals_sum = sum(goals_for) + sum(goals_against),
            goal_diff = sum(goals_for) - sum(goals_against),
            ycards = sum(ycards),
            rcards = sum(rcards),
            fouls = sum(fouls),
            thug_index = ((rcards * 25) + (ycards *5) + fouls) / played,
            pts = sum(points)) 
  

wld <- data %>% 
  na.omit() %>% 
  filter(group == groupname) %>%
  group_by(team, result) %>% 
  summarise(played = n()) %>% 
  pivot_wider(names_from = result, values_from = played, values_fill = 0)

grouptable <- table %>% 
  left_join(wld,by = c("team" = "team")) %>% 
  relocate(starts_with(c("W", "L", "D")), .after = team) %>%
  arrange(-pts, -goal_diff, ycards) %>% 
  knitr::kable(caption = paste0("**",groupname,"**"))

return(grouptable)

}
