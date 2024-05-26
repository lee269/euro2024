library(tidyr)
library(dplyr)
library(here)
library(readr)
library(purrr)

# Original data downloaded from fixturedownload.com
uefa_euro_2024 <- read_csv("data/uefa-euro-2024.csv")

matchdata <- uefa_euro_2024 %>% 
     rename_with(tolower) %>% 
     pivot_longer(cols = c(hometeam, awayteam), names_to = "homeaway", values_to = "team") %>% 
     mutate(homeaway = tolower(homeaway),
            goals = 0,
            penalties = 0,
            ycards = 0,
            rcards = 0,
            fouls = 0) %>% 
     select(-result)

# write_csv(matchdata, here("data", "matchdata.csv"))

matchdata <- read_csv(here("data", "matchdata.csv"))

matchdata_wide <- matchdata %>% 
  # mutate(goals = round(runif(102, min = 0, max = 4), digits = 0)) %>% 
  pivot_wider(names_from = homeaway,
              values_from = c(team, goals, penalties, ycards, rcards, fouls)) %>% 
  mutate(points_hometeam = case_when(goals_hometeam > goals_awayteam ~ 3,
                                     goals_hometeam == goals_awayteam ~ 1,
                                     TRUE ~ 0),
         points_awayteam = case_when(goals_awayteam > goals_hometeam ~ 3,
                                     goals_awayteam == goals_hometeam ~ 1,
                                     TRUE ~ 0))


hometeam <- matchdata_wide %>% 
            select(matchnumber, roundnumber, date, location, group,
                   team = team_hometeam,
                   goals_for = goals_hometeam,
                   goals_against = goals_awayteam,
                   penalties = penalties_hometeam,
                   ycards = ycards_hometeam,
                   rcards = rcards_hometeam,
                   fouls = fouls_hometeam,
                   points = points_hometeam)

awayteam <- matchdata_wide %>% 
  select(matchnumber, roundnumber, date, location, group,
         team = team_awayteam,
         goals_for = goals_awayteam,
         goals_against = goals_hometeam,
         penalties = penalties_awayteam,
         ycards = ycards_awayteam,
         rcards = rcards_awayteam,
         fouls = fouls_awayteam,
         points = points_awayteam)

matches <- hometeam %>% 
           bind_rows(awayteam)

league_table <- function(groupname) {
matches %>% 
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
            thug_index = sum(rcards) *2 + sum(ycards),
            fouls = sum(fouls),
            pts = sum(points)) %>% 
  arrange(-pts) %>% knitr::kable(caption = groupname)
}

groups <- c("Group A", "Group B", "Group C")

map(groups, league_table)
