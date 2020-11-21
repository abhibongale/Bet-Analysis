library('tidyverse')
library("fplr")
#library('fplscrapR')

# read-dataset
players_all <- fpl_get_player_all()

# generate best players through points_per_game
top_points_per_games <- function(.data, position, gameweek) {
  return(.data %>%
    filter(element_type == position & 
             minutes >= (60*gameweek) &
             quantile(points_per_game,probs = .9)<= points_per_game &
             quantile(value_season, probs = .9) <= value_season
           ) %>%
    arrange(desc(points_per_game))
    )
}

forwards <- top_points_per_games(players_all, 4, 8)
midfielders <- top_points_per_games(players_all,3, 8)


# compare forwards players per gameweek
top_forwards <- as_tibble()
for (ids in forwards$id) {
  top_forwards <- rbind(fpl_get_player_current(ids) %>% 
                          mutate(players_all %>% 
                                   filter(ids == players_all$id) %>% 
                                   select(web_name)), 
                        top_forwards)
}

top_forwards %>% 
  filter(round %in% 1:8) %>%
  select(web_name,round,total_points) %>% # selecting the relevant columns
  ggplot() + # plotting using ggplot2
  geom_line(aes(x=round,y=total_points,group=web_name,colour=web_name),size=1) +
  theme_bw() +
  labs(x="Gameweek",y="Total score",title="Top forwards of this season")

summary(lm(top_forwards$assists~top_forwards$creativity+top_forwards$bps+top_forwards$influence))
confint(lm(top_forwards$assists~top_forwards$creativity+top_forwards$bps+top_forwards$influence))
       