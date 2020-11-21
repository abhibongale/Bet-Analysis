# Library----------------------------------------------------------------------
library(tidyverse)


# read_dataset-----------------------------------------------------------------
attackers <- read_csv('./data/attackers.csv')
midfielders <- read_csv('./data/midfielders.csv')
defenders <- read_csv("./data/defenders.csv")

# visualization-----------------------------------------------------------------  a
attackers %>%
  select(-id, -in_dreamteam, -second_name, -web_name, -status, -team_code, -saves, -penalties_saved) %>%
  cor() %>%
  view()

avg_attack <- avg_position_set(attackers)
avg_midfielders <- avg_position_set(midfielders)
avg_defenders <- avg_position_set(defenders)

ggplot(avg_attack, aes(creativity, influence)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_text(
    label=rownames(avg_attack), 
    nudge_x = 0.45, nudge_y = 0.45, 
    check_overlap = T,
    color = "red",
  )

ggplot(avg_attack, aes(total_points, points_per_game)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_text(
    label=rownames(avg_attack), 
    nudge_x = 0.45, nudge_y = 0.45, 
    check_overlap = T,
    color = "red",
  )


ggplot(avg_attack, aes(total_points, bps)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_text(
    label=rownames(avg_attack), 
    nudge_x = 0.45, nudge_y = 0.45, 
    check_overlap = T,
    color = "red",
  )


ggplot(avg_attack, aes(total_points, goals_scored)) +
  geom_point() +
  geom_smooth(method="loess") +
  geom_text(
    label=rownames(avg_attack), 
    nudge_x = 0.45, nudge_y = 0.45, 
    check_overlap = T,
    color = "red",
  )


ggplot(avg_midfielders, aes(total_points, points_per_game)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_text(
    label=rownames(avg_midfielders), 
    nudge_x = 0.45, nudge_y = 0.45, 
    check_overlap = T,
    color = "red",
  )





ggplot(avg_attack, aes(total_points, form)) +
  geom_point() +
  geom_smooth(method="glm") +
  geom_text(
    label=rownames(avg_attack), 
    nudge_x = 0.45, nudge_y = 0.45, 
    check_overlap = T,
    color = "red",
  )

mean(avg_attack$form)

cor(avg_attack) %>%
  view()

## defenders-------------------------------------------------------------------

ggplot(avg_defenders, aes(total_points, points_per_game)) +
  geom_point() +
  geom_smooth(method="loess") +
  geom_text(
    label=rownames(avg_defenders), 
    nudge_x = 0.45, nudge_y = 0.45, 
    check_overlap = T,
    color = "red",
  )

ggplot(avg_defenders, aes(total_points, points_per_game)) +
  geom_point() +
  geom_smooth(method="glm") +
  geom_text(
    label=rownames(avg_defenders), 
    nudge_x = 0.45, nudge_y = 0.45, 
    check_overlap = T,
    color = "red",
  )

ggplot(avg_defenders, aes(points_per_game, bps)) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_text(
    label = rownames(avg_defenders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )

ggplot(avg_defenders, aes(bonus, bps)) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_text(
    label = rownames(avg_defenders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )

ggplot(avg_defenders, aes(total_points, points_per_game)) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_text(
    label = rownames(avg_defenders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )

ggplot(avg_defenders, aes(total_points, bps)) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_text(
    label = rownames(avg_defenders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )
ggplot(avg_defenders, aes(total_points, points_per_game)) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_text(
    label = rownames(avg_defenders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )


game_points_value <- avg_defenders$points_per_game/mean(avg_defenders$value)
avg_defenders <- avg_defenders %>%
  add_column(game_points_value) %>%
  rename(game_points_per_value = "game_points_value")

ggplot(avg_defenders, aes(total_points, game_points_value)) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_text(
    label = rownames(avg_defenders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )

## midfielders-----------------------------------------------------------------
ggplot(avg_midfielders, aes(total_points, points_per_game)) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_text(
    label = rownames(avg_defenders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )

ggplot(avg_midfielders, aes(total_points, bps)) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_text(
    label = rownames(avg_midfielders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )

ggplot(avg_midfielders, aes(form, bps)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_text(
    label = rownames(avg_midfielders),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )


  ## attackers-------------------------------------------------------------------

ggplot(avg_attack, aes(form, bps)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_text(
    label = rownames(avg_attack),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )


ggplot(avg_attack, aes(total_points, points_per_game)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_text(
    label = rownames(avg_attack),
    #nudge_x = 0.45, nudge_y = 0.45,
    check_overlap = T,
    color = "red"
  )

midfielders %>%
  filter(team == 6)

defenders %>%
  filter(team == 10) %>% view()
