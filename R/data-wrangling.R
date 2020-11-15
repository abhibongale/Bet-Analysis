# library-----------------------------------------------------------------------
library(tidyverse)
library(fplr)

# Read dataset------------------------------------------------------------------

players_all <- fpl_get_player_all()
players_all <- fpl_get_players() %>%
  select(-(chance_of_playing_next_round:dreamteam_count), -(ep_next:first_name),
         -news, -news_added, -photo, -special,
         -squad_number,-(corners_and_indirect_freekicks_order:penalties_text))


## goalkeepers------------------------------------------------------------------
goalkeepers <- players_all %>%
  filter(element_type == 1) %>%
  select(-element_type)

## defenders--------------------------------------------------------------------
defenders <- players_all %>%
  filter(element_type == 2) %>%
  select(-element_type)

## midfielders--------------------------------------------------------------------
midfielders <- players_all %>%
  filter(element_type == 3) %>%
  select(-element_type)

## attackers--------------------------------------------------------------------
attackers <- players_all %>%
  filter(element_type == 4) %>%
  select(-element_type)

## average_data set-----------------------------------------------------------
avg_position_set <- function(data) {
avg_data <- aggregate(data$influence, list(data$team), mean) %>%
    select(x) %>%
    rename("influence" = x)
avg_data <- avg_data %>%
    add_column(aggregate(data$creativity, list(data$team), mean) %>%
                 select(x) %>%
                 rename("creativity" = x)) %>%
    add_column(aggregate(data$threat, list(data$team), mean) %>%
                 select(x) %>%
                 rename("threat" = x)) %>%
    add_column(aggregate(data$points_per_game, list(data$team), mean) %>%
                 select(x) %>%
                 rename("points_per_game" = x)) %>%
    add_column(aggregate(data$total_points, list(data$team), mean) %>%
                 select(x) %>%
                 rename("total_points" = x)) %>%
    add_column(aggregate(data$goals_scored, list(data$team), mean) %>%
                 select(x) %>%
                 rename("goals_scored" = x)) %>%
    add_column(aggregate(data$assists, list(data$team), mean) %>%
                 select(x) %>%
                 rename("assists" = x)) %>%
    add_column(aggregate(data$bonus, list(data$team), mean) %>%
                 select(x) %>%
                 rename("bonus" = x)) %>%
    add_column(aggregate(data$bps, list(data$team), mean) %>%
                 select(x) %>%
                 rename("bps" = x)) %>%
    add_column(aggregate(data$form, list(data$team), mean) %>%
                 select(x) %>%
                 rename("form" = x)) %>%
    add_column(aggregate(data$value_form, list(data$team), mean) %>%
                 select(x) %>%
                 rename("value" = x)) %>%
    add_column(aggregate(data$minutes, list(data$team), mean) %>%
                 select(x) %>%
                 rename("minutes" = x)) %>%
    add_column(aggregate(data$selected_by_percent, list(data$team), mean) %>%
                 select(x) %>%
                 rename("selected_by_percent" = x)) %>%
    add_column(aggregate(data$transfers_in, list(data$team), mean) %>%
                 select(x) %>%
                 rename("transfers_in" = x)) %>%
    add_column(aggregate(data$transfers_out, list(data$team), mean) %>%
                 select(x) %>%
                 rename("transfers_out" = x)) %>%
    add_column(aggregate(data$yellow_cards, list(data$team), mean) %>%
                 select(x) %>%
                 rename("yellow_cards" = x)) %>%
    add_column(aggregate(data$red_cards, list(data$team), mean) %>%
                 select(x) %>%
                 rename("red_cards" = x))%>%
    add_column(aggregate(data$transfers_in_event, list(data$team), mean) %>%
                 select(x) %>%
                 rename("transfer_in_event" = x)) %>%
    add_column(aggregate(data$transfers_out_event, list(data$team), mean) %>%
                 select(x) %>%
                 rename("transfer_out_event" = x))
return(avg_data)
}

avg_attack <- avg_position_set(attackers)
avg_midfielders <-avg_position_set(midfielders)
avg_defenders <- avg_position_set(defenders)

# write csv---------------------------------------------------------------------
write.csv(goalkeepers, "./data/goalkeepers.csv", col.names=TRUE, row.names = FALSE)
write.csv(attackers, "./data/attackers.csv", col.names=TRUE, row.names = FALSE)
write.csv(defenders, "./data/defenders.csv", col.names=TRUE, row.names = FALSE)
write.csv(midfielders, "./data/midfielders.csv", col.names=TRUE, row.names = FALSE)
