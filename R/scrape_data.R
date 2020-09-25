# Libraries---------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, purrr, polite,  
               ggplot2, ggtext, tibble, stringr,
               rvest)

# Scrape Function --------------------------------------------------------------
get_teams <- function() {
  base_url <- bow("https://www.transfermarkt.co.in/premier-league/startseite/wettbewerb/GB1")
  base_raw <- scrape(base_url) %>%
    html_nodes(".vereinprofil_tooltip") %>%
    html_attr("href") %>%
    unique() %>%
    str_subset("/startseite") %>%
    str_remove_all("/") %>%
    str_remove_all("saison_id2020") %>%
    str_remove_all("startseiteverein")
  
  # Scrape team_id
  team_id <- base_raw %>%
    str_match_all("[0-9]+") %>%
    unlist %>%
    as.numeric %>%
    tibble
  id <- rename(team_id, id = .)
  
  # Scrape team_name  
  club <- base_raw  %>%
    str_remove_all("/") %>%
    str_remove_all("saison_id2020") %>%
    str_remove_all("startseiteverein") %>%
    str_remove_all("[0-9]+") %>%
    unlist %>%
    as.character %>%
    tibble
  club <- rename(club, club = .)
  
  team_df <- bind_cols(club, id)
  
  return(team_df)
}

team_df <- get_teams()
write.csv(team_df, file = "./data/teams.csv", row.names = FALSE, col.names = TRUE)

# create team_df tibble---------------------------------------------------------
add_links <- function() {
  base_url <- bow("https://www.transfermarkt.co.in/premier-league/startseite/wettbewerb/GB1")
  
  #Scrape links
  team_links <- scrape(base_url) %>%
    html_nodes(".vereinprofil_tooltip") %>%
    html_attr("href") %>%
    unique() %>%
    str_subset("/startseite") %>%
    as.character %>%
    tibble
  
  team_links <- rename(team_links, links = .)
  teams_df <- bind_cols(team_df, team_links)
  return(teams_df)
}


# team-players------------------------------------------------------------------
players_csv_data <- function() {
  for(i in 1 : 20) {
    club <- as.character(team_df[i, 1])
    id <- as.character(team_df[i,2])
    base_url <- glue::glue("https://www.transfermarkt.com/{club}/kader/verein/{id}")
    base_raw <- scrape(bow(base_url)) %>%
      html_nodes(".spielprofil_tooltip") %>%
      html_attr("href") %>%
      unique() %>%
      str_remove_all("/") %>%
      str_remove_all("profilspieler")
    # players names
    player_name <- base_raw %>%
      str_remove_all("[0-9]+") %>%
      unlist %>%
      as.character %>%
      data.frame() %>%
      rename(!!club := .)
    
    # players id
    player_id <- base_raw %>%
      str_match_all("[0-9]+") %>%
      unlist %>%
      as.numeric %>%
      data.frame() %>%
      rename(!!id := .)
    
    # players dataframe
    players_df = cbind(player_name, player_id)
    
    # write csv file in ./data
    write.csv(x = players_df, file = glue::glue("./data/players/{club}_players.csv"), col.names = TRUE)
  
  }
}

# function call
players_csv_data()


#fpl data-----------------------------------------------------------------------

library(fplr)

players <- fpl_get_player_all()
View(players)

t <- fpl_get_teams()
View(t)

names(t)
