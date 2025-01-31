library(dplyr)

game_data <- readRDS("/Users/francesbaldridge/Desktop/STAT 4800/data.rds")

teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
num_teams <- length(teams)

transition_matrix <- matrix(0, nrow = num_teams, ncol = num_teams, dimnames = list(teams, teams))

for (i in 1:nrow(game_data)) {
  visitor <- game_data$Visiting_Team[i]
  home <- game_data$Home_Team[i]
  
  visitor_score <- game_data$Visiting_Score[i]
  home_score <- game_data$Home_Score[i]
  
  if (visitor_score < home_score) {
    transition_matrix[visitor, home] <- transition_matrix[visitor, home] + 1
  } else if (home_score < visitor_score) {
    transition_matrix[home, visitor] <- transition_matrix[home, visitor] + 1
  }
}

row_totals <- rowSums(transition_matrix)

transition_matrix <- transition_matrix / row_totals

transition_matrix
