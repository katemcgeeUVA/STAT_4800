library(dplyr)

game_data <- readRDS("/Users/francesbaldridge/Desktop/STAT 4800/data.rds")

#find the number of teams for matrix dimensions
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
num_teams <- length(teams)

#intialize empty matrix
transition_matrix <- matrix(0, nrow = num_teams, ncol = num_teams, dimnames = list(teams, teams))

#loop to assign the score to the corresponding team
for (i in 1:nrow(game_data)) {
  visitor <- game_data$Visiting_Team[i]
  home <- game_data$Home_Team[i]
  
  visitor_score <- game_data$Visiting_Score[i]
  home_score <- game_data$Home_Score[i]
 
#assign the +1 to the winning team 
  if (visitor_score < home_score) {
    transition_matrix[visitor, home] <- transition_matrix[visitor, home] + 1
  } else if (home_score < visitor_score) {
    transition_matrix[home, visitor] <- transition_matrix[home, visitor] + 1
  }
}

#normalize the matrix
row_totals <- rowSums(transition_matrix)
transition_matrix <- transition_matrix / row_totals

transition_matrix



#method A 
count <- 20000  
remove <- 1000  # Remove first 1000 samples

# Get team names
teams <- rownames(transition_matrix)
num_teams <- length(teams)

# Start with a random team
current_team <- sample(teams, 1)
coin_counts <- setNames(rep(0, num_teams), teams)  # Initialize coin counts

# Perform Markov Chain sampling
for (i in 1:count) {
  # Choose next team based on current team's row in the transition matrix
  current_team <- sample(teams, 1, prob = transition_matrix[current_team, ])
  
  # Count only after burn-in period
  if (i > remove) {
    coin_counts[current_team] <- coin_counts[current_team] + 1
  }
}

# Normalize counts to get proportions
steady_state <- coin_counts / sum(coin_counts)

# Print final rankings
ranking <- sort(steady_state, decreasing = TRUE)
ranking
