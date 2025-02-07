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
  
  # Removing the 1000 samples
  if (i > remove) {
    coin_counts[current_team] <- coin_counts[current_team] + 1
  }
}

# Normalize counts to get proportions
#steady_state <- coin_counts / sum(coin_counts)

# Rankings
#ranking <- sort(steady_state, decreasing = TRUE)
#final_rank <- rank(-ranking, ties.method = "first")

#ranking
#final_rank


#The question we will be answering is which teams were the best during each season. By
#modifying the transtion matrix to calculate the rankings separately for each year, we 
#can draw conclusions about teams progress over time. Using this and otheroutisde 
#information we would hope to gather about ownership and roster changes, we can begin 
#to explore which teams have been consistently successful and which have rapidly improved.

# Find the unique teams for matrix dimensions
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
num_teams <- length(teams)

# Create a list to store transition matrices for each year
years <- unique(game_data$season)
transition_matrices <- list()

# Loop over each year and calculate the transition matrix
for (year in years) {
  # Filter data for the current year
  yearly_data <- filter(game_data, season == year)
  
  # Initialize an empty transition matrix for the year
  transition_matrix <- matrix(0, nrow = num_teams, ncol = num_teams, dimnames = list(teams, teams))
  
  # Populate the transition matrix
  for (i in 1:nrow(yearly_data)) {
    visitor <- yearly_data$Visiting_Team[i]
    home <- yearly_data$Home_Team[i]
    visitor_score <- yearly_data$Visiting_Score[i]
    home_score <- yearly_data$Home_Score[i]
    
    # Assign +1 to the winning team
    if (visitor_score < home_score) {
      transition_matrix[visitor, home] <- transition_matrix[visitor, home] + 1
    } else if (home_score < visitor_score) {
      transition_matrix[home, visitor] <- transition_matrix[home, visitor] + 1
    }
  }
  
  # Normalize the matrix (handle cases where a team has no outgoing transitions)
  row_totals <- rowSums(transition_matrix)
  row_totals[row_totals == 0] <- 1  # Avoid division by zero
  transition_matrix <- transition_matrix / row_totals
  
  # Store the matrix in the list
  transition_matrices[[as.character(year)]] <- transition_matrix
}

# Test year - 2023
transition_matrices[["2023"]]

# Markov Chain sampling 
year_to_analyze <- "2023"
transition_matrix <- transition_matrices[[year_to_analyze]]

# Method A
count <- 20000
remove <- 1000
teams <- rownames(transition_matrix)
num_teams <- length(teams)

# Start with a random team
current_team <- sample(teams, 1)
coin_counts <- setNames(rep(0, num_teams), teams)

# Perform Markov Chain sampling
for (i in 1:count) {
  current_team <- sample(teams, 1, prob = transition_matrix[current_team, ])
  
  if (i > remove) {
    coin_counts[current_team] <- coin_counts[current_team] + 1
  }
}

# Normalize counts to get proportions
steady_state <- coin_counts / sum(coin_counts)

# Rankings
#ranking <- sort(steady_state, decreasing = TRUE)
#final_rank <- rank(-ranking, ties.method = "first")

#ranking
#final_rank
