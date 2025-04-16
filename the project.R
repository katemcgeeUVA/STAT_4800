# Redzone Play Call Tendencies Simulation

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Assume required helper functions and models are sourced from GitHub-based lab files:
# - get_yards() to sample yards gained
# - get_play_call() to decide run or pass
# - get_drive_outcome() to resolve end-of-drive scenarios
# These would ideally be implemented in files like utils.R, run_play.R, etc.

# Constants
REDZONE_START <- 20
NUM_SIMULATIONS <- 10000

# Define scoring function
evaluate_score <- function(yard_line, play_type, down, yards_to_go) {
  # Simulate yards gained
  yards_gained <- sample_yards(yard_line, down, yards_to_go, play_type)
  
  new_yard_line <- max(yard_line - yards_gained, 0)
  new_down <- ifelse(yards_gained >= yards_to_go, 1, down + 1)
  
  # Determine if score happened
  if (new_yard_line == 0) {
    return(list(points = 7, result = "TD"))
  } else if (new_down > 4) {
    return(list(points = 0, result = "Turnover on Downs"))
  } else {
    return(list(points = 0, result = "Continue", 
                new_yard_line = new_yard_line,
                new_down = new_down,
                new_yards_to_go = ifelse(yards_gained >= yards_to_go, 10, yards_to_go - yards_gained)))
  }
}

# Sample yards function (placeholder)
sample_yards <- function(yard_line, down, yards_to_go, play_type) {
  # Placeholder: sample from a normal or gamma distribution based on play_type
  if (play_type == "run") {
    return(max(round(rnorm(1, mean = 3, sd = 2)), 0))
  } else {
    return(max(round(rnorm(1, mean = 6, sd = 5)), 0))
  }
}

# Play call function (placeholder)
get_play_call <- function(down, yard_line, run_prob) {
  if (runif(1) < run_prob) return("run") else return("pass")
}

# Run simulation for one redzone drive
simulate_drive <- function(run_prob = 0.5) {
  yard_line <- sample(1:REDZONE_START, 1)
  down <- 1
  yards_to_go <- 10
  
  total_yards <- 0
  drive_log <- c()
  
  while (TRUE) {
    play_type <- get_play_call(down, yard_line, run_prob)
    result <- evaluate_score(yard_line, play_type, down, yards_to_go)
    
    drive_log <- c(drive_log, result$result)
    
    if (result$result != "Continue") {
      return(list(points = result$points, outcome = result$result))
    } else {
      yard_line <- result$new_yard_line
      down <- result$new_down
      yards_to_go <- result$new_yards_to_go
    }
  }
}

# Run full simulation set
set.seed(4800)
sim_results <- data.frame()

for (strategy in c(0.2, 0.5, 0.8)) {  # Run-heavy, balanced, pass-heavy
  sim_outcomes <- replicate(NUM_SIMULATIONS, simulate_drive(run_prob = strategy), simplify = FALSE)
  sim_df <- do.call(rbind, lapply(sim_outcomes, function(x) data.frame(points = x$points, outcome = x$outcome)))
  sim_df$strategy <- paste0("RunProb_", strategy)
  sim_results <- rbind(sim_results, sim_df)
}

# Summarize results
summary_df <- sim_results %>%
  group_by(strategy) %>%
  summarize(avg_points = mean(points),
            td_rate = mean(outcome == "TD"),
            turnover_rate = mean(outcome == "Turnover on Downs"))

print(summary_df)

# Plotting

ggplot(summary_df, aes(x = strategy, y = avg_points, fill = strategy)) +
  geom_bar(stat = "identity") +
  labs(title = "Expected Points by Redzone Play Call Tendency",
       x = "Run Probability Strategy",
       y = "Expected Points") +
  theme_minimal()

