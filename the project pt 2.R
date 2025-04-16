# Redzone Play Call Tendencies Simulation (Updated with Full Project Features)

library(dplyr)
library(ggplot2)
library(tidyr)

REDZONE_START <- 20
NUM_SIMULATIONS <- 10000

# Parametric Yard Gain Models - Adjusted based on down and yard_line zone
get_yard_distribution <- function(yard_line, down, yards_to_go, play_type) {
  zone <- ifelse(yard_line <= 10, "low", "high")
  if (play_type == "run") {
    if (zone == "low" & yards_to_go <= 3) return(c(3, 1.5))
    else return(c(3, 2))
  } else {
    if (zone == "low" & down == 3) return(c(4, 3))
    else return(c(6, 5))
  }
}

sample_yards <- function(yard_line, down, yards_to_go, play_type) {
  params <- get_yard_distribution(yard_line, down, yards_to_go, play_type)
  return(max(round(rnorm(1, mean = params[1], sd = params[2])), 0))
}

# Forced turnover logic
check_turnover <- function(play_type) {
  if (play_type == "pass" && runif(1) < 0.03) return("Interception")
  if (play_type == "run" && runif(1) < 0.01) return("Fumble")
  return(NULL)
}

# Play call function with fixed probability
get_play_call <- function(down, yard_line, run_prob) {
  if (runif(1) < run_prob) return("run") else return("pass")
}

# Field goal logic
attempt_field_goal <- function(yard_line) {
  fg_range <- 35  # 52-yard attempt from 35-yard line (endzone + 17-yard snap distance)
  make_prob <- ifelse(yard_line <= fg_range, 0.95, 0.75)
  return(ifelse(runif(1) < make_prob, 3, 0))
}

# Evaluate play outcome
evaluate_score <- function(yard_line, play_type, down, yards_to_go) {
  turnover <- check_turnover(play_type)
  if (!is.null(turnover)) return(list(points = 0, result = turnover))
  
  yards_gained <- sample_yards(yard_line, down, yards_to_go, play_type)
  new_yard_line <- max(yard_line - yards_gained, 0)
  new_down <- ifelse(yards_gained >= yards_to_go, 1, down + 1)
  
  if (new_yard_line == 0) {
    extra_pts <- ifelse(runif(1) < 0.94, 1, 2)  # XP vs 2PT choice implicit
    return(list(points = 6 + extra_pts, result = "TD"))
  } else if (new_down > 4) {
    # FG attempt if within range
    fg_pts <- attempt_field_goal(yard_line)
    result_type <- ifelse(fg_pts > 0, "FG Made", "FG Missed")
    return(list(points = fg_pts, result = result_type))
  } else {
    return(list(points = 0, result = "Continue",
                new_yard_line = new_yard_line,
                new_down = new_down,
                new_yards_to_go = ifelse(yards_gained >= yards_to_go, 10, yards_to_go - yards_gained)))
  }
}

# Simulate one drive
simulate_drive <- function(run_prob = 0.5) {
  yard_line <- sample(1:REDZONE_START, 1)
  down <- 1
  yards_to_go <- 10
  
  while (TRUE) {
    play_type <- get_play_call(down, yard_line, run_prob)
    result <- evaluate_score(yard_line, play_type, down, yards_to_go)
    
    if (result$result != "Continue") {
      return(list(points = result$points, outcome = result$result))
    } else {
      yard_line <- result$new_yard_line
      down <- result$new_down
      yards_to_go <- result$new_yards_to_go
    }
  }
}

# Full simulation loop
set.seed(4800)
sim_results <- data.frame()

for (strategy in c(0.2, 0.5, 0.8)) {
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
            fg_rate = mean(outcome == "FG Made"),
            turnover_rate = mean(outcome %in% c("FG Missed", "Turnover on Downs", "Interception", "Fumble")))

print(summary_df)

# Plot expected points

ggplot(summary_df, aes(x = strategy, y = avg_points, fill = strategy)) +
  geom_bar(stat = "identity") +
  labs(title = "Expected Points by Redzone Play Call Tendency",
       x = "Run Probability Strategy",
       y = "Expected Points") +
  theme_minimal()

