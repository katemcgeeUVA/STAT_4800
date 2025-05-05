
library(dplyr)

pbp <- readRDS("/Users/francesbaldridge/Desktop/STAT 4800/pbp2014-2024.rds")

# Filter red zone plays and label play type
data_redzone <- pbp %>% 
  filter(!is.na(play_type), play_type %in% c("run", "pass"), yardline_100 <= 20) %>%
  mutate(yards_gained = ifelse(yards_gained < -10, -10, ifelse(yards_gained > 30, 30, yards_gained)))

# Fit normal distributions to red zone yards gained by play type
run_params <- data_redzone %>% filter(play_type == "run") %>% summarise(mu = mean(yards_gained), sd = sd(yards_gained))
pass_params <- data_redzone %>% filter(play_type == "pass") %>% summarise(mu = mean(yards_gained), sd = sd(yards_gained))

simulate_play <- function(play_call, fp) {
  # Simulate yards gained based on play type
  yards <- if (play_call == "run") {
    round(rnorm(1, run_params$mu, run_params$sd))
  } else {
    round(rnorm(1, pass_params$mu, pass_params$sd))
  }
  
  # Simulate turnover
  turnover_prob <- if (play_call == "pass") 0.07 else 0.02
  turnover <- runif(1) < turnover_prob
  
  list(yards = yards, turnover = turnover)
}

simulate_drive <- function(strategy, n_sim = 1000) {
  points <- numeric(n_sim)
  turnovers <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    down <- 1
    ytg <- 10
    fp <- sample(80:99, 1)  # Red zone starting FP
    drive_ended <- FALSE
    
    while (!drive_ended) {
      # Choose play type based on strategy
      p_run <- switch(strategy,
                      "primarily_run" = 0.7,
                      "primarily_pass" = 0.3,
                      "even_split" = 0.5)
      play_call <- sample(c("run", "pass"), 1, prob = c(p_run, 1 - p_run))
      
      result <- simulate_play(play_call, fp)
      
      if (result$turnover) {
        turnovers[i] <- 1
        drive_ended <- TRUE
        break
      }
      
      fp <- fp + result$yards
      ytg <- max(ytg - result$yards, 1)
      
      if (fp >= 100) {
        # Touchdown
        points[i] <- 7
        drive_ended <- TRUE
        break
      }
      
      if (ytg <= 0) {
        down <- 1
        ytg <- 10
      } else {
        down <- down + 1
      }
      
      if (down > 4) {
        # Try a field goal (50% success from red zone)
        fg_success <- runif(1) < 0.5
        points[i] <- ifelse(fg_success, 3, 0)
        drive_ended <- TRUE
      }
    }
  }
  
  list(avg_points = mean(points), turnover_rate = mean(turnovers))
}

# Run simulations
set.seed(4800)
primarily_run <- simulate_drive("primarily_run")
primarily_pass <- simulate_drive("primarily_pass")
even_split <- simulate_drive("even_split")

# Create result table
result_table <- data.frame(
  Strategy = c("Primarily Run", "Primarily Pass", "Even Split"),
  Avg_Expected_Points = c(primarily_run$avg_points, primarily_pass$avg_points, even_split$avg_points),
  Turnover_Rate = c(primarily_run$turnover_rate, primarily_pass$turnover_rate, even_split$turnover_rate)
)

result_table
