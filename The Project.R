pbp <- readRDS("/Users/francesbaldridge/Desktop/STAT 4800/pbp2014-2024.rds")

data_redzone <- pbp %>% 
  filter(!is.na(play_type), play_type %in% c("run", "pass"), yardline_100 <= 20) %>%
  mutate(yards_gained = ifelse(yards_gained < -10, -10, ifelse(yards_gained > 30, 30, yards_gained)))

run <- data_redzone %>% filter(play_type == "run") %>% summarize(mu = mean(yards_gained), sd = sd(yards_gained))
pass <- data_redzone %>% filter(play_type == "pass") %>% summarize(mu = mean(yards_gained), sd = sd(yards_gained))

simulate_play <- function(play_call, fp) {
  yards <- if (play_call == "run") {
    round(rnorm(1, run$mu, run$sd))
  } else {
    round(rnorm(1, pass$mu, pass$sd))
  }
  
  turnover_prob <- if (play_call == "pass") 0.07 else 0.02
  turnover <- runif(1) < turnover_prob
  
  list(yards = yards, turnover = turnover)
}

simulate_drive <- function(strategy, n = 1000) {
  points <- numeric(n)
  turnovers <- numeric(n)
  
  for (i in 1:n) {
    down <- 1
    ytg <- 10
    fp <- sample(80:99, 1) 
    drive_ended <- FALSE
    
    while (!drive_ended) {
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
        fg_success <- runif(1) < 0.5
        points[i] <- ifelse(fg_success, 3, 0)
        drive_ended <- TRUE
      }
    }
  }
  
  list(avg_points = mean(points), turnover_rate = mean(turnovers))
}

# SIMULATION

set.seed(4800)
primarily_run <- simulate_drive("primarily_run")
primarily_pass <- simulate_drive("primarily_pass")
even_split <- simulate_drive("even_split")

results <- data.frame(
  Strategy = c("Primarily Run", "Primarily Pass", "Even Split"),
  Avg_Expected_Points = c(primarily_run$avg_points, primarily_pass$avg_points, even_split$avg_points),
  Turnover_Rate = c(primarily_run$turnover_rate, primarily_pass$turnover_rate, even_split$turnover_rate)
)

results
