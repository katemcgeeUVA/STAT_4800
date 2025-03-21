library(mixtools) 

simulate_play <- function(position = NULL, red_zone = FALSE) {
  play_type <- sample(c("run", "pass"), 1, prob = c(0.5, 0.5))
  fumble <- sample(c(TRUE, FALSE), 1, prob = c(0.05, 0.95))
  interception <- ifelse(play_type == "pass", sample(c(TRUE, FALSE), 1, prob = c(0.03, 0.97)), FALSE)
  incompletion <- ifelse(play_type == "pass", sample(c(TRUE, FALSE), 1, prob = c(0.35, 0.65)), FALSE)
  
  if (incompletion) return(0)
  
  yards <- sample_yards(play_type, red_zone)
  
  if (fumble || interception) {
    yards <- -yards  # Negative yards represent turnover return yards
  }
  
  return(yards)
}

sample_yards <- function(play_type, red_zone) {
  if (play_type == "run") {
    mu <- c(3, 10, 20)  
    sigma <- c(2, 5, 8)  
    lambda <- c(0.6, 0.3, 0.1)  
  } else {
    mu <- c(5, 15, 30) 
    sigma <- c(3, 7, 12)
    lambda <- c(0.5, 0.4, 0.1)
  }
  
  if (red_zone) {
    mu <- pmin(mu, 20) 
  }
  
  component <- sample(1:length(lambda), 1, prob = lambda)
  return(rnorm(1, mean = mu[component], sd = sigma[component]))
}

set.seed(42)
simulated_plays <- replicate(1000, simulate_play(red_zone = FALSE))
summary(simulated_plays)
