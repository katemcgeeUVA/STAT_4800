expected_points <- function(down, ytg, fp, num_epochs = 100) {
  results <- numeric(num_epochs)
  for (i in 1:num_epochs) {
    results[i] <- epoch(down, ytg, fp)
  }
  return(mean(results))
}

epoch <- function(down, ytg, fp) {
  team_possession <- 1
  max_drives <- 10
  
  for (i in 1:max_drives) {
    new_state <- drive(down, ytg, fp)
    points_scored <- score(new_state)
    if (points_scored > 0) {
      return(team_possession * points_scored)
    }
    team_possession <- team_possession * -1  # Switch possession
  } 
  return(0)
}

drive <- function(down, ytg, fp) {
  new_fp <- fp + sample(-10:40, 1)
  new_fp <- max(0, min(120, new_fp))
  state <- list(down = 1, ytg = 10, fp = new_fp)
  return(state)
}

score <- function(state) {
  if (100 < state$fp & state$fp <= 110) {
    return(7)
  } else if (110 < state$fp & state$fp <= 120) {
    return(3)
  } else {
    return(0)
  }
}
