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
    new_state <- simulate_drive(down, ytg, fp)
    if (is_score(new_state)) {
      return(team_possession * get_score(new_state))
    }
    team_possession <- team_possession * -1  # Switch possession
  }
  return(0)
}

drive <- function(down, ytg, fp) {
  new_fp <- fp + sample(0:40, 1)
  new_fp <- max(0, min(120, new_fp))  # Clamp between 0 and 120
  return(list(down = 1, yards_to_go = 10, field_position = new_field_position))
}

is_score <- function(state) {
  return(state$field_position > 100)
}

get_score <- function(state) {
  if (state$field_position > 100 && state$field_position <= 110) {
    return(7)  # Touchdown
  } else if (state$field_position > 110 && state$field_position <= 120) {
    return(3)  # Field goal
  }
  return(0)
}


