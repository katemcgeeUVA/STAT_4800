# Top-Level Function
simulate_game_state <- function(down, yards_to_go, field_position, num_epochs = 100) {
  results <- numeric(num_epochs)
  for (i in 1:num_epochs) {
    results[i] <- simulate_epoch(down, yards_to_go, field_position)
  }
  return(mean(results))
}

# Epoch Function
simulate_epoch <- function(down, yards_to_go, field_position) {
  team_possession <- 1  # 1 for "our team", -1 for opponent
  max_drives <- 10
  for (i in 1:max_drives) {
    new_state <- simulate_drive(down, yards_to_go, field_position)
    if (is_score(new_state)) {
      return(team_possession * get_score(new_state))
    }
    team_possession <- team_possession * -1  # Switch possession
  }
  return(0)  # Return 0 if no score occurs after max_drives
}

# Drive Function
simulate_drive <- function(down, yards_to_go, field_position) {
  new_field_position <- field_position + sample(-10:40, 1)
  new_field_position <- max(0, min(120, new_field_position))  # Clamp between 0 and 120
  return(list(down = 1, yards_to_go = 10, field_position = new_field_position))
}

# Helper Functions
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


