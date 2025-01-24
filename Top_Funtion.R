# Top-Level Function
simulate_game_state <- function(down, yards_to_go, field_position, num_epochs = 100) {
  """
  Simulates the game state and computes the expected points from a given state.

  Parameters:
    down (int): Current down (1-4).
    yards_to_go (int): Yards to go for a first down or touchdown.
    field_position (int): Current field position (0-100).
    num_epochs (int): Number of epochs to simulate.

  Returns:
    float: Expected points from the given state.
  """
  results <- numeric(num_epochs)
  for (i in 1:num_epochs) {
    results[i] <- simulate_epoch(down, yards_to_go, field_position)
  }
  return(mean(results))
}

# Epoch Function
simulate_epoch <- function(down, yards_to_go, field_position) {
  """
  Simulates a single epoch of the game from the given state.

  Parameters:
    down (int): Current down.
    yards_to_go (int): Yards to go for a first down or touchdown.
    field_position (int): Current field position.

  Returns:
    int: Score for the epoch.
  """
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
  """
  Simulates a single drive and returns the new state.

  Parameters:
    down (int): Current down.
    yards_to_go (int): Yards to go for a first down or touchdown.
    field_position (int): Current field position.

  Returns:
    list: List with new down, yards_to_go, and field_position.
  """
  new_field_position <- field_position + sample(-10:40, 1)
  new_field_position <- max(0, min(120, new_field_position))  # Clamp between 0 and 120
  return(list(down = 1, yards_to_go = 10, field_position = new_field_position))
}

# Helper Functions
is_score <- function(state) {
  """Determines if a score has occurred based on the field position."""
  return(state$field_position > 100)
}

get_score <- function(state) {
  """Returns the score based on the field position."""
  if (state$field_position > 100 && state$field_position <= 110) {
    return(7)  # Touchdown
  } else if (state$field_position > 110 && state$field_position <= 120) {
    return(3)  # Field goal
  }
  return(0)
}

# Main Function (optional for testing)
if (interactive()) {
  # Example usage
  expected_points <- simulate_game_state(down = 1, yards_to_go = 10, field_position = 50, num_epochs = 1000)
  print(paste("Expected points:", expected_points))
}
