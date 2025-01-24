# Top-Level Function
def simulate_game_state(down, yards_to_go, field_position, num_epochs=100):
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
    results = []
    for _ in range(num_epochs):
        score = simulate_epoch(down, yards_to_go, field_position)
        results.append(score)
    return sum(results) / len(results) if results else 0

# Epoch Function
def simulate_epoch(down, yards_to_go, field_position):
    """
    Simulates a single epoch of the game from the given state.

    Parameters:
        down (int): Current down.
        yards_to_go (int): Yards to go for a first down or touchdown.
        field_position (int): Current field position.

    Returns:
        int: Score for the epoch.
    """
    team_possession = 1  # 1 for "our team", -1 for opponent
    max_drives = 10
    for _ in range(max_drives):
        new_state = simulate_drive(down, yards_to_go, field_position)
        if is_score(new_state):
            return team_possession * get_score(new_state)
        team_possession *= -1  # Switch possession
    return 0  # Return 0 if no score occurs after max_drives

# Drive Function
def simulate_drive(down, yards_to_go, field_position):
    """
    Simulates a single drive and returns the new state.

    Parameters:
        down (int): Current down.
        yards_to_go (int): Yards to go for a first down or touchdown.
        field_position (int): Current field position.

    Returns:
        tuple: (down, yards_to_go, field_position)
    """
    # For simplicity, randomly adjust the field position
    import random
    new_field_position = field_position + random.randint(-10, 40)
    new_field_position = max(0, min(120, new_field_position))  # Clamp between 0 and 120
    return (1, 10, new_field_position)  # Reset to 1st and 10

# Helper Functions
def is_score(state):
    """Determines if a score has occurred based on the field position."""
    _, _, field_position = state
    return field_position > 100

def get_score(state):
    """Returns the score based on the field position."""
    _, _, field_position = state
    if 100 < field_position <= 110:
        return 7  # Touchdown
    elif 110 < field_position <= 120:
        return 3  # Field goal
    return 0

# Main Function (optional for testing)
if __name__ == "__main__":
    # Example usage
    expected_points = simulate_game_state(down=1, yards_to_go=10, field_position=50, num_epochs=1000)
    print(f"Expected points: {expected_points}")

