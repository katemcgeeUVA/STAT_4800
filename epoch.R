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
