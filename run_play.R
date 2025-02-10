run_play <- function(down, ytg, fp){
  # Randomly select the down (1st, 2nd, 3rd, or 4th)
  down <- sample(1:4, 1)
  
  # Call the appropriate down-specific function
  if(down == 1) {
    down_one(down, ytg, fp)
  } else if(down == 2) {
    down_two(down, ytg, fp)
  } else if(down == 3) {
    down_three(down, ytg, fp)
  } else {
    down_four(down, ytg, fp)
  }
}

down_one <- function(down, ytp, fp){
  # simulate the first down
  ytg_new <- sample(0:5, 1)  # yards gained on first down
  ytg <- max(ytg - ytg_new, 0)  # update YTG
  
  # continue or finish the drive
  exit_drive <- ifelse(ytg == 0, 0, 1)  # 1st down ends if YTG = 0
  
  # update FP and return new state
  fp_new <- fp + ytg_new
  list(down=down, ytg=ytg, fp=fp_new, exit_drive=exit_drive)
}

down_two <- function(down, ytg, fp){
  # simulate the second down
  ytg_new <- sample(0:6, 1)  # yards gained on second down
  ytg <- max(ytg - ytg_new, 0)  # update YTG
  
  # continue or finish the drive
  exit_drive <- ifelse(ytg == 0, 0, 1)
  
  # update FP and return new state
  fp_new <- fp + ytp_new
  list(down=down, ytg=ytg, fp=fp_new, exit_drive=exit_drive)
}

down_three <- function(down, ytg, fp){
  # simulate the third down
  ytg_new <- sample(0:7, 1)  # yards gained on third down
  ytg <- max(ytg - ytg_new, 0)  # update YTG
  
  # continue or finish the drive
  exit_drive <- ifelse(ytg == 0, 0, 1)
  
  # update FP and return new state
  fp_new <- fp + ytg_new
  list(down=down, ytg=ytg, fp=fp_new, exit_drive=exit_drive)
}

down_four <- function(down, ytg, fp){
  # simulate the fourth down with different play types
  play_type <- sample(c("go_for_it", "punt", "field_goal"), 1)
  
  if(play_type == "field_goal") {
    # simulate a field goal attempt
    field_goal_success <- run_field_goal(fp)
    if(field_goal_success) {
      # if successful, return state with FP set to 115 (score)
      list(down=1, ytg=10, fp=115, exit_drive=1)
    } else {
      # if failed, switch possession
      list(down=down, ytg=ytg, fp=fp, exit_drive=1)
    }
    
  } else if(play_type == "punt") {
    # simulate a punt (change possession)
    list(down=down, ytg=ytg, fp=fp, exit_drive=1)
  } else if(play_type == "go_for_it") {
    # simulate "go for it" on 4th down
    go_for_it_success <- sample(c(TRUE, FALSE), 1)
    if(go_for_it_success) {
      # if successful, continue the drive with new FP
      fp_new <- fp + sample(1:10, 1)  # random yards gained
      list(down=1, ytg=10, fp=fp_new, exit_drive=0)
    } else {
      # if not successful, switch possession
      list(down=down, ytg=ytg, fp=fp, exit_drive=1)
    }
  }
}

run_field_goal <- function(fp) {
  # simulate field goal success/failure based on FP
  field_goal_probability <- ifelse(fp > 80, 0.5, 0.8)  # higher success rate closer to end zone
  runif(1) < field_goal_probability
}
