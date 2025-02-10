source("run_play.R")

pbpdata <- readRDS("~/Desktop/Stat/pbp2014-2024.rds")

run_drive <- function(down, ytg, fp){
  
  # get new state
  new_state <- run_play(down, ytg, fp)
  
  # Check if we should return the state or run a new play
  if(new_state$exit_drive==0){
    
    # if we should stay with the current drive, simply call it again with the
    # new state
    run_drive(new_state$down, new_state$ytg, new_state$fp)
    
  } else {
    
    # otherwise, return the current state to the run_epoch function
    list(down=new_state$down, YTG=new_state$ytg, fp=new_state$fp)
  }
}
