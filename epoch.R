source("drive.R")
source("utils.R")

run_epoch <- function(down, ytg, fp) {
  team_status <- -1
  max_drives <- 10 
  cumulative_drives <- 0 
  no_score <- TRUE 
  
  print("Remember, the state of the first drive should be the input state.")
  
  while(no_score & (cumulative_drives < max_drives)) {
    team_status <- team_status * -1
    cumulative_drives <- cumulative_drives + 1

    print(paste0("starting down: ", down, ", ytg: ", ytg, ", fp: ", fp, 
                 ", drive number: ", cumulative_drives,
                 ", team status flag: ", team_status,"."))
    
    tmp_state <- run_drive(down, ytg, fp)
    
    down <- tmp_state$down
    ytg <- tmp_state$ytg
    fp <- tmp_state$fp
    
    no_score <- (fp <= 100)
  }
  
  score <- team_status * compute_score(fp)

  print(paste0("final fp: ", fp, 
               ", scoring drive number: ", cumulative_drives,
               ", team status flag: ", team_status, ", score: ", score, "."))  
  score
}

############################# Test that run_epoch works ########################

run_epoch(2, 7, 34) 
