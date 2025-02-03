source("epoch.R")

get_EP <- function(down, ytg, fp) {
 
  n <- 1000 
  points <- rep(NA, n)
  
  for(i in 1:n) {
    points[i] <- run_epoch(down, ytg, fp)
  } 
  
  mean(points)
}

############################# Test that get_EP works ###########################

get_EP(2, 5, 34)
get_EP(1, 10, 61)
