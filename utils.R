compute_score <- function(fp){
  
  if(fp <= 100) {
    0
  } else if (fp <= 110) {
    7
  } else {
    3
  }
}
