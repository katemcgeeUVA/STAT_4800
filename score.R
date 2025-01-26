score <- function(state) {
  if (100 < state$fp & state$fp <= 110) {
    return(7)
  } else if (110 < state$fp & state$fp <= 120) {
    return(3)
  } else {
    return(0)
  }
}
