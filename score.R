score <- function(state) {
  if (100 < state$fp & state$fp <= 110) {
    7
  } else if (110 < state$fp & state$fp <= 120) {
    3
  } else {
    0
  }
}
