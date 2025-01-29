drive <- function(down, ytg, fp) {
  new_fp <- fp + sample(-10:40, 1)
  new_fp <- max(0, min(120, new_fp))
  state <- list(down = 1, ytg = 10, fp = new_fp)
  state
}
