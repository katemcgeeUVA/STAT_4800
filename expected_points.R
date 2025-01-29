expected_points <- function(down, ytg, fp, num_epochs = 100) {
  results <- numeric(num_epochs)
  for (i in 1:num_epochs) {
    results[i] <- epoch(down, ytg, fp)
  }
  mean(results)
}
