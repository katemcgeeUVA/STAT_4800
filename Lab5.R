#Step 1 Response:
pbpdata = readRDS("~/Desktop/Stat/pbp2014-2024.rds")

pbpdata$field_goal_success =  as.integer(pbpdata$play_type == "field_goal" & pbpdata$play_result == "made")

# Fit logistic regression model
fg_model = glm(field_goal_success ~ fp, data = pbpdata, family = binomial)

run_field_goal <- function(fp) {
  fg_prob = predict(fg_model, newdata = data.frame(fp = fp), type = "response") 
  runif(1) < fg_prob  # Compare to a random value to determine success
}

#Step 2 Response:
library(nnet)

fourth_down_plays <- pbpdata[pbpdata$down == 4, ]

fourth_down_plays$decision <- factor(ifelse(
  fourth_down_plays$play_type == "field_goal", "field_goal",
  ifelse(fourth_down_plays$play_type == "punt", "punt", "go_for_it")
))

#multinomial logistic regression model
fourth_down_model <- multinom(decision ~ fp + ytg, data = fourth_down_plays)

down_four <- function(down, ytg, fp) {
    decision_probs <- predict(fourth_down_model, newdata = data.frame(fp = fp, ytg = ytg), type = "probs")
    play_type <- sample(c("go_for_it", "punt", "field_goal"), 1, prob = decision_probs)
 
  if(play_type == "field_goal") {
    field_goal_success <- run_field_goal(fp)
    if(field_goal_success) {
      list(down = 1, ytg = 10, fp = 115, exit_drive = 1)  # Success
    } else {
      list(down = down, ytg = ytg, fp = fp, exit_drive = 1)  # Fail
    }
  } else if(play_type == "punt") {
    list(down = down, ytg = ytg, fp = fp, exit_drive = 1) 
  } else {
    go_for_it_success <- sample(c(TRUE, FALSE), 1)
    if(go_for_it_success) {
      fp_new <- fp + sample(1:10, 1)  #how many yards gained if successful
      list(down = 1, ytg = 10, fp = fp_new, exit_drive = 0)
    } else {
      list(down = down, ytg = ytg, fp = fp, exit_drive = 1)  # turnover
    }
  }
}

