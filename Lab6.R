
library(dplyr)
library(ggplot2)
library(readr)


nhl_data <- read.csv("~/Desktop/Sports/nhl_pbp20162017.csv")

#Shot Rate Model (Time + Point Differential)
nhl_data <- nhl_data %>%
  mutate(PointDiff = Home_Score - Away_Score)
nhl_data <- nhl_data %>%
  mutate(PeriodTimeBlock = cut(Seconds_Elapsed, breaks = c(0, 400, 800, 1200), labels = c("Early", "Mid", "Late"), include.lowest = TRUE),
         PointDiff = as.numeric(PointDiff))  

# Poisson Regression Model
shot_rate_model1 <- glm(Event ~ PeriodTimeBlock + PointDiff, data = nhl_data, family = poisson)

summary(shot_rate_model1)


# Shot Rate Model 
nhl_data <- nhl_data %>%
  mutate(Region = case_when(
    xC < -25 ~ "Left",
    xC > 25 ~ "Right",
    yC < -20 ~ "Low",
    yC > 20 ~ "High",
    TRUE ~ "Center"
  ))

# Poisson Regression 
shot_rate_model2 <- glm(Shot ~ Region, data = nhl_data, family = poisson)

summary(shot_rate_model2)


#Shot Success Rate Model
nhl_data <- nhl_data %>%
  mutate(Success = ifelse(ShotResult == "ON GOAL", 1, 0))

# Logistic Regression
shot_success_model <- glm(Success ~ xC + yC, data = nhl_data, family = binomial)

summary(shot_success_model)
