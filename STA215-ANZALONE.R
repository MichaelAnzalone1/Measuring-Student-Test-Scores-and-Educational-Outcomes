# STA215-ANZALONE.R

# 1. Set working directory

setwd("C:/Users/Michael/Desktop/sta215")
# install.packages("psych")  # For describe()
library(readr)
library(ggplot2)
library(dplyr)

# 3. Load the complete_data
data <- read.csv("raw_data.csv")

## Project:  STA 215, Spring 2024, Final Project
# Located:   Posit Cloud
# File Name: Reading Test Scores
# Date:      2024_12_4
# Who:       Michael Anzalone


# Complete case analysis
complete_data <- na.omit(data)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
# Load necessary library


         
table(complete_data$absent_problem)
table(complete_data$grades_offered)


summary(complete_data$student_teacher_ratio)
mean(complete_data$student_teacher_ratio)
sd(complete_data$student_teacher_ratio)


##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
ggplot(complete_data, aes(x = absent_problem, y = reading_score)) +
  geom_boxplot() +
  labs(title = "Box Plot of absent problem and student teacher ratio",
       x = "absent problem",
       y = "student teacher ratio") +
  theme_minimal()

anova <- aov(reading_score ~ absent_problem, data = data)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
plot(complete_data$student_teacher_ratio, complete_data$reading_score)

# add x line and y line for means
meany <- mean(complete_data$student_teacher_ratio)
meanx <- mean(complete_data$reading_score)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")

linear_relationship <- lm(complete_data$reading_score ~ student_teacher_ratio, data = complete_data)
summary(linear_relationship)


# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(complete_data$student_teacher_ratio, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################


# Generate a contingency table for 'absent_problem' and 'grades_offered'
table2 <- table(complete_data$absent_problem, complete_data$grades_offered)

# Print the contingency table
print(table2)

# Perform a chi-squared test
chi_sq_test <- chisq.test(table2)

# Print the results of the chi-squared test
print(chi_sq_test)
# Load necessary library
library(dplyr)

# Load the dataset (replace 'raw_data.csv' with your actual file path)
raw_data <- read.csv("raw_data.csv")

# Summary of the "reading_score" variable
summary(raw_data$reading_score)

# Mean of "reading_score"
mean_reading_score <- mean(raw_data$reading_score, na.rm = TRUE)
print(paste("Mean:", mean_reading_score))

# Standard deviation of "reading_score"
sd_reading_score <- sd(raw_data$reading_score, na.rm = TRUE)
print(paste("Standard Deviation:", sd_reading_score))

