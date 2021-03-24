rm(list=ls()) #removes all variable stored previously
library(Hmisc) #import

data <- read.csv("~/Covid 19/COVID19_line_list_data.csv")

describe(data) #Hmisc command

data$death_dummy <- as.integer(data$death !=0)
#death rate
sum(data$death_dummy) / nrow(data)

#Age
#Claim: People who die are older
dead = subset(data, death_dummy==1)
alive = subset(data, death_dummy==0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
#Is it Statistically Significant?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
#if p-value is < 0.05 , we reject null Hypothesis
#here p-values is ~ 0 (ie. 2.2e-16), so we reject the null hypothesis 
#conclude that this statistically significant

#Gender
#Claim: Gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%
mean(women$death_dummy, na.rm = TRUE) #3.7%
#Is it statistically Significant ?
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.09)
#99% Confidence: men have from 0.8% to 8.8% higher chance of dying
#p-value = 0.002 < 0.05 , we reject null Hypothesis
#This is Statistically significant

