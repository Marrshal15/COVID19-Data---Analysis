# https://www.youtube.com/watch?v=D_CNmYkGRUc

rm(lists=ls()) #remove previously stored variables

# Read data & view
library(readxl)
# covData <- read_excel("CSUCI/Spring 2024/DataAnalyticsNotes/RProjects/Covid_R/COVID19_line_list_data.xlsx")
covData <- read.csv("~/CSUCI/Spring 2024/DataAnalyticsNotes/RProjects/Covid_R/COVID19_line_list_data.csv")
View(covData)

# to describe data
# Hmisc has some miscellaneous R functions
library(Hmisc)
str(covData)
describe(covData)

# some columns has inconsistent data
#for example col. in death, some values contain dates instead of 1s & 0s
# so to make it consistent
# we'll make only 0 & 1s for death and cured
covData$deathDummy <- as.integer(covData$death != 0)
# to check unique values
unique(covData$deathDummy)

# finding death rate
sum(covData$deathDummy) / nrow(covData)

# Proving a claim using data
# claim: people who died during corona were older in age
dead = subset(covData, deathDummy == 1) # subset of dead people
alive = subset(covData, deathDummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE) # na.rm is used to remove "N/A" values

# is this statistically significant? How far is this true.
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)

# claim: men have a higher death rate than women
men = subset(covData, gender == "male") # subset of men & women
women = subset(covData, gender == "female")
mean(men$deathDummy, na.rm = TRUE)
mean(women$deathDummy, na.rm = TRUE)
t.test(men$deathDummy, women$deathDummy, alternative = "two.sided", conf.level = 0.99)

china = subset(covData, country == "China")
japan = subset(covData, country == "Japan")
mean(china$deathDummy, na.rm = TRUE)
mean(japan$deathDummy, na.rm = TRUE)
t.test(china$deathDummy, japan$deathDummy, alternative = "two.sided", conf.level = 0.99)

covData$gender = as.factor(covData$gender)
anova_result = aov(age ~ gender, data = covData)
summary(anova_result)
