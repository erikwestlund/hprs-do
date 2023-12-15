library(tidyverse)

# Start by generating a sample of people by race/ethnicity, roughly
# proportionate to the US population. We will use the characteristics of the
# US to then generate realistic covariates for each person, starting from
# Race/Eth. As a side note: we could start with basically any property
# and work from there with similar results were the data good.
n <- 10000

d <- tibble(
  race_eth = sample(c(
    rep(1, 13), # African American
    rep(2, 19), # Hispanic
    rep(3, 1),  # Middle Eastern 
    rep(4, 58), # Caucasian-Other
    rep(5, 2),  # Caucsaian-Jewish
    rep(6, 6),  # Asian
    rep(7, 1)  # Missing
    # rep(9, 10)   # Native American/Pacific Islander
  ), n, replace = TRUE)
) 


# For each person, generate a set of realistic values.
genPatientCovariates <- function(race_eth) {
  
  race_eth <- sample(d$race_eth, 1)
  gender <- sample(c(1,0), 1, prob = c(0.5, 0.5))
  age_cat <- sample(c(1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6), 1)
  
  income <- case_when(
    race_eth == 1 ~ rnorm(1, 52000, 10000),
    race_eth == 2 ~ rnorm(1, 62500, 12000),
    race_eth == 3 ~ rnorm(1, 60000, 15000), # Guess
    race_eth == 4 ~ rnorm(1, 84000, 20000),
    race_eth == 5 ~ rnorm(1, 125000, 40000), # Rough from quick google
    race_eth == 6 ~ rnorm(1, 109000, 30000),
    race_eth == 7 ~ rnorm(1, 20000, 10000),
    # race_eth == 9 ~ NA,
  )
  
  income <- round(ifelse(income < 0, 0, income), -3)
  income_cat <- case_when(
    income <= 25000 ~ 1,
    income <= 50000 ~ 2,
    income <= 75000 ~ 3,
    income <= 100000 ~ 4,
    income <= 125000 ~ 5,
    income <= 150000 ~ 6,
    income > 150000 ~ 7,
  )  

  education <- case_when(
    race_eth == 1 ~ sample(c(rep(1,10), rep(2,60), rep(3,15), rep(4,10)), 1, replace = TRUE),
    race_eth == 2 ~ sample(c(rep(1,20), rep(2,65), rep(3,10), rep(4,5)), 1, replace = TRUE),
    race_eth == 3 ~ sample(c(rep(1,10), rep(2,60), rep(3,15), rep(4,10)), 1, replace = TRUE),
    race_eth == 4 ~ sample(c(rep(1,5), rep(2,50), rep(3,30), rep(4,15)), 1, replace = TRUE),
    race_eth == 5 ~ sample(c(rep(1,5), rep(2,25), rep(3,50), rep(4,20)), 1, replace = TRUE),
    race_eth == 6 ~ sample(c(rep(1,10), rep(2,30), rep(3,40), rep(4,20)), 1, replace = TRUE),
    race_eth == 7 ~ sample(c(rep(1,10), rep(2,65), rep(3,20), rep(4,5)), 1, replace = TRUE),
    # race_eth == 9 ~ NA, # Less than HS
  )

  # Generate a random number of visits for each person. We want to make this a function of 
  # the person's age, income, and education. But we also want it realistically distributed,
  # using a Poisson distribution. We will use the values of age, gender, race, and income
  # to generate a weight, and then use that mean to generate a random number using rpois.
  age_weight <- case_when(
    age_cat == 1 ~ 0.5, # Older
    age_cat == 2 ~ 0.75,
    age_cat == 3 ~ 0.9,
    age_cat == 4 ~ 1,
    age_cat == 5 ~ 1.1,
    age_cat == 6 ~ 0.5, # Younger
  )
  
  gender_weight <- case_when(
    gender == 1 ~ 1, 
    gender == 0 ~ 1.1,
  )

  income_weight <- case_when(
    income_cat == 1 ~ 0.5, # < 25k
    income_cat == 2 ~ 0.75,
    income_cat == 3 ~ 0.9,
    income_cat == 4 ~ 1,
    income_cat == 5 ~ 1.25,
    income_cat == 6 ~ 1.4,
    income_cat == 7 ~ 1.5, # > 150k
  )
  
  race_weight <- case_when(
    race_eth == 1 ~ 0.5, 
    race_eth == 2 ~ 0.5,
    race_eth == 3 ~ 0.4,
    race_eth == 4 ~ 1,
    race_eth == 5 ~ 1.1,
    race_eth == 6 ~ 0.75,
    race_eth == 7 ~ 0.5, 
  )
  
  total_weight <- age_weight + gender_weight + income_weight + race_weight
  
  total_mental_health_visits <- rpois(1, 0.5 * total_weight)
  
  data = tibble(
    "gender" = gender,
    "race_eth" = race_eth,
    "age_cat" = age_cat,
    "income_cat" = income_cat,
    "education" = education,
    "total_mental_health_visits" = total_mental_health_visits
  )
  
  return(data)
}

data <- lapply(d$race_eth, genPatientCovariates) |> 
  bind_rows() 

renamed_data <- tibble(
  raceth = data$race_eth,
  agecat = data$age_cat,
  incomecat = data$income_cat,
  educationcat = data$education,
  genderbinary = data$gender,
  AMVSum = data$total_mental_health_visits
)

summary(data$total_mental_health_visits)

write.csv(renamed_data, "sim_data.csv", row.names = FALSE, col.names = TRUE)

# Questions:
# Missing data - strategy?
# Modeling - Poisson model?


# Notes from reading SQL:
# Visits after 2/1/2019

# Notes on SQL.
#   time = month_service + n*12 derived from "year service" -- this really measures in units of years
#   Would make sure to report/interpret appropriately.

# Measures:

# AMV: Any Mental Health Visit
#   1 = Therapy
#   2 = Psychiatry
#   3 = Testing
#   4 = Neuropsych

# PCMHV: Precovid mental health visit, dereived from AMV and service_from_date_1
#   Any visit before 3/1/2020 
#     Because of subset of visits noted above (>2/1/2019), does this mean it's capped between 2/1/2019 and 3/1/2020?
# 
# Mean_AMVSUM - generated from average of AMVSum, which I'm assuming is a sum of all mental health visits. Must make sure this query is on a
# dataset where 1 record = 1 person
#
# raceth  Not sure what categories these are.   Do these all come from EthnicIQ_V2?
#   1   African American
#   2   Hispanic
#   3   Middle Eastern
#   4   Caucasian-Other
#   5   Caucsaian-Jewish
#   6   Asian
#   7   Native American/Pacific Islander
#   9   Comes from 999 -- Do we know why these are not in the other categories? Non response vs "Other" for example?
#
#  Note, on the SQL code, it looks like this updates raceth directly, which is destructive of the source measure. Might not be a big deal, but it's
#  nice to always generate a new column/varaible so you can compare the two for validation.
#
# Income  -- Does this come from IncomeEQ_plus_V3? Is this individual or household income?
#   1 <= 24,999
#   2 <= 49,999
#   3 <= 74,999
#   4 <= 99,999
#   5 <= 124,999
#   6 <= 149,999
#   7 >= 150,000
#   
#   9     Comes from 9999 derived from else statement. Do we know why missing?

# Education -- Does this come from AIQ_educationIQ_V2?
#   1   Less than HS
#   2   HS
#   3   BA (Is some college HS?)
#   4   Graduate
#   999   Again, do we know missingness mechanisms?

# Age -- I'm assuming these categories in the SQL code are already generated to mean "less than" otherwise it would only pick cases with exactly that YOB.
#   1   YOB = 1957
#   2   YOB = 1967
#   3   YOB = 1977
#   4   YOB = 1987
#   5   YOB = 1997
#   6   YOB = 2004
#   999 other

# COVID == 0, 1, or 2 -- what do these mean?
# genderbinary = 1 for M (assuming male in the data), 0 for not male
# 
# Number of mental health visits per month per patient
#
# The descriptives are groued by TIMEN. What is TIMEN? Assumming it splits the data in a sensible way.


