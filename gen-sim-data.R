library(dplyr)
library(ggplot2)
library(EnvStats)

# This code genarates a sample of people by race/ethnicity, roughly
# proportionate to the US population. It then uses the characteristics of the
# US to then generate realistic covariates for each person, starting from simulated
# race/ethnicity. As a side note: we could start with basically any property
# and work from there with similar results were the data good.

# Set seed to make this deterministic.
set.seed(123)

# Pick your sample size. 
n <- 100000

# Function to generate realistic values of key covariates, given race.
# You can limit the max mental health visits visits allowed
# Weight multiplier can be adjusted to increase/decrease the number of visits
genPatientCovariates <- function(race_eth, weight_mulitiplier = 0.25, max_visits = 50) {
  
  # 1 = M, 0 = Not M
  gender <- sample(c(1,0), 1, prob = c(0.5, 0.5))
  
  # For six categories in the data, roughly proportionate to US demography
  # 1 = older, 6 = younger
  age_cat <- sample(c(rep(1, 4), rep(2, 4), rep(3, 5), rep(4, 4), rep(5, 3), rep(6,4)), 1)
  
  # Generate an income for a person based upon their race.
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
  
  # Make sure it's positive or 0.
  income <- round(ifelse(income < 0, 0, income), -3)
  
  # Categorize as in the data.
  income_cat <- case_when(
    income <= 25000 ~ 1,
    income <= 50000 ~ 2,
    income <= 75000 ~ 3,
    income <= 100000 ~ 4,
    income <= 125000 ~ 5,
    income <= 150000 ~ 6,
    income > 150000 ~ 7,
  )  

  # Generate categorical education values roughly proportionate to educational
  # attainment by race/ethnicity in the US.
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

  # Generate a random number of total mental health visits for each person. We want to make this a function of 
  # the person's age, race, income, and education. But we also want it realistically distributed,
  # using a Poisson distribution. We will use the values of age, gender, race, and income
  # to generate a weight, and then use that mean to generate a random number using rpois.
  
  # These can be tweaked to change the distribution of visits. Higher weights yield
  # higher visit counts.
  age_weight <- case_when(
    age_cat == 1 ~ 0.5, # Older
    age_cat == 2 ~ 0.75,
    age_cat == 3 ~ 1.25,
    age_cat == 4 ~ 1.5,
    age_cat == 5 ~ 2,
    age_cat == 6 ~ 1.5, # Younger
  )
  
  gender_weight <- case_when(
    gender == 1 ~ 1, 
    gender == 0 ~ 1.1,
  )

  income_weight <- case_when(
    income_cat == 1 ~ 0.2, # < 25k
    income_cat == 2 ~ 0.75,
    income_cat == 3 ~ 0.9,
    income_cat == 4 ~ 1,
    income_cat == 5 ~ 1.25,
    income_cat == 6 ~ 1.4,
    income_cat == 7 ~ 1.5, # > 150k
  )
  
  race_weight <- case_when(
    race_eth == 1 ~ 0.3, 
    race_eth == 2 ~ 0.5,
    race_eth == 3 ~ 0.4,
    race_eth == 4 ~ 1,
    race_eth == 5 ~ 1.1,
    race_eth == 6 ~ 0.75,
    race_eth == 7 ~ 0.5, 
  )
  
  total_weight <- age_weight + gender_weight + income_weight + race_weight
  
  # 0.75 is a multiplier. Increase or decrease to alter the mean of the distribution.
  # total_mental_health_visits <- rpois(1, 0.75 * total_weight)
  total_mental_health_visits <- round(rpareto(1, weight_mulitiplier *total_weight))
  
  valid <- FALSE
  
  while(!valid) {
    if(total_mental_health_visits > max_visits) {
      total_mental_health_visits <- round(rpareto(1, weight_mulitiplier *total_weight))
    } else {
      valid <- TRUE
    }
  }
  
  total_mental_health_visits <- total_mental_health_visits - 1

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

race_eth <- sample(c(
    rep(1, 13), # African American
    rep(2, 19), # Hispanic
    rep(3, 1),  # Middle Eastern 
    rep(4, 58), # Caucasian-Other
    rep(5, 2),  # Caucsaian-Jewish
    rep(6, 6),  # Asian
    rep(7, 1)  # Missing
    # rep(9, 10)   # Native American/Pacific Islander
  ), n, replace = TRUE)

# This generates a data frame of N number of
data <- lapply(race_eth, genPatientCovariates) |> 
  bind_rows() 

# This renames the data frame to match the names in the SQL.
renamed_data <- tibble(
  raceth = data$race_eth,
  agecat = data$age_cat,
  incomecat = data$income_cat,
  educationcat = data$education,
  genderbinary = data$gender,
  AMVSum = data$total_mental_health_visits
)

write.csv(renamed_data, "sim_data.csv", row.names = FALSE)

