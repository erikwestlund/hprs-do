library(dplyr)
library(ggplot2)
library(EnvStats)

# This code generates a sample of people by race/ethnicity, roughly
# proportionate to the US population. It then uses the characteristics of the
# US to then generate realistic covariates for each person, starting from simulated
# race/ethnicity. As a side note: we could start with basically any property
# and work from there with similar results were the data good.

# Set seed to make this deterministic.
set.seed(123)

# Pick your sample size. 
n <- 3720000

# Function to generate realistic values of key covariates, given race, for one person.
# race_eth = race/ethnicity code of person
#
# weight_divider = parameter to arbitrarily scale weight up or down. use to get rough
# proportion of visits right
#
# weight_multiplier = parameter to scale lambda of poisson distribution for weight
#
# visit_multiplier = parameter to scale up visit counts from poisson
#
# max_visits = a max number of visits. Currently set to about 2 visits per week.

# Note about `sample` function probability weights: while the parameter is named "prob,"
# they are actually weights that get normalized, so no need to make them 
# probabilities that sum exactly to 1.
genPatientCovariates <- function(
    race_eth,
    weight_divider = 4, 
    weight_multiplier = 1,
    visit_multiplier = 34,
    max_visits = 200
  ) {
  
  # 1 = M, 0 = Not M
  gender <- sample(c(1,0), 1, prob = c(0.39, 0.61))
  
  # For six categories in the data, roughly proportionate to US demography
  # 1 = 65+, 2 = 55-64, 3 = 46-54, 4 = 36-45, 5 = 26-35, 6 = 18-25
  age_cat <- sample(1:6, 1, replace = TRUE, prob = c(27, 17, 16, 19, 18, 3))

  # Generate an income for a person based upon their race.
  # Use a log-normal distribution to generate skew, then multiply to get
  # a reasonable range of incomes for each group.
  income <- case_when(
    race_eth == 1 ~ rlnorm(1, 4, 0.5) * 750, # Af Am
    race_eth == 2 ~ rlnorm(1, 4.25, 0.5)* 750, # Latine
    race_eth == 3 ~ rlnorm(1, 4.2, 0.5) * 750, # Middle Eastern
    race_eth == 4 ~ rlnorm(1, 4.75, 0.5) * 750, # White Other
    race_eth == 5 ~ rlnorm(1, 5, 0.5) * 750, # White Jewish
    race_eth == 6 ~ rlnorm(1, 4.9, 0.5) * 750, # Asian/Pac Isl
    race_eth == 7 ~ rlnorm(1, 3.75, 0.5) * 750,  # Native Am 
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
    race_eth == 1 ~ sample(1:4, 1, replace = TRUE, prob=c(15, 65, 15, 5)),
    race_eth == 2 ~ sample(1:4, 1, replace = TRUE, prob=c(15, 65, 15, 5)),
    race_eth == 3 ~ sample(1:4, 1, replace = TRUE, prob=c(15, 65, 15, 5)),
    race_eth == 4 ~ sample(1:4, 1, replace = TRUE, prob=c(5, 40, 40, 15)),
    race_eth == 5 ~ sample(1:4, 1, replace = TRUE, prob=c(5, 20, 50, 25)),
    race_eth == 6 ~ sample(1:4, 1, replace = TRUE, prob=c(5, 30, 45, 20)),
    race_eth == 7 ~ sample(1:4, 1, replace = TRUE, prob=c(20, 30, 15, 5))
    # race_eth == 9 ~ NA, # Less than HS
  )

  # Generate a random number of total mental health visits for each person. We want to make this a function of 
  # the person's age, race, income, and education. But we also want it realistically distributed, so we will
  # sample from a poisson distribution, using weights from those factors. To get our DV, we need to divide by 34,
  # the total number of months of data.
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
    gender == 0 ~ 1.3,
  )

  income_weight <- case_when(
    income_cat == 1 ~ 0.25, # < 25k
    income_cat == 2 ~ 0.5,
    income_cat == 3 ~ 0.75,
    income_cat == 4 ~ 1,
    income_cat == 5 ~ 1.25,
    income_cat == 6 ~ 1.5,
    income_cat == 7 ~ 1.75, # > 150k
  )
  
  race_weight <- case_when(
    race_eth == 1 ~ 0.3, 
    race_eth == 2 ~ 0.5,
    race_eth == 3 ~ 0.4,
    race_eth == 4 ~ 1.5,
    race_eth == 5 ~ 1.25,
    race_eth == 6 ~ 0.75,
    race_eth == 7 ~ 0.5, 
  )
  
  # Divide by 2 to get a lambda around 1.  This generates a number of 0s, most between 1-2 visits a month and a handful with many
  total_weight <- (age_weight + gender_weight + income_weight + race_weight)
  
  # Let's do a simulation where we draw an initial probability of ever attending, aiming at about 2/3 of people attending
  # Then lets simulate by month
  ever_attend_p <- 2/3
  
  # high weights should be more likely, low weights should be less likely, so let's scale the p appropriately
  # divide by four because average weight should be around 4
  ever_attend_p <- ever_attend_p * (total_weight/weight_divider)
  ever_attend_p <- ifelse(ever_attend_p > 1, 1, ever_attend_p)
  ever_attend_p <- ifelse(ever_attend_p < 0, 0, ever_attend_p)
  
  # sim attendance
  ever_attended <- rbinom(1, 1, ever_attend_p)

  # sim visits
  if(ever_attended == 1){
    # sim total visits
    total_mental_health_visits <- rpois(1, total_weight * weight_multiplier) * visit_multiplier
  } else {
    total_mental_health_visits <- 0
  }
  
  average_visits_per_month <- total_mental_health_visits/34
  
  data = tibble(
    "gender" = gender,
    "race_eth" = race_eth,
    "age_cat" = age_cat,
    "income_cat" = income_cat,
    "education" = education,
    "ever_attended" = ever_attended,
    "ever_attend_p" = ever_attend_p,
    "total_weight" = total_weight,
    "average_visits_per_month" = average_visits_per_month,
    "total_mental_health_visits" = total_mental_health_visits
  )
  
  return(data)
}

race_eth <- sample(1:7, n, replace = TRUE, prob=c(
 9,   # African American
 17,  # Hispanic
 1,   # Middle Eastern 
 65,  # Caucasian-Other
 3,   # Caucasian-Jewish
 3,   # Asian
 1    # Native American/Pacific Islander
))

# This generates a data frame of N number of
data <- lapply(race_eth, genPatientCovariates, weight_divider = 4.25, visit_multiplier = 25) |> 
  bind_rows() 

summary((data |> dplyr::filter(ever_attended == 1) |> dplyr::select(total_mental_health_visits))$total_mental_health_visits)/34
table(data$total_mental_health_visits)
hist(data$total_mental_health_visits)

# This renames the data frame to match the names in the SQL.
renamed_data <- tibble(
  raceth = data$race_eth,
  agecat = data$age_cat,
  incomecat = data$income_cat,
  educationcat = data$education,
  genderbinary = data$gender,
  AMVSum = data$total_mental_health_visits,
  Mean_AMVSUm = data$average_visits_per_month
)

write.csv(renamed_data, "sim_data.csv", row.names = FALSE)
