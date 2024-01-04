
## RODBC-based Analysis in R ----------------------------------------------- ##

library(RODBC)
# library(ggplot2)
# library(dplyr)
library(pscl)
# library(MASS)
library(bench)

# Establish Database Connection--------------------------------------------

# Alter these to match your Snowflake instance.
server_hostname <- 'hprs-postgres.lan'
server_driver <- '/opt/homebrew/Cellar/psqlodbc/16.00.0000/lib/psqlodbcw.so'
server_database_name <- 'postgres'
server_username <- 'hprs'
server_password <- 'hprs'
server_port <- '5432'

channel <- RODBC::odbcDriverConnect(
  paste0(
    'driver=', server_driver, ';',
    'server=', server_hostname, ';',
    'database=', server_database_name, ';',
    'uid=', server_username, ';',
    'pwd=', server_password, ';',
    'port=', server_port, ';'
  )
)


# Reading In The Data -----------------------------------------------------


# Performance on local network to read in data:
# 1000 records = 3.57s
# 10000 records = 3.4s
# 100000 records = 3.48s
# 1000000 records = 4.03s
# 3700000 records = 3.4s

table_name <- 'sim_data' # Needs to correspond to the table name in SQL
max <- 0 # Zero fetches all records. This can be set low to test code quickly.

data <- RODBC::sqlFetch(
  channel = channel,
  sqtable = table_name,
  max = max
)  


# Convert And Relevel Factor Variables ---------------------------------------
# The below segments set the integer variables to factors and then set a 
# reference category.

# These can be changed by just swapping out the number for the correct
# category.

# Race: reference = 4 (White Other)
data <- within(data, raceth <- relevel(factor(raceth), ref = 3))

# Gender: reference = 1 (M)
data <- within(data, genderbinary <- relevel(factor(genderbinary), ref = 1)) # No change

# Income: reference = 75-99.99k (2)
# This is a difficult one because the categories are ordered but last category is
# uncapped. Treating it categorically makes comparison possible and allows
# relationship of income to vary by category, which is a benefit, but doesn't
# allow "simple" interpetation of having a (preferably) logged income variable.
data <- within(data, incomecat <- relevel(factor(incomecat), ref = 3)) 

# Education: reference = 3 (BA)
data <- within(data, educationcat <- relevel(factor(educationcat), ref = 3)) 

# Age: reference = 4 (36-45)
# Like with income, the cateogries are ordered but the boundaries are
# uncapped.
data <- within(data, agecat <- relevel(factor(agecat), ref = 4)) 


# Modeling ----------------------------------------------------------------

# Poisson Regression
# Note: May need to fix the case of the variables depending on how RODBC 
#        imports them.

# Laptop (12-core M3 Pro Macbook): Takes 4.5 minutes to run (3,720,000 records)
bench::mark(
  model1 <- glm(
    amvsum ~ raceth + genderbinary + incomecat + educationcat + agecat,
    family = poisson(link = "log"), 
    data = data
  )
)
summary(model1)

performance::check_overdispersion(model1)
performance::check_zeroinflation(model1)

exp(model1$coefficients)

# Zero-Inflated Negative Binomial Poisson Regression

# Both the count model and the zero-inflation model have the same predictors.
# If that is correct, the model formula after the "|" can be removed. Otherwise,
# can specify each model's predictors separately.

# This model has no interactions.
# Laptop (12-core M3 Pro Macbook): Takes 4.5 minutes to run (3,720,000 records)
bench::mark(
  model1_zif <- zeroinfl(
    amvsum ~ 
      raceth + genderbinary + incomecat + educationcat + agecat | # Count Model
      raceth + genderbinary + incomecat + educationcat + agecat, # Zero-Inflation Logit Model
    data=data, 
    dist="negbin",
    link="logit"
  )
)

summary(model1_zif)

# This model has interaction of race and income.
bench::mark(
  model2_zif <- zeroinfl(
    amvsum ~ 
      raceth*incomecat + genderbinary + educationcat + agecat | # Count Model
      raceth*incomecat + genderbinary + educationcat + agecat, # Zero-Inflation Logit Model
    data=data, 
    dist="negbin",
    link="logit"
  )
)

summary(model2_zif)