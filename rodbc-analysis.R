
## RODBC-based Analysis in R ----------------------------------------------- ##

library(RODBC)
library(ggplot2)
library(dplyr)
library(forcats)
library(pscl)
library(MASS)
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

data$raceth <- fct_recode(
  data$raceth,
  "African American" = "1",
  "Hispanic" = "2",
  "Middle Eastern" = "3",
  "White (Other)" = "4",
  "White (Jewish)" = "5",
  "Asian" = "6",
  "Native American/Pacific Islander" = "7"
)

# Gender: reference = 1 (M)
data <- within(data, genderbinary <- relevel(factor(genderbinary), ref = 1)) # No change

# Pick appropriate labels basedd upon actual data
data$genderbinary <- fct_recode(
  data$genderbinary,
  "M" = "1",
  "Not-M" = "0"
)


# Income: reference = 75-99.99k (2)
# This is a difficult one because the categories are ordered but last category is
# uncapped. Treating it categorically makes comparison possible and allows
# relationship of income to vary by category, which is a benefit, but doesn't
# allow "simple" interpetation of having a (preferably) logged income variable.
data <- within(data, incomecat <- relevel(factor(incomecat), ref = 3)) 

data$incomecat <- fct_recode(
  data$incomecat,
  "< 25k" = "1",
  "25k-50k" = "2",
  "50k-75k" = "3",
  "75k-100k" = "4",
  "100k-125k" = "5",
  "125-150k" = "6",
  "> 150k" = "7"
)


# Education: reference = 3 (BA)
data <- within(data, educationcat <- relevel(factor(educationcat), ref = 3)) 

# Confirm these labels
data$educationcat <- fct_recode(
  data$educationcat,
  "Less than HS" = "1",
  "HS" = "2",
  "BA" = "3",
  "Advanced" = "4"
)

# Age: reference = 4 (36-45)
# Like with income, the cateogries are ordered but the boundaries are
# uncapped.
data <- within(data, agecat <- relevel(factor(agecat), ref = 4)) 

# Confirm these are correctly linked up
data$agecat <- fct_recode(
  data$agecat,
  "18-24" = "6",
  "25-34" = "5",
  "35-44" = "4",
  "45-54" = "3",
  "55-64" = "2",
  ">65" = "1"
)

# ANOVA & T-tests ---------------------------------------------------------

# Below are some examples of running t-tests in R. The variable on the left-hand
# side is the dependent variable and the variable on the right-hand side is the
# independent variable. 
# 
# Before running, I think it's useful to do some plotting. I take a sub sample
# here to make things easier to see. 
ggplot(data |> sample_n(5000)) +
  aes(x = genderbinary, y = amvsum, color = genderbinary) +
  geom_jitter() +
  theme(legend.position = "none")

t.test(amvsum ~ genderbinary, data = data)

# Of note: a bivariate regressoin yields same difference in means and similar
# p-value (slightly different degrees of freedom, but trivial here. They are 
# identical if you you use pooled variance.)
lm(amvsum ~ genderbinary, data=data) |> summary()

# And ANOVA. Let's first plot:
# 
ggplot(data |> sample_n(70000)) +
  aes(x = raceth, y = amvsum, color = raceth) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(data |> sample_n(70000)) +
  aes(x = educationcat, y = amvsum, color = educationcat) +
  geom_jitter() +
  theme(legend.position = "none")

aov(amvsum ~ raceth, data=data) |> summary()
aov(amvsum ~ educationcat, data=data) |> summary()
aov(amvsum ~ incomecat, data=data) |> summary()

# Modeling ----------------------------------------------------------------

# Poisson Regression
# Note: May need to fix the case of the variables depending on how RODBC 
#        imports them.

# Laptop (12-core M3 Pro Macbook): Takes 4.5 minutes to run (3,720,000 records)
model1 <- glm(
  amvsum ~ raceth + genderbinary + incomecat + educationcat + agecat,
  family = poisson(link = "log"), 
  data = data
)

summary(model1)

# This will check for overdispersion and zero-inflation, which may be present
# and would justify using negative binomial poisson regression and/or a zero-
# inflation model.
performance::check_overdispersion(model1)
performance::check_zeroinflation(model1)

exp(model1$coefficients)


# Zero-Inflated Negative Binomial Poisson Regression

# Both the count model and the zero-inflation model have the same predictors.
# If that is correct, the model formula after the "|" can be removed. Otherwise,
# can specify each model's predictors separately.

# This model has no interactions.
# Laptop (12-core M3 Pro Macbook): Takes 4.5 minutes to run (3,720,000 records)
model1_zif <- zeroinfl(
  amvsum ~ 
    raceth + genderbinary + incomecat + educationcat + agecat | # Count Model
    raceth + genderbinary + incomecat + educationcat + agecat, # Zero-Inflation Logit Model
  data=data, 
  dist="negbin",
  link="logit"
)


summary(model1_zif)

# Running separate logit and neg bin to avoid memory/speed issues

# Binomial with logit link
# Need to create a new variable for "has visit"/"no visit" -- zero-inflation
# looks at no visit as outcome (0)
data <- data |> mutate(
  no_visit = amvsum < 1,
  has_visit = amvsum > 0
)

model1_logit <- glm(
  no_visit ~ raceth + genderbinary + incomecat + educationcat + agecat,
  family = binomial(link = "logit"), 
  data = data
)

summary(model1_logit) # same as zeroinfl function

# Negative Binomial with log link -- almost identical to zeoinfl function
model1_negbin <- glm.nb(
  amvsum ~ raceth + genderbinary + incomecat + educationcat + agecat,
  data = data |> filter(has_visit),
  link = "log"
)

summary(model1_negbin)


# This model has interaction of race and income.
model2_zif <- zeroinfl(
  amvsum ~ 
    raceth*incomecat + genderbinary + educationcat + agecat | # Count Model
    raceth*incomecat + genderbinary + educationcat + agecat, # Zero-Inflation Logit Model
  data=data, 
  dist="negbin",
  link="logit"
)

summary(model2_zif)