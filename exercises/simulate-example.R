# Packages ----------------------------------------------------------------

library(tidyverse)
devtools::load_all()

# CASE STUDY 1 - Logistic Regression

# predicting the children performance (0/1) on the exam
# as a function of the:
# - children age (numeric)
# - parent judgement about children school involvment (numeric, 0-10)
# - if the child is involved in after-school programs (categorical, "yes" or "no")

# Simulating data

n <- 100 # numbers of childrens
age <- round(runif(n, 6, 12)) # age between 6 and 12 years

age_bin <- case_when(
    age < 8 ~ "6-8",
    age >= 8 & age < 10 ~ "8-10",
    age >= 10 ~ "10-12"
)

# two errors in coding
parent_score <- c(sample(0:10, n-2, TRUE), 20, 25)
      
# the distribution of being involved in after-school
# is different according to the age

after_school <- age_bin |> 
    data.frame() |> 
    count(age_bin) |> 
    mutate(p = c(0.7, 0.3, 0.5)) |> 
    mutate(x = map2(n,p, ~sample(c("yes", "no"), .x,.y))) |> 
    unnest(x) |> 
    pull(x)

# teacher judgment about children involvment

teacher_score <- sample(0:10, n, TRUE)

dat <- data.frame(
    id = 1:n,
    age,
    after_school,
    parent_score,
    teacher_score
)


dat$age0 <- dat$age - mean(dat$age)
dat$after_school <- factor(dat$after_school)
contrasts(dat$after_school) <- contr.sum(2)/2
dat$after_school_c <- contrasts(dat$after_school)[dat$after_school]
 
b0 <- qlogis(0.2)
b1 <- 0
b2 <- 0.3
b3 <- 0.05
b4 <- -log(odds_ratio(0.7, 0.3))

dat <- sim_data(dat, plogis(b0 + b1 * age0 + b2 * teacher_score + b3 * parent_score + b4 * after_school_c))

dat_save <- dat |> select(id, age, after_school, parent_score, teacher_score, y)
dat_save <- filor::add_random_na(dat_save, n = 15, exclude_cols = "id")

saveRDS(dat_save, "exercises/data/kid_school_inv.rds")
