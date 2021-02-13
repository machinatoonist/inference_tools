# INFERENCE TOOLS ----
# * Examples and exploration of the infer package ----
#
# * Libraries ----
library(infer)
library(tidyverse)

# * Load in the dataset ----
data(gss)

# take a look at its structure
dplyr::glimpse(gss)


# * Description of infer package ----
# ?infer::calculate()

vignette("infer")
# Infer consolidates the principles shared by common hypothesis tests into the
# following verbs:
# specify(): Specifying Response (and Explanatory) Variables
# hypothesize(): Declaring the Null Hypothesis
# generate(): Generating the Null Distribution
# calculate(): Calculating Summary Statistics
# Other Utilities
# Theoretical Methods
#
# INFER WORKFLOW ----
# * 1. Specify ----
#
# This step specifies which variables in the dataset you are interested in
# and to define which are thought to be explanatory and response variables.
# In addition a particular success response can be defined.

obj <- gss %>%
  specify(response = age)
class(obj)

gss %>%
  specify(age ~ partyid)

gss %>%
  specify(response = age, explanatory = partyid)

# specifying for inference on proportions
gss %>%
  specify(response = college, success = "degree")

# * 2. Hypothesis ----
#
# The next step is to declare a null hypothesis
# The first step is to supply one of “independence”
# or “point” to the null argument.
#
# If assuming independence between two variables use "independence".
# If on the other hand the null hypothesis is that the mean estimate
# of a variable for each group is the same as the mean for population
# use "point" as the null argument and also provide the estimate for
# mu (the true mean).  Also possible to specify the true proportion of
# successes between 0 and 1 as "p", the true median "med" and the true
# standard deviation "sigma"
# Here are some examples:
gss %>%
  specify(college ~ partyid, success = "degree") %>%
  hypothesize(null = "independence")

gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40)

# * Define population statistics ----

hours_stats_tot_pop = gss %>% select(hours) %>%
  summarise(
    mu = mean(hours),
    median = median(hours),
    sigma = sd(hours),
    q_05 = quantile(hours, p = 0.05),
    q_95 = quantile(hours, p = 0.95),
    min = min(hours),
    max = max(hours)
    )

mu <- hours_stats_tot_pop$mu
median <- hours_stats_tot_pop$median
sigma <- hours_stats_tot_pop$sigma

hours_stats_by_sex = gss %>%
  group_by(sex) %>%
  summarise(
    mu = mean(hours),
    median = median(hours),
    sigma = sd(hours),
    q_05 = quantile(hours, p = 0.05),
    q_95 = quantile(hours, p = 0.95)
  )

hours_stats_by_sex_college = gss %>%
  group_by(sex, college) %>%
  summarise(
    mu = mean(hours),
    median = median(hours),
    sigma = sd(hours),
    q_05 = quantile(hours, p = 0.05),
    q_95 = quantile(hours, p = 0.95),
    sample_size = n()
  )

weight_stats_by_sex_college = gss %>%
  group_by(sex, college) %>%
  summarise(
    mu = mean(weight),
    median = median(weight),
    sigma = sd(weight),
    q_05 = quantile(weight, p = 0.05),
    q_95 = quantile(weight, p = 0.95),
    sample_size = n()
  )

# * 3. Generate ----
# Construct the null hypothesis using hypothesize()
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = mu) %>%
  generate(reps = 1000, type = "bootstrap")

# Create a null distribution for independence of two variables
gss %>%
  specify(partyid ~ age) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")

# * 4. Calculate ----
# If using computation based inference you supply calculate() with
# the output of generate(),  If using theory based inference you will
# supply calculate() with the output of hypothesize().
#
# The hypothesize function takes in a stat argument which can be on of:
# c(“mean”, “median”, “sum”, “sd”, “prop”,
# “count”, “diff in means”, “diff in medians”, “diff in props”,
# “Chisq”, “F”, “t”, “z”, “slope”, “correlation”).
#
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>%
  summarise(
    mu = mean(stat),
    median = median(stat),
    sigma = sd(stat),
    q_05 = quantile(stat, p = 0.05),
    q_95 = quantile(stat, p = 0.95),
    min = min(stat),
    max = max(stat)
  )

# To find the difference in mean age of those that have
# a college degree and those that don’t:
gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate("diff in means", order = c("degree", "no degree"))
