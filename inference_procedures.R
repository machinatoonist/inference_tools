# INFERENCE TOOLS ----
# * Examples and exploration of the infer package ----
#
# * Libraries ----
library(infer)
library(tidyverse)
library(stats)
library(forcats)
library(glue)

# * Load in the dataset ----
data(gss)

# take a look at its structure
dplyr::glimpse(gss)

# Contingency table to get a sense of the distribution
gss %>%
  # Count the rows by college status and income
  count(college, income)

# Find proportion of each class who were finished college
gss %>%
  # Group by sex
  group_by(class, sex) %>%
  # Calculate college proportion summary stat
  summarize(college_prop = mean(college == "degree"))

gss %>% filter(college == "degree")

gss %>%
  mutate(college_lgl = (college == "degree")) %>% pull(college_lgl) %>%
  mean()

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

gss %>% class()

# find the point estimate
point_estimate <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")

# Generate a null distribution ----
null_dist <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

null_dist %>% visualise() +
  shade_p_value(obs_stat = point_estimate, direction = "two-sided")

# Get a two-tailed p-value ----
# If less than the significance level alpha
# you decided on before running this analysis it would be statistically
# significant.  In the gss dataset the at alpha = 0.05 the p-value of
# 0.042 indicates the difference is statistically significant.

p_value <- null_dist %>%
  get_p_value(obs_stat = point_estimate, direction = "two-sided")

p_value

# Confidence interval ----
# start with the null distribution
null_dist %>%
  # calculate the confidence interval around the point estimate
  get_confidence_interval(point_estimate = point_estimate,
                          # at the 95% confidence level
                          level = .95,
                          # using the standard error
                          type = "se")

# The confidence interval is 40.1 to 42.7.  40hrs is not contained in
# this interval.
#
null_f_distn <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

null_f_distn_theoretical <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "F")

F_hat <- gss %>%
  specify(age ~ partyid) %>%
  calculate(stat = "F")

visualize(null_f_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

visualize(null_f_distn, method = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

help(package = "infer")
# Does completing college predict income?  ----
# Question:  Does getting a college degree predict a higher income?

gss %>% glimpse()

gss %>%
  select_if(is.numeric) %>%
  map(~ unique(.) %>% length())

gss %>%
  select(income) %>% unique()

gss_prep_tbl <-  gss %>%
  mutate(above_25k =
           as_factor(ifelse(income == "$25000 or more", "Yes", "No")))

point_estimate <- gss_prep_tbl %>%
  specify(response = above_25k, explanatory = college, success = "Yes") %>%
  hypothesise(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("degree", "no degree"))

null_dist <- gss_prep_tbl %>%
  specify(above_25k ~ college, success = "Yes") %>%
  hypothesise(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("no degree", "degree"))

null_dist %>%
  ggplot(aes(x = stat)) +
  geom_density()

null_dist %>% visualise()

gss_prep_tbl %>%
  count(above_25k, class, college)

# Find proportion of respondents with or without college degrees who
# earn over $25k
gss_prep_tbl %>%
  # Group by college
  group_by(college) %>%
  # Calculate proportion above $25k summary stat
  summarize(prop_above25k = mean(above_25k == "Yes"))

# * Calculate the observed difference in promotion rate ----
diff_orig <- gss_prep_tbl %>%
  # Group by college
  group_by(college) %>%
  # Summarize to calculate fraction with income above $25k
  summarise(prop_above25k = mean(above_25k == "Yes")) %>%
  # Summarize to calculate difference
  summarise(stat = diff(prop_above25k)) %>%
  pull()

diff_orig_2 <- gss_prep_tbl %>%
  # Group by class and college
  group_by(college, class) %>%
  # Summarize to calculate fraction with income above $25k
  summarise(prop_above25k = mean(above_25k == "Yes")) %>%
  ungroup() %>%
  mutate(college_class = as_factor(glue("{college} {class}"))) %>%
  select(-college, -class) %>%
  relocate(college_class) %>%
  # Summarize to calculate difference
  summarise(stat = diff(prop_above25k)) %>%
  pull()

?diff

# * Step through the permutation ----
# Replicate the entire data frame, permuting the promote variable
gss_perm <- gss_prep_tbl %>%
  specify(above_25k ~ college, success = "Yes") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")

gss_perm %>%
  # Group by replicate
  group_by(replicate) %>%
  # Count per group
  count(above_25k, college)

gss_perm <- gss_perm %>%
  # Calculate difference in proportion, degree then no degree
  calculate(stat = "diff in props", order = c("degree", "no degree"))

# This seems like an open and shut case.  A college degree appears to
# predict a higher proportion earn above $25k
gss_perm %>%
  summarise(proportion = mean(diff_orig >= stat))

gss_perm %>%
# Plot permuted differences, diff_perm
ggplot(aes(x = stat)) +
  # Add a density layer
  geom_density() +
  # Add a vline layer with intercept diff_orig
  geom_vline(aes(xintercept = diff_orig), color = "red")

# Compare permuted differences to observed difference
gss_perm %>%
  summarize(n_perm_le_obs = sum(stat <= diff_orig))

