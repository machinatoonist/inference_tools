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
  summarise(prop_above25k = mean(above_25k == "Yes"),
            n_obs = n()) %>%
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
# Compare permuted differences to observed difference
gss_perm %>%
  summarise(proportion = mean(diff_orig >= stat),
            n_perm_le_obs = sum(stat <= diff_orig))

gss_perm %>%
# Plot permuted differences, diff_perm
ggplot(aes(x = stat)) +
  # Add a density layer
  geom_density() +
  # Add a vline layer with intercept diff_orig
  geom_vline(aes(xintercept = diff_orig), color = "red")


gss_prep_tbl = gss %>% select(hours) %>%
  summarise(
    mu = mean(hours),
    median = median(hours),
    sigma = sd(hours),
    q_05 = quantile(hours, p = 0.05),
    q_95 = quantile(hours, p = 0.95),
    min = min(hours),
    max = max(hours)
  )

gss_perm %>%
  summarize(
    # Find the 0.01 quantile of stat
    q.01 = quantile(stat, p = 0.01),
    # ... and 0.05
    q.05 = quantile(stat, p = 0.05),
    # ... and 0.1
    q.10 = quantile(stat, p = 0.1),
    # Find the 0.9 quantile of diff_perm's stat
    q.90 = quantile(stat, p = 0.9),
    # ... and the 0.95 quantile
    q.95 = quantile(stat, p = 0.95),
    # ... and the 0.99 quantile
    q.99 = quantile(stat, p = 0.99)
  )

# Histogram of permuted distribution statistic compared to the population
# This is a highly significant conclusion.  A college degree predicts a
# higher income.  But what about the effect of class?
ggplot(gss_perm, aes(x = stat)) +
  geom_histogram(binwidth = 0.01) +
  # Change the x-axis intercept to diff_orig_big
  geom_vline(aes(xintercept = diff_orig), color = "red")

# The value of a college degree for working class respondents ----
# First take the subset of people in the working class category
#
gss_working_class_prep_tbl <- gss_prep_tbl %>%
  filter(class == "working class")

# * Calculate the observed difference in promotion rate ----
diff_orig_working_class <- gss_working_class_prep_tbl %>%
  # Group by college
  group_by(college) %>%
  # Summarize to calculate fraction with income above $25k
  summarise(prop_above25k = mean(above_25k == "Yes")) %>%
  # Summarize to calculate difference
  summarise(stat = diff(prop_above25k)) %>%
  pull()

# * Step through the permutation for the working class subset ----
# Replicate the entire data frame, permuting the promote variable
gss_working_class_perm <- gss_working_class_prep_tbl %>%
  specify(above_25k ~ college, success = "Yes") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")

gss_working_class_perm %>%
  # Group by replicate
  group_by(replicate) %>%
  # Count per group
  count(above_25k, college)

gss_working_class_perm_diff <- gss_working_class_perm %>%
  # Calculate difference in proportion, degree then no degree
  calculate(stat = "diff in props", order = c("degree", "no degree"))

# This seems like an open and shut case.  A college degree appears to
# predict a higher proportion earn above $25k
# Compare permuted differences to observed difference
gss_working_class_perm_diff %>%
  summarise(proportion = mean(diff_orig_working_class >= stat),
            n_perm_le_obs = sum(stat <= diff_orig_working_class))

gss_working_class_perm_diff %>%
  # Plot permuted differences, diff_perm
  ggplot(aes(x = stat)) +
  # Add a density layer
  geom_density() +
  # Add a vline layer with intercept diff_orig
  geom_vline(aes(xintercept = diff_orig_working_class), color = "red")

# Histogram of permuted distribution statistic compared to the population
# This is a highly significant conclusion.  A college degree predicts a
# higher income.  But what about the effect of class?
ggplot(gss_working_class_perm_diff, aes(x = stat)) +
  geom_histogram(binwidth = 0.01) +
  # Change the x-axis intercept to diff_orig_big
  geom_vline(aes(xintercept = diff_orig_working_class), color = "red")

gss_working_class_perm_diff %>%
  summarize(
    # Find the 0.01 quantile of stat
    q.01 = quantile(stat, p = 0.01),
    # ... and 0.05
    q.05 = quantile(stat, p = 0.05),
    # ... and 0.1
    q.10 = quantile(stat, p = 0.1),
    # Find the 0.9 quantile of diff_perm's stat
    q.90 = quantile(stat, p = 0.9),
    # ... and the 0.95 quantile
    q.95 = quantile(stat, p = 0.95),
    # ... and the 0.99 quantile
    q.99 = quantile(stat, p = 0.99)
  )

diff_orig_working_class

# When looking at just the working class population a college degree is
# highly predictive of a significantly higher proportion of people earning
# above $25k.  p = 0.01

# Let's repeat the analysis for the lowest income class ----
gss_lower_class_prep_tbl <- gss_prep_tbl %>%
  filter(class == "lower class")

# * Calculate the observed difference in proportion of people earning > $25k ----
diff_orig_lower_class <- gss_lower_class_prep_tbl %>%
  # Group by college
  group_by(college) %>%
  # Summarize to calculate fraction with income above $25k
  summarise(prop_above25k = mean(above_25k == "Yes")) %>%
  # Summarize to calculate difference
  summarise(stat = diff(prop_above25k)) %>%
  pull()

# * Step through the permutation for the working class subset ----
# Replicate the entire data frame, permuting the promote variable
gss_lower_class_perm <- gss_lower_class_prep_tbl %>%
  specify(above_25k ~ college, success = "Yes") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")

gss_lower_class_perm %>%
  # Group by replicate
  group_by(replicate) %>%
  # Count per group
  count(above_25k, college)

gss_lower_class_perm_diff <- gss_lower_class_perm %>%
  # Calculate difference in proportion, degree then no degree
  calculate(stat = "diff in props", order = c("degree", "no degree"))

# This seems like an open and shut case.  A college degree appears to
# predict a higher proportion earn above $25k
# Compare permuted differences to observed difference
gss_lower_class_perm_diff %>%
  summarise(proportion = mean(diff_orig_lower_class >= stat),
            n_perm_le_obs = sum(stat <= diff_orig_lower_class))

gss_lower_class_perm_diff %>%
  # Plot permuted differences, diff_perm
  ggplot(aes(x = stat)) +
  # Add a density layer
  geom_density() +
  # Add a vline layer with intercept diff_orig
  geom_vline(aes(xintercept = diff_orig_lower_class), color = "red")

# Histogram of permuted distribution statistic compared to the population
# This is a highly significant conclusion.  A college degree predicts a
# higher income.  But what about the effect of class?
ggplot(gss_lower_class_perm_diff, aes(x = stat)) +
  geom_histogram(binwidth = 0.01) +
  # Change the x-axis intercept to diff_orig_big
  geom_vline(aes(xintercept = diff_orig_lower_class), color = "red")

gss_lower_class_perm_diff %>%
  summarize(
    # Find the 0.01 quantile of stat
    q.01 = quantile(stat, p = 0.01),
    # ... and 0.05
    q.05 = quantile(stat, p = 0.05),
    # ... and 0.1
    q.10 = quantile(stat, p = 0.1),
    # Find the 0.9 quantile of diff_perm's stat
    q.90 = quantile(stat, p = 0.9),
    # ... and the 0.95 quantile
    q.95 = quantile(stat, p = 0.95),
    # ... and the 0.99 quantile
    q.99 = quantile(stat, p = 0.99)
  )

diff_orig_lower_class

# The analysis confirms that a college degree does not improve the
# proportion of people earning over $25k per year for the lower class.

# EXAMPLE INFERENCE STUDY ----
# The study:
# Seventy-five students were assigned to the control group and were presented
# with two options. Each student could either buy the video or not buy the video.
# Another 75 students were assigned to the treatment group and were given a
# slight variation on the same two options. The first option was also to buy the
# DVD, but the second option was to not buy the DVD while being reminded that
# the money could also be saved.

# Data from: Frederick S, Novemsky N, Wang J, Dhar R, Nowlis S. 2009.
# Opportunity Cost Neglect. Journal of Consumer Research
# Code from:
# https://campus.datacamp.com/courses/foundations-of-inference-in-r/hypothesis-testing-errors-opportunity-cost?ex=2

# * Find the sample statistics ----
# Tabulate the data
opportunity %>%
  count(decision, group)

# Find the proportion who bought the DVD in each group
opportunity %>%
  group_by(group) %>%
  summarize(buy_prop = mean(decision == "buyDVD"))

# * Visualise the difference between the treatment and control groups ----
# Plot group, filled by decision
ggplot(opportunity, aes(x = group, fill = decision)) +
  # Add a bar layer, with position "fill"
  geom_bar(position = "fill")

# * Generate a distribution of differences ----
# Calculate the observed difference in purchase rate
diff_obs <- opportunity %>%
  # Group by group
  group_by(group) %>%
  # Calculate proportion deciding to buy a DVD
  summarise(prop_buy = mean(decision == "buyDVD")) %>%
  # Calculate difference between groups
  summarise(stat = diff(prop_buy)) %>%
  pull()

diff_obs

# Create data frame of permuted differences in purchase rates
opp_perm <- opportunity %>%
  # Specify decision vs. group, where success is buying a DVD
  specify(decision ~ group, success = "buyDVD") %>%
  # Set the null hypothesis to independence
  hypothesize(null = "independence") %>%
  # Generate 1000 reps of type permute
  generate(reps = 1000, type = "permute") %>%
  # Calculate the summary stat difference in proportions
  calculate(stat = "diff in props", order = c("treatment", "control"))

# Review the result
opp_perm

# Using the permuation data, plot stat
ggplot(opp_perm, aes(x = stat)) +
  # Add a histogram layer with binwidth 0.005
  geom_histogram(binwidth = 0.005) +
  # Add a vline layer with intercept diff_obs
  geom_vline(aes(xintercept = diff_obs), color = "red")

# * Visualize the statistic ----
opp_perm %>%
  visualize(obs_stat = diff_orig, direction = "less")

# Calculate the p-value using `get_p_value`
opp_perm %>%
  get_p_value(obs_stat = diff_orig, direction = "less")

# Calculate the p-value using `summarize`
opp_perm %>%
  summarize(p_value = mean(stat <= diff_orig))

# * Calculate the two-sided p-value ----
# The p-value measures the likelihood of data as or more extreme than the
# observed data, given the null hypothesis is true. Therefore, the appropriate
# p-value for a two-sided alternative hypothesis is a two-sided p-value.  Use
# a two sided p value when interested in any difference in the statistic.
#
opp_perm %>%
  summarize(p_value = 2*mean(stat <= diff_orig))

# * Resampling from a sample ----
# Compute p-hat for each poll
ex1_props <- all_polls %>%
  # Group by poll
  group_by(poll) %>%
  # Calculate proportion of yes votes
  summarise(stat = mean(vote == "yes"))

# Review the result
ex1_props

# Select one poll from which to resample
one_poll <- all_polls %>%
  # Filter for the first poll
  filter(poll == 1) %>%
  # Select vote
  select(vote)

# Compute p-hat* for each resampled poll
ex2_props <- one_poll %>%
  # Specify vote as the response, where yes means success
  specify(response = vote, success = "yes") %>%
  # Generate 1000 reps of type bootstrap
  generate(reps = 1000, type = "bootstrap") %>%
  # Calculate the summary stat "prop"
  calculate(stat = "prop")

ex2_props <- all_polls %>%
  filter(poll == 1) %>%
  select(vote) %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop")

# Calculate variability of p-hat
ex1_props %>%
  summarize(
    variability = sd(stat))

# * Visualise the variability of the statistic ----
# # Combine data from both experiments
both_ex_props <- bind_rows(ex1_props, ex2_props, .id = "experiment")

# Using both_ex_props, plot stat colored by experiment
ggplot(both_ex_props, aes(stat, color = experiment)) +
  # Add a density layer with bandwidth 0.1
  geom_density(bw = 0.1)

# * Empirical Rule ----
# 95% of the sample statistics will occur within 2 standard
# errors of the central tendency.  This is called the t interval.
#
# Proportion of yes votes by poll
props <- all_polls %>%
  group_by(poll) %>%
  summarize(prop_yes = mean(vote == "yes"))

# The true population proportion of yes votes
true_prop_yes <- 0.6

# Proportion of polls within 2SE
props %>%
  # Add column: is prop_yes in 2SE of 0.6
  mutate(is_in_conf_int = abs(prop_yes - true_prop_yes) < 2 * sd(prop_yes)) %>%
  # Calculate  proportion in conf int
  summarize(prop_in_conf_int = mean(is_in_conf_int))

# * Bootstrap t confidence interval ----
# These methods work for any statistic and parameter, as long as the following
# technical conditions hold:
#   1) the distribution of the statistic is reasonably symmetric and bell-shaped and
#   2) the sample size is reasonably large

one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)
one_poll_boot <- one_poll %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop")

p_hat <- one_poll %>%
  # Calculate proportion of yes votes
  summarize(stat = mean(vote == "yes")) %>%
  pull()

# Method 1 - Create an interval of plausible values
one_poll_boot %>%
  summarize(
    # Lower bound is p_hat minus 2 std errs
    lower = p_hat - 2*sd(stat),
    # Upper bound is p_hat plus 2 std errs
    upper = p_hat + 2*sd(stat)
  )


# Method 2 - Using quantiles
one_poll_boot %>%
  summarize(
    lower = quantile(stat, 0.025),
    upper = quantile(stat, 0.975)
  )

# Calculate the same interval, more conveniently
percentile_ci <- one_poll_boot %>%
  get_confidence_interval(level = 0.95)

# Review the value
percentile_ci

one_poll_boot %>%
  # Visualize in-between the endpoints given by percentile_ci
  visualize(endpoints = percentile_ci, direction = "between")

calc_t_conf_int <- function(resampled_dataset) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}

# Find the bootstrap t-confidence interval for 30 resamples
calc_t_conf_int(one_poll_boot)

# ... and for 300 resamples
calc_t_conf_int(one_poll_boot_300)

# ... and for 3 resamples
calc_t_conf_int(one_poll_boot_3)

calc_p_hat <- function(dataset) {
  dataset %>%
    summarize(stat = mean(vote == "yes")) %>%
    pull()
}
calc_t_conf_int <- function(resampled_dataset, p_hat) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}

# Find proportion of yes votes from original population
p_hat <- calc_p_hat(one_poll)

# Review the value
p_hat

# Calculate bootstrap t-confidence interval (original 0.6 param)
calc_t_conf_int(one_poll_boot, p_hat)

# Find proportion of yes votes from new population
p_hat_0.8 <- calc_p_hat(one_poll_0.8)

# Review the value
p_hat_0.8

# Calculate the bootstrap t-confidence interval (new 0.8 param)
calc_t_conf_int(one_poll_boot_0.8, p_hat_0.8)


# Percentile effects on CI endpoints ----

# Calculate a 95% bootstrap percentile interval
one_poll_boot %>%
  get_confidence_interval(level = 0.95)

# Calculate a 99% bootstrap percentile interval
one_poll_boot %>%
  get_confidence_interval(level = 0.99)

# Calculate a 90% bootstrap percentile interval
one_poll_boot %>%
  get_confidence_interval(level = 0.90)

# Plot ci_endpoints vs. ci_percent to compare the intervals
ggplot(conf_int_data, aes(y = ci_endpoints, x = ci_percent)) +
  # Add a line layer
  geom_line()

# CATEGORICAL INFERENCE WITH GSS DATA ----

gss2016 <- gss %>%
  filter(year == 2016)

gss %>% glimpse()

gss %>%
  group_by(year, sex) %>%
  summarise(prop_with_degree = mean(college == "degree")) %>%
  drop_na() %>%
  ggplot(aes(x = year, y = prop_with_degree, color = sex)) +
  geom_line()


gss2016 <- gss %>%
  filter(year == 2016)

ggplot(gss2016, aes(x = college)) +
  geom_bar()

# Compute proportion with college degree
p_hat_2016 <- gss2016 %>%
  summarize(prop_high = mean(college == "degree")) %>%
  pull()

p_hat <- gss %>%
  group_by(year) %>%
  summarize(prop_high = mean(college == "degree")) %>%
  pull()

boot1 <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 1, type = "bootstrap")



boot1 <- gss2016 %>%
  specify(response = college, success = "degree") %>%
  generate(reps = 1, type = "bootstrap")

# Using boot1, plot consci
boot1 %>%
  ggplot(aes(college)) +
  # Add bar layer
  geom_bar()

# Compute proportion with degree
boot1 %>%
  summarize(prop_degree = mean(college == "degree")) %>%
  pull()

# Create bootstrap distribution for proportion with degree
boot_dist <- gss2016 %>%
  # Specify the response and success
  specify(response = college, success = "degree") %>%
  # Generate 500 bootstrap reps
  generate(reps = 500, type = "bootstrap") %>%
  # Calculate proportions
  calculate(stat = "prop")

# See the result
boot_dist

# Plot bootstrap distribution of stat
boot_dist %>% ggplot(aes(stat)) +
  # Add density layer
  geom_density()

# Compute the standard error by summarizing the distribution
SE_2016 <- boot_dist %>%
  summarize(se = sd(stat)) %>%
  pull()

# Create CI
c(p_hat_2016 - 2 * SE_2016, p_hat_2016 + 2 * SE_2016)

