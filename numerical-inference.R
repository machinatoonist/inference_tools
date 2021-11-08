# From DataCamp Course: Inference for Numerical Data in R

# Bootstrapping for estimating a parameter ----

library(infer)
library(tidyverse)
library(openintro)

gss %>% glimpse()
data("us_rent_income")

# Simulate a sample ----
# Simulate a sample of rental data for Manhattan.  Assume lognormal distribution
# That is, the log(rent) values are normally distributed.
# 1) Create normally distributed log(rent values)
# 2) Take the exp(log(rent)) to extract the rent estimates
manhattan <- tibble(rent = round(exp(rnorm(20, mean = 7.76, sd = .25)), 0))

true_mean <- exp(7.76)
true_mean

true_sd <- exp(.25)
true_sd

manhattan

manhattan %>%
  ggplot(aes(rent)) +
  geom_histogram()

# Generate bootstrap distributions of medians ----
rent_med_ci <- manhattan %>%
  specify(response = rent) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "median")

str(rent_med_ci)

head(rent_med_ci)
tail(rent_med_ci)

rent_med_ci %>%
  ggplot(aes(stat)) +
  geom_histogram(binwidth = 50)

# Calculate bootstrap 95% confidence intervals (CI) ----
# * Percentile Method ----
rent_med_ci %>%
  summarize(
    l = quantile(stat, p = 0.025),
    u = quantile(stat, p = 0.975)
  )

# * Standard Error Method ----
# Calculate the observed median rent ----
rent_med_obs <- manhattan %>%
  summarise(median_rent = median(rent)) %>%
  pull()

rent_med_obs

degrees_of_freedom <- nrow(manhattan) - 1

degrees_of_freedom

# Determine the critical value for a 95% confidence interval ----
t_star <- qt(p = 0.975, degrees_of_freedom)

t_star

# Calculate the 95% bootstrap CI using the standard error method ----
# This method is more accurate than the percentil method
rent_med_ci %>%
  # Calculate the standard error of the statistic
  summarize(boot_se = sd(stat)) %>%
  # Calculate the lower and upper limits of the CI
  summarize(
    l = rent_med_obs - t_star * boot_se,
    u = rent_med_obs + t_star * boot_se
  )

# Doctor visits during pregnancy ----

data("ncbirths")

ncbirths

ncbirths %>% glimpse()
ncbirths %>%
  summarize_at(vars(visits), list(mean))
?summarize_at()

ncbirths %>%
  summarize(across(visits, ~ mean(.x, na.rm = TRUE)))

ncbirths %>%
  ggplot(aes(visits)) +
  geom_histogram()

ncbirths %>%
  summarize(across(visits, ~ median(.x, na.rm = TRUE)))

ncbirths %>%
  count(visits)

ncbirths %>%
  filter(is.na(visits))

ncbirths_complete_visits <- ncbirths %>%
  filter(!is.na(visits))

visits_mean_ci <- ncbirths %>%
  specify(response = visits) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "mean")

# Calculate 90% CI using percentile method ----
visits_mean_ci %>%
  summarize(
    l = quantile(stat, probs = 0.025),
    u = quantile(stat, probs = 0.975)
  )

# Calculate 15000 bootstrap standard deviations of visits
visit_sd_ci <- ncbirths_complete_visits %>%
  specify(response = visits) %>%
  generate(15000, type = "bootstrap") %>%
  calculate(stat = "sd")

visit_sd_ci

# * Percentile Method 90% CI ----
visit_sd_ci %>%
  summarize(
    l = quantile(stat, p = 0.05),
    u = quantile(stat, p = 0.95)
  )

# Hypothesis Testing ----

# * Evidence of median rent being above a threshold ----
# Evaluate whether sample data provides evidence that median rent > $2800

n_replicates <- 15000

rent_med_ht <- manhattan %>%
  specify(response = rent) %>%
  hypothesise(null = "point", med = 2800) %>%
  generate(reps = n_replicates, type = "bootstrap") %>%
  calculate(stat = "median")

rent_med_ht

rent_med_obs <- manhattan %>%
  summarise(median_rent = median(rent)) %>%
  pull()

# p-value = The proportion of simulations that yield a sample statistic at least
# as favourable to the alternative hypothesis as the observed sample statistic
rent_med_ht %>%
  # Filter for bootstrap stat greater than or equal to observed stat
  filter(stat >= rent_med_obs) %>%
  # Calculate p-value
  summarise(p_val = n()/nrow(rent_med_ht))

# * Test for average weight of babies ----
n_replicates <- 1500

weight_mean_ht <- ncbirths %>%
  specify(response = weight) %>%
  hypothesise(null = "point", mu = 7.0) %>%
  generate(1500, type = "bootstrap") %>%
  calculate(stat = "mean")

weight_mean_ht

weight_mean_obs <- ncbirths %>%
  summarise(mean_weight = mean(weight)) %>%
  pull()

# Calculate p-value
weight_mean_ht %>%
  filter(stat >= weight_mean_obs) %>%
  # p-value
  summarise(
    one_sided_p_val = n()/n_replicates,
    two_sided_p_val = 2 * one_sided_p_val
  )

# These data do not provide convincing evidence that the average birth weight
# is not 7 pounds

# Probabilities under the t-distribution ----
# * Probability under the t-distribution
# P(T < 3) for df = 10
(x <- pt(3, df = 10))

# P(T > 3) for df = 100
(z <- 1 - x)

# P(T > 3) for df = 100
(z <- 1 - pt(3, df = 100))

# * Cutoffs under the t-distribution
# For a given probability p and a given degrees of freedom (df)
# qt(p, df) gives us the cutoff value for the t-distribution with df degrees
# of freedom for which the probability under the curve is p
?pt()

# Student t-distrubution functions ----
# * dt() - density
# * pt() - distribution function
# * qt() - quantile function
# * rt() - random deviates
# 95th percentile for df = 10
(x <- qt(0.95, df = 10))

# Upper bound of middle 95th percentile for df = 10
(y <- qt(.975, df = 10))

# Upper bound of middle 95th percentile for df = 100
(z <- qt(.975, 100))

y == z
y >z
y<z

# Estimating a mean with a t-interval ----
# Central Limit Theorem
# SE (standard error) = standard deviation of the sampling distribution
# population standard deviation is unknown
# SE = s/sqrt(n)
# Use t(df = n-1) for inference a the mean
# Conditions:
# 1) Independent observations
#  1.1) random sampling
#  1.2) if sampling without replacement, n < 10% of population
# 2) Sample size/skew.  The more skewed the original population, the larger
#    the sample size should be

gss %>% glimpse()
data("gss2010")
gss2010 %>% glimpse()
data("gss_cat")
gss_cat
data("acs12")

acs12 %>% glimpse()

acs12 %>%
  count(employment, sort = TRUE)

acs12_emp <- acs12 %>%
  filter(employment == "employed")

?t.test()
t.test(acs12_emp$time_to_work, conf.level = 0.99)

t.test(acs12_emp$time_to_work)

t.test(acs12_emp$hrs_work)

acs12_emp %>% glimpse()

acs12_emp %>%
  ggplot(aes(hrs_work)) +
  geom_histogram()

# The mean difference coming from two dependent groups ----
# For example the results of scores in different subjects are not independent
# eg: A student scoring well in reading is more likely to score well in writing
textbooks
t.test(textbooks$diff, conf.level = 0.9)

t.test(textbooks$diff, conf.level = 0.95)

t.test(textbooks$diff, conf.level = 0.99)

textdiff_med_sci <- textbooks %>%
  specify(response = diff) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "median")

# Calculate the 95% CI using the percentile method
textdiff_med_sci %>%
  summarise(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  )

hsb2

hsb2_diff <- hsb2 %>%
  mutate(diff = math - science)

n_replicates <- 15000

scorediff_med_ht <- hsb2_diff %>%
  specify(response = diff) %>%
  hypothesise(null = "point", med = 0) %>%
  generate(reps = n_replicates, type = "bootstrap") %>%
  calculate(stat = "median")

scorediff_med_ht

# Test for difference in median test scores ----
n_replicates <- 15000
hsb2_tbl <- hsb2 %>%
  mutate(diff = math - science)

scorediff_med_ht <- hsb2_tbl %>%
  specify(response = diff) %>%
  hypothesize(null = "point", med = 0) %>%
  generate(reps = n_replicates, type = "bootstrap") %>%
  calculate(stat = "median")

scorediff_med_obs <- hsb2_tbl %>%
  summarize(median_diff = median(diff)) %>%
  pull()

# Calculate two-sided p-value ----
# A p-value is the probability of observing data at least as extreme as yours,
# given the null hypothesis is true
scorediff_med_ht %>%
  filter(stat >= scorediff_med_obs) %>%
  summarize(
    one_sided_p_val = n()/n_replicates,
    two_sided_p_val = 2 * one_sided_p_val
  )

# Hypothesis testing for comparing two means ----
# Does a treatment using embryonic stem cells help improve heart function
# following a heart attack more than traditional therapy?
data("stem_cell")
stem_cell

diff_mean_ht <- stem_cell %>%
  mutate(change = after - before) %>%
  specify(formula = change ~ trmt) %>%
  # H0: mu_esc = mu_ctrl
  # HA: mu_esc > mu_ctrl
  hypothesise(null = "independence") %>% # response and explanatory variables are independent
  generate(reps = n_replicates, type = "permute") %>% # permute labels
  calculate(stat = "diff in means", order = c("esc", "ctrl")) # type of statistic to calculate

# Actuals
stem_cell_tbl <- stem_cell %>%
  mutate(change = after - before)

diff_mean <- stem_cell_tbl %>%
  group_by(trmt) %>%
  summarize(mean_change = mean(change)) %>%
  pull() %>%
  diff()

?diff()
diff_mean

# The data provide convincing evidence of a difference in means.
# There is a 0.01% chance that the difference occurred by chance.
# Significance 99.9%
diff_mean_ht %>%
  filter(stat > diff_mean) %>%
  summarise(p_val = n()/n_replicates)

# Evaluate the effect of smoking during pregnancy ----
ncbirths %>%
  glimpse()

ncbirths_complete_habit <- ncbirths %>%
  filter(!is.na(habit))

diff_mean_obs <- ncbirths_complete_habit %>%
  group_by(habit) %>%
  summarize(mean_weight = mean(weight)) %>%
  pull() %>%
  diff()

# Generate 1000 differences by randomisation
n_replicates <- 1000

diff_mean_ht <- ncbirths_complete_habit %>%
  specify(response = weight, explanatory = habit) %>%
  hypothesise(null = "independence") %>%
  generate(reps = n_replicates, type = "permute") %>%
  calculate(stat = "diff in means", order = c("nonsmoker", "smoker"))

diff_mean_ht

# * Calculate p-value ----
diff_mean_ht %>%
  # Identify simulated statistic at least as extreme as observed
  filter(stat <= diff_mean_obs) %>%
  summarise(
    one_sided_p_val = n()/n_replicates,
    two_sided_p_val = 2 * one_sided_p_val
  )

# * Construct a confidence interval ----
diff_mean_ci <- ncbirths_complete_habit %>%
  specify(formula = weight ~ habit) %>%
  generate(reps = 1500, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("nonsmoker", "smoker"))
# * Quantile method ----
diff_mean_ci %>%
  summarize(
    l = quantile(stat, p = 0.025),
    u = quantile(stat, p = 0.975)
  )

# * Standard Error Method ----
degrees_of_freedom <- nrow(ncbirths_complete_habit) - 1

degrees_of_freedom

t_star <- qt(p = 0.975, degrees_of_freedom)

t_star

diff_mean_ci %>%
  # Calculate the standard error of the statistic
  summarize(boot_se = sd(stat)) %>%
  # Calculate the lower and upper limits of the CI
  summarize(
    l = -diff_mean_obs - t_star * boot_se,
    u = -diff_mean_obs + t_star * boot_se
  )

# Median length of pregnancies for smoking/non-smoking mothers ----
ncbirths_complete_habit_weeks <- ncbirths %>%
  filter(!is.na(habit), !is.na(weeks))

# * Bootstrap difference in medians ----
diff_med_ci <- ncbirths_complete_habit_weeks %>%
  specify(formula = weeks ~ habit) %>%
  generate(reps = 1500, type = "bootstrap") %>%
  calculate(stat = "diff in medians", order = c("nonsmoker", "smoker"))

# * Confidence interval ----
diff_med_ci %>%
  summarize(
    l = quantile(stat, p = 0.025),
    u = quantile(stat, p = 0.975)
  )

# * Standard Error Method ----
diff_median_obs <- ncbirths_complete_habit_weeks %>%
  group_by(habit) %>%
  summarize(median = median(weeks)) %>%
  pull() %>%
  diff()
diff_median_obs

degrees_of_freedom <- nrow(ncbirths_complete_habit_weeks) - 1

degrees_of_freedom

t_star <- qt(p = 0.975, degrees_of_freedom)

t_star

diff_med_ci %>%
  # Calculate the standard error of the statistic
  summarize(boot_se = sd(stat)) %>%
  # Calculate the lower and upper limits of the CI
  summarize(
    l = diff_median_obs - t_star * boot_se,
    u = diff_median_obs + t_star * boot_se
  )

# Comparing means with a t-test ----
# American Community Survey (ACS)
acs12_tbl <- acs12 %>%
  mutate(hrly_rate = income/(hrs_work * 52))

acs12_tbl %>%
  filter(!is.na(hrly_rate)) %>%
  group_by(citizen) %>%
  summarise(
    x_bar = round(mean(hrly_rate), 2),
    s = round(sd(hrly_rate), 2),
    n = length(hrly_rate)
  )

t.test(hrly_rate ~ citizen, data = acs12_tbl, null = 0,
       alternative = "two.sided")

# No significant difference in income