# From DataCamp Course: Inference for Numerical Data in R

# Bootstrapping for estimating a parameter ----

library(infer)
library(tidyverse)

gss %>% glimpse()
data("us_rent_income")

# Simulate a sample ----
# Simulate a sample of rental data for Manhattan.  Assume lognormal distribution
# That is, the log(rent) values are normally distributed.
# 1) Create normally distributed log(rent values)
# 2) Take the exp(log(rent)) to extract the rent estimates
manhattan <- tibble(rent = round(exp(rnorm(20, mean = 7.76, sd = .25)), 0))

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
library(openintro)

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
