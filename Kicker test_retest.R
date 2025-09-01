#---- Load required libraries ----

library(tidyverse)
library(readxl)
library(psych)
library(janitor)

#---- Load data ----
kicker <- read_csv("Kicker Test Data copy.csv")

#---- Data wrangling ----

kicker <- clean_names(kicker) %>%
  select(name, date_utc, l_max_force_n, r_max_force_n) 

head(kicker)

# Find the first date for each participant and # add it as a new column

kicker <- kicker %>%
  group_by(name) %>%
  mutate(first_date = min(date_utc)) %>%
  ungroup()

# Create a new column with the first data as "Test 1" and the second data as "Test 2"

kicker <- kicker %>%
  mutate(test = ifelse(date_utc == first_date, "Test1", "Test2")) %>%
  arrange(name, date_utc)
# Convert the test column to a factor
kicker$test <- factor(kicker$test, levels = c("Test1", "Test2"))

# we will want long format at some point 

kicker_long <- kicker %>%
  pivot_longer(cols = c(l_max_force_n, r_max_force_n), 
               names_to = "Test", 
               values_to = "Value")

view(kicker_long)

#---- Visualise the data ----

# Boxplot with jittered points for Kicker

l1 = ggplot(kicker, aes(x = test, y = l_max_force_n)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Kicker Left Max Force", x = "Test", y = "Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "none",
    panel.grid = element_blank()  # Remove gridlines
  )
print(l)

l2 = ggplot(kicker, aes(x = test, y = r_max_force_n)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Kicker Right Max Force", x = "Test", y = "Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "none",
    panel.grid = element_blank()  # Remove gridlines
  )
print(l)

l1 + l2

#---- Scatter plot ----

ggplot(kicker, aes(x = l_max_force_n, y = r_max_force_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Left Max Force (N)", y = "Right Max Force (N)") +
  theme_minimal() +
  legend.position = "none"
panel.grid = element_blank()  # Remove gridlines


# Combine left + right for each row
kicker <- kicker %>%
  mutate(total_force = l_max_force_n + r_max_force_n)

# Pivot wider so each participant has Test 1 and Test 2 in separate columns
kicker_wide <- kicker %>%
  select(name, test, total_force) %>%
  pivot_wider(names_from = test, values_from = total_force)

view(kicker_wide)

ggplot(kicker, aes(x = test, y = total_force)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Left Max Force", x = "Test", y = "Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

# Scatter plot Test 1 vs Test 2
ggplot(kicker_wide, aes(x = Test1, y = Test2)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Test-Retest Reliability: Kicker Test Total Force",
       x = "Test 1 Total Force (N)",
       y = "Test 2 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

# Paired t-test
t_test_result <- t.test(kicker_wide$Test1, kicker_wide$Test2, paired = TRUE)
print(t_test_result)

# Pearson's correlation
correlation_result <- cor.test(kicker_wide$Test1, kicker_wide$Test2, method = "pearson")
print(correlation_result)

# Select only the Test 1 and Test 2 columns

icc_kicker<- kicker_wide %>%
  select(Test1, Test2)

icc_result <- ICC(icc_kicker)
print(icc_result)

#---- Calculate Typical Error ----

# Calculate differences
diff_scores <- kicker_wide$Test1 - kicker_wide$Test2

# Typical Error
typical_error <- sd(diff_scores, na.rm = TRUE) / sqrt(2)
print(typical_error)

#---- Calculate SEM ----
SEM <- typical_error
print(paste("Standard Error of Measurement (SEM):", round(SEM, 2)))

#---- Calculate confidence intervals for typical error ----
lower_bound <- typical_error - 1.96 * typical_error
upper_bound <- typical_error + 1.96 * typical_error

print(paste("Confidence Interval for Typical Error: [", round(lower_bound, 2), ", ", round(upper_bound, 2), "]", sep = ""))

#--- calculate Minimal Detectable Change ---
MDC95 <- SEM * 1.96 * sqrt(2)
print(paste("Minimal Detectable Change (95% CI):", round(MDC95, 2)))


# Assign test values
test1 <- kicker_wide$Test1
test2 <- kicker_wide$Test2

# 1. Calculate differences
diff_scores <- test1 - test2

# 2. Calculate Typical Error (TE)
typical_error <- sd(diff_scores, na.rm = TRUE) / sqrt(2)

# 3. Mean of both tests combined
mean_total <- mean(c(test1, test2), na.rm = TRUE)

# 4. TE as % of mean
te_percent_mean <- (typical_error / mean_total) * 100

# 5. Degrees of freedom
df <- length(na.omit(diff_scores)) - 1

# 6. 95% CI for TE
alpha <- 0.05
chi_upper <- qchisq(alpha / 2, df, lower.tail = FALSE)
chi_lower <- qchisq(1 - alpha / 2, df, lower.tail = FALSE)

ci_lower <- sqrt(df * typical_error^2 / chi_upper)
ci_upper <- sqrt(df * typical_error^2 / chi_lower)

# 7. Minimal Detectable Change (MDC @ 95%)
MDC95 <- typical_error * 1.96 * sqrt(2)

# 8. Print results
cat("Typical Error (TE):", round(typical_error, 2), "\n")
cat("95% CI for TE: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]\n")
cat("TE as % of Mean:", round(te_percent_mean, 2), "%\n")
cat("Minimal Detectable Change (MDC95):", round(MDC95, 2), "N\n")

