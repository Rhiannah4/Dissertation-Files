
#---- Load required libraries ----
install.packages("patchwork")
library(readxl)
library(tidyverse)
library(janitor)
library(psych)
library(dplyr)
library(tidyr)
library(knitr)

#---- Load data ----

biodex_60 <- read_excel("Biodex Test Data.xlsx", sheet = "Biodex_60")
View(biodex_60)

biodex_60  <- clean_names(biodex_60)


#---- Data wrangling ----

ggplot(biodex_60  , aes(x = test, y = right_peak_ext_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Right Max Torque", x = "Test", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

# mutate all columns except id and test to numeric

biodex_60 <- biodex_60 %>%
  mutate(across(-c(id, test), as.numeric))

peak_ext_60_wide <- biodex_60 %>%
  select(id, test, right_peak_ext_torque,left_peak_ext_torque) %>%
  pivot_wider(names_from = test, values_from = c(right_peak_ext_torque, left_peak_ext_torque))
view(peak_ext_60_wide)

# Calculate mean and SD for each numeric column
peak_ext_60_summary <- peak_ext_60_wide %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE))))

# View results
view(peak_ext_60_summary)

peak_flex_60_wide <- biodex_60 %>%
  select(id, test, right_peak_flex_torque,left_peak_flex_torque) %>%
  pivot_wider(names_from = test, values_from = c(right_peak_flex_torque, left_peak_flex_torque))
view(peak_flex_60_wide)

# Calculate mean and SD for each numeric column
peak_flex_60_summary <- peak_flex_60_wide %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE))))

# View results
view(peak_flex_60_summary)


t.test(
  peak_ext_60_wide$`right_peak_ext_torque_Test 2`,
  peak_ext_60_wide$`right_peak_ext_torque_Test 1`,
  paired = TRUE
)

cor.test(
  peak_ext_60_wide$`right_peak_ext_torque_Test 2`,
  peak_ext_60_wide$`right_peak_ext_torque_Test 1`,
  method = "pearson"
)

icc_biodex_60 <- peak_ext_60_wide %>%
  select(`right_peak_ext_torque_Test 2`, `right_peak_ext_torque_Test 1`)
icc_result <- ICC(icc_biodex_60)
print(icc_result)


# Replace these column names with your actual dataset
test1 <- peak_ext_60_wide$`right_peak_ext_torque_Test 1`
test2 <- peak_ext_60_wide$`right_peak_ext_torque_Test 2`


# 1. Calculate differences
diff_scores <- test1 - test2

# 2. Calculate Typical Error (TE)
typical_error <- sd(diff_scores, na.rm = TRUE) / sqrt(2)

# 3. Degrees of freedom
df <- length(diff_scores) - 1

# 4. Confidence interval parameters
alpha <- 0.05  # 95% confidence

# 5. Chi-squared critical values
chi_upper <- qchisq(alpha / 2, df, lower.tail = FALSE)
chi_lower <- qchisq(1 - alpha / 2, df, lower.tail = FALSE)

# 6. Calculate 95% CI for Typical Error
ci_lower <- sqrt(df * typical_error^2 / chi_upper)
ci_upper <- sqrt(df * typical_error^2 / chi_lower)

# 7. Mean of measurements (for TE as %)
mean_value <- mean(c(test1, test2), na.rm = TRUE)

# 8. TE as % of mean
TE_percent <- (typical_error / mean_value) * 100

# 9. Minimal Detectable Change (95% CI)
MDC95 <- typical_error * 1.96 * sqrt(2)   # ≈ 2.77 × TE

# 10. Print results
cat("Typical Error (TE):", round(typical_error, 2), "\n")
cat("TE as % of mean:", round(TE_percent, 2), "%\n")
cat("95% CI for TE: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]\n")
cat("Minimal Detectable Change (MDC95):", round(MDC95, 2), "\n")

# Scatter plot Test 1 vs Test 2

ggplot(peak_ext_60_wide, aes(x = `right_peak_ext_torque_Test 2`, 
                             y = `right_peak_ext_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Test-Retest Reliability: Iso30 Total Force",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  theme_minimal()

#Left Peak Extension force

peak_ext_60_wide <- biodex_60 %>%
  select(id, test, left_peak_ext_torque,right_peak_ext_torque) %>%
  pivot_wider(names_from = test, values_from = c(left_peak_ext_torque, right_peak_ext_torque))
view(peak_ext_60_wide)

t.test(
  peak_ext_60_wide$`left_peak_ext_torque_Test 2`,
  peak_ext_60_wide$`left_peak_ext_torque_Test 1`,
  paired = TRUE
)

cor.test(
  peak_ext_60_wide$`left_peak_ext_torque_Test 2`,
  peak_ext_60_wide$`left_peak_ext_torque_Test 1`,
  method = "pearson"
)

icc_biodex_60 <- peak_ext_60_wide %>%
  select(`left_peak_ext_torque_Test 2`, `left_peak_ext_torque_Test 1`)
icc_result <- ICC(icc_biodex_60)
print(icc_result)


# Replace these column names with your actual dataset
test1 <- peak_ext_60_wide$`left_peak_ext_torque_Test 1`
test2 <- peak_ext_60_wide$`left_peak_ext_torque_Test 2`

# 1. Calculate differences
diff_scores <- test1 - test2

# 2. Calculate Typical Error (TE)
typical_error <- sd(diff_scores, na.rm = TRUE) / sqrt(2)

# 3. Degrees of freedom
df <- length(diff_scores) - 1

# 4. Confidence interval parameters
alpha <- 0.05  # 95% confidence

# 5. Chi-squared critical values
chi_upper <- qchisq(alpha / 2, df, lower.tail = FALSE)
chi_lower <- qchisq(1 - alpha / 2, df, lower.tail = FALSE)

# 6. Calculate 95% CI for Typical Error
ci_lower <- sqrt(df * typical_error^2 / chi_upper)
ci_upper <- sqrt(df * typical_error^2 / chi_lower)

# 7. Mean of measurements (for TE as %)
mean_value <- mean(c(test1, test2), na.rm = TRUE)

# 8. TE as % of mean
TE_percent <- (typical_error / mean_value) * 100

# 9. Minimal Detectable Change (95% CI)
MDC95 <- typical_error * 1.96 * sqrt(2)   # ≈ 2.77 × TE

# 10. Print results
cat("Typical Error (TE):", round(typical_error, 2), "\n")
cat("TE as % of mean:", round(TE_percent, 2), "%\n")
cat("95% CI for TE: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]\n")
cat("Minimal Detectable Change (MDC95):", round(MDC95, 2), "\n")



# Scatter plot Test 1 vs Test 2

ggplot(peak_ext_60_wide, aes(x = `left_peak_ext_torque_Test 2`, 
                             y = `left_peak_ext_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Test-Retest Reliability: Iso30 Total Force",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  theme_minimal()

#Right peak flexion force
peak_flex_60_wide <- biodex_60 %>%
  select(id, test, right_peak_flex_torque,left_peak_flex_torque) %>%
  pivot_wider(names_from = test, values_from = c(right_peak_flex_torque, left_peak_flex_torque))
view(peak_flex_60_wide)


t.test(
  peak_flex_60_wide$`right_peak_flex_torque_Test 2`,
  peak_flex_60_wide$`right_peak_flex_torque_Test 1`,
  paired = TRUE
)

cor.test(
  peak_flex_60_wide$`right_peak_flex_torque_Test 2`,
  peak_flex_60_wide$`right_peak_flex_torque_Test 1`,
  method = "pearson"
)

icc_biodex_60 <- peak_flex_60_wide %>%
  select(`right_peak_flex_torque_Test 2`, `right_peak_flex_torque_Test 1`)
icc_result <- ICC(icc_biodex_60)
print(icc_result)


# Replace these column names with your actual dataset
test1 <- peak_flex_60_wide$`right_peak_flex_torque_Test 1`
test2 <- peak_flex_60_wide$`right_peak_flex_torque_Test 2`

# 1. Calculate differences
diff_scores <- test1 - test2

# 2. Calculate Typical Error (TE)
typical_error <- sd(diff_scores, na.rm = TRUE) / sqrt(2)

# 3. Degrees of freedom
df <- length(diff_scores) - 1

# 4. Confidence interval parameters
alpha <- 0.05  # 95% confidence

# 5. Chi-squared critical values
chi_upper <- qchisq(alpha / 2, df, lower.tail = FALSE)
chi_lower <- qchisq(1 - alpha / 2, df, lower.tail = FALSE)

# 6. Calculate 95% CI for Typical Error
ci_lower <- sqrt(df * typical_error^2 / chi_upper)
ci_upper <- sqrt(df * typical_error^2 / chi_lower)

# 7. Mean of measurements (for TE as %)
mean_value <- mean(c(test1, test2), na.rm = TRUE)

# 8. TE as % of mean
TE_percent <- (typical_error / mean_value) * 100

# 9. Minimal Detectable Change (95% CI)
MDC95 <- typical_error * 1.96 * sqrt(2)   # ≈ 2.77 × TE

# 10. Print results
cat("Typical Error (TE):", round(typical_error, 2), "\n")
cat("TE as % of mean:", round(TE_percent, 2), "%\n")
cat("95% CI for TE: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]\n")
cat("Minimal Detectable Change (MDC95):", round(MDC95, 2), "\n")

ggplot(peak_flex_60_wide, aes(x = `right_peak_flex_torque_Test 2`, 
                             y = `right_peak_flex_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Test-Retest Reliability: Iso30 Total Force",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  theme_minimal()

#Left peak flexion force
peak_flex_60_wide <- biodex_60 %>%
  select(id, test, left_peak_flex_torque,right_peak_flex_torque) %>%
  pivot_wider(names_from = test, values_from = c(left_peak_flex_torque, right_peak_flex_torque))
view(peak_flex_60_wide)


t.test(
  peak_flex_60_wide$`left_peak_flex_torque_Test 2`,
  peak_flex_60_wide$`left_peak_flex_torque_Test 1`,
  paired = TRUE
)

cor.test(
  peak_flex_60_wide$`left_peak_flex_torque_Test 2`,
  peak_flex_60_wide$`left_peak_flex_torque_Test 1`,
  method = "pearson"
)

icc_biodex_60 <- peak_flex_60_wide %>%
  select(`left_peak_flex_torque_Test 2`, `left_peak_flex_torque_Test 1`)
icc_result <- ICC(icc_biodex_60)
print(icc_result)


# Replace these column names with your actual dataset
test1 <- peak_flex_60_wide$`left_peak_flex_torque_Test 1`
test2 <- peak_flex_60_wide$`left_peak_flex_torque_Test 2`

# Replace these column names with your actual dataset
test1 <- peak_flex_60_wide$`left_peak_flex_torque_Test 1`
test2 <- peak_flex_60_wide$`left_peak_flex_torque_Test 2`

# 1. Calculate differences
diff_scores <- test1 - test2

# 2. Calculate Typical Error (TE)
typical_error <- sd(diff_scores, na.rm = TRUE) / sqrt(2)

# 3. Degrees of freedom
df <- length(diff_scores) - 1

# 4. Confidence interval parameters
alpha <- 0.05  # 95% confidence

# 5. Chi-squared critical values
chi_upper <- qchisq(alpha / 2, df, lower.tail = FALSE)
chi_lower <- qchisq(1 - alpha / 2, df, lower.tail = FALSE)

# 6. Calculate 95% CI for Typical Error
ci_lower <- sqrt(df * typical_error^2 / chi_upper)
ci_upper <- sqrt(df * typical_error^2 / chi_lower)

# 7. Mean of measurements (for TE as %)
mean_value <- mean(c(test1, test2), na.rm = TRUE)

# 8. TE as % of mean
TE_percent <- (typical_error / mean_value) * 100

# 9. Minimal Detectable Change (95% CI)
MDC95 <- typical_error * 1.96 * sqrt(2)   # ≈ 2.77 × TE

# 10. Print results
cat("Typical Error (TE):", round(typical_error, 2), "\n")
cat("TE as % of mean:", round(TE_percent, 2), "%\n")
cat("95% CI for TE: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]\n")
cat("Minimal Detectable Change (MDC95):", round(MDC95, 2), "\n")


library(ggplot2)
library(patchwork)

# Calculate the global y-axis limits across all the data
y_min <- min(c(min(biodex_60$right_peak_ext_torque), min(biodex_60$left_peak_ext_torque),
               min(biodex_60$right_peak_flex_torque), min(biodex_60$left_peak_flex_torque)))
y_max <- max(c(max(biodex_60$right_peak_ext_torque), max(biodex_60$left_peak_ext_torque),
               max(biodex_60$right_peak_flex_torque), max(biodex_60$left_peak_flex_torque)))

# Right Max Extension Torque
p1 <- ggplot(biodex_60, aes(x = test, y = right_peak_ext_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Right Max Extension Torque", x = "", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ylim(y_min, y_max)

# Left Max Extension Torque
p2 <- ggplot(biodex_60, aes(x = test, y = left_peak_ext_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Left Max Extension Torque", x = "", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ylim(y_min, y_max)

# Right Max Flexion Torque
p3 <- ggplot(biodex_60, aes(x = test, y = right_peak_flex_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Right Max Flexion Torque", x = "", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ylim(y_min, y_max)

# Left Max Flexion Torque
p4 <- ggplot(biodex_60, aes(x = test, y = left_peak_flex_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Left Max Flexion Torque", x = "", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ylim(y_min, y_max)

# Arrange the 2x2 plots with an overall title
(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Biodex 60°/Second",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# Calculate the global y-axis limits for extension and flexion separately
y_min_extension <- min(c(min(biodex_60$right_peak_ext_torque), min(biodex_60$left_peak_ext_torque)))
y_max_extension <- max(c(max(biodex_60$right_peak_ext_torque), max(biodex_60$left_peak_ext_torque)))

y_min_flexion <- min(c(min(biodex_60$right_peak_flex_torque), min(biodex_60$left_peak_flex_torque)))
y_max_flexion <- max(c(max(biodex_60$right_peak_flex_torque), max(biodex_60$left_peak_flex_torque)))

# Right Max Extension Torque
p1 <- ggplot(biodex_60, aes(x = test, y = right_peak_ext_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Right Max Extension Torque", x = "", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ylim(y_min_extension, y_max_extension)  # Apply y-axis limits for extension

# Left Max Extension Torque
p2 <- ggplot(biodex_60, aes(x = test, y = left_peak_ext_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Left Max Extension Torque", x = "", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ylim(y_min_extension, y_max_extension)  # Apply y-axis limits for extension

# Right Max Flexion Torque
p3 <- ggplot(biodex_60, aes(x = test, y = right_peak_flex_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Right Max Flexion Torque", x = "", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ylim(y_min_flexion, y_max_flexion)  # Apply y-axis limits for flexion

# Left Max Flexion Torque
p4 <- ggplot(biodex_60, aes(x = test, y = left_peak_flex_torque)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Left Max Flexion Torque", x = "", y = "Torque (Nm)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ylim(y_min_flexion, y_max_flexion)  # Apply y-axis limits for flexion

# Arrange the 2x2 plots with an overall title
(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Biodex 60°/Second",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

p1 <- ggplot(peak_flex_60_wide, aes(x = `right_peak_flex_torque_Test 2`, 
                              y = `right_peak_flex_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Right Max Flexion Torque",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) + # Remove grid lines
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

p2 <- ggplot(peak_flex_60_wide, aes(x = `left_peak_flex_torque_Test 2`, 
                                         y = `left_peak_flex_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Left Max Flexion Torque",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) + # Remove grid lines
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

p3 <- ggplot(peak_ext_60_wide, aes(x = `right_peak_ext_torque_Test 2`, 
                                    y = `right_peak_ext_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Right Max Extension Torque",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) + # Remove grid lines
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

p4 <- ggplot(peak_ext_60_wide, aes(x = `left_peak_ext_torque_Test 2`, 
                                    y = `left_peak_ext_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Left Max Flexion Torque",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) + # Remove grid lines
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

# Arrange the 2x2 plots with an overall title
(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Biodex 60°/Second",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )


# Arrange the 2x2 plots with an overall title
(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Biodex 60°/Second",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )


# Define a consistent theme
theme_settings <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    legend.position = "none"
  )

# Plot 1: Flexion
p1 <- ggplot(peak_flex_60_wide, aes(x = `right_peak_flex_torque_Test 2`, 
                                    y = `right_peak_flex_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "60°/s Right Flex Torque",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  theme_settings 

# Plot 2: Extension
p2 <- ggplot(peak_ext_60_wide, aes(x = `right_peak_ext_torque_Test 2`, 
                                   y = `right_peak_ext_torque_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "60°/s Right Ext Torque",
       x = "Test 2 Peak Torque (Nm)",
       y = "Test 1 Peak Torque (Nm)") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  theme_settings 


# Plot 3: Kicker
p3 <- ggplot(kicker_wide, aes(x = `r_max_force_n_Test 2`, 
                              y = `r_max_force_n_Test 1`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Kicker Right Leg",
       x = "Test 2 Total Force (N)", 
       y = "Test 1 Total Force (N)") +
  theme_settings 
  

# Plot 4: Isometric
p4 <- ggplot(nord_wide, aes(x = Test2, y = Test1)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Isometric 30",
       x = "Test 2 Total Force (N)",
       y = "Test 1 Total Force (N)") +
  theme_settings 

# Combine into 2x2 layout
(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )