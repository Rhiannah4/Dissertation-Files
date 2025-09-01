#---- Load required libraries ----

library(tidyverse)
library(readxl)
library(psych)
library(janitor)

#---- Load data ----

nord <- read_csv("~/Library/CloudStorage/OneDrive-Personal/Teesside University/Semester 2/R Studio/Work Area (D)/Iso 30 Test Data copy.csv")


#---- Data wrangling ----

nord <- clean_names(nord) %>%
  select(name, date_utc, l_max_force_n, r_max_force_n) 

head(nord)

# Find the first date for each participant and # add it as a new column

nord <- nord %>%
  group_by(name) %>%
  mutate(first_date = min(date_utc)) %>%
  ungroup()

# Create a new column with the first data as "Test 1" and the second data as "Test 2"

nord <- nord %>%
  mutate(test = ifelse(date_utc == first_date, "Test1", "Test2")) %>%
  arrange(name, date_utc)
# Convert the test column to a factor
nord$test <- factor(nord$test, levels = c("Test1", "Test2"))

# we will want long format at some point 

nord_long <- nord %>%
  pivot_longer(cols = c(l_max_force_n, r_max_force_n), 
               names_to = "Test", 
               values_to = "Value")

view(nord_long)

#---- Visualise the data ----

# Boxplot with jittered points for Iso30

l = ggplot(nord, aes(x = test, y = l_max_force_n)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Left Max Force", x = "Test", y = "Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

#---- Scatter plot ----

ggplot(nord, aes(x = l_max_force_n, y = r_max_force_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Scatter Plot of Iso30 Max Force", x = "Left Max Force (N)", y = "Right Max Force (N)") +
  theme_minimal()

# Combine left + right for each row
nord <- nord %>%
  mutate(total_force = l_max_force_n + r_max_force_n)

# Pivot wider so each participant has Test 1 and Test 2 in separate columns
nord_wide <- nord %>%
  select(name, test, total_force) %>%
  pivot_wider(names_from = test, values_from = total_force)

view(nord_wide)

boxplot_plot <- ggplot(nord, aes(x = test, y = total_force)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "", x = "Test", y = "Total Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "none",
    panel.grid = element_blank()  # Remove gridlines
  )


# Scatter plot Test 1 vs Test 2 with perfect correlation line
scatter_plot <- ggplot(nord_wide, aes(x = Test1, y = Test2)) +
  geom_point(size = 3, alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Perfect correlation line
  labs(title = "",
       x = "Test 1 Total Force (N)",
       y = "Test 2 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_equal()  # keeps x and y scales equal for better visual comparison

# Combine the box plot above the scatter plot using patchwork and add labels "a)" and "b)"
boxplot_plot / scatter_plot +
  plot_annotation(
    title = "Isometric30 Force Test Reliability",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)),
    tag_levels = c('a', 'b')  # Adds "a)" for the boxplot and "b)" for the scatter plot
  )

# Save the plot with custom dimensions
ggsave("iso30_plot.png", 
       plot = boxplot_plot / scatter_plot + 
         plot_annotation(
           title = "Isometric30 Force Test Reliability",
           theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
         ),
       width = 14,   # Width of the plot in inches
       height = 12,  # Height of the plot in inches
       dpi = 300)    # Resolution (dots per inch)


# Box plot (total_force) with jittered points
boxplot_plot <- ggplot(nord, aes(x = test, y = total_force)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "", x = "Test", y = "Total Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "none",
    panel.grid = element_blank()  # Remove gridlines
  ) +
  # Manually add "a)" label to the top-left of the box plot
  annotate("text", x = -0.3, y = max(nord$total_force) + 50, label = "a)", hjust = 0, size = 4)

# Scatter plot Test 1 vs Test 2 with perfect correlation line
scatter_plot <- ggplot(nord_wide, aes(x = Test1, y = Test2)) +
  geom_point(size = 3, alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Perfect correlation line
  labs(title = "",
       x = "Test 1 Total Force (N)",
       y = "Test 2 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_equal() +  # keeps x and y scales equal for better visual comparison
  # Manually add "b)" label to the top-left of the scatter plot
  annotate("text", x = min(nord_wide$Test1) - 0.2, y = max(nord_wide$Test2) + 200, label = "b)", hjust = 0, size = 4)

# Arrange the 2x2 plots with an overall title
(boxplot_plot | scatter_plot) 
  plot_annotation(
    title = "",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )
  
# Save the plot with custom dimensions
  ggsave("iso30_test_plot.png", 
         plot = final_plot,
         width = 14,   # Width of the plot in inches
         height = 7,   # Height of the plot in inches (side-by-side)
         dpi = 300)    # Resolution (dots per inch)
  
  # Box plot (total_force) with jittered points
  boxplot_plot <- ggplot(nord, aes(x = test, y = total_force)) +
    geom_boxplot(aes(fill = test), alpha = 0.5) +
    geom_jitter(width = 0.2, aes(color = test), size = 2) +
    labs(title = "", x = "Test", y = "Total Force (N)") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    theme(
      legend.position = "none",
      panel.grid = element_blank()  # Remove gridlines
    ) +
    # Manually add "a)" label above the box plot (above the maximum value of the y-axis)
    annotate("text", x = min(nord_wide$Test1) - 0.2, y = max(nord$total_force) + 200, label = "a)", hjust = 0.5, size = 3)  # Adjust position above plot
  
  # Scatter plot Test 1 vs Test 2 with perfect correlation line
  scatter_plot <- ggplot(nord_wide, aes(x = Test1, y = Test2)) +
    geom_point(size = 3, alpha = 0.6, color = "darkblue") +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Perfect correlation line
    labs(title = "",
         x = "Test 1 Total Force (N)",
         y = "Test 2 Total Force (N)") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_equal() +  # keeps x and y scales equal for better visual comparison
    # Manually add "b)" label to the top-left of the scatter plot
    annotate("text", x = min(nord_wide$Test1) - 0.2, y = max(nord_wide$Test2) + 200, label = "b)", hjust = 0.5, size = 3)  # Position above the plot
  
  # Combine the box plot and scatter plot side by side using patchwork
  final_plot <- (boxplot_plot | scatter_plot) +
    plot_annotation(
      title = "Isometric30 Force Test Reliability",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  # Save the plot with custom dimensions
  ggsave("iso30_test_plot.png", 
         plot = final_plot,
         width = 14,   # Width of the plot in inches
         height = 7,   # Height of the plot in inches (side-by-side)
         dpi = 300)    # Resolution (dots per inch)

# Box plot (total_force) with jittered points
boxplot_plot <- ggplot(nord, aes(x = test, y = total_force)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "", x = "Test", y = "Total Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "none",
    panel.grid = element_blank()  # Remove gridlines
  ) +
  # Manually add "a)" label to the left of the box plot
  annotate("text", x = -0.3, y = max(nord$total_force) + 50, label = "a)", hjust = 0, size = 6)

# Scatter plot Test 1 vs Test 2 with perfect correlation line
scatter_plot <- ggplot(nord_wide, aes(x = Test1, y = Test2)) +
  geom_point(size = 3, alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Perfect correlation line
  labs(title = "",
       x = "Test 1 Total Force (N)",
       y = "Test 2 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_equal() +  # keeps x and y scales equal for better visual comparison
  # Manually add "b)" label to the left of the scatter plot and move it higher
  annotate("text", x = min(nord_wide$Test1) - 0.2, y = max(nord_wide$Test2) + 200, label = "b)", hjust = 0, size = 6)

# Combine the box plot and scatter plot side by side using patchwork
final_plot <- boxplot_plot | scatter_plot +
  plot_annotation(
    title = "Isometric30 Force Test Reliability",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# Save the plot with custom dimensions
ggsave("iso30_test_plot.png", 
       plot = final_plot,
       width = 14,   # Width of the plot in inches
       height = 7,   # Height of the plot in inches (reduce height to fit side-by-side)
       dpi = 300)    # Resolution (dots per inch)


# Replace these column names with your actual dataset
test1 <- nord_wide$Test1
test2 <- nord_wide$Test2

# 1. Calculate differences
diff_scores <- test1 - test2

# 2. Calculate Typical Error (TE)
typical_error <- sd(diff_scores, na.rm = TRUE) / sqrt(2)

# 3. Mean of both tests combined (pooled mean)
mean_total <- mean(c(test1, test2), na.rm = TRUE)

# 4. Calculate Typical Error as % of Mean
te_percent_mean <- (typical_error / mean_total) * 100

# 5. Degrees of freedom
df <- length(na.omit(diff_scores)) - 1  # Ensures NA values don't mess up df

# 6. Confidence interval parameters
alpha <- 0.05  # 95% confidence

# Chi-squared critical values
chi_upper <- qchisq(alpha / 2, df, lower.tail = FALSE)
chi_lower <- qchisq(1 - alpha / 2, df, lower.tail = FALSE)

# 7. Calculate 95% CI for Typical Error
ci_lower <- sqrt(df * typical_error^2 / chi_upper)
ci_upper <- sqrt(df * typical_error^2 / chi_lower)

# 8. Print results
cat("Typical Error (TE):", round(typical_error, 3), "\n")
cat("Typical Error (% of Mean):", round(te_percent_mean, 2), "%\n")
cat("95% CI for TE: [", round(ci_lower, 3), ",", round(ci_upper, 3), "]\n")

# 9. Minimal Detectable Change (MDC at 95% CI)
MDC95 <- typical_error * 1.96 * sqrt(2)   # ≈ 2.77 × TE
cat("Minimal Detectable Change (95% CI):", round(MDC95, 3), "\n")


