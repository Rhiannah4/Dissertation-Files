
kicker <- read_csv("Kicker Test Data copy.csv")
view(kicker)

kicker <- clean_names(kicker) %>%
  select(name, date_utc, l_max_force_n, r_max_force_n) 

head(kicker)

kicker <- kicker %>%
  group_by(name) %>%
  mutate(first_date = min(date_utc)) %>%
  ungroup()

view(kicker)

# Create a new column with the first data as "Test 1" and the second data as "Test 2"

kicker <- kicker %>%
  group_by(name) %>%
  mutate(first_date = min(date_utc)) %>%  # Create first_date within group
  ungroup() %>%                           # Ungroup before further operations
  mutate(test = ifelse(date_utc == first_date, "Test 1", "Test 2")) %>%
  arrange(name, date_utc)
view(kicker)

# Reshape the data to wide format for within-participant analysis

kicker_wide <- kicker %>%
  select(name, test, l_max_force_n, r_max_force_n) %>%
  pivot_wider(names_from = test, values_from = c(l_max_force_n, r_max_force_n), names_sep = "_")
view(kicker_wide)

# Scatter plot Test 1 vs Test 2 for Left Leg (l_max_force_n) from kicker_wide
p1 <- ggplot(kicker_wide, aes(x = `l_max_force_n_Test 2`, y = `l_max_force_n_Test 1`)) +
  geom_point(size = 3) +  # Plot points without color mapping to name (no legend)
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line with confidence interval
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Identity line for perfect correlation
  labs(title = "Left Leg",
       x = "Test 2 Total Force (N)", 
       y = "Test 1 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none")  # Remove legend

p2 <- ggplot(kicker_wide, aes(x = `r_max_force_n_Test 2`, y = `r_max_force_n_Test 1`)) +
  geom_point(size = 3) +  # Plot points without color mapping to name (no legend)
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line with confidence interval
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Identity line for perfect correlation
  labs(title = "Right Leg",
       x = "Test 2 Total Force (N)", 
       y = "Test 1 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none")  # Remove legend

# Arrange the 2x2 plots with an overall title
(p1 | p2) +
  plot_annotation(
    title = "Test-Retest Reliability: Kicker Test",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# Calculate mean and SD for l_max_force_n and r_max_force_n
# Calculate mean and SD for both tests and both right/left leg forces
kicker_wide_summary <- kicker_wide %>%
  summarise(
    mean_l_max_force_n_test_1 = mean(`l_max_force_n_Test 1`, na.rm = TRUE),
    sd_l_max_force_n_test_1 = sd(`l_max_force_n_Test 1`, na.rm = TRUE),
    mean_r_max_force_n_test_1 = mean(`r_max_force_n_Test 1`, na.rm = TRUE),
    sd_r_max_force_n_test_1 = sd(`r_max_force_n_Test 1`, na.rm = TRUE),
    mean_l_max_force_n_test_2 = mean(`l_max_force_n_Test 2`, na.rm = TRUE),
    sd_l_max_force_n_test_2 = sd(`l_max_force_n_Test 2`, na.rm = TRUE),
    mean_r_max_force_n_test_2 = mean(`r_max_force_n_Test 2`, na.rm = TRUE),
    sd_r_max_force_n_test_2 = sd(`r_max_force_n_Test 2`, na.rm = TRUE)
  )

ggplot(kicker_wide, aes(x = `l_max_force_n_Test 1`, y =`l_max_force_n_Test 2`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Test-Retest Reliability: Kicker Test Total Force",
       x = "Test 1 Total Force (N)",
       y = "Test 2 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")

# View the summary statistics
print(kicker_wide_summary)
view(kicker_wide_summary)

#---- Visualise the data ----

# Boxplot with jittered points for left and right max force

l = ggplot(kicker, aes(x = test, y = l_max_force_n)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Left Max Force", x = "Test", y = "Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

r = ggplot(kicker, aes(x = test, y = r_max_force_n)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Right Max Force", x = "Test", y = "Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

ggarrange(l, r, ncol = 2)

# Load the patchwork package
library(patchwork)

# Box plots (left and right leg)
l1 <- ggplot(kicker, aes(x = test, y = l_max_force_n)) +
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

l2 <- ggplot(kicker, aes(x = test, y = r_max_force_n)) +
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

# Scatter plots (left and right leg)
p1 <- ggplot(kicker_wide, aes(x = `l_max_force_n_Test 2`, y = `l_max_force_n_Test 1`)) +
  geom_point(size = 3) +  # Plot points without color mapping to name (no legend)
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line with confidence interval
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Identity line for perfect correlation
  labs(title = "Left Leg",
       x = "Test 2 Total Force (N)", 
       y = "Test 1 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none")  # Remove legend

p2 <- ggplot(kicker_wide, aes(x = `r_max_force_n_Test 2`, y = `r_max_force_n_Test 1`)) +
  geom_point(size = 3) +  # Plot points without color mapping to name (no legend)
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line with confidence interval
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Identity line for perfect correlation
  labs(title = "Right Leg",
       x = "Test 2 Total Force (N)", 
       y = "Test 1 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none")  # Remove legend

# Combine the box plots (l1 and l2) above the scatter plots (p1 and p2)
(l1 + l2) / (p1 + p2) +
  plot_annotation(
    title = "The Kicker Test",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# Box plots (left and right leg)
l1 <- ggplot(kicker, aes(x = test, y = l_max_force_n)) +
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

l2 <- ggplot(kicker, aes(x = test, y = r_max_force_n)) +
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

# Scatter plots (left and right leg)
p1 <- ggplot(kicker_wide, aes(x = `l_max_force_n_Test 2`, y = `l_max_force_n_Test 1`)) +
  geom_point(size = 3) +  # Plot points without color mapping to name (no legend)
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line with confidence interval
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Identity line for perfect correlation
  labs(title = "Left Leg",
       x = "Test 2 Total Force (N)", 
       y = "Test 1 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none")  # Remove legend

p2 <- ggplot(kicker_wide, aes(x = `r_max_force_n_Test 2`, y = `r_max_force_n_Test 1`)) +
  geom_point(size = 3) +  # Plot points without color mapping to name (no legend)
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line with confidence interval
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Identity line for perfect correlation
  labs(title = "Right Leg",
       x = "Test 2 Total Force (N)", 
       y = "Test 1 Total Force (N)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none")  # Remove legend

# Combine the box plots (l1 and l2) above the scatter plots (p1 and p2)
(l1 + l2) / (p1 + p2) +
  plot_annotation(
    title = "Test-Retest Reliability: Kicker Test",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)),
    tag_levels = c('a', 'b')  # Adds a) for box plots and b) for scatter plots
  )

ggsave("kicker_test_plot.png", 
       plot = (l1 + l2) / (p1 + p2) +
         plot_annotation(
           title = "Test-Retest Reliability: Kicker Test",
           theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)),
           tag_levels = c('a', 'b')  # Adds a) for box plots and b) for scatter plots
         ),
       width = 12,      # Width of the plot in inches
       height = 10,     # Height of the plot in inches
       dpi = 300)   
