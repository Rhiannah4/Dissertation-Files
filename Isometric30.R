
install.packages("psych")

library(tidyverse)
library(readxl)
library(psych)
library(janitor)
library(ggpubr)

nord <- read_csv("~/Library/CloudStorage/OneDrive-Personal/Teesside University/Semester 2/R Studio/Work Area (D)/Iso 30 Test Data copy.csv")

view(nord)

nord <- clean_names(nord) %>%
  select(name, date_utc, l_max_force_n, r_max_force_n) 

head(nord)

nord <- nord %>%
  group_by(name) %>%
  mutate(first_date = min(date_utc)) %>%
  ungroup()

view(nord)

# Create a new column with the first data as "Test 1" and the second data as "Test 2"

nord <- nord %>%
  mutate(test = ifelse(date_utc == first_date, "Test 1", "Test 2")) %>%
  arrange(name, date_utc)
# Convert the test column to a factor
nord$test <- factor(nord$test, levels = c("Test 1", "Test 2"))

view(nord)

# Reshape the data to wide format for within-participant analysis

nord_wide <- nord %>%
  select(name, test, l_max_force_n, r_max_force_n) %>%
  pivot_wider(names_from = test, values_from = c(l_max_force_n, r_max_force_n), names_sep = "_")
view(nord_wide)

nord_summary <- nord_wide %>%
  select(-name) %>%  # remove the name column
  summarise(across(everything(),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE))))

# View results
view(nord_summary)

# Combine left and right leg forces for each test, and calculate mean and SD for each test
nord_wide_summary <- nord_wide %>%
  mutate(
    combined_test_1 = `l_max_force_n_Test 1` + `r_max_force_n_Test 1`,  # Combine forces for Test 1
    combined_test_2 = `l_max_force_n_Test 2` + `r_max_force_n_Test 2`   # Combine forces for Test 2
  ) %>%
  summarise(
    mean_combined_test_1 = mean(combined_test_1, na.rm = TRUE),
    sd_combined_test_1 = sd(combined_test_1, na.rm = TRUE),
    mean_combined_test_2 = mean(combined_test_2, na.rm = TRUE),
    sd_combined_test_2 = sd(combined_test_2, na.rm = TRUE)
  )

# View the summary statistics
print(nord_wide_summary)
view(nord_wide_summary)

nord_wide <- nord_wide %>%
  mutate(
    combined_test_1 = `l_max_force_n_Test 1` + `r_max_force_n_Test 1`,
    combined_test_2 = `l_max_force_n_Test 2` + `r_max_force_n_Test 2`
  )

t_test_result <- t.test(
  nord_wide$combined_test_1,
  nord_wide$combined_test_2,
  paired = TRUE
)

print(t_test_result)

#---- Visualise the data ----

# Boxplot with jittered points for left and right max force

l = ggplot(nord, aes(x = test, y = l_max_force_n)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Left Max Force", x = "Test", y = "Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

r = ggplot(nord, aes(x = test, y = r_max_force_n)) +
  geom_boxplot(aes(fill = test), alpha = 0.5) +
  geom_jitter(width = 0.2, aes(color = test), size = 2) +
  labs(title = "Right Max Force", x = "Test", y = "Force (N)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

ggarrange(l, r, ncol = 2)
