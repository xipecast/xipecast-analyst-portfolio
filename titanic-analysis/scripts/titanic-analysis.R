# ============================================
# Titanic Survival Analysis
# Author: xipecast
# Date: 2026
# ============================================

library(tidyverse)
# Set once—applies to ALL plots automatically!
theme_set(theme_minimal(base_size = 14))
# Load Data
titanic <- read_csv("/home/boss/Documents/myDocuments/ProjectsOnLenovo/xipecast-analyst-portfolio/titanic-analysis/data/titanic.csv")

# Quick Look
glimpse(titanic)
# Survival Rates by Gender
titanic %>%
  group_by(Sex) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  )
# Survival Rates by Class
titanic %>%
  group_by(Pclass) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  )
# Combined Gender + Class Survival Rates
titanic %>%
  group_by(Sex, Pclass) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  )
# Combined Gender + Class Survival Rates
titanic %>%
  group_by(Sex, Pclass) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  )

# Combined Gender + Class Survival Rate Plot
titanic %>%
  group_by(Sex, Pclass) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  ) %>%
  ggplot(aes(x = factor(Pclass), y = survival_rate, fill = Sex)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(survival_rate, "%")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  labs(
    title = "Titanic Survival Rate by Gender & Class",
    subtitle = "Gender mattered more than class",
    x = "Passenger Class (1=First, 2=Second, 3=Third)",
    y = "Survival Rate (%)",
    fill = "Gender"
  ) +
  theme_minimal()

titanic %>%
  mutate(Survived = ifelse(Survived == 1, "Survived", "Died")) %>%
  ggplot(aes(x = Survived, y = Fare, fill = Survived)) +
  geom_boxplot() +
  labs(title = "Did Ticket Price Affect Survival?",
       x = "Status", y = "Fare Paid") +
  theme_minimal()

  # How much data is missing?
titanic %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), 
               names_to = "column", 
               values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  mutate(missing_pct = round(missing_count/891 * 100, 1)) %>%
  ggplot(aes(x = reorder(column, missing_pct), 
             y = missing_pct, 
             fill = column)) +
  geom_col() +
  geom_text(aes(label = paste0(missing_pct, "%")),
            hjust = -0.2) +
  coord_flip() +
  labs(
    title = "Missing Data by Column",
    subtitle = "Which columns have gaps?",
    x = "Column",
    y = "Missing %"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# Handle missing values
titanic_clean <- titanic %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  mutate(Embarked = ifelse(is.na(Embarked), "S", Embarked)) %>%
  select(-Cabin)  # Drop cabin entirely
# Check missing data is gone
titanic_clean %>%
  summarise(across(everything(), ~sum(is.na(.))))
# Family Size Analysis
titanic_clean %>%
  mutate(
    family_size = SibSp + Parch + 1,  # +1 includes themselves
    family_type = case_when(
      family_size == 1 ~ "Solo",
      family_size >= 2 & family_size <= 4 ~ "Small Family",
      family_size >= 5 ~ "Large Family"
    )
  ) %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  )
titanic_clean %>%
  mutate(
    family_size = SibSp + Parch + 1,
    family_type = case_when(
      family_size == 1 ~ "Solo",
      family_size >= 2 & family_size <= 4 ~ "Small Family",
      family_size >= 5 ~ "Large Family"
    )
  ) %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  )
titanic_clean %>%
  mutate(
    family_size = SibSp + Parch + 1,
    family_type = case_when(
      family_size == 1 ~ "Solo",
      family_size >= 2 & family_size <= 4 ~ "Small Family",
      family_size >= 5 ~ "Large Family"
    )
  ) %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  ) %>%
  ggplot(aes(x = reorder(family_type, survival_rate), 
             y = survival_rate, 
             fill = family_type)) +
  geom_col() +
  geom_text(aes(label = paste0(survival_rate, "%")),
            vjust = -0.5) +
  labs(
    title = "Survival Rate by Family Size",
    subtitle = "Did traveling with family help?",
    x = "Family Type",
    y = "Survival Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
titanic_clean %>%
  mutate(
    family_size = SibSp + Parch + 1,
    family_type = case_when(
      family_size == 1 ~ "Solo",
      family_size >= 2 & family_size <= 4 ~ "Small Family",
      family_size >= 5 ~ "Large Family"
    )
  ) %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  ) %>%
  ggplot(aes(x = reorder(family_type, survival_rate), 
             y = survival_rate, 
             fill = family_type)) +
  geom_col() +
  geom_text(aes(label = paste0(survival_rate, "%")),
            vjust = -0.5,
            size = 6) +
  labs(
    title = "Survival Rate by Family Size",
    subtitle = "Did traveling with family help?",
    x = "Family Type",
    y = "Survival Rate (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

titanic_clean %>%
  mutate(
    family_size = SibSp + Parch + 1,
    family_type = case_when(
      family_size == 1 ~ "Solo",
      family_size >= 2 & family_size <= 4 ~ "Small Family",
      family_size >= 5 ~ "Large Family"
    )
  ) %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  ) %>%
  ggplot(aes(x = reorder(family_type, survival_rate), 
             y = survival_rate, 
             fill = family_type)) +
  geom_col() +
  geom_text(aes(label = paste0(survival_rate, "%")),
            vjust = -0.5,
            size = 6) +
  labs(
    title = "Survival Rate by Family Size",
    subtitle = "Did traveling with family help?",
    x = "Family Type",
    y = "Survival Rate (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
titanic_clean %>%
  mutate(
    family_size = SibSp + Parch + 1,
    family_type = case_when(
      family_size == 1 ~ "Solo",
      family_size >= 2 & family_size <= 4 ~ "Small Family",
      family_size >= 5 ~ "Large Family"
    )
  ) %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  ) %>%
  ggplot(aes(x = reorder(family_type, survival_rate), 
             y = survival_rate, 
             fill = family_type)) +
  geom_col() +
  geom_text(aes(label = paste0(survival_rate, "%")),
            vjust = -0.5,
            size = 6) +
  labs(
    title = "Survival Rate by Family Size",
    subtitle = "Did traveling with family help?",
    x = "Family Type",
    y = "Survival Rate (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
titanic_clean %>%
  mutate(
    family_size = SibSp + Parch + 1,
    family_type = case_when(
      family_size == 1 ~ "Solo",
      family_size >= 2 & family_size <= 4 ~ "Small Family",
      family_size >= 5 ~ "Large Family"
    )
  ) %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  ) %>%
  ggplot(aes(x = reorder(family_type, survival_rate), 
             y = survival_rate, 
             fill = family_type)) +
  geom_col() +
  geom_text(aes(label = paste0(survival_rate, "%")),
            vjust = -0.5,
            size = 6) +
  labs(
    title = "Survival Rate by Family Size",
    subtitle = "Did traveling with family help?",
    x = "Family Type",
    y = "Survival Rate (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
# Output path
output_path <- "/home/boss/Documents/myDocuments/ProjectsOnLenovo/xipecast-analyst-portfolio/titanic-analysis/outputs/"

# Plot 1: Overall Survival
p1 <- titanic_clean %>%
  mutate(Survived = ifelse(Survived == 1, "Survived", "Died")) %>%
  ggplot(aes(x = Survived, fill = Survived)) +
  geom_bar() +
  labs(title = "Titanic Survival Count", x = "Status", y = "Count")

# Plot 2: Survival by Gender
p2 <- titanic_clean %>%
  mutate(Survived = ifelse(Survived == 1, "Survived", "Died")) %>%
  ggplot(aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Gender", x = "Gender", y = "Count")

# Plot 3: Survival by Class
p3 <- titanic_clean %>%
  mutate(Survived = ifelse(Survived == 1, "Survived", "Died")) %>%
  ggplot(aes(x = factor(Pclass), fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Class", x = "Class", y = "Count")

# Plot 4: Fare vs Survival
p4 <- titanic_clean %>%
  mutate(Survived = ifelse(Survived == 1, "Survived", "Died")) %>%
  ggplot(aes(x = Survived, y = Fare, fill = Survived)) +
  geom_boxplot() +
  labs(title = "Fare vs Survival", x = "Status", y = "Fare")

# Plot 5: Family Size
p5 <- titanic_clean %>%
  mutate(
    family_size = SibSp + Parch + 1,
    family_type = case_when(
      family_size == 1 ~ "Solo",
      family_size >= 2 & family_size <= 4 ~ "Small Family",
      family_size >= 5 ~ "Large Family"
    )
  ) %>%
  group_by(family_type) %>%
  summarise(
    total = n(),
    survived = sum(Survived),
    survival_rate = round(survived/total * 100, 1)
  ) %>%
  ggplot(aes(x = reorder(family_type, survival_rate),
             y = survival_rate,
             fill = family_type)) +
  geom_col() +
  geom_text(aes(label = paste0(survival_rate, "%")), vjust = -0.5) +
  labs(title = "Survival by Family Size", x = "Family Type", y = "Survival Rate (%)")

# Save All Plots
ggsave(paste0(output_path, "p1-survival-count.png"), plot = p1, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p2-survival-gender.png"), plot = p2, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p3-survival-class.png"), plot = p3, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p4-fare-survival.png"), plot = p4, width = 8, height = 6, dpi = 300)
ggsave(paste0(output_path, "p5-family-survival.png"), plot = p5, width = 8, height = 6, dpi = 300)

print("All plots saved! ✅")