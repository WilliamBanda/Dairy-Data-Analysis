## HEADER ####
## Who: <William Banda>
## What: C7090 Masters Research Project (MRP)
## Last edited: 2024-07-27


## CONTENTS ####
## 00 Loading necessarly Libraries
## 01 Importing data
## 02 Data Preparation
## 03 Data Visualiation
## 04 Statistical Analysis


## 00 Loading necessarly Libraries
# Install and load necessary packages
# Install reshape2 package if not already installed
install.packages("reshape2")
install.packages("reshape2")
install.packages(c("writexl", "shiny", "ggplot2"))
install.packages(c("writexl", "shiny", "ggplot2"))
install.packages("viridis")
install.packages("viridis")
library(readr)
library(viridis)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(readxl)
library(ggplot2)
library(shiny)
library(writexl)
library(plotly)
library(reshape2)
library(forcats)
install.packages("likert")
library(likert)
library(readxl)
library(broom)

## 01 Importing data
# URL of the raw Excel file
url <- "https://github.com/WilliamBanda/Dairy-Data-Analysis/raw/2763a55b2f9b3cf0de46f8931e757f9e5f91d4c5/Dashboard%20Survey.xlsx"

# Temporary file location
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel file
dsurvey <- read_excel(temp_file)

# View the first few rows of the data
head(dsurvey)

## 02 Data Preparation
# Prepare the data
plot_data <- dsurvey %>%
  select(dashboard_type, easy_to_understand, control_over_data, sufficient_visualization,
         yearly_comparison, adequate_features, user_friendly, option_overload, ease_of_use) %>%
  pivot_longer(cols = -dashboard_type, names_to = "question", values_to = "response") %>%
  group_by(dashboard_type, question, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(dashboard_type, question) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  mutate(question = fct_rev(factor(question))) %>%
  mutate(percentage = ifelse(response %in% c(1, 2), -percentage, percentage))

## 03 Data Visualiation
# Create diverging stacked bar chart
ggplot(plot_data, aes(x = percentage, y = question, fill = as.factor(response))) +
  geom_col() +
  geom_text(aes(label = as.numeric(response)),
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ dashboard_type, ncol = 1) +
  scale_fill_manual(values = c("#D8B365", "#F5CBA7", "#FFFFD4", "#AED6F1", "#5DADE2"),
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neutral", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("1", "2", "3", "4", "5")) +
  scale_x_continuous(labels = function(x) paste0(abs(x), "%"),
                     limits = c(-100, 100),
                     breaks = seq(-100, 100, 25)) +
  coord_cartesian(xlim = c(-100, 100)) +
  labs(title = "Dashboard Evaluation: Static vs. Interactive",
       x = "Percentage",
       y = "Question",
       fill = "Response") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))
# Print the questions being plotted
print(unique(plot_data$question))

# Create diverging stacked bar chart with percentages
ggplot(plot_data, aes(x = percentage, y = question, fill = as.factor(response))) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", abs(percentage))),
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~ dashboard_type, ncol = 1) +
  scale_fill_manual(values = c("#D8B365", "#F5CBA7", "#FFFFD4", "#AED6F1", "#5DADE2"),
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neutral", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("1", "2", "3", "4", "5")) +
  scale_x_continuous(labels = function(x) paste0(abs(x), "%"),
                     limits = c(-100, 100),
                     breaks = seq(-100, 100, 25)) +
  coord_cartesian(xlim = c(-100, 100)) +
  labs(title = "Dashboard Evaluation: Static vs. Interactive",
       x = "Percentage",
       y = "Question",
       fill = "Response") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

# create a bar plot of age distribution by dashbaord type
# Create age group labels
age_labels <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 and above")

# Convert Age to factor with proper labels
dsurvey$AgeGroup <- factor(dsurvey$age, levels = 1:7, labels = age_labels)

# Calculate frequency of age groups by dashboard type
age_freq <- dsurvey %>%
  group_by(dashboard_type, AgeGroup) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Calculate mean, mode, and median for age
age_stats <- dsurvey %>%
  group_by(dashboard_type) %>%
  summarise(
    Mean = mean(age),
    Mode = as.numeric(names(which.max(table(age)))),
    Median = median(age)
  )

#  Create the bar plot
p <- ggplot(age_freq, aes(x = AgeGroup, y = Frequency, fill = dashboard_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Frequency), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Age Distribution by Dashboard Type",
       x = "Age Group",
       y = "Frequency",
       fill = "Dashboard Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
# Print the plot
print(p)

# Create a bar plot showing levels of education frequency
# Create education level labels
edu_labels <- c("High school or equivalent", "Some college", "Associate degree",
                "Bachelor's degree", "Master's degree", "Doctoral degree")

# Convert Education to factor with proper labels
dsurvey$Education <- factor(dsurvey$education, levels = 1:6, labels = edu_labels)

#  Calculate frequency of education levels by dashboard type
edu_freq <- dsurvey %>%
  group_by(dashboard_type, Education) %>%
  summarise(Frequency = n(), .groups = 'drop')

#  Identify the mode for each dashboard type
mode_edu <- edu_freq %>%
  group_by(dashboard_type) %>%
  filter(Frequency == max(Frequency)) %>%
  ungroup()

# Create the bar plot
p <- ggplot(edu_freq, aes(x = Education, y = Frequency, fill = dashboard_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = Frequency), position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(x = "Education Level",
       y = "Frequency",
       fill = "Dashboard Type") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
  scale_y_continuous(breaks = seq(0, max(edu_freq$Frequency), by = 1))
# Add mode labels for both static and interactive
p <- p +
  geom_text(data = mode_edu,
            aes(label = "Mode", y = Frequency + 0.5),  # Adjust y position to ensure visibility
            position = position_dodge(width = 0.9),
            vjust = 0, color = "black", fontface = "bold", size = 3.5)
# Print the plot
print(p)

#Create a bar plot showing experience levels frequency

# Create experience level labels
exp_labels <- c("None", "Beginner", "Intermediate", "Advanced", "Expert")

# Convert Experience to factor with proper labels
dsurvey$Experience <- factor(dsurvey$experience, levels = 1:5, labels = exp_labels)

# Calculate frequency of experience levels by dashboard type
exp_freq <- dsurvey %>%
  group_by(dashboard_type, Experience) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Identify the mode for each dashboard type
mode_exp <- exp_freq %>%
  group_by(dashboard_type) %>%
  filter(Frequency == max(Frequency)) %>%
  ungroup()

# Create the bar plot
p <- ggplot(exp_freq, aes(x = Experience, y = Frequency, fill = dashboard_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = Frequency), position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(x = "Experience Level",
       y = "Frequency",
       fill = "Dashboard Type") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
  scale_y_continuous(breaks = seq(0, max(exp_freq$Frequency), by = 1))
# Add mode labels for both static and interactive
p <- p +
  geom_text(data = mode_exp,
            aes(label = "Mode", y = Frequency + 0.5),  # Adjust y position to ensure visibility
            position = position_dodge(width = 0.9),
            vjust = 0, color = "black", fontface = "bold", size = 3.5)
# Print the plot
print(p)

## 04 Statistical Analysis
# Lets explore measures of central tendency
# Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# List of variables to calculate statistics for
variables <- c("easy_to_understand", "control_over_data", "sufficient_visualization",
               "yearly_comparison", "adequate_features", "user_friendly",
               "option_overload", "ease_of_use", "satisfaction")

# Function to calculate statistics
calculate_statistics <- function(data, var) {
  data %>%
    reframe(
      Mean = mean(.data[[var]], na.rm = TRUE),
      Median = median(.data[[var]], na.rm = TRUE),
      Mode = calculate_mode(.data[[var]]),
      Std_Dev = sd(.data[[var]], na.rm = TRUE),
      Range_Min = min(.data[[var]], na.rm = TRUE),
      Range_Max = max(.data[[var]], na.rm = TRUE)
    )
}
# Calculate statistics for each variable by dashboard type
statistics <- lapply(variables, function(var) {
  dsurvey %>%
    group_by(dashboard_type) %>%
    calculate_statistics(var) %>%
    mutate(Variable = var)
})
# Combine results into a single dataframe
statistics_df <- bind_rows(statistics)
# Print the results
print(statistics_df)
# Create a dataframe with your data
data <- data.frame(
  Question = c("easy to understand", "control over data", "sufficient visualisation", "Yearly comparison",
               "adequate features", "user friendly", "option overload", "ease of use", "satisfaction"),
  Static_Mean = c(3.47, 3.57, 3.63, 4, 3.6, 3.37, 2.43, 3.73, 3.73),
  Static_SD = c(0.973, 0.774, 0.85, 0.91, 1.04, 0.982, 0.774, 0.691, 1.39),
  Interactive_Mean = c(4.03, 4.47, 4.17, 4.67, 4.13, 3.8, 2.4, 4, 3.73),
  Interactive_SD = c(0.999, 0.86, 0.592, 0.547, 0.86, 1.03, 0.968, 0.947, 0.691)
)

# Lets use t-test to see if there are significant differences in user perceptions between the 2 dashboards
# T-test function
# Provided data
data <- tibble(
  question = c("easy to understand", "control over data", "sufficient visualisation", "Yearly comparison", 
               "adequate features", "user friendly", "option overload", "ease of use", "satisfaction"),
  Mean_Static = c(3.47, 3.57, 3.63, 4.00, 3.60, 3.37, 2.43, 3.73, 3.73),
  SD_Static = c(0.973, 0.774, 0.85, 0.91, 1.04, 0.982, 0.774, 0.691, 1.39),
  Mean_Interactive = c(4.03, 4.47, 4.17, 4.67, 4.13, 3.80, 2.40, 4.00, 3.73),
  SD_Interactive = c(0.999, 0.86, 0.592, 0.547, 0.86, 1.03, 0.968, 0.947, 0.691)
)

# T-test function using means and standard deviations
get_p_value <- function(mean1, sd1, mean2, sd2, n1, n2) {
  t.test(x = rnorm(n1, mean1, sd1), y = rnorm(n2, mean2, sd2))$p.value
}

# Assuming sample sizes
n1 <- 30  # Static Dashboard
n2 <- 30  # Interactive Dashboard

# Perform t-tests
t_test_results <- data %>%
  rowwise() %>%
  mutate(P_Value = get_p_value(Mean_Static, SD_Static, Mean_Interactive, SD_Interactive, n1, n2))

# Print the results
print(t_test_results)


# Lets do Multiple linear regression to evaluate the effect of demographic factors on user perceptions
# Multiple linear regression
dependent_vars <- c("easy_to_understand", "control_over_data", "sufficient_visualization",
                    "yearly_comparison", "adequate_features", "user_friendly",
                    "option_overload", "ease_of_use", "satisfaction")

perform_regression <- function(data, dep_var) {
  tryCatch({
    formula <- as.formula(paste(dep_var, "~ age + experience + education"))
    model <- lm(formula, data = data)
    results <- tidy(model)
    results$dependent_var <- dep_var
    return(results)
  }, error = function(e) {
    warning(paste("Error in regression for", dep_var, ":", conditionMessage(e)))
    return(data.frame(dependent_var = dep_var, error = conditionMessage(e)))
  })
}

regression_results <- dsurvey %>%
  group_by(dashboard_type) %>%
  nest() %>%
  mutate(
    models = map(data, ~ map_dfr(dependent_vars, function(var) {
      perform_regression(.x, var)
    }))
  ) %>%
  unnest(models)

print(regression_results, n = Inf)

# lets do multiple linear regression for the effect of perceptual factors on overall satisfcation
# List of perceptual factors
perceptual_factors <- c("easy_to_understand", "control_over_data", "sufficient_visualization",
                        "yearly_comparison", "adequate_features", "user_friendly",
                        "option_overload", "ease_of_use")

# Function to perform regression and create plot
analyze_dashboard <- function(data, title) {
  
  # Perform regression
  model <- lm(satisfaction ~ ., data = data[, c("satisfaction", perceptual_factors)])
  
  # Summarize the model
  summary_result <- summary(model)
  print(paste("Results for", title))
  print(summary_result)
 
   # Get tidy results
  tidy_results <- tidy(model)
  
  # Create a coefficient plot
  plot <- ggplot(tidy_results[-1,], aes(x = reorder(term, estimate), y = estimate)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
    coord_flip() +
    labs(title = paste("Perceptual Factors Influencing Satisfaction -", title),
         x = "Factors",
         y = "Coefficient Estimate") +
    theme_minimal()
  print(plot)
  
  # Calculate standardized coefficients
  standardized_model <- lm(scale(satisfaction) ~ scale(easy_to_understand) +
                             scale(control_over_data) + scale(sufficient_visualization) +
                             scale(yearly_comparison) + scale(adequate_features) +
                             scale(user_friendly) + scale(option_overload) +
                             scale(ease_of_use), data = data)
  standardized_results <- tidy(standardized_model)
  print(paste("Standardized coefficients for", title))
  print(standardized_results)
  return(list(summary = summary_result, plot = plot, standardized = standardized_results))
}

# Analyze static dashboards
static_results <- analyze_dashboard(dsurvey[dsurvey$dashboard_type == "static", ], "Static Dashboards")

# Analyze interactive dashboards
interactive_results <- analyze_dashboard(dsurvey[dsurvey$dashboard_type == "Interactive", ], "Interactive Dashboards")

# Compare R-squared values
static_r2 <- static_results$summary$r.squared
interactive_r2 <- interactive_results$summary$r.squared
print(paste("R-squared for Static Dashboards:", round(static_r2, 3)))
print(paste("R-squared for Interactive Dashboards:", round(interactive_r2, 3)))

