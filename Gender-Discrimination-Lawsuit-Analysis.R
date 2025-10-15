# AN6003 Analytics Strategy
# Team Assignment: Gender Discrimination Lawsuit for the Houston College of Medicine
# Team 6: Liu Chang, Xu Anlan, Chen Zixuan, Li Ang, Arif Farhan Bukhori, Simon Eppig

# This file is structured into two parts:
# Part 1 (Lines 10 to 931): The complete set of analyses which led to the findings laid out in the presentation. This part might include results and visualisations which were not included in the final presentation, but were crucial for our understanding or serve as the basis for further analyses.
# Part 2 (Lines 935 to 1499): The metrics and visualisations used in the final presentation, structured by the slides on which they appear. This part is built on the results from part 1, therefore the successful execution of the code on this part requires the code in part 1 to be executed first.


# Part 1: Complete Set of Analyses

# Please specify your local path where the "Lawsuit.csv" file is located:
setwd("")

# === Loading package  ===
library(data.table)
library(tidyverse)
library(GGally)
library(corrplot)
library(scales)
library(patchwork)

# === Data reading and cleaning ===
data <- fread("Lawsuit.csv")

# Batch Factor Conversion
factor_map <- list(
  Dept   = c("Biochemistry","Physiology","Genetics","Pediatrics","Medicine","Surgery"),
  Gender = c("Female","Male"),
  Clin   = c("Primarily Research Emphasis","Primarily Clinical Emphasis"),
  Cert   = c("Not Certified","Board Certified"),
  Rank   = c("Assistant","Associate","Full Professor")
)

data_clean <- data %>%
  select(-ID) %>%
  mutate(
    Dept   = factor(Dept,   levels=1:6, labels=factor_map$Dept),
    Gender = factor(Gender, levels=0:1, labels=factor_map$Gender),
    Clin   = factor(Clin,   levels=0:1, labels=factor_map$Clin),
    Cert   = factor(Cert,   levels=0:1, labels=factor_map$Cert),
    Rank   = factor(Rank,   levels=1:3, labels=factor_map$Rank, ordered=TRUE)
  )

# check data agian
check_data <- function(df) {
  cat("dim：", dim(df), "\n")
  cat("repeat row：", sum(duplicated(df)), "\n")
  miss <- colSums(is.na(df))
  print(miss[miss > 0])
}
check_data(data_clean)

# Experience stratification(Result:<7, 7-12, >12)
data_clean <- data_clean %>%
  mutate(
    Exper_Group3 = cut(Exper,
                       breaks = quantile(Exper, probs = seq(0,1,length.out=4), na.rm=TRUE),
                       include.lowest = TRUE,
                       labels = c("Junior","Mid-career","Senior"))
  )

table(data_clean$Exper_Group3)
tapply(data_clean$Exper, data_clean$Exper_Group3, range, na.rm=TRUE)

# === 1.fundamental analysis ===
#1.1 correlation analysis 
core_vars <- c("Dept","Clin","Cert","Prate","Exper","Rank","Gender","Sal94","Sal95")

plot_corr <- function(df, title="Correlation Heatmap") {
  corr <- cor(df %>% select(all_of(core_vars)), use="complete.obs")
  corrplot(corr, method="color", type="upper", addCoef.col="black", title=title)
}

plot_corr(data_clean %>% mutate(across(where(is.factor), as.numeric)), "Overall correlation")
plot_corr(filter(data_clean, Gender=="Female") %>% mutate(across(where(is.factor), as.numeric)), "female group")
plot_corr(filter(data_clean, Gender=="Male")   %>% mutate(across(where(is.factor), as.numeric)), "male group")

#1.2 Distribution function of categorical variables
plot_cat_dist <- function(var) {
  df <- data_clean %>%
    count(Gender, !!sym(var)) %>%
    group_by(Gender) %>%
    mutate(perc = round(n/sum(n)*100,1))
  
  ggplot(df, aes(x=Gender, y=perc, fill=!!sym(var))) +
    geom_col(position="dodge") +
    geom_text(aes(label=paste0(perc,"%")), 
              position=position_dodge(width=0.9), vjust=-0.3, size=3) +
    labs(title=paste("Gender Distribution by", var), y="Percentage (%)") +
    theme_minimal()
}

plot_cat_dist("Clin")
plot_cat_dist("Cert")
plot_cat_dist("Dept")
plot_cat_dist("Rank")

#1.3 Continuous variable statistics and box plots
cont_vars <- c("Prate","Exper","Sal94","Sal95")

#1.4 Calculate the mean and the median
gender_stats <- data_clean %>%
  pivot_longer(cols=all_of(cont_vars)) %>%
  group_by(Gender, name) %>%
  summarise(mean=mean(value,na.rm=TRUE),
            median=median(value,na.rm=TRUE),
            sd=sd(value,na.rm=TRUE), .groups="drop")
print(gender_stats)

ggplot(data_clean %>% pivot_longer(all_of(cont_vars)), 
       aes(x=Gender, y=value, fill=Gender)) +
  geom_boxplot(alpha=0.7) +
  facet_wrap(~name, scales="free_y") +
  theme_minimal()

# === 2.Promotion disparity ===
# === 2.1 Impact of Work Experience ===
ggplot(data_clean, aes(x=Exper, fill=Gender)) +
  geom_histogram(aes(y=after_stat(density)), bins=12, alpha=0.5, position="identity") +
  geom_density(alpha=0.3) +
  facet_wrap(~Gender) +
  theme_minimal()

# 2.1.1 Rank Distribution by Gender across Experience Levels
data_clean <- data_clean %>%
  mutate(
    Exper_Group = cut(
      Exper,
      breaks = c(-Inf, 7, 12, Inf),  # Exper levels：0-7, 7-12, 12+
      labels = c("0-7", "7-12", "12+")
    )
  )

rank_dist <- data_clean %>%
  group_by(Gender, Exper_Group, Rank) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Gender, Exper_Group) %>%
  mutate(perc = round(n / sum(n) * 100, 1))
print(rank_dist)

ggplot(rank_dist, aes(x = Exper_Group, y = perc, fill = Rank)) +
  geom_col(position = "stack", alpha = 0.8) +
  facet_wrap(~Gender) +
  geom_text(aes(label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Rank Distribution by Gender across Experience Levels",
    x = "Experience Group (years)",
    y = "Percentage (%)",
    fill = "Rank"
  ) +
  theme_minimal()

# 2.1.2 Rank Counts by Gender across Experience Levels
rank_count <- data_clean %>%
  group_by(Exper_Group, Rank, Gender) %>%
  summarise(Count = n(), .groups = "drop")
print(rank_count)

ggplot(rank_count, aes(x = Exper_Group, y = Count, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~Rank) +
  labs(
    title = "Rank Counts by Gender across Experience Levels",
    x = "Experience Group (years)",
    y = "Number of Faculty",
    fill = "Gender"
  ) +
  theme_minimal()

# 2.2 Mean exper and promotion time of each rank
exp_stats <- data_clean %>%
  filter(Rank %in% c("Assistant", "Associate", "Full Professor")) %>%
  group_by(Gender, Rank) %>%
  summarise(Mean_Exper = mean(Exper, na.rm = TRUE), .groups = "drop")
print(exp_stats)

promotion_time <- exp_stats %>%
  pivot_wider(names_from = Rank, values_from = Mean_Exper) %>%
  mutate(
    Avg_Years_Asst_to_Assoc = Associate - Assistant,
    Avg_Years_Assoc_to_Full = `Full Professor` - Associate
  )
print(promotion_time)

# 2.4 Mean exper and promotion time of each rank - Visualization
promotion_long <- promotion_time %>%
  select(Gender, Avg_Years_Asst_to_Assoc, Avg_Years_Assoc_to_Full) %>%
  pivot_longer(
    cols = starts_with("Avg_Years"),
    names_to = "Stage",
    values_to = "Avg_Years"
  ) %>%
  mutate(
    Stage = recode(Stage,
                   "Avg_Years_Asst_to_Assoc" = "Assistant → Associate",
                   "Avg_Years_Assoc_to_Full" = "Associate → Full professor")
  )

ggplot(promotion_long, aes(x = Stage, y = Avg_Years, fill = Gender)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(Avg_Years, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  labs(
    title = "Average Years Required for Promotion by Gender",
    x = "Promotion Stage",
    y = "Average Years",
    fill = "Gender"
  ) +
  theme_minimal()

# === 3.Salary differences ===
# 3.1 The disparity in salary structure among different departments is the main cause of the overall salary gap
# 3.1.1 Average Salary (1995) by Department and Gender
salary_stats <- data_clean %>%
  group_by(Dept, Gender) %>%
  summarise(
    Mean_Sal94   = round(mean(Sal94, na.rm = TRUE), 1),
    Median_Sal94 = round(median(Sal94, na.rm = TRUE), 1),
    Mean_Sal95   = round(mean(Sal95, na.rm = TRUE), 1),
    Median_Sal95 = round(median(Sal95, na.rm = TRUE), 1),
    Count        = n(),
    .groups = "drop"
  )
print(salary_stats)

# visualization
ggplot(salary_stats, aes(x = Dept, y = Mean_Sal95, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Mean_Sal95, 0)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Average Salary (1995) by Department and Gender",
    x = "Department",
    y = "Average Salary (1995)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# 3.1.2 Average Salary (1995) by Departemnt, Gender, and Experience Stage
salary_stats <- data_clean %>%
  group_by(Dept, Gender, Exper_Group) %>%
  summarise(
    Mean_Sal94   = round(mean(Sal94, na.rm = TRUE), 1),
    Median_Sal94 = round(median(Sal94, na.rm = TRUE), 1),
    Mean_Sal95   = round(mean(Sal95, na.rm = TRUE), 1),
    Median_Sal95 = round(median(Sal95, na.rm = TRUE), 1),
    Count        = n(),
    .groups = "drop"
  )
print(salary_stats)

# visualization
ggplot(salary_stats, aes(x = Exper_Group, y = Mean_Sal95, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~Dept, scales = "free_y") +
  geom_text(aes(label = round(Mean_Sal95, 0)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 2.8) +
  labs(
    title = "Average Salary (1995) by Departemnt, Gender, and Experience Stage",
    x = "Experience Group (years)",
    y = "Average Salary (1995)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 9)
  )

# 3.2 Is clinical work the main source of the overall salary gap?
# 3.2.1 Average Salary (1995) by Clin and Gender
salary_stats <- data_clean %>%
  group_by(Clin, Gender) %>%
  summarise(
    Mean_Sal94   = round(mean(Sal94, na.rm = TRUE), 1),
    Median_Sal94 = round(median(Sal94, na.rm = TRUE), 1),
    Mean_Sal95   = round(mean(Sal95, na.rm = TRUE), 1),
    Median_Sal95 = round(median(Sal95, na.rm = TRUE), 1),
    Count        = n(),
    .groups = "drop"
  )
print(salary_stats)

ggplot(salary_stats, aes(x = Clin, y = Mean_Sal95, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Mean_Sal95, 0)), 
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Average Salary (1995) by Clin and Gender",
    x = "Clin",
    y = "Average Salary (1995)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# 3.2.2 Average Salary (1995) by Clin, Gender, and Experience Stage
salary_stats <- data_clean %>%
  group_by(Clin, Gender, Exper_Group) %>%
  summarise(
    Mean_Sal94   = round(mean(Sal94, na.rm = TRUE), 1),
    Median_Sal94 = round(median(Sal94, na.rm = TRUE), 1),
    Mean_Sal95   = round(mean(Sal95, na.rm = TRUE), 1),
    Median_Sal95 = round(median(Sal95, na.rm = TRUE), 1),
    Count        = n(),
    .groups = "drop"
  )
print(salary_stats)

# visualization
ggplot(salary_stats, aes(x = Exper_Group, y = Mean_Sal95, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~Clin, scales = "free_y") +
  geom_text(aes(label = round(Mean_Sal95, 0)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 2.8) +
  labs(
    title = "Average Salary (1995) by Clin, Gender, and Experience Stage",
    x = "Experience Group (years)",
    y = "Average Salary (1995)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 9)
  )

# Calculate the mean/median of the publication rate by department and position.
prate_stats <- data_clean %>%
  group_by(Dept, Clin) %>%
  summarise(
    Mean_Prate   = round(mean(Prate, na.rm = TRUE), 2),
    Median_Prate = round(median(Prate, na.rm = TRUE), 2),
    Count        = n(),
    .groups = "drop"
  )
print(prate_stats)

# visualization
ggplot(prate_stats, aes(x = Dept, y = Mean_Prate, fill = Clin)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = Mean_Prate),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Average Publication Rate by Dept and Clin/Research Emphasis",
    x = "Department",
    y = "Average Prate",
    fill = "Clin/Research Emphasis"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# 1. Calculate the number and proportion of clinical/research personnel in each department
clin_dept_stats <- data_clean %>%
  group_by(Dept, Clin) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Dept) %>%
  mutate(Percent = round(Count / sum(Count) * 100, 1))
print(clin_dept_stats)

# visualization
ggplot(clin_dept_stats, aes(x = Dept, y = Percent, fill = Clin)) +
  geom_col(position = "fill") +  
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_fill(vjust = 0.5), size = 3) +
  labs(
    title = "Clinical vs Research Emphasis by Department",
    x = "Department",
    y = "Proportion",
    fill = "Emphasis"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


# 4 The consistency of gender in salary growth rates
data_clean <- data_clean %>%
  mutate(
    Salary_Inc   = Sal95 - Sal94,
    Salary_Ratio = (Sal95 - Sal94) / Sal94 * 100  
  )

salary_inc_stats <- data_clean %>%
  group_by(Gender) %>%
  summarise(
    Mean_Inc   = round(mean(Salary_Inc, na.rm = TRUE), 1),
    Median_Inc = round(median(Salary_Inc, na.rm = TRUE), 1),
    Mean_Ratio = round(mean(Salary_Ratio, na.rm = TRUE), 2),
    Median_Ratio = round(median(Salary_Ratio, na.rm = TRUE), 2),
    Count = n(),
    .groups = "drop"
  )
print(salary_inc_stats)

# visualization
p1 <- ggplot(data_clean, aes(x = Gender, y = Salary_Inc, fill = Gender)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  labs(
    title = "Salary Increment (1994-1995) by Gender",
    x = "Gender",
    y = "Salary Increment"
  ) +
  theme_minimal()

p2 <- ggplot(data_clean, aes(x = Gender, y = Salary_Ratio, fill = Gender)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  labs(
    title = "Salary Growth Rate (%) from 1994 to 1995 by Gender",
    x = "Gender",
    y = "Growth Rate (%)"
  ) +
  theme_minimal()

print(p1)
print(p2)


# ==================Processed Data=================
# === Custom function: Calculate actual experience based on gender differences in working hours ===
# This function calculates the actual experience based on gender differences in working hours.
# It takes the input data and adds several columns related to actual working hours and years.
calc_actual_experience <- function(data) {
  data <- data %>%
    mutate(
      # Set weekly working hours based on gender. Males are assumed to work 62 hours per week.
      weekly_hours = ifelse(Gender == "male", 62, 51),
      # Set the number of working weeks per year based on gender. Males are assumed to work 47 weeks per year.
      annual_weeks = ifelse(Gender == "male", 47, 46),
      # Calculate the annual actual working hours by multiplying weekly hours and annual weeks.
      annual_actual_hours = weekly_hours * annual_weeks,
      # Calculate the total actual hours worked since getting the degree by multiplying annual actual hours and experience years.
      actual_hours_total = annual_actual_hours * Exper,
      # Calculate the annual hours for a male (used as a base for comparison).
      male_annual_hours = 62 * 47,
      # Calculate the actual years based on the male annual hours as a reference.
      actual_years_male_base = round(actual_hours_total / male_annual_hours, 2),
      # Calculate the actual years based on each individual's own gender-specific annual hours.
      actual_years_self_base = round(actual_hours_total / annual_actual_hours, 2)
    ) %>%
    # Select relevant columns including the newly calculated actual experience variable (based on male base).
    select(
      Dept, Gender, Clin, Cert, Prate, Rank, Sal94, Sal95,
      actual_years_male_base
    )
  return(data)
}

data_with_actual_exp <- calc_actual_experience(data_clean)


# 1.2 Calculate the mean and the median, then visualize
# Define the continuous variable for analysis, here only using actual_years_male_base.
cont_vars_exp <- c("actual_years_male_base")

# Calculate the mean, median, and standard deviation for the selected variable grouped by gender.
gender_stats_exp <- data_with_actual_exp %>%
  pivot_longer(cols=all_of(cont_vars_exp)) %>%
  group_by(Gender, name) %>%
  summarise(mean=mean(value,na.rm=TRUE),
            median=median(value,na.rm=TRUE),
            sd=sd(value,na.rm=TRUE),.groups="drop")
print(gender_stats_exp)

# Create a boxplot to visualize the distribution of the actual_years_male_base variable by gender.
ggplot(data_with_actual_exp %>% pivot_longer(all_of(cont_vars_exp)), 
       aes(x=Gender, y=value, fill=Gender)) +
  geom_boxplot(alpha=0.7) +
  facet_wrap(~name, scales="free_y") +
  theme_minimal()

# === 2. Analysis related to promotion disparity with actual experience ===
# === 2.1 Impact of Actual Work Experience ===
# Create a histogram and density plot to show the distribution of actual_years_male_base by gender.
ggplot(data_with_actual_exp, aes(x=actual_years_male_base, fill=Gender)) +
  geom_histogram(aes(y=after_stat(density)), bins=12, alpha=0.5, position="identity") +
  geom_density(alpha=0.3) +
  facet_wrap(~Gender) +
  theme_minimal()

# 2.1.1 Rank Distribution by Gender across Actual Experience Levels (using actual_years_male_base)
# Create a new variable to group the actual_years_male_base into different categories based on data distribution.
data_with_actual_exp <- data_with_actual_exp %>%
  mutate(
    # Group the actual_years_male_base into intervals and assign labels to each group.
    Actual_Years_Male_Base_Group = cut(
      actual_years_male_base,
      breaks = c(-Inf, 7, 12, Inf),  # Adjust the intervals to fit the data distribution.
      labels = c("0-7", "7-12", "12+")
    )
  )

# Calculate the percentage of each rank within each gender and actual experience group.
rank_dist_exp <- data_with_actual_exp %>%
  # Group by gender, actual experience group, and rank.
  group_by(Gender, Actual_Years_Male_Base_Group, Rank) %>%
  summarise(n = n(),.groups = "drop") %>%
  group_by(Gender, Actual_Years_Male_Base_Group) %>%
  mutate(perc = round(n / sum(n) * 100, 1))
print(rank_dist_exp)

# Visualize the rank distribution by gender and actual experience group using a stacked bar chart.
ggplot(rank_dist_exp, aes(x = Actual_Years_Male_Base_Group, y = perc, fill = Rank)) +
  geom_col(position = "stack", alpha = 0.8) +
  facet_wrap(~Gender) +
  geom_text(aes(label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Rank Distribution by Gender across Actual Experience Levels",
    x = "Actual Years (Male Base)",  # Update the x-axis label.
    y = "Percentage (%)",
    fill = "Rank"
  ) +
  theme_minimal()

# 2.1.2 Rank Counts by Gender across Actual Experience Levels (using actual_years_male_base)
# Calculate the count of each rank by gender and actual experience group.
rank_count_exp <- data_with_actual_exp %>%
  # Group by actual experience group, rank, and gender.
  group_by(Actual_Years_Male_Base_Group, Rank, Gender) %>%
  summarise(Count = n(),.groups = "drop")
print(rank_count_exp)

# Visualize the rank counts by gender and actual experience group using a dodged bar chart.
ggplot(rank_count_exp, aes(x = Actual_Years_Male_Base_Group, y = Count, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~Rank) +  
  labs(
    title = "Rank Counts by Gender across Actual Experience Levels (Male Base)",
    x = "Actual Years (Male Base)",  # Update the x-axis label.
    y = "Number of Faculty",
    fill = "Gender"
  ) +
  theme_minimal()

# 2.2.1 chi_test_by_actual_exp
# This function performs a chi-square test to check for associations between gender and rank within each actual experience group.
chi_test_by_actual_exp <- function(group_label) {
  filtered <- data_with_actual_exp %>%
    filter(Actual_Years_Male_Base_Group == group_label)
  
  if (nrow(filtered) == 0) {
    cat("\n=== Actual Experience Group:", group_label, "===\nNo valid data. Skipping.\n")
    return(NULL)
  }
  cross_tab <- table(filtered$Gender, filtered$Rank)
  chi_result <- chisq.test(cross_tab)
  
  cat("\n=== Actual Experience Group:", group_label, "===\n")
  cat("Gender × Rank table:\n"); print(cross_tab)
  cat("Chi-square =", round(chi_result$statistic, 3),
      " p-value =", round(chi_result$p.value, 3), "\n")
  cat(ifelse(chi_result$p.value > 0.05,
             "Conclusion: p>0.05 → no significant gender difference in promotion proportions.\n",
             "Conclusion: p≤0.05 → significant gender difference in promotion proportions.\n"))
  invisible(chi_result)
}

lapply(levels(data_with_actual_exp$Actual_Years_Male_Base_Group), chi_test_by_actual_exp)

# 2.2.2 chi_test_for_Experience_7-12
#=== Function: Chi-Square Test for Salary-Gender Correlation by Experience 7-12 Group ===#
chi_test_salary_gender_by_exper_group <- function(group_label) {
  # Filter data for the target experience group
  filtered_data <- data_clean %>%
    filter(Exper_Group == group_label)  
  
  # Check if filtered data is empty
  if (nrow(filtered_data) == 0) {
    cat("\n=== Experience Group:", group_label, "===\nNo valid data. Skipping.\n")
    return(NULL)
  }
  
  # Discretize salary variable (Sal95) into 3 groups (Low/Medium/High) using quartiles
  filtered_data <- filtered_data %>%
    mutate(
      Sal95_level = cut(
        Sal95,
        breaks = quantile(Sal95, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE),  # Quartile-based binning
        include.lowest = TRUE,
        labels = c("Low", "Medium", "High")  # Labels for salary levels
      )
    )
  
  # Generate contingency table: Gender vs. Salary Level
  salary_gender_contingency <- table(
    Gender = filtered_data$Gender, 
    Salary_Level = filtered_data$Sal95_level
  )
  
  # Print contingency table
  cat("\n=== Contingency Table: Gender vs. Salary Level (Experience Group", group_label, ") ===\n")
  print(salary_gender_contingency)
  
  # Perform chi-square test (or Fisher's exact test if expected cell counts <5)
  chi_result <- chisq.test(salary_gender_contingency)
  
  # Output test results
  cat("\n=== Chi-Square Test Results (Experience Group", group_label, ": Salary Level vs. Gender) ===\n")
  cat("Chi-square statistic =", round(chi_result$statistic, 3), "\n")
  cat("Degrees of freedom =", chi_result$parameter, "\n")
  cat("p-value =", round(chi_result$p.value, 3), "\n")
  cat(ifelse(chi_result$p.value > 0.05,
             "Conclusion: p > 0.05 → No significant association between salary level and gender.\n",
             "Conclusion: p ≤ 0.05 → Significant association between salary level and gender.\n"))
  
  invisible(chi_result)
}

# === Apply Function to Target Experience Group ("7-12") ===
# List of experience groups to test (adjust based on your data's actual groups)
target_exper_groups <- c("7-12")  # Focus on "7-12" group; add others if needed

# Run the test for each target group
lapply(target_exper_groups, chi_test_salary_gender_by_exper_group)

# 2.3 Mean actual experience (male base) and promotion time of each rank
# Calculate the mean actual experience (based on male base) for each rank and gender.
exp_stats_exp <- data_with_actual_exp %>%
  filter(Rank %in% c("Assistant", "Associate", "Full Professor")) %>%
  group_by(Gender, Rank) %>%
  # Calculate the mean of actual_years_male_base for each group.
  summarise(Mean_Actual_Years_Male_Base = mean(actual_years_male_base, na.rm = TRUE),.groups = "drop")
print(exp_stats_exp)

# Calculate the average time difference for promotion between ranks based on the male base actual years.
promotion_time_exp <- exp_stats_exp %>%
  pivot_wider(names_from = Rank, values_from = Mean_Actual_Years_Male_Base) %>%
  mutate(
    # Calculate the average years from Assistant to Associate rank based on male base.
    Avg_Years_Asst_to_Assoc_Male_Base = Associate - Assistant,
    # Calculate the average years from Associate to Full professor rank based on male base.
    Avg_Years_Assoc_to_Full_Male_Base = `Full Professor` - Associate
  )
print(promotion_time_exp)

# 2.3 Mean actual experience (male base) and promotion time of each rank - Visualization
# Prepare the data for visualization by reshaping it to a long format for plotting.
promotion_long_exp <- promotion_time_exp %>%
  # Select relevant columns for visualization including gender and promotion time columns.
  select(Gender, Avg_Years_Asst_to_Assoc_Male_Base, Avg_Years_Assoc_to_Full_Male_Base) %>%
  pivot_longer(
    cols = starts_with("Avg_Years"),
    names_to = "Stage",
    values_to = "Avg_Years"
  ) %>%
  mutate(
    Stage = recode(Stage,
                   "Avg_Years_Asst_to_Assoc_Male_Base" = "Assistant to Associate",
                   "Avg_Years_Assoc_to_Full_Male_Base" = "Associate to Full Professor")
  )

# Visualize the average years required for promotion by gender using a dodged bar chart.
ggplot(promotion_long_exp, aes(x = Stage, y = Avg_Years, fill = Gender)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(Avg_Years, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  labs(
    title = "Average Years (Male Base) Required for Promotion by Gender",
    x = "Promotion Stage",
    y = "Average Years (Male Base)",
    fill = "Gender"
  ) +
  theme_minimal()

# === 3. Analysis of salary differences with actual experience ===
# 3.1 Average Salary (1995) by Department, Gender, and Actual Experience Stage (Male Base)
# Create a new variable to group the actual_years_male_base for salary analysis based on data distribution.
data_with_actual_exp <- data_with_actual_exp %>%
  mutate(
    # Group the actual_years_male_base into intervals for salary analysis and assign labels.
    Actual_Years_Male_Base_Group_Sal = cut(
      actual_years_male_base,
      breaks = c(-Inf, 7, 12, Inf),  # Adjust the intervals as per the data distribution for salary analysis.
      labels = c("0-7", "7-12", "12+")
    )
  )

# 3.1.1 Average Salary (1995) by Department, Gender, and Actual Experience Stage (Male Base)
# Calculate the mean, median, and count of salaries (Sal94 and Sal95) by department, gender, and actual experience group.
salary_stats_dept_stage_exp <- data_with_actual_exp %>%
  group_by(Dept, Gender, Actual_Years_Male_Base_Group_Sal) %>%
  summarise(
    Mean_Sal94   = round(mean(Sal94, na.rm = TRUE), 1),
    Median_Sal94 = round(median(Sal94, na.rm = TRUE), 1),
    Mean_Sal95   = round(mean(Sal95, na.rm = TRUE), 1),
    Median_Sal95 = round(median(Sal95, na.rm = TRUE), 1),
    Count        = n(),
    .groups = "drop"
  )
print(salary_stats_dept_stage_exp)

# Visualize the average salary (Sal95) by department, gender, and actual experience group using a dodged bar chart.
ggplot(salary_stats_dept_stage_exp, aes(x = Actual_Years_Male_Base_Group_Sal, y = Mean_Sal95, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~Dept, scales = "free_y") +
  geom_text(aes(label = round(Mean_Sal95, 0)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 2.8) +
  labs(
    title = "Average Salary (1995) by Department, Gender, and Actual Experience Stage",
    x = "Actual Years (Male Base) Group",
    y = "Average Salary (1995)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 9)
  )

# 3.2 Analysis on whether clinical work is related to salary gap with actual experience (Male Base)
# 3.2.1 Average Salary (1995) by Clin, Gender, and Actual Experience Stage (Male Base)
# Create a new variable to group the actual_years_male_base for clinical work and salary analysis.
data_with_actual_exp <- data_with_actual_exp %>%
  mutate(
    # Group the actual_years_male_base into intervals for clinical work and salary analysis and assign labels.
    Actual_Years_Male_Base_Group_Clin = cut(
      actual_years_male_base,
      breaks = c(-Inf, 7, 12, Inf),  # Adjust the intervals according to the data distribution for this analysis.
      labels = c("0-7 Years", "7-12 Years", "12+ Years")
    )
  )

# Calculate the mean, median, and count of salaries (Sal94 and Sal95) by clinical work emphasis, gender, and actual experience group.
salary_stats_clin_stage_exp <- data_with_actual_exp %>%
  group_by(Clin, Gender, Actual_Years_Male_Base_Group_Clin) %>%
  summarise(
    Mean_Sal94   = round(mean(Sal94, na.rm = TRUE), 1),
    Median_Sal94 = round(median(Sal94, na.rm = TRUE), 1),
    Mean_Sal95   = round(mean(Sal95, na.rm = TRUE), 1),
    Median_Sal95 = round(median(Sal95, na.rm = TRUE), 1),
    Count        = n(),
    .groups = "drop"
  )
print(salary_stats_clin_stage_exp)

# Visualize the average salary (Sal95) by clinical work emphasis, gender, and actual experience group using a dodged bar chart.
ggplot(salary_stats_clin_stage_exp, aes(x = Actual_Years_Male_Base_Group_Clin, y = Mean_Sal95, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~Clin, scales = "free_y") +
  geom_text(aes(label = round(Mean_Sal95, 0)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 2.8) +
  labs(
    title = "Average Salary (1995) by Clin, Gender, and Actual Experience Stage",
    x = "Actual Years (Male Base) Group",
    y = "Average Salary (1995)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 9)
  )

# === 3.3 Gender Proportion Analysis ===
# Universal proportion calculation function (reusable)
calculate_gender_proportion <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var), Gender) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(!!sym(group_var)) %>%
    mutate(Percent = round(n / sum(n) * 100, 1)) %>%
    ungroup() %>%
    arrange(!!sym(group_var), Gender)
}

# 3.3.1 Overall Gender Proportion
overall_gender_prop <- data_clean %>%
  count(Gender) %>%
  mutate(Percent = round(n / sum(n) * 100, 1))

cat("=== Overall Gender Proportion ===\n")
print(overall_gender_prop)

# Visualization
p_overall <- ggplot(overall_gender_prop, aes(x = Gender, y = Percent, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(Percent, "%")), 
            vjust = -0.3, size = 5, fontface = "bold") +
  labs(title = "Overall Gender Proportion Distribution", 
       x = "Gender", 
       y = "Percentage (%)") +
  ylim(0, max(overall_gender_prop$Percent) * 1.2) +
  theme_minimal()
print(p_overall)

# 3.3.2 Gender Proportion by Department
dept_gender_prop <- calculate_gender_proportion(data_clean, "Dept")

cat("\n=== Gender Proportion by Department ===\n")
print(dept_gender_prop)

# Visualization (faceted) 
p_dept <- ggplot(dept_gender_prop, aes(x = Gender, y = Percent, fill = Gender)) +
  geom_col(position = "dodge") +
  
  geom_text(aes(label = paste0(Percent, "%")), 
            position = position_dodge(width = 0.9),  
            vjust = -0.3,  
            size = 3.5) + 
 
  facet_wrap(~Dept, scales = "fixed", ncol = 2) +  
 
  scale_y_continuous(
    limits = c(0, 100),          
    breaks = seq(0, 100, by = 20)
  ) +
  labs(
    title = "Gender Proportion Distribution by Department", 
    x = "Gender", 
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),  
    strip.text = element_text(face = "bold")            
  )

print(p_dept)


# 3.3.3 Gender Proportion by Original Experience Group
exper_group_gender_prop <- calculate_gender_proportion(data_clean, "Exper_Group")

cat("\n=== Gender Proportion by Original Experience Group ===\n")
print(exper_group_gender_prop)

# Visualization
p_exper_group <- ggplot(exper_group_gender_prop, aes(x = Exper_Group, y = Percent, fill = Gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(Percent, "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5) +
  labs(title = "Gender Proportion Distribution by Original Experience Group", 
       x = "Experience Group (Years)", 
       y = "Percentage (%)") +
  ylim(0, max(exper_group_gender_prop$Percent) * 1.2) +
  theme_minimal()
print(p_exper_group)

# 3.3.4 Gender Proportion by Actual Experience Group (Male Baseline)
actual_exper_group_gender_prop <- calculate_gender_proportion(data_with_actual_exp, "Actual_Years_Male_Base_Group")

cat("\n=== Gender Proportion by Actual Experience Group (Male Baseline) ===\n")
print(actual_exper_group_gender_prop)

# Visualization
p_actual_exper_group <- ggplot(actual_exper_group_gender_prop, aes(x = Actual_Years_Male_Base_Group, y = Percent, fill = Gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(Percent, "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5) +
  labs(title = "Gender Proportion Distribution by Actual Experience Group (Male Baseline)", 
       x = "Actual Experience Group (Male Baseline Years)", 
       y = "Percentage (%)") +
  ylim(0, max(actual_exper_group_gender_prop$Percent) * 1.2) +
  theme_minimal()
print(p_actual_exper_group)

# === 4.0) Course-style Logistic Regression (glm + OR + Confusion Matrix) ===
# Aligns with Unit 5 & Ex 5.1/5.2: use glm(family=binomial), interpret via Odds Ratios,
# and evaluate with a confusion matrix at a 0.5 cutoff.

# Helper: safely compute Odds Ratios and 95% CI from model coefficients
get_or_table <- function(m) {
  est <- coef(m)
  ci <- tryCatch(suppressMessages(confint(m)), error = function(e) confint.default(m))
  tibble::tibble(
    term    = names(est),
    OR      = exp(est),
    CI_low  = exp(ci[, 1]),
    CI_high = exp(ci[, 2]),
    p_value = summary(m)$coefficients[, 4]
  )
}

# Helper: confusion-matrix metrics at a given cutoff (default 0.5)
cm_metrics <- function(prob, y, cutoff = 0.5) {
  pred <- ifelse(prob > cutoff, 1, 0)
  cm <- table(Predicted = pred, Actual = y)
  TP <- sum(pred == 1 & y == 1)
  TN <- sum(pred == 0 & y == 0)
  FP <- sum(pred == 1 & y == 0)
  FN <- sum(pred == 0 & y == 1)
  tibble::tibble(
    Accuracy = (TP + TN) / (TP + TN + FP + FN),
    TPR_Sensitivity = TP / (TP + FN),
    FPR = FP / (FP + TN),
    TNR_Specificity = TN / (TN + FP),
    FNR = FN / (TP + FN),
    TP = TP, TN = TN, FP = FP, FN = FN
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))
}

# Modeling dataframe (drop NAs on used cols; set Gender baseline = "male" for interpretability)
mod_df <- data_clean |>
  dplyr::select(Dept, Gender, Clin, Cert, Prate, Exper, Rank, Sal95) |>
  tidyr::drop_na() |>
  dplyr::mutate(Gender = stats::relevel(Gender, ref = "Male"))

# ---------- Outcome C: High Salary (top quartile) vs Not (jury-friendly salary angle) ----------
q75 <- stats::quantile(mod_df$Sal95, 0.75, na.rm = TRUE)
mod_df <- mod_df |>
  dplyr::mutate(high_salary = ifelse(Sal95 > q75, 1, 0))

# C1) Adjusted (include Rank since pay bands differ by rank)
m_sal_a <- glm(high_salary ~ Gender + Rank + Exper + Dept + Clin + Cert + Prate,
               family = binomial, data = mod_df)
cat("\n=== C1) High Salary ~ Gender + Rank + Exper + Dept + Clin + Cert + Prate (Adjusted) ===\n"); 
print(summary(m_sal_a))
cat("\nOdds Ratios (C1):\n"); print(get_or_table(m_sal_a))

# Confusion matrix (C1)
prob_sal <- predict(m_sal_a, type = "response")
cat("\nConfusion Matrix & Metrics (C1, cutoff = 0.5):\n"); print(cm_metrics(prob_sal, mod_df$high_salary))

# ---------- Outcome D:  ----------
m0 <- lm(Sal95 ~ Gender, data = data_clean)
m1 <- lm(Sal95 ~ Exper, data = data_clean)
m2 <- lm(Sal95 ~ Gender + Dept + Rank + Exper + Prate + Cert + Clin, data = data_clean)
m3 <- lm(Sal94 ~ Gender + Dept + Rank + Exper + Prate + Cert + Clin, data = data_clean)

# Print results to console
# ---------------------------
cat("\n=== Model m0: Sal95 ~ Gender (Baseline) ===\n"); print(summary(m0))
cat("\n=== Model m1: Sal95 ~ Exper (Baseline) ===\n"); print(summary(m1))
cat("\n=== Model m2: Sal95 ~ Gender + Dept + Rank + Exper + Prate + Cert + Clin ===\n"); print(summary(m2))
cat("\n=== Model m3: Sal94 ~ Gender + Dept + Rank + Exper + Prate + Cert + Clin ===\n"); print(summary(m3))



# Part 2: Presentation Metrics and Graphs

# Setting colour scheme
# Colours for unordered categorical variables
cust_colours <- c("#e85f74", "#ef8d9c", "#f5bbc3", "#eb7688", "#ee9fab", "#e98493", "#de8793", "#e8a9b2")

# Colours for 3 to 4 ordered categorical variables
cust_colours2 <- c("#f8d1d7", "#f1a4af", "#eb7687", "#e4495f")

# Colours for 2 ordered categorical variables (also used for gender, which is an unordered categorical variable but requires a consistent colour scheme throughout the presentation)
cust_colours3 <- c("#f8d1d7", "#eb7687")


# Slide 2: "Staff Facts at Houston College of Medicine"
# Number of Employees
nrow(data_clean)

# Salary Distribution
ggplot(data_clean, aes(x=Sal95)) +
  geom_histogram(fill = "#eb7687", color = "white", aes(y=after_stat(density)), bins=14, alpha=0.4) +
  geom_density(color = "#AB3748", size = 2) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(
    hjust = 0.5,
    size = 48,
    face = "bold",
    ),
    axis.text.x = element_text(
    hjust = 0.5,
    size = 24,
    face = "bold"
    )) +
  scale_x_continuous(labels = function(x) ifelse(x < 450000 & x > 50000, scales::comma(x), "")) +
  scale_y_continuous(labels = NULL) +
  labs(
    title = "Salary Distribution (1995)",
    x = NULL,
    y = NULL
  )

# Ranks
rank_table <- data_clean %>%
  group_by(Rank) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(rank_table, aes(x = "", y = Count, fill = Rank)) +
  geom_col(color = "White", size = 1) +
  geom_text(aes(label = paste0(Rank, "\n", "(", Count, ")")),
            position = position_stack(vjust = 0.5),
            size = 12,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Ranks")


# Slide 3: "Five Ways to Categorise Our Staff"
# Departments
dept_table <- data_clean %>%
  group_by(Dept) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(dept_table, aes(x = "", y = Count, fill = reorder(Dept, Count))) +
  geom_col(color = "White", size = 1) +
  geom_text(aes(label = paste0(Dept, " ", "(", Count, ")")),
            position = position_stack(vjust = 0.5),
            size = 10,
            fontface = "bold") +
  scale_fill_manual(values = sample(cust_colours)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Departments")

# Experience
data_clean <- data_clean %>%
  mutate(
    Exper_Group3 = cut(Exper,
                       breaks = quantile(Exper, probs = seq(0,1,length.out=4), na.rm=TRUE),
                       include.lowest = TRUE,
                       labels = c("0-7 Years","7-12 Years","12+ Years"))
  )

exper_table <- data_clean %>%
  group_by(Exper_Group3) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(exper_table, aes(x = "", y = Count, fill = Exper_Group3)) +
  geom_col(color = "White", size = 1) +
  geom_text(aes(label = paste0(Exper_Group3, "\n", "(", Count, ")")),
            position = position_stack(vjust = 0.5),
            size = 10,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Experience")

# Emphasis
emph_table <- data_clean %>%
  group_by(Clin) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(emph_table, aes(x = "", y = Count, fill = reorder(Clin, Count))) +
  geom_col(color = "White", size = 1) +
  geom_text(aes(label = paste0(Clin, "\n", "(", Count, ")")),
            position = position_stack(vjust = 0.5),
            size = 10,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Emphasis")

# Certification
cert_table <- data_clean %>%
  group_by(Cert) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(cert_table, aes(x = "", y = Count, fill = reorder(Cert, Count))) +
  geom_col(color = "White", size = 1) +
  geom_text(aes(label = paste0(Cert, "\n", "(", Count, ")")),
            position = position_stack(vjust = 0.5),
            size = 10,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Certification")

# Gender
gender_table <- data_clean %>%
  group_by(Gender) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(gender_table, aes(x = "", y = Count, fill = Gender)) +
  geom_col(color = "White", size = 1) +
  geom_text(aes(label = paste0(Gender, "\n", "(", Count, ")")),
            position = position_stack(vjust = 0.5),
            size = 10,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(
    hjust = 0.5,
    size = 48,
    face = "bold",
    ),
  ) +
  labs(x = NULL, y = NULL, title = "Gender")


# Slide 4: "Three Main Factors Indicate Higher Ranks"
# Department
dept_rank_table <- data_clean %>%
  group_by(Dept, Rank) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Dept) %>%
  mutate(perc = round(n / sum(n) * 100, 2), perc_round = round(perc, 0))

dept_order <- dept_rank_table %>%
  filter(Rank == "Full Professor") %>%
  select(Dept, perc_full = perc) %>%
  arrange(perc_full)

dept_rank_table <- dept_rank_table %>%
  mutate(Dept = factor(Dept, levels = dept_order$Dept))

ggplot(dept_rank_table, aes(x = "", y = perc, fill = Rank)) +
  geom_col(color = "White", size = 1, width = 1.4) +
  facet_wrap(~Dept, nrow=1) +
  geom_text(aes(label = paste0(label = paste0(Rank, "\n", perc_round, "%"))),
            position = position_stack(vjust = 0.5),
            size = 8,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 30, face = "bold"),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Ranks by Department")

# Experience
exp_rank_table <- data_clean %>%
  group_by(Exper_Group3, Rank) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Exper_Group3) %>%
  mutate(perc = round(n / sum(n) * 100, 2), perc_round = round(perc, 0))

ggplot(exp_rank_table, aes(x = "", y = perc, fill = Rank)) +
  geom_col(color = "White", size = 1, width = 1.4) +
  facet_wrap(~Exper_Group3) +
  geom_text(aes(label = ifelse(perc_round > 9, paste0(Rank, "\n", perc_round, "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 8,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 30, face = "bold"),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Ranks by Experience")

# Gender
rank_gender_table <- data_clean %>%
  group_by(Gender, Rank) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Gender) %>%
  mutate(perc = round(n / sum(n) * 100, 2), perc_round = round(perc, 0))


ggplot(rank_gender_table, aes(x = "", y = perc, fill = Rank)) +
  geom_col(color = "White", size = 1, width = 1.4) +
  facet_wrap(~Gender) +
  geom_text(aes(label = paste0(label = paste0(Rank, "\n", perc_round, "%"))),
            position = position_stack(vjust = 0.5),
            size = 10,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 30, face = "bold"),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Ranks by Gender")


# Slide 5: "Five Main Factors Indicate Higher Salaries"
# Department
ggplot(data=data_clean, aes(x = reorder(Dept, Sal95, FUN = mean), y = Sal95, fill = Dept)) +
  geom_boxplot(size = 0.8, width = 0.9) +
  scale_fill_manual(values = sample(cust_colours)) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size = 24,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "Salary by Department")

data_clean[, mean(Sal95), Dept]

# Experience
ggplot(data=data_clean, aes(x = Exper_Group3, y = Sal95, fill = Exper_Group3)) +
  geom_boxplot(size = 0.8, width = 0.9) +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size = 34,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "Salary by Experience")

# Gender
ggplot(data=data_clean, aes(x = Gender, y = Sal95, fill = Gender)) +
  geom_boxplot(size = 0.8, width = 0.9) +
  scale_fill_manual(values = cust_colours3) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size = 34,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "Salary by Gender")

# Emphasis
ggplot(data=data_clean, aes(x = Clin, y = Sal95, fill = Clin)) +
  geom_boxplot(size = 0.8, width = 0.9) +
  scale_fill_manual(values = cust_colours3) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size = 28,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  scale_x_discrete(labels = label_wrap(5)) +
  labs(x = NULL, y = NULL, title = "Salary by Emphasis")

# Certification
ggplot(data=data_clean, aes(x = Cert, y = Sal95, fill = Cert)) +
  geom_boxplot(size = 0.8, width = 0.9) +
  scale_fill_manual(values = cust_colours3) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size = 30,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "Salary by Certification")


# Slide 6: "Adjusting Work Experience for Working Hours"
# Ranks by Adjusted Experience
actual_exp_gender_rank_table <- data_with_actual_exp %>%
  group_by(Actual_Years_Male_Base_Group_Clin, Rank, Gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Actual_Years_Male_Base_Group_Clin, Gender) %>%
  mutate(perc = (n / sum(n) * 100), perc_round = round(perc, 0))

ggplot(actual_exp_gender_rank_table, aes(x = Actual_Years_Male_Base_Group_Clin, y = perc, fill = Rank)) +
  geom_col(color = "White", size = 1, width = 0.99) +
  facet_wrap(~Gender) +
  geom_text(aes(label = ifelse(perc_round>7, paste0(Rank, "\n", perc_round, "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 6,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 30, face = "bold"),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
      axis.text.x = element_text(
      hjust = 0.5,
      size = 24,
      face = "bold"),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Ranks by Adjusted Experience")


# Slide 7: "Female Doctors Get Promoted Faster"
# Average Years Required for Promotion by Gender
ggplot(promotion_long_exp, aes(x = Stage, y = Avg_Years, fill = Gender)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f", Avg_Years)),
            size = 7,
            fontface = "bold",
            position = position_dodge(width = 0.9), vjust = -0.3) +
  labs(
    title = "Average Years Required for Promotion by Gender",
    x = NULL,
    y = NULL,
    fill = "Gender"
  ) +
  scale_fill_manual(values = cust_colours3) +
  theme_minimal() +
  theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  strip.text = element_text(size = 24, face = "bold"),
  plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
  legend.text = element_text(size = 32, face = "bold"),
  legend.title = element_text(size = 34, face = "bold"),
  axis.text.x = element_text(size = 30, face = "bold")
  )


# Slide 8: "Strong Differences in Experience and Department"
# Experience by Gender
ggplot(data=data_clean, aes(x = Gender, y = Exper, fill = Gender)) +
  geom_boxplot(size = 0.8, width = 0.9) +
  scale_fill_manual(values = cust_colours3) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size =30,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "Experience by Gender")

data_clean[,mean(Exper), by=Gender]

# Salary and Gender by Department
dept_gen_table <- data_clean %>%
  group_by(Dept, Gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Dept) %>%
  mutate(perc = round(n / sum(n) * 100, 2), perc_round = round(perc, 0))

dept_order <- data_clean %>%
  group_by(Dept) %>%
  summarise(mean_salary = mean(Sal95, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_salary) %>%
  pull(Dept)

dept_gen_table <- dept_gen_table %>%
  mutate(Dept = factor(Dept, levels = dept_order))

p1 <- ggplot(data=data_clean, aes(x = factor(Dept, levels = dept_order), y = Sal95, fill = Dept)) +
  geom_boxplot(size = 0.8, width = 0.9) +
  scale_fill_manual(values = sample(cust_colours)) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size = 30,
      face = "bold")
  ) +
  scale_y_continuous(labels = NULL) +
  scale_x_discrete(labels = NULL) +
  labs(x = NULL, y = NULL, title = "Salary and Gender by Department")

p2 <- ggplot(dept_gen_table, aes(x = "", y = perc, fill = Gender)) +
  geom_col(color = "White", size = 1, width = 1.4) +
  facet_wrap(~Dept, nrow=1) +
  geom_text(aes(label = ifelse(perc_round>15, paste0(Gender, "\n", perc_round, "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 6,
            fontface = "bold") +
  scale_fill_manual(values = cust_colours3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 24, face = "bold"),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL)

p1/p2


# Slide 9: "Experience and Department Explain Gender Pay Gap"
# Average Salary by Department, Gender, and Adjusted Experience
salary_stats_dept_actual <- data_with_actual_exp %>%
  group_by(Dept, Gender, Actual_Years_Male_Base_Group_Sal) %>%
  summarise(
    Mean_Sal94   = round(mean(Sal94, na.rm = TRUE), 1),
    Median_Sal94 = round(median(Sal94, na.rm = TRUE), 1),
    Mean_Sal95   = round(mean(Sal95, na.rm = TRUE), 1),
    Median_Sal95 = round(median(Sal95, na.rm = TRUE), 1),
    Count        = n(),
    .groups = "drop"
  )

ggplot(salary_stats_dept_actual, aes(x = Actual_Years_Male_Base_Group_Sal, y = Mean_Sal95, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.4)) +
  facet_wrap(~Dept, scales = "free_y") +
  scale_fill_manual(values = cust_colours3) +
  labs(
    title = "Average Salary by Department, Gender, and Adjusted Experience",
    x = NULL,
    y = NULL,
    fill = "Gender"
  ) +
  theme_minimal() +
    theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 24, face = "bold"),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 22, face = "bold", angle = 25, hjust = 1)
  )

# Salary Growth Rate from 1994 to 1995 by Gender
ggplot(data_clean, aes(x = Gender, y = Salary_Ratio, fill = Gender)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = cust_colours3) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size = 30,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "Salary Growth Rate from 1994 to 1995 by Gender")

data_clean[,mean(Salary_Ratio), Gender]