

  data=read.csv("G:/programing lecturs/final projects/WA_Fn-UseC_-HR-Employee-Attrition.csv")
  library(ggplot2)
  library(dplyr)

  
  # Recode Education
  data$Education <- recode(data$Education,
                           "1" = "Below College",
                           "2" = "College",
                           "3" = "Bachelor",
                           "4" = "Master",
                           "5" = "Doctor")
  
 
  
  
 
  # Recode EnvironmentSatisfaction
  data$EnvironmentSatisfaction <- recode(data$EnvironmentSatisfaction,
                                         "1" = "Low",
                                         "2" = "Medium",
                                         "3" = "High",
                                         "4" = "Very High")
  
  # Recode JobInvolvement
  data$JobInvolvement <- recode(data$JobInvolvement,
                                "1" = "Low",
                                "2" = "Medium",
                                "3" = "High",
                                "4" = "Very High")
  
  # View the first few rows to confirm recoding
  head(data[, c("Education", "EnvironmentSatisfaction", "JobInvolvement")])
  
  
  
  
### 1. Demographic Analysis:

   ##What is the distribution of employee age, gender, and educationlevel?

##. Distribution of Employee Age, Gender, and Education Level


# Define the variables and their corresponding properties
plot_list <- list(
  list(var = "Age", geom = "histogram", binwidth = 5, fill = "blue", title = "Distribution of Employee Age"),
  list(var = "Education", geom = "bar", fill = "lightgreen", title = "Distribution of Education Level"),
  list( geom = "bar", fill = "pink", title = "Gender Distribution")
)

# Loop through each plot specification and generate plots
for (plot_info in plot_list) {
  
  # Create the plot using tidy evaluation
  p <- ggplot(data, aes(.data[[plot_info$var]])) + 
    {if (plot_info$geom == "histogram") geom_histogram(binwidth = plot_info$binwidth, fill = plot_info$fill, color = "black")} +
    {if (plot_info$geom == "bar") geom_bar(fill = plot_info$fill, color = "black")} +
    theme_minimal() +
    labs(title = plot_info$title, x = plot_info$var, y = "Count")
  
  # Print the plot
  print(p)
}

summary(data $Age)
summary(data $Gender)
gender_count <- table(data$Gender)
print(gender_count)

education_count <- table(data$Education)
print(education_count)

head(data)

str(data)

## Are there significant differences in job satisfaction between genders?


df <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Convert Gender to factor
data$Gender <- as.factor(data$Gender)

# Perform Mann-Whitney U test (Wilcoxon rank-sum test) for non-parametric data
test_result <- wilcox.test(JobSatisfaction ~ Gender, data = df)
print(test_result)

# Boxplot visualization
ggplot(df, aes(x = Gender, y = JobSatisfaction, fill = Gender)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Job Satisfaction by Gender", x = "Gender", y = "Job Satisfaction")


summary(data$JobSatisfaction)
table(data$Gender)

# Summarize Job Satisfaction by Gender
satisfaction_summary <-data %>%
  group_by(Gender) %>%
  summarise(
    MeanSatisfaction = mean(JobSatisfaction, na.rm = TRUE),
    MedianSatisfaction = median(JobSatisfaction, na.rm = TRUE),
    SDSatisfaction = sd(JobSatisfaction, na.rm = TRUE),
    Count = n()
  )

print(satisfaction_summary)


   ## Does income vary significantly across age groups?



  #Create age groups
data <- data %>%
  mutate(AgeGroup = cut(Age, breaks = c(20, 30, 40, 50, 60, 70),
                        labels = c("20-30", "31-40", "41-50", "51-60", "61-70")))


# Summarize income by age group
income_summary <- data %>%
  group_by(AgeGroup) %>%
  summarise(MeanIncome = mean(MonthlyIncome , na.rm = TRUE),
            MedianIncome = median(MonthlyIncome , na.rm = TRUE),
            SDIncome = sd(MonthlyIncome , na.rm = TRUE),
            Count = n())

print(income_summary)
# Visualize Income Distribution Across Age Groups
ggplot(data, aes(x = AgeGroup, y = MonthlyIncome, fill = AgeGroup)) +
  geom_boxplot() +
  labs(title = "Income Distribution Across Age Groups", x = "Age Group", y = "MonthlyIncome") +
  theme_minimal()


# Check normality assumption using Shapiro-Wilk test for each age group
shapiro_test_results <- data %>%
  group_by(AgeGroup) %>%
  summarise(p_value = shapiro.test(MonthlyIncome)$p.value)

print(shapiro_test_results)


# Apply ANOVA if normality assumption holds
if (all(shapiro_test_results$p_value > 0.05)) {
  anova_result <- aov(MonthlyIncome ~ AgeGroup, data = data)
  print(summary(anova_result))
} else {
  # Apply Kruskal-Wallis test if normality assumption is violated
  kruskal_result <- kruskal.test(MonthlyIncome ~ AgeGroup, data = data)
  print(kruskal_result)
}


# Boxplot of Monthly Income by Age Group
ggplot(data, aes(x = AgeGroup, y = MonthlyIncome)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Monthly Income by Age Group", x = "Age Group", y = "Monthly Income")





 ### 2 - Attrition Analysis:
# #Factors Significantly Associated with Employee Attrition
# Convert categorical variables to factors
categorical_vars <- c("Attrition", "BusinessTravel", "Department", "EducationField", 
   
                      "Gender", "JobRole", "MaritalStatus", "OverTime")
                      
                      
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

# Summary of the dataset
summary(data)
# Visualizing attrition distribution
ggplot(data, aes(x = Attrition, fill = Attrition)) +
  geom_bar() +
  ggtitle("Employee Attrition Count") +
  theme_minimal()

data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = FALSE)
data$Attrition <- as.factor(data$Attrition)



unique(data$Attrition)                     
str(data)



# Summary statistics of attrition
table(data$Attrition)
summary(data$Attrition)

# Check for missing values
sum(is.na(data))

# Summary of the data
summary(data)


# Exploratory Data Analysis
ggplot(data, aes(x = Attrition, fill = Attrition)) +
  geom_bar() +
  ggtitle("Employee Attrition Count")



# Select numeric columns
numeric_data <- data %>% select_if(is.numeric)

# Compute correlation matrix
corr_matrix <- cor(numeric_data, use = "complete.obs")
View(numeric_data)
library(corrplot)
# Visualize the correlation matrix
corrplot(corr_matrix, method = "circle")

# Find columns with zero variance
zero_variance_cols <- sapply(numeric_data, function(x) var(x, na.rm = TRUE) == 0)

# Print names of columns with zero variance
print(names(numeric_data)[zero_variance_cols])

# Inspect the structure of the dataset
str(numeric_data)



# Logistic regression
attrition_model <- glm(Attrition ~ Age + JobSatisfaction + MonthlyIncome + 
                        YearsAtCompany + OverTime + DistanceFromHome, 
                       data = data, family = "binomial")
summary(attrition_model)

# Extract significant variables (p-value < 0.05)
significant_vars <- summary(attrition_model)$coefficients
significant_factors <- rownames(significant_vars[significant_vars[,4] < 0.05,])
print("Significant Factors Identified:")
print(significant_factors)

 cat("Factors significantly associated with attrition based on Logistic Regression:\n")
      print(significant_factors)


# # Difference in Attrition Rates Based on Job Roles or Departments

      
# Calculate attrition rate by Job Role
attrition_by_role <- data %>%
        group_by(JobRole) %>%
        summarize(Attrition_Count = sum(Attrition == "Yes"),
                  Total_Count = n(),
                  Attrition_Rate = round((Attrition_Count / Total_Count) * 100, 2))
      
# Print attrition rates by job role
  print(attrition_by_role)
      
 # Calculate attrition rate by Department
 attrition_by_dept <- data %>%
        group_by(Department) %>%
        summarize(Attrition_Count = sum(Attrition == "Yes"),
                  Total_Count = n(),
                  Attrition_Rate = round((Attrition_Count / Total_Count) * 100, 2))
      
 # Print attrition rates by department
      print(attrition_by_dept)
      

 # Plot attrition rates by Job Role
  ggplot(attrition_by_role, aes(x = reorder(JobRole, -Attrition_Rate), y = Attrition_Rate, fill = JobRole)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        ggtitle("Attrition Rate by Job Role") +
        xlab("Job Role") +
        ylab("Attrition Rate (%)") +
        theme_minimal()
      
 # Plot attrition rates by Department
 ggplot(attrition_by_dept, aes(x = Department, y = Attrition_Rate, fill = Department)) +
        geom_bar(stat = "identity") +
        ggtitle("Attrition Rate by Department") +
        xlab("Department") +
        ylab("Attrition Rate (%)") +
        theme_minimal()
      
      
     
      
# Define the list of plot configurations
      plot_list <- list(
        list(data = attrition_by_role, 
             x_var = "JobRole", 
             title = "Attrition Rate by Job Role", 
             x_label = "Job Role",
             flip = TRUE),
        
        list(data = attrition_by_dept, 
             x_var = "Department", 
             title = "Attrition Rate by Department", 
             x_label = "Department",
             flip = FALSE)
      )
      
# Loop through each plot specification and generate plots
      for (plot_info in plot_list) {
        
 # Create the plot dynamically
        p <- ggplot(plot_info$data, aes_string(x = plot_info$x_var, y = "Attrition_Rate", fill = plot_info$x_var)) +
          geom_bar(stat = "identity") +
          ggtitle(plot_info$title) +
          xlab(plot_info$x_label) +
          ylab("Attrition Rate (%)") +
          theme_minimal()
        
 # Apply coordinate flip for Job Role plot
        if (plot_info$flip) {
          p <- p + coord_flip()
        }
        
 # Print the plot
        print(p)
      }
      
      
 # Create a contingency table for Job Role and Attrition
      job_role_table <- table(data$JobRole, data$Attrition)
      
 # Perform chi-square test
      chi_sq_role <- chisq.test(job_role_table)
      print(chi_sq_role)
      
      
      
 # Create a contingency table for Department and Attrition
      dept_table <- table(data$Department, data$Attrition)
      
# Perform chi-square test
    chi_sq_dept <- chisq.test(dept_table)
      print(chi_sq_dept)
      
      



# # Do Employees with Higher Job Satisfaction Have Lower Attrition Rates?

#Recode JobSatisfaction
 data$JobSatisfaction <- recode(data$JobSatisfaction,
                                     "1" = "Very Low",
                                     "2" = "Low",
                                     "3" = "Medium",
                                     "4" = "High")
      
      
 # Calculate attrition rates by job satisfaction level
 attrition_by_satisfaction <- data %>%
        group_by(JobSatisfaction) %>%
        summarize(Attrition_Count = sum(Attrition == "Yes"),
                  Total_Count = n(),
                  Attrition_Rate = round((Attrition_Count / Total_Count) * 100, 2))
      
 # Print attrition rates by job satisfaction
 print(attrition_by_satisfaction)
      

 # Bar plot of attrition rate by job satisfaction
 ggplot(attrition_by_satisfaction, aes(x = JobSatisfaction, y = Attrition_Rate, fill = JobSatisfaction)) +
        geom_bar(stat = "identity") +
        ggtitle("Attrition Rate by Job Satisfaction Level") +
        xlab("Job Satisfaction Level (1 - Low, 4 - High)") +
        ylab("Attrition Rate (%)") +
        theme_minimal()
      
# Create a contingency table of Job Satisfaction and Attrition
  satisfaction_table <- table(data$JobSatisfaction, data$Attrition)
      
 # Perform chi-square test
 chi_sq_result <- chisq.test(satisfaction_table)
      
 # Print test results
  print(chi_sq_result)
      
      
 # Logistic regression model
 logistic_model <- glm(Attrition ~ JobSatisfaction, data = data, family = binomial)
      
  # Summary of the model
summary(logistic_model)
      
  

### 3. Performance and Compensation:

# Reload the dataset (if necessary)
data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Check the structure of PerformanceRating
str(data$PerformanceRating)

# Check unique values in PerformanceRating
unique(data$PerformanceRating)

# Count occurrences of each value, including NAs
table(data$PerformanceRating, useNA = "always")



####Is monthly income correlated with job level and performance rating?
      
# Subset the relevant columns
correlation_data <- data %>%
select(MonthlyIncome, JobLevel, PerformanceRating)
      
 # Calculate correlation matrix
 correlation_matrix <- cor(correlation_data, use = "complete.obs")
      
# Print the correlation matrix
 print(correlation_matrix)
      
# Visualize correlation matrix
 corrplot(correlation_matrix, method = "circle", type = "upper")
      
 ## Monthly Income vs. Job Level
      
ggplot(data, aes(x = JobLevel, y = MonthlyIncome)) +
        geom_jitter(alpha = 0.5) +
        geom_smooth(method = "lm", color = "blue") +
        ggtitle("Monthly Income vs Job Level") +
        xlab("Job Level") +
        ylab("Monthly Income") +
        theme_minimal()
      
##Monthly Income vs. Performance Rating
      
ggplot(data, aes(x = PerformanceRating, y = MonthlyIncome)) +
        geom_jitter(alpha = 0.5) +
        geom_smooth(method = "lm", color = "red") +
        ggtitle("Monthly Income vs Performance Rating") +
        xlab("Performance Rating") +
        ylab("Monthly Income") +
        theme_minimal()
      
# Test correlation between Monthly Income and Job Level
 cor_test_joblevel <- cor.test(data$MonthlyIncome, data$JobLevel)
      print(cor_test_joblevel)
      
# Test correlation between Monthly Income and Performance Rating
      cor_test_performance <- cor.test(data$MonthlyIncome, data$PerformanceRating)
      print(cor_test_performance)
      
      


# # Differences in Income Across Different Education Fields

# Calculate average income by education field
 income_by_education <- data %>%
        group_by(EducationField) %>%
        summarize(Average_Income = mean(MonthlyIncome, na.rm = TRUE),
                  Median_Income = median(MonthlyIncome, na.rm = TRUE),
                  SD_Income = sd(MonthlyIncome, na.rm = TRUE),
                  Count = n())
      
 # Print income summary by education field
   print(income_by_education)     
      
      
# Boxplot to visualize income distribution across education fields
      ggplot(data, aes(x = EducationField, y = MonthlyIncome, fill = EducationField)) +
        geom_boxplot() +
        ggtitle("Monthly Income Distribution by Education Field") +
        xlab("Education Field") +
        ylab("Monthly Income") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
 # Apply Shapiro-Wilk test for normality across each education field
      normality_tests <- data %>%
        group_by(EducationField) %>%
        summarise(p_value = shapiro.test(MonthlyIncome)$p.value)
      
 # Print the results of the normality test
      print(normality_tests)
 # Histogram for Monthly Income distribution by Education Field
      ggplot(data, aes(x = MonthlyIncome)) +
        geom_histogram(binwidth = 5000, fill = "steelblue", color = "black") +
        facet_wrap(~EducationField, scales = "free") +
        ggtitle("Monthly Income Distribution by Education Field") +
        xlab("Monthly Income") +
        theme_minimal()
      
# QQ-plot to check normality visually
      ggplot(data, aes(sample = MonthlyIncome)) +
        stat_qq() +
        stat_qq_line() +
        facet_wrap(~EducationField, scales = "free") +
        ggtitle("QQ Plot of Monthly Income by Education Field") +
        theme_minimal()
# Kruskal-Wallis test (non-parametric alternative)
      kruskal_result <- kruskal.test(MonthlyIncome ~ EducationField, data = data)
      print(kruskal_result)
      

# # Do Employees with Certain Education Levels Receive Higher Performance Ratings?

 # Calculate average performance rating by education level
      performance_by_education <- data %>%
        group_by(Education) %>%
        summarize(Average_Performance = mean(PerformanceRating, na.rm = TRUE),
                  Median_Performance = median(PerformanceRating, na.rm = TRUE),
                  SD_Performance = sd(PerformanceRating, na.rm = TRUE),
                  Count = n())
      
      # Print performance rating summary by education level
      print(performance_by_education)
      
      data$Education <- as.factor(data$Education)
      # Boxplot of performance rating by education level
      ggplot(data, aes(x = Education, y = PerformanceRating, fill = Education)) +
        geom_boxplot() +
        ggtitle("Performance Rating by Education Level") +
        xlab("Education Level") +
        ylab("Performance Rating") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        aes(group = Education)
      
    
      
      # Apply Shapiro-Wilk test for normality across each education level
      normality_tests <- data %>%
        group_by(Education) %>%
        summarise(p_value = shapiro.test(PerformanceRating)$p.value)
      
      # Print the results of the normality test
      print(normality_tests)
      
      
      
      # Histogram for Performance Rating distribution by Education Level
      ggplot(data, aes(x = PerformanceRating)) +
        geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
        facet_wrap(~Education, scales = "free") +
        ggtitle("Performance Rating Distribution by Education Level") +
        theme_minimal()
      
      # QQ-plot to check normality visually
      ggplot(data, aes(sample = PerformanceRating)) +
        stat_qq() +
        stat_qq_line() +
        facet_wrap(~Education, scales = "free") +
        ggtitle("QQ Plot of Performance Rating by Education Level") +
        theme_minimal()
      
      # Perform Kruskal-Wallis test
      kruskal_result <- kruskal.test(PerformanceRating ~ Education, data = data)
      
      # Display Kruskal-Wallis test results
      print(kruskal_result)
      
      
    
      # Fit a linear regression model
       lm_model <- lm(PerformanceRating ~ Education, data = data)
# Display the model summary
summary(lm_model)


library(ggplot2)
ggplot(data, aes(x = Education, y = PerformanceRating)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Education Level vs. Performance Rating", 
       x = "Education Level (1=Below College, 5=Doctorate)", 
       y = "Performance Rating")





## 4 -Job Satisfaction and Work-Life Balance:
#  Is there a relationship between work-life balance and jobsatisfaction?

# Convert relevant columns to factors
data$WorkLifeBalance <- as.factor(data$WorkLifeBalance)
data$JobSatisfaction <- as.factor(data$JobSatisfaction)

# Check for missing values
sum(is.na(data$WorkLifeBalance))
sum(is.na(data$JobSatisfaction))

# Summary of Work-Life Balance and Job Satisfaction

summary(data$WorkLifeBalance)
summary(data$JobSatisfaction)


# Calculate distribution of Job Satisfaction by Work-Life Balance
satisfaction_by_wlb <- data %>%
  group_by(WorkLifeBalance, JobSatisfaction) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 2))

# Print the summarized table
print(satisfaction_by_wlb)

ggplot(data, aes(x = WorkLifeBalance, fill = JobSatisfaction)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Work-Life Balance vs Job Satisfaction") +
  xlab("Work-Life Balance (1 - Low, 4 - High)") +
  ylab("Percentage") +
  theme_minimal()

data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Convert factors to numeric for correlation analysis
data$WorkLifeBalance <- as.numeric(as.character(data$WorkLifeBalance))
data$JobSatisfaction <- as.numeric(as.character(data$JobSatisfaction))

# Compute Spearman correlation
correlation_result <- cor.test(data$WorkLifeBalance, data$JobSatisfaction, method = "spearman")

# Print correlation result
print(correlation_result)

# Create a contingency table
contingency_table <- table(data$WorkLifeBalance, data$JobSatisfaction)

# Perform Chi-Square test
chi_sq_result <- chisq.test(contingency_table)

# Print Chi-Square test result
print(chi_sq_result)




##Do employees with higher years at the company report higher jobsatisfaction?
  
# Convert relevant columns to appropriate types
data$YearsAtCompany <- as.numeric(data$YearsAtCompany)
data$JobSatisfaction <- as.factor(data$JobSatisfaction)

# Check for missing values
sum(is.na(data$YearsAtCompany))
sum(is.na(data$JobSatisfaction))



# Summary of YearsAtCompany and JobSatisfaction
summary(data$YearsAtCompany)
summary(data$JobSatisfaction)

# Calculate job satisfaction distribution by years at the company
satisfaction_by_years <- data %>%
  group_by(YearsAtCompany, JobSatisfaction) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 2))

# Print the summarized table
print(satisfaction_by_years)

##. Scatterplot to observe the trend

ggplot(data, aes(x = YearsAtCompany, y = as.numeric(JobSatisfaction))) +
  geom_jitter(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Years at Company vs Job Satisfaction") +
  xlab("Years at Company") +
  ylab("Job Satisfaction (1-4)") +
  theme_minimal()

##Boxplot of job satisfaction by years grouped

ggplot(data, aes(x = as.factor(YearsAtCompany), y = as.numeric(JobSatisfaction), fill = as.factor(YearsAtCompany))) +
  geom_boxplot() +
  ggtitle("Job Satisfaction Distribution by Years at Company") +
  xlab("Years at Company") +
  ylab("Job Satisfaction Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Convert JobSatisfaction to numeric for correlation analysis
data$JobSatisfaction <- as.numeric(as.character(data$JobSatisfaction))

# Compute Spearman correlation
correlation_result <- cor.test(data$YearsAtCompany, data$JobSatisfaction, method = "spearman")

# Print correlation result
print(correlation_result)

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(JobSatisfaction ~ YearsAtCompany, data = data)

# Display Kruskal-Wallis test results
print(kruskal_result)


 # Fit a linear regression model
  lm_model <- lm(JobSatisfaction ~ YearsAtCompany, data = data)

# Display the summary of the model
summary(lm_model)
  
# Scatter plot with regression line
ggplot(data, aes(x = YearsAtCompany, y = JobSatisfaction)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Years at Company vs Job Satisfaction",
       x = "Years at Company",
       y = "Job Satisfaction (1=Low, 4=High)")



##Is job involvement related to overall satisfaction? 

## Relationship Between Job Involvement and Overall Satisfaction


# Create Overall Satisfaction as the average of the components
data$OverallSatisfaction <- rowMeans(data[, c("JobSatisfaction", "RelationshipSatisfaction", "EnvironmentSatisfaction", "WorkLifeBalance")], na.rm = TRUE)

# Check the new column
head(data$OverallSatisfaction)


# Calculate Pearson correlation
correlation <- cor(data$JobInvolvement, data$OverallSatisfaction, method = "pearson")

# Print the correlation result
cat("Correlation between Job Involvement and Overall Satisfaction:", correlation, "\n")


#Select satisfaction-related variables
satisfaction_data <- data[, c("JobSatisfaction", "RelationshipSatisfaction", "EnvironmentSatisfaction", "WorkLifeBalance")]

# Check the structure of the data
str(satisfaction_data)




#install.packages("psych")

#library(psych)

# Load necessary libraries
library(psych)
library(GPArotation)




# Check data suitability
KMO_result <- KMO(satisfaction_data )
print(KMO_result)



# Determine the number of factors
fa_parallel <- fa.parallel(satisfaction_data , fm = "ml", fa = "fa")

# Perform Factor Analysis
fa_result <- fa(satisfaction_data , nfactors = 2, rotate = "varimax", fm = "ml")
print(fa_result)

# Visualize factor loadings
fa.diagram(fa_result)



### 5 -Survival Analysis (Tenure):

# Install required packages if not already installed
#install.packages("survival")
#install.packages("survminer")

# Load the libraries
library(survival)
library(survminer)




    ##What is the median tenure of employees before leaving the company?

# Convert Attrition column to factor for filtering
data$Attrition <- as.factor(data$Attrition)
data$YearsAtCompany <- as.numeric(data$YearsAtCompany)

# Check for missing values in YearsAtCompany
sum(is.na(data$YearsAtCompany))


# Check if it's a dataframe
print(class(left_employees))  # Should return "data.frame"

# Verify correct column names
# Ensure filtering is done correctly
left_employees <- data %>% 
  dplyr::filter(data$Attrition == "Yes")

# Check the structure of the resulting object
print(class(left_employees))  # Should return "data.frame"


# Summary of YearsAtCompany for employees who left
summary(left_employees$YearsAtCompany)


# Calculate the median tenure of employees before leaving
median_tenure <- median(left_employees$YearsAtCompany, na.rm = TRUE)

# Print the median tenure
print(paste("The median tenure before attrition is:", median_tenure, "years"))


library(dplyr)
# Calculate attrition rate for each tenure year
attrition_by_years <- data %>%
  group_by(YearsAtCompany) %>%
  summarise(Attrition_Count = sum(Attrition == "Yes"),
            Total_Count = n(),
            Attrition_Rate = round((Attrition_Count / Total_Count) * 100, 2))

# Print attrition rates by years
print(attrition_by_years)
    #Line Plot to Show Attrition Over Time
ggplot(attrition_by_years, aes(x = YearsAtCompany, y = Attrition_Rate)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", ) +
  ggtitle("Attrition Rate by Years at Company") +
  xlab("Years at Company") +
  ylab("Attrition Rate (%)") +
  theme_minimal()

   #Boxplot to Observe Distribution of Attrition by Years
ggplot(data, aes(x = Attrition, y = YearsAtCompany, fill = Attrition)) +
  geom_boxplot() +
  ggtitle("Distribution of Years at Company by Attrition Status") +
  xlab("Attrition (Yes/No)") +
  ylab("Years at Company") +
  theme_minimal()




    #How does the likelihood of attrition change with years spent at the
company?

data$Attrition_Binary <- as.numeric(data$Attrition)  # Use Attrition directly
table(data$Attrition_Binary)

# Create the survival object
surv_obj <- Surv(data$YearsAtCompany, data$Attrition_Binary)

# Kaplan-Meier fit
km_fit <- survfit(surv_obj ~ 1, data = data)

# Plot the overall survival function
ggsurvplot(
  fit = km_fit, 
  data = data, 
  conf.int = TRUE, 
  title = "Overall Survival Function",
  xlab = "Years at Company",
  ylab = "Survival Probability",
  risk.table = TRUE
)




    #Does the probability of attrition vary between departments?

# Ensure Attrition_Binary is correctly encoded as numeric
data$Attrition_Binary <- as.numeric(data$Attrition)

# Create the survival object
surv_obj <- Surv(data$YearsAtCompany, data$Attrition_Binary)

# Fit a Cox proportional hazards model
cox_model <- coxph(surv_obj ~ Department + Age, data = data)

# Summary of the model
summary(cox_model)

# Visualize the Cox model
ggforest(cox_model, data = data)





### 6. Hypothesis Testing:


### Is the mean monthly income of employees with advanced degrees significantly different from those without?


# Load necessary libraries
library(dplyr)

# Define advanced degrees as Education levels 4 (Doctorate) and 5 (Masters)
data <- data %>%
  mutate(AdvancedDegree = ifelse(Education >= 4, "Advanced", "Non-Advanced"))

# Check the mean monthly income for each group
group_means <- data %>%
  group_by(AdvancedDegree) %>%
  summarise(MeanMonthlyIncome = mean(MonthlyIncome, na.rm = TRUE))
print(group_means)
# Convert MonthlyIncome to numeric
data$MonthlyIncome <- as.numeric(data$MonthlyIncome)

# Check for missing values
sum(is.na(data$MonthlyIncome))

# Summary statistics of MonthlyIncome
summary(data$MonthlyIncome)

shapiro_test <- shapiro.test(data$MonthlyIncome)

# Print test results
print(shapiro_test)

ks_test <- ks.test(data$MonthlyIncome, "pnorm", mean(data$MonthlyIncome, na.rm = TRUE), sd(data$MonthlyIncome, na.rm = TRUE))

# Print test results
print(ks_test)


qqnorm(data$MonthlyIncome, main = "Q-Q Plot of Monthly Income")
qqline(data$MonthlyIncome, col = "red")
# Define advanced degrees as Education levels 4 (Doctorate) and 5 (Masters)
data <- data %>%
  mutate(AdvancedDegree = ifelse(Education >= 4, "Advanced", "Non-Advanced"))

# Mann-Whitney U Test (Non-parametric test)
wilcox_test_result <- wilcox.test(MonthlyIncome ~ AdvancedDegree , data = data)

# Print test result
print(wilcox_test_result)




### Are there significant differences in work-life balance ratings across job roles?


# Check unique values in WorkLifeBalance and JobRole
unique(data$WorkLifeBalance)
unique(data$JobRole)



  #Convert relevant columns to appropriate data types
data$JobRole <- as.factor(data$JobRole)
data$WorkLifeBalance <- as.numeric(data$WorkLifeBalance)

# Check for missing values
sum(is.na(data$JobRole))
sum(is.na(data$WorkLifeBalance))

# Summary of the data
summary(data$JobRole)
summary(data$WorkLifeBalance)

# Group data by job role and summarize work-life balance
wlb_summary <- data %>%
  group_by(JobRole) %>%
  summarize(
    Avg_WLB = mean(WorkLifeBalance, na.rm = TRUE),
    Median_WLB = median(WorkLifeBalance, na.rm = TRUE),
    SD_WLB = sd(WorkLifeBalance, na.rm = TRUE),
    Count = n()
  )


# Boxplot to visualize work-life balance ratings by job role
ggplot(data, aes(x = JobRole, y = WorkLifeBalance, fill = JobRole)) +
  geom_boxplot() +
  ggtitle("Work-Life Balance Ratings by Job Role") +
  xlab("Job Role") +
  ylab("Work-Life Balance Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Test normality for the entire work-life balance data
shapiro_test <- shapiro.test(data$WorkLifeBalance)

# Print test result
print(shapiro_test)



# Print summary statistics
print(wlb_summary)

# Perform Kruskal-Wallis test (non-parametric alternative to ANOVA)
kruskal_result <- kruskal.test(WorkLifeBalance ~ JobRole, data = data)

# Print test results
print(kruskal_result)



### Is the mean performance rating of employees in technical roles higher than that of employees in managerial roles?


# Define technical and managerial roles
technical_roles <- c("Laboratory Technician", "Research Scientist", "Technical Support")
managerial_roles <- c("Manager", "Director", "Senior Manager")

# Create a new column for role type
data$RoleType <- ifelse(data$JobRole %in% technical_roles, "Technical", 
                        ifelse(data$JobRole %in% managerial_roles, "Managerial", NA))

# Filter out employees who are not in either group
filtered_data <- data[!is.na(data$RoleType), ]

# Check group means
group_means <- aggregate(PerformanceRating ~ RoleType, data = filtered_data, mean)
print(group_means)
# Remove missing values
performance_ratings <- na.omit(data$PerformanceRating)

# Conduct Shapiro-Wilk normality test
shapiro_test <- shapiro.test(performance_ratings)

# Print test result
print(shapiro_test)

# Mann-Whitney U Test (Non-parametric test)
wilcox_test_result <- wilcox.test(PerformanceRating ~ RoleType  , data = data)

# Print test result
print(wilcox_test_result)


# Perform a t-test






## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
