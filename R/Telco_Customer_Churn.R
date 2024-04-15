library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

# Load the dataset
df_churn <- read.csv("customer-churn.csv") 

# Display first 6 rows
head(df_churn)

###Descriptive Analysis ###
###########################

# Let's check the dimension
dimension <- paste("Dataset Dimension:",dim(df_churn)[1],"rows",dim(df_churn)[2],"columns")
print(dimension)

# Type of each column
types <- sapply(df_churn, typeof)

num_int_columns <- sum(types == "integer")
num_char_columns <- sum(types == "character")

cat("Number of integer columns:", num_int_columns)
cat("Number of integer columns:", num_char_columns)

# Check if there are any duplicates
duplicates <- duplicated(df_churn)
print(df_churn[duplicates, ]) #"No duplicates found

# For my analysis, the 'customerID' variable doesn't really matter, so I will drop it.
df_churn <- df_churn[, !names(df_churn) %in% "customerID"]

# Provide a brief overview of statistics
column_summary <- summary(df_churn)

# Get summary for character columns
character_columns_summary <- sapply(df_churn[sapply(df_churn, is.character)], table)

# Get summary for integer columns
int_columns_summary <- sapply(df_churn[sapply(df_churn, is.integer)], table)

# Unique values by features
for (feature in names(df_churn)) {
  uniq <- unique(df_churn[[feature]])
  print(paste0(feature, " ", length(uniq)))
}

######Data Wrangling & EDA #######
###########################

# Check if there any missing values
missing_values <- colSums(is.na(df_churn))
print(missing_values)

# Find indices of rows where TotalCharges is NA
na_values_tc <- which(is.na(df_churn$TotalCharges))
print(df_churn[na_values_tc, ])


pastel_color <- c("#9ACBD9")
# Plot TotalCharges against tenure
plot(df_churn$tenure, df_churn$TotalCharges, 
     xlab = "Tenure", ylab = "Total Charges", 
     main = "Total Charges vs Tenure", 
     col = pastel_colors[1], pch = 1)
grid()

# Replace the missing values in the 'TotalCharges' column with 0
df_churn$TotalCharges <- replace(df_churn$TotalCharges, is.na(df_churn$TotalCharges), 0)

# Define numeric columns
numeric_col <- c('tenure', 'MonthlyCharges', 'TotalCharges')

# Create a boxplot for each numeric column
par(mfrow=c(1, 3), mar=c(5, 5, 2, 2))
for (col in numeric_col) {
  boxplot(df_churn[[col]], main=col)
}

# The data analysis indicates positive skewness in tenure and TotalCharges, as their means exceed the medians, while MonthlyCharges display negative skewness, with a lower mean than the median. No outliers were detected.
plot_distribution <- function(data, group_var, target) {
  library(ggplot2)
    ggplot(data, aes(x = {{group_var}}, fill = {{target}})) +
    geom_bar(position = "dodge", color = "black") +
    labs(title = paste0("Distribution of ", deparse(substitute(target)), " by ", deparse(substitute(group_var))),
         x = deparse(substitute(group_var)),
         y = "Count",
         fill = deparse(substitute(target))) +
    theme_minimal()
}

plot_distribution(df_churn,gender,Churn)
churn_by_gender <- table(df_churn$gender, df_churn$Churn)

# As we observe, there's a similar quantity of males and females. The count of No-Churn instances notably exceeds that of Churn instances, indicating a slight data imbalance.

# Distribution of Churn
churn_percentage <- prop.table(table(df_churn$Churn)) * 100

pie(churn_percentage,
    labels = paste(names(churn_percentage), "(", round(churn_percentage), "%)"),
    main = "Distribution of Churn")

# Churn by SeniorCitizen
senior_churn_percentage <- prop.table(table(df_churn$SeniorCitizen, df_churn$Churn), margin = 1)

# Senior citizens
senior_churn_percentages <- senior_churn_percentage["1", ]

pie(senior_churn_percentages,
    labels = paste(names(senior_churn_percentages), "(", round(senior_churn_percentages * 100), "%)"),
    main = "Distribution of Churn for Senior Citizens")

### Churn Rate is up to 42% for Senior Citizen ###

# Churn by Partner
partner_churn_percentage <- prop.table(table(df_churn$Partner, df_churn$Churn), margin = 1)

# People with partner
partner_churn_percentages <- partner_churn_percentage["Yes", ]

pie(partner_churn_percentages,
    labels = paste(names(partner_churn_percentages), "(", round(partner_churn_percentages * 100), "%)"),
    main = "Distribution of Churn for People with partner")

# People without partner
partner_churn_percentages <- partner_churn_percentage["No", ]

pie(partner_churn_percentages,
    labels = paste(names(partner_churn_percentages), "(", round(partner_churn_percentages * 100), "%)"),
    main = "Distribution of Churn for People without partner")

### Churn Rate is up to 33% for people without partner, but 20% for people with partner ###

# Churn by PaperlessBilling
paperlessbilling_churn_percentage <- prop.table(table(df_churn$PaperlessBilling, df_churn$Churn), margin = 1)

# People with Paperless Billing
paperlessbilling_churn_percentages <- paperlessbilling_churn_percentage["Yes", ]

pie(paperlessbilling_churn_percentages,
    labels = paste(names(paperlessbilling_churn_percentages), "(", round(paperlessbilling_churn_percentages * 100), "%)"),
    main = "Distribution of Churn for People with Paperless Billing")

# People without Paperless Billing
paperlessbilling_churn_percentages <- paperlessbilling_churn_percentage["No", ]

pie(paperlessbilling_churn_percentages,
    labels = paste(names(paperlessbilling_churn_percentages), "(", round(paperlessbilling_churn_percentages * 100), "%)"),
    main = "Distribution of Churn for People without Paperless Billing")

### Churn percent is higher in case of cutsomers having paperless billing option.

# Churn by Dependents
dependents_churn_percentage <- prop.table(table(df_churn$Dependents, df_churn$Churn), margin = 1)

# People with Dependents
dependents_churn_percentages <- dependents_churn_percentage["Yes", ]

pie(dependents_churn_percentages,
    labels = paste(names(dependents_churn_percentages), "(", round(dependents_churn_percentages * 100), "%)"),
    main = "Distribution of Churn for People with Dependents")

# People without Dependents
dependents_churn_percentages <- dependents_churn_percentage["No", ]

pie(dependents_churn_percentages,
    labels = paste(names(dependents_churn_percentages), "(", round(dependents_churn_percentages * 100), "%)"),
    main = "Distribution of Churn for People with Dependents")

### The churn rate stands at 34% for individuals without dependents, compared to 16% for those with dependents ###
