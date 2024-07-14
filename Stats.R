# Load necessary libraries
library(tidyverse)
library(caret)
library(class)
library(ggplot2)
library(gridExtra)
library(randomForest)

# Read the dataset
df <- read.csv("dataset.csv")
head(df)

options(warn=-1)

df$Income_type <- as.numeric(as.factor(df$Income_type))
df$Education_type <- as.numeric(as.factor(df$Education_type))
df$Family_status <- as.numeric(as.factor(df$Family_status))
df$Housing_type <- as.numeric(as.factor(df$Housing_type))
df$Occupation_type <- as.numeric(as.factor(df$Occupation_type))
df$Age <- as.integer(df$Age)

head(df)

# Get basic information about the dataset
dim(df)
str(df)
summary(df)
sapply(df, function(x) length(unique(x)))
sum(duplicated(df))

# Checking null values
colSums(is.na(df))

# Identify categorical columns
categorical_col <- df %>% select_if(is.factor) %>% colnames()
categorical_col

# Display unique values for each categorical column
for (col in categorical_col) {
  print(paste("Unique values for column", col, "are:"))
  print(table(df[[col]]))
}

# Calculating correlation matrix
corr <- cor(df)

# Converting correlation matrix to long format for ggplot2
corr_melted <- melt(corr)

# Ploting heatmap using ggplot2
ggplot(corr_melted, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creating EDA before preprocessing
# Plotting boxplot for features against the Target
p1 <- ggplot(df, aes(x=factor(Target), y=Age)) + geom_boxplot() + ggtitle("Age by Target")
p2 <- ggplot(df, aes(x=factor(Target), y=Total_income)) + geom_boxplot() + ggtitle("Total Income by Target")
p3 <- ggplot(df, aes(x=factor(Target), y=Years_employed)) + geom_boxplot() + ggtitle("Years Employed by Target")
p4 <- ggplot(df, aes(x=factor(Target), y=Num_children)) + geom_boxplot() + ggtitle("Number of children by Target")
p5 <- ggplot(df, aes(x=factor(Target), y=Num_family)) + geom_boxplot() + ggtitle("Number of family members by Target")
p6 <- ggplot(df, aes(x=factor(Target), y=Account_length)) + geom_boxplot() + ggtitle("Account length by Target")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

# Function to count outliers
count_outliers <- function(df) {
  num_col <- df %>% select_if(is.numeric) %>% colnames()
  outliers <- sapply(num_col, function(col) {
    upper_limit <- quantile(df[[col]], 0.99)
    sum(df[[col]] > upper_limit)
  })
  return(outliers)
}

# Count outliers in the dataset
outliers_count <- count_outliers(df %>% select(-ID))
print("Count of outliers:")
print(outliers_count)

# Function to remove outliers
remove_outliers <- function(df) {
  num_col <- df %>% select_if(is.numeric) %>% colnames()
  for (col in num_col) {
    upper_limit <- quantile(df[[col]], 0.99)
    df <- df %>% filter(df[[col]] <= upper_limit)
  }
  return(df)
}

# Remove outliers from the dataset
df <- remove_outliers(df)



# Creating EDA after preprocessing
# Subplot1: Boxplot
# Creating a list to store plots
plots1[[1]] <- ggplot(df, aes(x = Target, y = Age, fill = Target)) +
  geom_boxplot(fill = "green") +
  labs(title = "Age by Target") +
  facet_wrap(~ Target)

plots1[[2]] <- ggplot(df, aes(x = Target, y = Total_income, fill = Target)) +
  geom_boxplot(fill = "green") +
  labs(title = "Total Income by Target") +
  facet_wrap(~ Target)

plots1[[3]] <- ggplot(df, aes(x = Target, y = Years_employed, fill = Target)) +
  geom_boxplot(fill = "green") +
  labs(title = "Years Employed by Target") +
  facet_wrap(~ Target)

plots1[[4]] <- ggplot(df, aes(x = Target, y = Num_children, fill = Target)) +
  geom_boxplot(fill = "green") +
  labs(title = "Number of Children by Target") +
  facet_wrap(~ Target)

plots1[[5]] <- ggplot(df, aes(x = Target, y = Num_family, fill = Target)) +
  geom_boxplot(fill = "green") +
  labs(title = "Number of Family Members by Target") +
  facet_wrap(~ Target)

plots1[[6]] <- ggplot(df, aes(x = Target, y = Account_length, fill = Target)) +
  geom_boxplot(fill = "green") +
  labs(title = "Account Length by Target") +
  facet_wrap(~ Target)

# Arranging plots in a grid
grid.arrange(grobs = plots1, ncol = 3)

# Subplot2: Countplot
# Creating a list to store plots
plots2 <- list()
plots2[[1]] <- ggplot(df, aes(x = Own_car, fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Own Car by Target")
plots2[[2]] <- ggplot(df, aes(x = Own_property, fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Own Property by Target")
plots2[[3]] <- ggplot(df, aes(x = Work_phone, fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Work Phone by Target")
plots2[[4]] <- ggplot(df, aes(x = Phone, fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Phone by Target")
plots2[[5]] <- ggplot(df, aes(x = Email, fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Email by Target")
plots2[[6]] <- ggplot(df, aes(x = Unemployed, fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Unemployed by Target")

# Arranging plots in a grid
grid.arrange(grobs = plots2, ncol = 3)


# Perform train-test split
set.seed(42)
train_index <- createDataPartition(df$Target, p=0.7, list=FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Train KNN classifier
knn_model <- train(Target ~ ., data=train_data, method="knn", tuneGrid=data.frame(k=2))
print(knn_model)



# Build the Random Forest model
rf_model <- randomForest(Target ~ ., data = train_data, importance = TRUE)
print(rf_model)


# Pie chart for target distribution
target_counts <- table(df$Target)
pie(target_counts, labels=c("Ineligible for Credit Card", "Eligible for Credit Card"), main="Credit Card Eligibility Chart", col=c("lightblue", "salmon"))

# Calculate and print imbalance ratio
print(paste("Eligible Count / Ineligible Count =", target_counts[2], "/", target_counts[1]))

