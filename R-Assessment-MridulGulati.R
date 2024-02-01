library(ggplot2)
library(randomForest)
library(caret)
library(readxl)

#load the excel file
df <- read_excel("Bike.xlsx")

# Display summary statistics
summary(df)

# Check for missing values
any(is.na(df))

# Convert 'dteday' to datetime
df$dteday <- as.Date(df$dteday)

# Monthly bike count bar plot
month_df <- aggregate(cnt ~ mnth, data = df, sum)
month_df$mnth <- factor(month_df$mnth, levels = c(1:12), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

ggplot(month_df, aes(x = mnth, y = cnt)) +
  geom_bar(stat = 'identity', fill = 'maroon') +
  labs(x = 'Month', y = 'Number of Bikes', title = 'Number of Bikes Rented per Month')

# Yearly bike count bar plot
year_df <- aggregate(cnt ~ yr, data = df, sum)
year_df$yr <- factor(year_df$yr, levels = c(0, 1), labels = c('2011', '2012'))

ggplot(year_df, aes(x = yr, y = cnt)) +
  geom_bar(stat = 'identity', fill = 'maroon') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Years', y = 'Number of Bikes', title = 'Number of Bikes Rented per Year')

# Boxplots for outlier analysis
boxplot(df[, c('weathersit','temp','atemp','hum','windspeed','casual','registered','cnt')],
        main = 'Boxplots for Outlier Analysis',
        xlab = 'Parameters', ylab = 'Values')

# Split the dataset
set.seed(10)
split_index <- createDataPartition(df$cnt, p = 0.75, list = FALSE)
train_data <- df[split_index, ]
test_data <- df[-split_index, ]

# Random Forest model
rf_model <- randomForest(cnt ~ mnth + season + temp + atemp + hum + casual + registered + windspeed,
                         data = train_data, ntree = 500, random_state = 10)

# Make predictions on the test data
y_pred <- predict(rf_model, newdata = test_data)

# Calculate metrics
mae <- mean(abs(test_data$cnt - y_pred))
rmse <- sqrt(mean((test_data$cnt - y_pred)^2))
mape <- mean(abs((test_data$cnt - y_pred) / test_data$cnt) * 100)

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "\n")

