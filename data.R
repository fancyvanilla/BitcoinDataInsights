# Install necessary libraries
install.packages(c("ggplot2", "lubridate", "GGally", "prophet", "forecast", "dplyr", "tidyr"))

# Load required libraries
library(ggplot2)
library(lubridate)
library(GGally)
library(prophet)
library(forecast)
library(dplyr)
library(tidyr)

# Set the working directory (Adjust to your working directory)
setwd("C:/Users/marye/Documents/R") 

# Read the dataset
data <- read.csv("data.csv", header = TRUE)

# Convert to appropriate data types
data$Date <- mdy(data$Date)  # Convert Date
data$Price <- as.numeric(gsub(",", "", data$Price))  # Convert Price to numeric
data$Open <- as.numeric(gsub(",", "", data$Open))  # Convert Open to numeric
data$High <- as.numeric(gsub(",", "", data$High))  # Convert High to numeric
data$Low <- as.numeric(gsub(",", "", data$Low))  # Convert Low to numeric
data$Vol <- as.numeric(gsub("K", "", data$Vol)) * 1000  # Convert Volume to numeric
data$Change <- as.numeric(gsub("%", "", data$Change)) / 100  # Convert Change to decimal

# Check for missing data
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# Handling missing values by removing rows with NA
data <- na.omit(data)

#Viewing the data
head(data)
View(data)

# Descriptive statistics
summary(data[, c("Price", "Open", "High", "Low", "Vol", "Change")])

# Detecting outliers with boxplots
boxplot(data[, c("Price", "Open", "High", "Low", "Vol", "Change")], main = "Outlier Detection")

# Correlation matrix with visualizations
ggpairs(data[, c("Price", "Open", "High", "Low", "Vol", "Change")], title = "Correlation Matrix")

# Bitcoin price over time
ggplot(data, aes(x = Date, y = Price)) +
  geom_line() +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  ylim(0, 70000) +
  theme_minimal() +
  ggtitle("Bitcoin Price Over Time")

# Box plots for key metrics
ggplot(data, aes(y = Price)) +
  geom_boxplot() +
  ggtitle("Boxplot for Bitcoin Price")

# Histogram 
ggplot(data, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  ggtitle("Histogram of Bitcoin Price")

# Density plots
ggplot(data, aes(x = Price)) +
  geom_density(fill = "green") +
  ggtitle("Density Plot of Bitcoin Price")

# Fitting the data with Prophet model
colnames(data)[colnames(data) == "Date"] <- "ds"
colnames(data)[colnames(data) == "Price"] <- "y"

# Fitting the model
model_prophet <- prophet(data, daily.seasonality = TRUE)
future_dates <- make_future_dataframe(model_prophet, periods = 365)
forecast <- predict(model_prophet, future_dates)

# Viewing the forecast
plot(model_prophet, forecast) +
  ggtitle("Forecasted Bitcoin Price with Prophet") +
  xlab("Date") +
  ylab("Predicted Price")

# Viewing the interactive forecast
dyplot.prophet(model_prophet, forecast)

# Viewing patterns in the data
prophet_plot_components(model_prophet, forecast)




