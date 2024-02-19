library(caTools)
library(ggplot2)
set.seed(123)

# loading the walmart data
data = read.csv("Walmart.csv")

# Dropping the date column
new_data = subset(data, select = -c(Date))

# attempt at normalizing the data gone wrong
# data_normalized = data.frame(scale(new_data["Weekly_Sales"], center=TRUE, scale = FALSE))

# splitting the data
split = sample.split(new_data$Weekly_Sales, SplitRatio = 0.8)
training_set = subset(new_data, split==TRUE)
test_set = subset(new_data, split == FALSE)

# getting the regressor object
regressor = lm(formula = Weekly_Sales ~ ., data = training_set)
print(regressor)

# predicting training and test set
y_pred_train = predict(regressor, newdata = training_set)
y_pred_test = predict(regressor, newdata = test_set)

# plotting training set prediction of weekly sales
ggplot() +
  geom_point(aes(x = training_set$CPI,
                 y = training_set$Weekly_Sales),
             colour = "red") + 
  
  geom_point(aes(x = training_set$CPI,
                 y = predict(regressor, newdata = training_set)),
             colour = "blue") + 
  ggtitle("Prediction Plot") + 
  xlab("CPI") + 
  ylab("Weekly Sales")

# plotting test set prediction of weekly sales
ggplot() + 
  geom_point(aes(x = test_set$CPI,
                 y = test_set$Weekly_Sales),
             colour = "red") + 
  geom_line(aes(x = test_set$CPI,
                y = y_pred_test),
            colour = "blue") + 
  ggtitle("Prediction Plot") +
  xlab("CPI") + 
  ylab("Weekly Sales")
