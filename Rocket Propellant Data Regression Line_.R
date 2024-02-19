dataset=read.csv("The Rocket propellant Data.csv")
# Splitting the Dataset into Training and Testing set
install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Shear.strength, SplitRatio = 0.8)
print(split)

training_set = subset(dataset, split== TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting the Multiple Linear Regression Model using Training Set

regressor = lm(formula = Shear.strength ~ Age.of.propellant, data = training_set)
print(regressor)

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)

print(y_pred)
print(test_set$Shear.strength)

# Residual Plot
install.packages("ggplot2")

library(ggplot2)

ggplot() + 
  geom_point(aes(x= training_set$Age.of.propellant,
                 y= training_set$Shear.strength),
             colour= "red") +
  geom_line(aes(x= training_set$Age.of.propellant, 
                y= predict(regressor, newdata = training_set)),
            colour = "blue")
