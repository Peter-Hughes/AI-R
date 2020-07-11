# Load some classic statistics datasets (including iris):
library(datasets)

# Explore iris data:
nrow(iris)
head(iris)
summary(iris)

# Make a dummay variable that discriminates 'virginica' from the other species: 
# Virginica vs not Virginica 
iris$dummy.virginica <- 0
iris$dummy.virginica
# Set the rows that are virginica to be 1 
iris$dummy.virginica[iris$Species == 'virginica'] <- 1
iris$dummy.virginica

summary(iris)

# let's make a 'fake' column of random numbers that we can use as a 
# useless feature
set.seed(3060)
# our 'fake' featue will just have a random value between 0 and 100:
# Looking at petal width (good predictor variable) and bad predictor variable 
# This is our bad feature "Rand.Feature"
iris$Rand.Feature = sample(0:100, nrow(iris), rep=TRUE)
summary(iris)

# make training and test datasets
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# randomly shuffle rows:
iris_shuffled <- iris[sample(nrow(iris)),]
head(iris_shuffled)

# first 80% will be training data:
training_data = iris_shuffled[1:120, ]
test_data = iris_shuffled[121:150, ]

# install.packages('ggplot2')
library(ggplot2)

# plot a histogram:
plt <- ggplot(training_data, aes(x=Petal.Width, fill=as.factor(dummy.virginica))) +
  geom_histogram(binwidth=.2, alpha=.5, position='identity')
plt 

ggsave('histo_petal_width.png',scale=0.7,dpi=400)   


# Logistic regression with Petal.Width
# ====================================
glmfit<-glm(dummy.virginica ~ Petal.Width, 
            data = training_data, 
            family = 'binomial') 

summary(glmfit)
# We can see in the summary that Petal.Width is a highly significant predictor


# Prediction 
# =========================================

newdata = as.data.frame(c(1.444, 1.555, 1.666)) 
colnames(newdata) = 'Petal.Width'

head(newdata)

predicted = predict(glmfit, newdata, type="response")

predicted

x.range = range(training_data[["Petal.Width"]])

# Maxing 1000 values between the min and max inclusive 
x.values = seq(x.range[1], x.range[2], length.out=1000)
# 1000 values on the x axis 
x.values

fitted.curve <- data.frame(Petal.Width = x.values)
# Getting the prediction for the each x axis 
fitted.curve[["dummy.virginica"]] = predict(glmfit, fitted.curve, type="response")

# Plot the training data and the fitted curve:
# Y is the probability of it being virginica 
plt <-ggplot(training_data, aes(x=Petal.Width, y=dummy.virginica)) + 
  geom_point(aes(colour = factor(dummy.virginica)), 
             show.legend = T, position="dodge")+
  geom_line(data=fitted.curve, colour="orange", size=1)

plt

ggsave('glm_fitted_curve.png',scale=0.7,dpi=400)   


# Assuming a p > 0.5 cut-off
remove(x.values)

test_data[["predicted_val"]] = predict(glmfit, test_data, type="response")
test_data[["predicted_class"]] = 0
test_data[["predicted_class"]][test_data[["predicted_val"]] > 0.5] = 1

correct_items = test_data[["predicted_class"]] == test_data[["dummy.virginica"]] 

# proportion correct:
nrow(test_data[correct_items,])/nrow(test_data)

# proportion incorrect:
nrow(test_data[!correct_items,])/nrow(test_data)
