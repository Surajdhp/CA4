# Reading the dataset
df <- read.csv("C:/Users/SurajPC/Documents/CA4/suraj_final1.csv")
View(df)

# Applying linear model to find the relation between two columns of dataset
simple_linear_model <- lm(Dairy.products ~ Bakery.products, data=df)

simple_linear_model

# Plotting the model
plot(df$Bakery.products,df$Dairy.products,
     xlab="Bakery Products",
     ylab="Dairy Products",
     main = "Scatter plot showing regression line
     for Dairy Products prdicted from Bakery Products")

abline(simple_linear_model)

# Graph shows that the data is linearly plotted
summary(simple_linear_model)


# Finding the correlation
cor(df$Bakery.products, df$Dairy.products)
# Examining the 95% confidence intervals for the model
confint(simple_linear_model)


summary(simple_linear_model)



library(car)

help(scatterplotMatrix)


scatter.smooth(x = df$Dairy.products, 
               y = df$Bakery.products, 
               main = "Dairy Products ~ Bakery Products",
               xlab = "Dairy Products",
               ylab = "Bakery Products")




par(mfrow = c(1, 2)) # divide graph area in 2 columns
boxplot(df$Dairy.products, main = "Dairy Products", sub = paste("Outlier rows: ", boxplot.stats(df$Dairy.products)$out)) # box plot for 'speed'
boxplot(df$Bakery.products, main = "Bakery Products", sub = paste("Outlier rows: ", boxplot.stats(df$Bakery.products)$out)) # box plot for 'distance'
# 1 outlier in distance

# Skewness function to examine normality of data
install.packages("e1071")
library(e1071)
# divide graph area in 2 columns
par(mfrow = c(1, 2))
# density plot for 'speed'
plot(density(df$Dairy.products), main = "Density Plot: Dairy Products", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(df$Dairy.products), 2)))


# Plotting the area under the density plot in red
polygon(density(df$Dairy.products), col = "red")

plot(density(df$Bakery.products), 
     main = "Density Plot: Bakery Products", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(df$Bakery.products), 2))) # density plot for 'dist'
# And also fill the area within the density plot to red
polygon(density(df$Bakery.products), col = "red")



# build linear regression model on full data
linearMod <- lm(Dairy.products ~ Bakery.products, data = df)
print(linearMod)

# model summary
summary(linearMod)

# demo of how to calculate t-statistic and p-values
# capture model summary as an object
model_summary <- summary(linearMod)

# model coefficients
model_coeffs <- model_summary$coefficients

# get beta estimate for speed
beta.estimate <- model_coeffs["Bakery.products", "Estimate"]

# get std.error for speed
std_error <- model_coeffs["Bakery.products", "Std. Error"]

# calc t statistic
t_value <- beta.estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = nrow(linearMod) - ncol(linearMod)) # calc p Value
f_statistic <- linearMod$fstatistic[1] # fstatistic
f <- summary(linearMod)$fstatistic # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower = FALSE)

# AIC
AIC(linearMod)

# BIC
BIC(linearMod)

# --------------------------------------------------------------------

# Create Training and Test data
# setting seed to reproduce results of random sampling
# Fitting the dataset in the model
set.seed(200)

# sample chooses a random sample
# from 1:all records from cars, 80% of rows
no_of_records <- sample(1:nrow(df), 0.8 * nrow(df))
# model training data
training_data <- df[no_of_records,]
training_data
# test data
testing_data <- df[-no_of_records,]
testing_data


lr_model <- lm(Dairy.products ~ Bakery.products, data = training_data)

# model summary
summary(lr_model)

# predict distance from testing data
BakeryProducts_predicted <- predict(lr_model, testing_data)

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals = testing_data$Bakery.products, 
                                  predicted = BakeryProducts_predicted))
head(actuals_preds)

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

#plotting of predictions
install.packages("ggplot2")
#library(ggplot2)
#library(lattice)
ylim <- c(0,max(actuals_preds[c('actuals','predicted')])*1.8);
cols <- c('red','blue');
barplot(
  t(actuals_preds[c('actuals','predicted')]),
  beside=T,
  ylim=ylim,
  border=cols,
  col='white',
  names.arg=rownames(actuals_preds),
  xlab='Actuals ~ Predicted',
  ylab='Value',
  legend.text=c('Actuals','Predicted'),
  args.legend=list(text.col=cols,col=cols,border=cols,bty='n'))

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape

