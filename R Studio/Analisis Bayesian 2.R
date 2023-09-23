library(readr)
df = read.csv("D:/Kuliah/BINUS UNIVERSITY/SEMESTER 5/Bayesian Data Analysis/uas_bayes/heart.csv")
View(df)
summary(df)
str(df)

head(df)

# Model Binary Logistic Regression
model1 = glm(output~cp+chol+trtbps+thalachh+age, data = df, family = "binomial")
summary(model1)

# Model Binary Logistic Regression
model2 = glm(output~as.factor(cp)+trtbps+thalachh, data = df, family = "binomial")
summary(model2)


# H0: Model sudah sesuai
# H1: Model belum sesuai
# Uji kelayakan Model2
library(ResourceSelection)
hoslem.test(df$output, fitted(model2))
# p-value > dari alpha 
# Gagal tolak H0 maka kesimpulannya model sudah sesuai. Maka model2 bisa dipakai

# Uji signifikansi parameter secara parsial dilihat dari nilai pr
# H0: beta i = 0
# H1: beta i != 0
# Tolak H0 jika p-value < alpha

AIC(model1)
AIC(model2)

# Evaluasi Model2
# yang dilihat nilai akurasi, presisi ,recall
yhat = fitted(model2)
y = df$output

prediksi = as.factor(ifelse(round(yhat)==1, "Yes", "No"))
aktual = as.factor(ifelse(y==1, "Yes", "No"))

library(caret)
confusionMatrix(prediksi, aktual)


# Model Bayesian
library(rstanarm)
model3 = stan_glm(output~as.factor(cp)+trtbps+thalachh, data = df, family = binomial(), 
                  prior = normal(), prior_intercept = normal())
summary(model3)

#Checking Coefficients
model3$coefficients

# Uji signifikansi
posterior_interval(model3)


# H0: Model sudah sesuai
# H1: Model belum sesuai
# Uji kelayakan Model3
library(ResourceSelection)
hoslem.test(df$output, fitted(model3))
# p-value > dari alpha 
# Gagal tolak H0 maka kesimpulannya model sudah sesuai. Maka model2 bisa dipakai

# Evaluasi model3
y_bayes = df$output
yhat_bayes = model3$fitted.values

predict = as.factor(ifelse(round(yhat_bayes)==1, "Yes", "No"))
actual = as.factor(ifelse(y_bayes==1, "Yes", "No"))
library(caret)
confusionMatrix(predict,actual)


# Nilai MSE model2
library(nortest)
err = residuals(model2)
MSE_model2 = mean(err^2)
MSE_model2

# MSE model3
prediksibayes = model3$fitted.values
errorbayes = y-prediksibayes
head(errorbayes)

MSE_model3 = mean(errorbayes^2)
MSE_model3