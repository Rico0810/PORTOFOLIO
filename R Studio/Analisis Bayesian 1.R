library(readxl)
car = read.csv("D:/Kuliah/BINUS UNIVERSITY/SEMESTER 5/Bayesian Data Analysis/AOL/Car_Purchasing_Data.csv")
View(car)

dataset = car[-c(1,2,3)]
View(dataset)

# Model Regresi
model1 = lm(dataset$Car.Purchase.Amount~., data = dataset)
summary(model1)

model2 = lm(dataset$Car.Purchase.Amount~dataset$Age+dataset$Annual.Salary+dataset$Net.Worth, data = dataset)
summary(model2)

# Bandingkan Nilai AIC
AIC(model1)
AIC(model2)

# Uji Asumsi
fit = fitted(model2)
error = resid(model2)

# ASUMSI I -> UJI NORMALITY
library(nortest)
lillie.test(error)
shapiro.test(error)

# ASUMSI II -> UJI HOMOKEDASTIS
library(lmtest)
bptest(model2)

# ASUMSI III- -> UJI AUTOKORELASI
dwtest(model2)

# Cari Nilai MSE
library(nortest)
err = residuals(model2)
MSE = mean(err^2)
MSE

library(rstanarm)
# Model Bayesian
model3 = stan_glm(Car.Purchase.Amount~Age+Annual.Salary+Net.Worth, data = dataset)
posterior_interval(model3)

y = dataset$Car.Purchase.Amount
x = dataset[c(2,3,5)]

library(BLR)
modelbayes = BLR(y,x)

# Melihat nilai Intercept
modelbayes$mu

# Nilai dari setiap parameter parameter
modelbayes$bF

summary(modelbayes)
modelbayes$SD.bF

# MSE
prediksibayes = modelbayes$yHat
errorbayes = y-prediksibayes
head(errorbayes)

MSE = mean(errorbayes^2)
MSE

prediksibayes = modelbayes$yHat
#Error hitung secara manual
errorbayes = y-prediksibayes
#selisih antara y dengan yhat
head(errorbayes)

# Bandingkan bayes dengan OLS pake MSE aja
MSE = mean((errorbayes)^2)
MSE
