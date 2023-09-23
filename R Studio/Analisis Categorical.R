library(readxl)
mydata = read_excel("D:/Kuliah/BINUS UNIVERSITY/SEMESTER 5/Categorical Data Analysis/bahan_uas.xlsx")
View(mydata)
head(mydata)

mydata$Improvement[mydata$Improvement == 'No'] <- 0
mydata$Improvement[mydata$Improvement == 'Yes'] <- 1

mydata$Improvement = as.integer(mydata$Improvement)
str(mydata)

# Interaction
model1 = glm(Improvement~Sex+Treatment+(Sex*Treatment), data = mydata, family = "binomial")
summary(model1)

# Likelihood
model2 = glm(Improvement~1, data = mydata, family = "binomial")
anova(model2,model3,test="LRT")

# Wald test
summary(model1)

# Estimasi Parameter
model3 = glm(Improvement~Sex+Treatment, data = mydata, family = "binomial")
summary(model3)
coef(model3)

# Predict dengan interaksi
prob_model1 = exp(-0.3795-1.9231+1.6323+0.6703)/(1+exp(-0.3795-1.9231+1.6323+0.6703))
prob_model1

# Predict model tanpa interaksi
prob_model2 = exp(-0.4351-1.4687+1.7817)/(1+exp(-0.4351-1.4687+1.7817))
prob_model2
1-prob_model2
