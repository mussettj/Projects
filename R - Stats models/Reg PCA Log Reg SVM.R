df=read.csv("boston.csv",header=TRUE)
View(df)

#regression model

model=lm(data=df,HousePrice~.)
summary(model)

#rebuild mdoel drop AGE and INDUS
model=lm(data=df,HousePrice~.-AGE-INDUS)
summary(model)

#check assumptions of regression
par(mfrow=c(2,2))
plot(model)

# plot residuals
par(mfrow=c(1,1))


hist(model$residuals,col = 3)

# identify outliers
cd=cooks.distance(model)

# cd>4/(n-k-1)
dim(df)
cd>4/(506-10-1)
df[cd>4/(506-10-1),]

#create a new dataframe by dropping outliers
df_new=df[cd<=4/(506-10-1),]
dim(df)
dim(df_new)

model_new=lm(data=df_new,HousePrice~.)
summary(model_new)

#drop INDUS

model_new=lm(data=df_new,HousePrice~.-INDUS)
summary(model_new)

par(mfrow=c(2,2))
plot(model_new)

par(mfrow=c(1,1))
hist(model_new$residuals,col = 3)


# VIF: auto correlation
library(car)
car::vif(model_new)

install.packages('car')

#drop RAD based on VIF
model_3=lm(data=df_new,HousePrice~.-INDUS-RAD)
car::vif(model_3)

summary(model_3)

# drop CRIM
model_4=lm(data=df_new,HousePrice~.-INDUS-RAD-CRIM)
car::vif(model_4)

summary(model_4)

hist(model_4$residuals,col = 3)
par(mfrow=c(2,2))
plot(model_4)



#boxcox transformation
model_5=lm(data=df_new,bcPower(HousePrice,2)~.-INDUS-CRIM)
summary(model_5)

plot(model_5)

#log transformation
model_5=lm(data=df_new,log(HousePrice)~.-INDUS-AGE)
summary(model_5)

plot(model_5)

#drop rows 401, 402, 410
data=df_new[-c(401,402,410),]
model_6=lm(data=data,log(HousePrice)~.-INDUS-AGE)
summary(model_6)

plot(model_6)


par(mfrow=c(1,1))
hist(model_6$residuals)


exp(df$HousePrice)
sin(df$HousePrice)

library(car)
bcPower(df$HousePrice,1)

library(dplyr)
houseData=select(df,-HousePrice)
View(houseData)
predict(model_6,houseData)

# PCA
walmart=read.csv("Walmart.csv")

library(factoextra)

x=walmart[,2:length(walmart)]
View(x)

resPca=prcomp(x,scale. = TRUE)

fviz_eig(resPca)


biplot(resPca)

var=resPca$sdev^2
var


#scale var
(var/sum(var))*100

#cumulative sum
cumsum((var/sum(var))*100)

library(psych)
pcaModel=principal(x,nfactors = 7)
pcaModel

x_cmp=predict(pcaModel,x)
View(x_cmp)


#classification: Logistic Regression

data=read.csv("gold fund.csv")

View(data)

#dropping ID
library(dplyr)
data=select(data,-ID)

classModel=glm(data=data,
               Gold_Fund~.,
               family = 'binomial')

summary(classModel)

#VIF
car::vif(classModel)
# drop RelationshipSize
classModel_2=glm(data=data,
                 Gold_Fund~.-RelationshipSize,
                 family = 'binomial')
car::vif(classModel_2)
summary(classModel_2)

# drop AccountSince
classModel_3=glm(data=data,
                 Gold_Fund~.-RelationshipSize-AccountSince,
                 family = 'binomial')
car::vif(classModel_3)
summary(classModel_3)


View(data)
unique(data$Gold_Fund)
# drop column based on p-value
classModel_4=glm(data=data,
                 Gold_Fund~Gender+AvgMonthlyTxn+
                   MF_nos+Balanced_Funds+
                   ELSS,family='binomial')
summary(classModel_4)

# predict
predictedProb=predict(classModel_4,data,
                      type = "response")
summary(predictedProb)

predictedClass=ifelse(predictedProb>=0.5,1,0)
table(data$Gold_Fund,predictedClass)

#accuracy
(800+16)/nrow(data)


# ROC
data$prob=predictedProb
library(pROC)
res=roc(Gold_Fund~prob,data=data)
plot(res)


library(InformationValue)

optimalCutoff(data$Gold_Fund,predictedProb)


predictedClass_1=ifelse(predictedProb>0.4184539,1,0)

table(data$Gold_Fund,predictedClass_1)

(800+18)/nrow(data)



# SVM
View(iris)


library(ggplot2)
library(GGally)
ggpairs(iris,ggplot2::aes(color=Species,alpha=0.4))

library(e1071)

svm_model=svm(Species~.,data=iris,kernel='linear')
pred_linear=predict(svm_model,iris)

table(Actual=iris$Species,Predicted=pred_linear)
145/150


plot(svm_model,data=iris,Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))

#Polynomial Model
svm_model_poly=svm(Species~.,data=iris,
                   kernel='polynomial')
pred_poly=predict(svm_model_poly,iris)

tab=table(Actual=iris$Species,Predicted=pred_poly)
tab
sum(diag(tab))/nrow(iris)


plot(svm_model_poly,data=iris,Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))

#Radial Model
svm_model_radial=svm(Species~.,data=iris,
                   kernel='radial')
pred_radial=predict(svm_model_radial,iris)

tab=table(Actual=iris$Species,Predicted=pred_radial)
tab
sum(diag(tab))/nrow(iris)


plot(svm_model_radial,data=iris,Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))

#plot all three
par(mfrow=c(1,1))
plot(svm_model,data=iris,Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))
plot(svm_model_poly,data=iris,Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))

plot(svm_model_radial,data=iris,Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))

#tune model
set.seed(10)
tmodel=tune(svm,Species~.,data=iris,ranges = list(cost=2^(2:7)))
plot(tmodel)
summary(tmodel)

tmodel=tune(svm,Species~.,data=iris,
            ranges = list(epsilon=seq(0,1,.1),cost=2^(2:7),
                          kernel=c("radial","polynomial")))
plot(tmodel)
summary(tmodel)
summary(tmodel$best.model)
mymodel=tmodel$best.model
plot(mymodel,data=iris,Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))


tab=table(Actual=iris$Species,Predicted=predict(mymodel,iris))
tab

sum(diag(tab))/nrow(iris)


