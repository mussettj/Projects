df=read.csv("gold fund.csv",header = TRUE)

#split data into train and test

## 75% of data into train
nrow(df)
sample_size=floor(0.75*nrow(df))

sample_size
#set the seeds so that we all get same split
set.seed(123)
train_indices=sample(seq_len(nrow(df)),size=sample_size)
train_indices

library(dplyr)

train_data=df[train_indices,]
test_data=df[-train_indices,]

y_train=train_data$Gold_Fund
y_test=test_data$Gold_Fund

x_train=select(train_data,-Gold_Fund)
x_test=select(test_data,-Gold_Fund)

#build knn
library(class)

test_pred=knn(train = x_train,
              test = x_test,cl=y_train,k=1)

test_pred
dim(train_data)
dim(test_data)
length(trainLabel)
length(testLabel)

#evaluate model

library(gmodels)
CrossTable(x=y_test,y=test_pred,prop.chisq = FALSE)


table(y_train)

191/207
191/201
191/219




#Iris Data: Naive Bayes Model
library(e1071)
library(caTools)
library(caret)


#splitting data
set.seed(10)
split=sample.split(iris,SplitRatio = 0.7)
split
train_data=subset(iris,split==TRUE)
test_data=subset(iris,split==FALSE)
dim(train_data)
dim(test_data)

#scale data
train_scale=scale(train_data[,1:4])
test_scale=scale(test_data[,1:4])
help("naiveBayes")

model=naiveBayes(Species~.,data=train_data)
model

#predict test data
test_pred=predict(model,test_data)

CrossTable(x=test_data$Species,
           y=test_pred,prop.chisq = FALSE)

58/60

library(GGally)
library(ggplot2)
ggpairs(iris,ggplot2::aes(color=Species))


creditData=read.csv("Credit.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)

dim(creditData)
table(creditData$Credit)

#Split data
sample_size=floor(0.75*nrow(creditData))
sample_size
set.seed(120)
train_indices=sample(seq_len(nrow(creditData)),size = sample_size)
train_data=creditData[train_indices,]
test_data=creditData[-train_indices,]

View(train_data)

#scale data
train_data[,2:11]=scale(train_data[,2:11])
View(train_data)
test_data[,2:11]=scale(test_data[,2:11])

library(party)
help(ctree)


loanModel=ctree(factor(Credit)~.,data=train_data)

loanModel
plot(loanModel,cex=0.4,cex.main=2)

plot(loanModel,type="simple")




######
library(tree)
creditModel=tree(factor(Credit)~.,data=train_data)
plot(creditModel)
text(creditModel,pretty=0)





library(rpart)
library(rpart.plot)

#grow a tree

model=rpart(factor(Credit)~.,data=train_data,method="class",
            control =rpart.control(minsplit =1, cp=0))


test_pred=predict(model,test_data,type="class")
test_pred


table(test_data$Credit,test_pred)

plot(model,uniform=TRUE)
text(model,use.n=TRUE,all=TRUE,cex=.8)

model=rpart(factor(Credit)~.,data=train_data,method="class",
            control =rpart.control(minsplit =10,maxdepth=5))


plot(model,uniform=TRUE)
text(model,use.n=TRUE,all=TRUE,cex=.8)



(1444+53)/nrow(test_data)


# Random Forest Model
library(randomForest)
rfModel=randomForest(factor(Credit)~.,
                     data=train_data)
print(rfModel)
importance(rfModel)


#prediction
test_pred_rf=predict(rfModel,test_data,type="class")
table(test_data$Credit,test_pred_rf)


#change number trees to 1000
rfModel=randomForest(factor(Credit)~.,
                     data=train_data,ntree=1000)
print(rfModel)
importance(rfModel)


#prediction
test_pred_rf=predict(rfModel,test_data,type="class")
table(test_data$Credit,test_pred_rf)


#K fold cross validation

library(caret)
set.seed(100)
train_control=trainControl(method = "cv",number=10)

cvModel=train(factor(Credit)~.,data=train_data,
              trControl=train_control,
              method="logreg")

print(cvModel)

help("train")



View(train_data)
x_train=train_data[,2:ncol(train_data)]
x_test=test_data[,2:ncol(test_data)]


clModel=kmeans(x_train,10)
clModel$cluster

table(train_data$Credit,clModel$cluster)

clModel$betweenss


clModel$withinss

clModel$tot.withinss
set.seed(100)
ttSSW=vector()
for (i in 1:20){
  clModel=kmeans(x_train,i)
  ttSSW=c(ttSSW,clModel$betweenss)
}

plot(1:20,ttSSW)


ttSSW






