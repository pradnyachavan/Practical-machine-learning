m_train<- data.frame(read.csv(file="pml-training.csv",head=TRUE,sep=","))
m_test<- data.frame(read.csv(file="pml-testing.csv",head=TRUE,sep=","))

library(caret)

sub_train<-data.frame(m_train[,c('num_window','roll_belt','pitch_belt','yaw_belt','total_accel_belt','gyros_belt_x','gyros_belt_y','gyros_belt_z','accel_belt_x',
'accel_belt_y','accel_belt_z','magnet_belt_x','magnet_belt_y','magnet_belt_z','roll_arm','pitch_arm','yaw_arm','total_accel_arm',
'gyros_arm_x','gyros_arm_y', 'gyros_arm_z','accel_arm_x','accel_arm_y','accel_arm_z','magnet_arm_y','magnet_arm_z','roll_dumbbell',
'pitch_dumbbell','yaw_dumbbell',
'total_accel_dumbbell','gyros_dumbbell_x','gyros_dumbbell_y','gyros_dumbbell_z','accel_dumbbell_x',
'accel_dumbbell_y','accel_dumbbell_z','magnet_dumbbell_x','magnet_dumbbell_y','magnet_dumbbell_z','roll_forearm','pitch_forearm',
'yaw_forearm','total_accel_forearm','gyros_forearm_x','gyros_forearm_y','gyros_forearm_z','accel_forearm_x','accel_forearm_y',
'accel_forearm_z','magnet_forearm_x','magnet_forearm_y','magnet_forearm_z','classe')])


test<-data.frame(m_test[,c('num_window','roll_belt','pitch_belt','yaw_belt','total_accel_belt','gyros_belt_x','gyros_belt_y','gyros_belt_z','accel_belt_x',
'accel_belt_y','accel_belt_z','magnet_belt_x','magnet_belt_y','magnet_belt_z','roll_arm','pitch_arm','yaw_arm','total_accel_arm',
'gyros_arm_x','gyros_arm_y', 'gyros_arm_z','accel_arm_x','accel_arm_y','accel_arm_z','magnet_arm_y','magnet_arm_z','roll_dumbbell',
'pitch_dumbbell','yaw_dumbbell',
'total_accel_dumbbell','gyros_dumbbell_x','gyros_dumbbell_y','gyros_dumbbell_z','accel_dumbbell_x',
'accel_dumbbell_y','accel_dumbbell_z','magnet_dumbbell_x','magnet_dumbbell_y','magnet_dumbbell_z','roll_forearm','pitch_forearm',
'yaw_forearm','total_accel_forearm','gyros_forearm_x','gyros_forearm_y','gyros_forearm_z','accel_forearm_x','accel_forearm_y',
'accel_forearm_z','magnet_forearm_x','magnet_forearm_y','magnet_forearm_z')])


#intrain<-createDataPartition(y=sub_train$classe,p=0.7,list=FALSE)
#training<-m_train[intrain,]
#validation<-m_train[-intrain,]


require(caTools)
sample = sample.split(sub_train$classe, SplitRatio = .60)
train = subset(sub_train, sample == TRUE)
valid = subset(sub_train, sample == FALSE)


nsv <- nearZeroVar(train, saveMetrics=TRUE)
nsv

train<-subsettrain[,-c('min_yaw_belt')]

corr<-abs(cor(train[,unlist(lapply(train, is.numeric))]))
diag(corr)<-0
which(corr>0.8,arr.ind = T)

sapply(train, sd,na.rm=TRUE)



mean(train$num_window)
sd(train$num_window)
hist(train$roll_belt,main="",xlab="num_window")
library(caret)
check<-
preobj<-preProcess(train[,c(1,2,3)],method=c("center","scale"))
train_roll_belt<-predict(preobj,train[,c(1,2,3)])$roll_belt
sapply(train_roll_belt, sd,na.rm=TRUE)
sd(train_roll_belt)
hist(train_roll_belt,main="",xlab="num_window")

train$classe<-factor(train$classe)
library(randomForest)

corr<-abs(cor(m_train[,unlist(lapply(m_train, is.numeric))]))
diag(corr)<-0
which(corr>0.8,arr.ind = T)

##knn

modelfit_knn<-train(classe ~.,method="knn",data=train,
                    trControl = trainControl(method = "adaptive_cv"))
                    
print(modelfit_knn$finalModel)


valid_knn<-predict(modelfit_knn,newdata=valid)
confusionMatrix(valid_knn,valid$classe)



##Classification tree
library(rpart)
modelfit_tree<-train(classe ~.,method="rpart",data=train)

library(rattle)
fancyRpartPlot(modelfit_tree$finalModel)

valid_tree<-predict(modelfit_tree,newdata=valid)


test_tree<-predict(modelfit_tree,newdata=test)
confusionMatrix(valid_tree,valid$classe)
confusionMatrix(valid_tree,m_test$classe)




##Random forest
library(randomForest)

modFit_prerf <- randomForest(classe ~. ,preProcess=c("center","scale"),trControl=trainControl(method = "cv", number = 4), data=train)
valid_prerf <- predict(modFit_prerf,valid, type = "class")
pred_test_rf <- predict(modFit_prerf,test, type = "class")
print(pred_test_rf)
confusionMatrix(valid_prerf, valid$classe)

Accu <- matrix(c("CART",0.4974,"KNN",0.9194,"Random Forest",0.9968),ncol=2,byrow=TRUE)
colnames(Accu) <- c("Model","Accuracy")
Accu<-as.table(Accu)
Accu



###END##################################################################################

table(train$num_window)
qplot(num_window,classe,data=train)
num_train<-train[,unlist(lapply(train, is.numeric))]
library(polycor)
hetcor(train$roll_belt,train$classe) 
cor(num_train$num_window,num_train$classe)


names(train2)




