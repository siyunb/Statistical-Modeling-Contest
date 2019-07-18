###########读取train数据
library(DMwR)
setwd("D:/bigdatahw/上财东正杯/竞赛数据/整理数据/newdata/newdata")  #设置工作路径
train = read.csv('train.csv',head=TRUE)        #读取测试集的总体数据
train=train[-c(1,2,3,4,6,9)]
train$Y<-as.factor(train$Y)
train$担保人代还<-as.factor(train$担保人代还)
train$展期.延期.<-as.factor(train$展期.延期.)
train$state_bool_y<-as.factor(train$state_bool_y)
train$class5_state_bool<-as.factor(train$class5_state_bool)
train$type_bool<-as.factor(train$type_bool)
train$state_bool_x<-as.factor(train$state_bool_x)
train$Y_FRAUD<-as.factor(train$Y_FRAUD)
train$HAS_FUND<-as.factor(train$HAS_FUND)

train_balance<-SMOTE(Y~.,train,perc.over=600,perc.under=100)
train_balance$Y<-as.factor(train_balance$Y)
write.csv(train_balance,"D:/bigdatahw/上财东正杯/竞赛数据/整理数据/newdata/newdata/take_it_for_zuo2.csv",row.names = TRUE)  
