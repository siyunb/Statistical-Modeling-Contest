###########��ȡtrain����
library(DMwR)
setwd("D:/bigdatahw/�ϲƶ�����/��������/��������/newdata/newdata")  #���ù���·��
train = read.csv('train.csv',head=TRUE)        #��ȡ���Լ�����������
train=train[-c(1,2,3,4,6,9)]
train$Y<-as.factor(train$Y)
train$�����˴���<-as.factor(train$�����˴���)
train$չ��.����.<-as.factor(train$չ��.����.)
train$state_bool_y<-as.factor(train$state_bool_y)
train$class5_state_bool<-as.factor(train$class5_state_bool)
train$type_bool<-as.factor(train$type_bool)
train$state_bool_x<-as.factor(train$state_bool_x)
train$Y_FRAUD<-as.factor(train$Y_FRAUD)
train$HAS_FUND<-as.factor(train$HAS_FUND)

train_balance<-SMOTE(Y~.,train,perc.over=600,perc.under=100)
train_balance$Y<-as.factor(train_balance$Y)
write.csv(train_balance,"D:/bigdatahw/�ϲƶ�����/��������/��������/newdata/newdata/take_it_for_zuo2.csv",row.names = TRUE)  