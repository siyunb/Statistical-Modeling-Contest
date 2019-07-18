########加载工具包
library(plyr)
library(dplyr)
library(mice)
library(stringr)   #字符串处理
########test数据处理
setwd("D:/bigdatahw/上财东正杯/竞赛数据/整理数据/newdata/newdata")  #设置工作路径
test = read.csv('test.csv',head=TRUE)        #读取测试集的总体数据
test=test[-c(1,2,3,4,6,9)]
test$IS_LOCAL = as.factor(ifelse(test$IS_LOCAL == '本地籍', '1','0')) 
a=test$EDU_LEVEL[87]
test$EDU_LEVEL = as.factor(ifelse(test$EDU_LEVEL == a, '0',test$EDU_LEVEL)) 
test$MARRY_STATUS = as.factor(ifelse(test$MARRY_STATUS == '离异','1',test$MARRY_STATUS))
marry_status= c("1" = "1","2" = "1","4" = "2","5" = "3","6" = "4","7" = "5")
test$MARRY_STATUS=as.factor(marry_status[test$MARRY_STATUS])
test$HAS_FUND = ifelse(is.na(test$HAS_FUND), 0, test$HAS_FUND) #公积金为NA的，保守起见全部为0

test$HAS_FUND <- as.factor(test$HAS_FUND)
test$state_bool_x<-as.factor(test$state_bool_x)
test$state_bool_y<-as.factor(test$state_bool_y)
test$class5_state_bool<-as.factor(test$class5_state_bool)
test$type_bool<-as.factor(test$type_bool)
test$Y_FRAUD<-as.factor(test$Y_FRAUD)

test$EDU_LEVEL=as.numeric(as.character(test$EDU_LEVEL))
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==3|test$EDU_LEVEL==7|test$EDU_LEVEL==8), 1, test$EDU_LEVEL)
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==2), 2, test$EDU_LEVEL)
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==9), 3, test$EDU_LEVEL)
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==10|test$EDU_LEVEL==4|test$EDU_LEVEL==5), 4, test$EDU_LEVEL)
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==0|test$EDU_LEVEL==6), 5, test$EDU_LEVEL)
test$EDU_LEVEL=as.factor(test$EDU_LEVEL)

#整理数据one-hot编码
ohe_feats = c( 'EDUCATION', 'MARRIAGE')
dummies <- dummyVars(~ EDU_LEVEL + MARRY_STATUS, data = test)
df_all_ohe <- as.data.frame(predict(dummies, newdata = test))
test<- cbind(df_all_ohe,test[,-c(2,3)])

#######test数据违约状况预测
#利用predict函数进行预测
test1<-apply(test,2,as.numeric) 
pred<-predict(bst,test1)
pred<-round(pred, 0)  #转化为分类变量
table(as.factor(pred))	 #逾期状况预测结果


