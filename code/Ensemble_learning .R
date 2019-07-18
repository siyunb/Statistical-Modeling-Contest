library(reshape2)
library(dplyr)
library(pryr) 
#########���ݴ���
setwd("D:/bigdatahw/�ϲƶ�����/��������/��������")  #���ù���·��
train = read.csv('ѵ����2.csv',head=TRUE)        #��ȡ���Լ�����������
train$IS_LOCAL = as.factor(ifelse(train$IS_LOCAL == '���ؼ�', '1','0')) 
a=train$EDU_LEVEL[9]
train$EDU_LEVEL = as.factor(ifelse(train$EDU_LEVEL == a, '0',train$EDU_LEVEL)) 
train$MARRY_STATUS = as.factor(ifelse(train$MARRY_STATUS == '����','1',train$MARRY_STATUS))
marry_status= c("1" = "1","3" = "2","4" = "3","5" = "4","6" = "5")
train$MARRY_STATUS=as.factor(marry_status[train$MARRY_STATUS])


test = read.csv('���Լ�2.csv',head=TRUE)        #��ȡ���Լ�����������
test$IS_LOCAL = as.factor(ifelse(test$IS_LOCAL == '���ؼ�', '1','0')) 
test$EDU_LEVEL = as.factor(ifelse(test$EDU_LEVEL == a, '0',test$EDU_LEVEL)) 
test$MARRY_STATUS = as.factor(ifelse(test$MARRY_STATUS == '����','1',test$MARRY_STATUS))
test$MARRY_STATUS=as.factor(marry_status[test$MARRY_STATUS])

#########������������
test$HAS_FUND <- as.factor(test$HAS_FUND)
test$Y <- as.factor(test$Y)
test$state_bool_x<-as.factor(test$state_bool_x)
test$state_bool_y<-as.factor(test$state_bool_y)
test$class5_state_bool<-as.factor(test$class5_state_bool)
test$type_bool<-as.factor(test$type_bool)
test$Y_FRAUD<-as.factor(test$Y_FRAUD)


train$HAS_FUND <- as.factor(train$HAS_FUND)
train$Y <- as.factor(train$Y)
train$state_bool_x<-as.factor(train$state_bool_x)
train$state_bool_y<-as.factor(train$state_bool_y)
train$class5_state_bool<-as.factor(train$class5_state_bool)
train$type_bool<-as.factor(train$type_bool)
train$Y_FRAUD<-as.factor(train$Y_FRAUD)

write.csv(train,"D:/bigdatahw/�ϲƶ�����/��������/��������/newdata/newdata/trian.csv",row.names = TRUE)  
write.csv(test,"D:/bigdatahw/�ϲƶ�����/��������/��������/newdata/newdata/test.csv",row.names = TRUE)  

#��ѧ�����кϲ�����Ϊ�����о��������ƣ�ר�ƣ�ר�������Լ�����
train$EDU_LEVEL=as.numeric(as.character(train$EDU_LEVEL))
train$EDU_LEVEL = ifelse((train$EDU_LEVEL ==3|train$EDU_LEVEL==7|train$EDU_LEVEL==8), 1, train$EDU_LEVEL)
train$EDU_LEVEL = ifelse((train$EDU_LEVEL ==2), 2, train$EDU_LEVEL)
train$EDU_LEVEL = ifelse((train$EDU_LEVEL ==9), 3, train$EDU_LEVEL)
train$EDU_LEVEL = ifelse((train$EDU_LEVEL ==10|train$EDU_LEVEL==4|train$EDU_LEVEL==5), 4, train$EDU_LEVEL)
train$EDU_LEVEL = ifelse((train$EDU_LEVEL ==0|train$EDU_LEVEL==6), 5, train$EDU_LEVEL)
train$EDU_LEVEL=as.factor(train$EDU_LEVEL)

test$EDU_LEVEL=as.numeric(as.character(test$EDU_LEVEL))
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==3|test$EDU_LEVEL==7|test$EDU_LEVEL==8), 1, test$EDU_LEVEL)
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==2), 2, test$EDU_LEVEL)
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==9), 3, test$EDU_LEVEL)
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==10|test$EDU_LEVEL==4|test$EDU_LEVEL==5), 4, test$EDU_LEVEL)
test$EDU_LEVEL = ifelse((test$EDU_LEVEL ==0|test$EDU_LEVEL==6), 5, test$EDU_LEVEL)
test$EDU_LEVEL=as.factor(test$EDU_LEVEL)

#����XGBoostģ�Ͳ��Լ�������ۺ�����
#����Accuracy��recall��precision��F_measure�ĸ�����ָ��
#Accuracy����ȷ�ʣ�recall����׼ȷ�ʣ�precision����׼ȷ�ʣ�F_measureָ��
index2=function(table) {
  Accuracy=table[1,1]+table[2,2]
  precision=table[2,2]/sum(table[,2])
  recall=table[2,2]/sum(table[2,])
  F_measure=2*precision*recall/(precision+recall)#����Recall��Precision��F-measure
  results=data.frame(Accuracy=Accuracy,recall=recall,precision=precision,F_measure=F_measure)
  return(results)
}

######Bagging���ɷ��������þ�������Ϊ��������
library(randomForest)
train_bag=randomForest(Y~., train, mtry=round(sqrt(ncol(train)),0)) 
#������Ļ��������ľ��������ñ���Ϊ���б�����p^1/2����  

###��Baggingģ�����ڲ��Լ�
train_bag.pred = predict(train_bag, test[,-5]) #ȥ��ѵ�������ݣ���ͬʱȥ��Ԥ�����

accuracy=mean(train_bag.pred==test$Y) * 100    #����׼ȷ��,��ȷ�ʴﵽ79.25%
print(sprintf('The bagging accuracy is %f', accuracy))

######���ɭ��
###�������ɭ��ģ�͡������Ҳ����bagging+������
train_rf=randomForest(Y~., train,importance=T) #��ʾ������Ҫ��
###����
train_rf.pred = predict(train_rf, test[,-5])        #ѡȡ�����������������Ŀ�������Ĭ�ϵ�
accuracy=mean(train_rf.pred==test$Y) * 100      
print(sprintf('The randomforest accuracy is %f', accuracy)) #���bagging���ƣ�ֻ����������Ŀ�����ʹ��׼ȷ���������


######Boosting,��gbm��ͨ����������������������
###����Boostingģ��
library(gbm)            #����gbm��
library(caret)
library(mice)
#�����ݶ���������Gradient Boosting
train_boost=gbm(as.numeric(as.character(Y))~., train, distribution='bernoulli', 
                 n.trees=5000, interaction.depth=4)         
#����������Ϊ���������γɵĻ�������Ϊ5000����Ҳ���ǵ�������Ϊ5000��
#ʹ���������Ե�ķֲ���Ҳ������ʧ����Ϊ��Ŭ���ֲ�����������һ��ѡ��bernoulli�ֲ�
#ÿ�������������Ϊ4��������
#ע����ֱ�������Ϊ0,1,��Ϊ��ֵ��
train_boost.pred = predict(train_boost, test[,-5], n.trees=5000)
#������������
confusionMatrix(as.numeric(as.character(test$Y)) > 0, train_boost.pred > 0)
print('The Gradient Boosting accuracy is 90.81%')

####adaboost
library(adabag)
library(pryr)
model.AdaBoost <- boosting(Y~.,data = train)
model.pred <- predict(model.AdaBoost,newdata = test,type='class')
model.pred$confusion
print('The Gradient Boosting accuracy is 93.09%')


#������Ҫ����xgboost���İ�װ��ֻ����R������install.packages(��xgboost��)���ɡ���
library('xgboost')
library(caret)
#��������one-hot����
ohe_feats = c( 'EDUCATION', 'MARRIAGE')
dummies <- dummyVars(~ +  EDU_LEVEL + MARRY_STATUS, data = train)
df_all_ohe <- as.data.frame(predict(dummies, newdata = train))
train <- cbind(df_all_ohe,train[,-c(2,3)])

ohe_feats = c( 'EDUCATION', 'MARRIAGE')
dummies <- dummyVars(~ EDU_LEVEL + MARRY_STATUS, data = test)
df_all_ohe <- as.data.frame(predict(dummies, newdata = test))
test<- cbind(df_all_ohe,test[,-c(2,3)])

x<-train[,c(1:12,14:56)]
y<-train[,13]
x<-apply(x,2,as.numeric)               #��x���б���ת��Ϊ������
y<-as.numeric(y)-1              #��yҲת��Ϊ�����ͱ���

bst<-xgboost(data=x,label=y,max.depth=3,eta=1,nround=51,objective='binary:logistic')
#�ڴ˴�������ѡȡ������Ϊ2��ѧϰ����Ϊ1������������Ϊ10��Ŀ�꺯���ǻ��ڶ����������logistic��ʧ����ģ�ͽ�����
# xgboostѵ������
#Ҳ�������ú���xgb.cv�����ݽ��н�����֤���������ý�����֤ȷ���������,���ý������ȷ����ѵ�������
cv.res<-xgb.cv(data=x,label=y,max.depth=3,eta=1,nround=100,objective='binary:logistic',nfold=5)

#����predict��������Ԥ��
test1=test[,c(1:12,14:56)]
test1<-apply(test1,2,as.numeric) 
pred<-predict(bst,test1)
pred<-round(pred, 0)  #ת��Ϊ�������
true<-as.factor(test[,13])
table_XG=table(true,pred)/nrow(test)	 #������������
table_XG                    #�ӻ��������п���������  
a=index2(table_XG) 
a

#����ROC����
library(pROC) 
xg.train.modelroc <- roc(test[,13], pred)  
plot(xg.train.modelroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),  
     grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE) 

#������Ҫ��
names <- dimnames(data.matrix(test1))[[2]]
importance <- xgb.importance(names, model = bst)  
head(importance) 
xgb.plot.importance(importance[1:10,],xlab='Gain',col ='blue')

#������֤ͼ
cv.res <- as.data.frame(cv.res$evaluation_log)
cv.res<-melt(cv.res[,c(1,2,4)],id = c("iter"),     
             variable.name = "type", 
             value.name = "cases",
             na.rm = TRUE)                #�������б������±��в��������ɵı�����case��ʶ
ggplot(data=cv.res, aes(x=iter, y=cases, group=type, colour=type)) +
  geom_line(size=1.6) +
  geom_point() +
  xlab("����������") + ylab("�������")+
  ggtitle('�������ȷ�����ſ���')+
  theme(plot.title = element_text(hjust =0.5,family="myFont",size=20,color="red"), 
        panel.background=element_rect(fill='aliceblue',color='black'),panel.grid.minor = element_blank())
