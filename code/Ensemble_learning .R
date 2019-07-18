library(reshape2)
library(dplyr)
library(pryr) 
#########数据处理
setwd("D:/bigdatahw/上财东正杯/竞赛数据/整理数据")  #设置工作路径
train = read.csv('训练集2.csv',head=TRUE)        #读取测试集的总体数据
train$IS_LOCAL = as.factor(ifelse(train$IS_LOCAL == '本地籍', '1','0')) 
a=train$EDU_LEVEL[9]
train$EDU_LEVEL = as.factor(ifelse(train$EDU_LEVEL == a, '0',train$EDU_LEVEL)) 
train$MARRY_STATUS = as.factor(ifelse(train$MARRY_STATUS == '离异','1',train$MARRY_STATUS))
marry_status= c("1" = "1","3" = "2","4" = "3","5" = "4","6" = "5")
train$MARRY_STATUS=as.factor(marry_status[train$MARRY_STATUS])


test = read.csv('测试集2.csv',head=TRUE)        #读取测试集的总体数据
test$IS_LOCAL = as.factor(ifelse(test$IS_LOCAL == '本地籍', '1','0')) 
test$EDU_LEVEL = as.factor(ifelse(test$EDU_LEVEL == a, '0',test$EDU_LEVEL)) 
test$MARRY_STATUS = as.factor(ifelse(test$MARRY_STATUS == '离异','1',test$MARRY_STATUS))
test$MARRY_STATUS=as.factor(marry_status[test$MARRY_STATUS])

#########整理变量类型
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

write.csv(train,"D:/bigdatahw/上财东正杯/竞赛数据/整理数据/newdata/newdata/trian.csv",row.names = TRUE)  
write.csv(test,"D:/bigdatahw/上财东正杯/竞赛数据/整理数据/newdata/newdata/test.csv",row.names = TRUE)  

#将学历进行合并，分为五类研究生，本科，专科，专科以下以及其他
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

#定义XGBoost模型测试集结果评价函数，
#包含Accuracy，recall，precision，F_measure四个评价指标
#Accuracy总正确率，recall后验准确率，precision先验准确率，F_measure指数
index2=function(table) {
  Accuracy=table[1,1]+table[2,2]
  precision=table[2,2]/sum(table[,2])
  recall=table[2,2]/sum(table[2,])
  F_measure=2*precision*recall/(precision+recall)#计算Recall，Precision和F-measure
  results=data.frame(Accuracy=Accuracy,recall=recall,precision=precision,F_measure=F_measure)
  return(results)
}

######Bagging集成方法，利用决策树作为基分类器
library(randomForest)
train_bag=randomForest(Y~., train, mtry=round(sqrt(ncol(train)),0)) 
#所构造的基分类器的决策树采用变量为所有变量的p^1/2个。  

###将Bagging模型用于测试集
train_bag.pred = predict(train_bag, test[,-5]) #去掉训练集数据，并同时去掉预测变量

accuracy=mean(train_bag.pred==test$Y) * 100    #计算准确率,正确率达到79.25%
print(sprintf('The bagging accuracy is %f', accuracy))

######随机森林
###构造随机森林模型。随机树也就是bagging+决策树
train_rf=randomForest(Y~., train,importance=T) #显示变量重要性
###测试
train_rf.pred = predict(train_rf, test[,-5])        #选取树的特征变量和树的棵数都是默认的
accuracy=mean(train_rf.pred==test$Y) * 100      
print(sprintf('The randomforest accuracy is %f', accuracy)) #与把bagging类似，只是提高了树的棵数，使得准确率有所提高


######Boosting,用gbm包通过集成来提升决策树能力
###建立Boosting模型
library(gbm)            #载入gbm包
library(caret)
library(mice)
#采用梯度提升方法Gradient Boosting
train_boost=gbm(as.numeric(as.character(Y))~., train, distribution='bernoulli', 
                 n.trees=5000, interaction.depth=4)         
#基分类器仍为决策树，形成的基分类器为5000个，也就是迭代次数为5000，
#使用修正测试点的分布，也就是损失函数为伯努利分布，分类问题一般选择bernoulli分布
#每棵树的做大深度为4减轻过拟合
#注意二分变量必须为0,1,且为数值型
train_boost.pred = predict(train_boost, test[,-5], n.trees=5000)
#建立混淆矩阵
confusionMatrix(as.numeric(as.character(test$Y)) > 0, train_boost.pred > 0)
print('The Gradient Boosting accuracy is 90.81%')

####adaboost
library(adabag)
library(pryr)
model.AdaBoost <- boosting(Y~.,data = train)
model.pred <- predict(model.AdaBoost,newdata = test,type='class')
model.pred$confusion
print('The Gradient Boosting accuracy is 93.09%')


#首先需要进行xgboost包的安装，只需在R中运行install.packages(‘xgboost’)即可。。
library('xgboost')
library(caret)
#整理数据one-hot编码
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
x<-apply(x,2,as.numeric)               #将x的列变量转化为数字型
y<-as.numeric(y)-1              #将y也转化为数字型变量

bst<-xgboost(data=x,label=y,max.depth=3,eta=1,nround=51,objective='binary:logistic')
#在此处，我们选取最大深度为2，学习速率为1，决策树棵树为10，目标函数是基于二分类问题的logistic损失进行模型建立。
# xgboost训练过程
#也可以利用函数xgb.cv对数据进行交叉验证分析，利用交叉验证确定均方误差,利用交叉检验确定最佳迭代次数
cv.res<-xgb.cv(data=x,label=y,max.depth=3,eta=1,nround=100,objective='binary:logistic',nfold=5)

#利用predict函数进行预测
test1=test[,c(1:12,14:56)]
test1<-apply(test1,2,as.numeric) 
pred<-predict(bst,test1)
pred<-round(pred, 0)  #转化为分类变量
true<-as.factor(test[,13])
table_XG=table(true,pred)/nrow(test)	 #混淆数量矩阵
table_XG                    #从混淆矩阵中看出并不好  
a=index2(table_XG) 
a

#绘制ROC曲线
library(pROC) 
xg.train.modelroc <- roc(test[,13], pred)  
plot(xg.train.modelroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),  
     grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE) 

#变量重要性
names <- dimnames(data.matrix(test1))[[2]]
importance <- xgb.importance(names, model = bst)  
head(importance) 
xgb.plot.importance(importance[1:10,],xlab='Gain',col ='blue')

#交叉验证图
cv.res <- as.data.frame(cv.res$evaluation_log)
cv.res<-melt(cv.res[,c(1,2,4)],id = c("iter"),     
             variable.name = "type", 
             value.name = "cases",
             na.rm = TRUE)                #控制两列变量在新表中不动新生成的变量用case标识
ggplot(data=cv.res, aes(x=iter, y=cases, group=type, colour=type)) +
  geom_line(size=1.6) +
  geom_point() +
  xlab("决策树棵数") + ylab("均方误差")+
  ggtitle('交叉检验确定最优棵数')+
  theme(plot.title = element_text(hjust =0.5,family="myFont",size=20,color="red"), 
        panel.background=element_rect(fill='aliceblue',color='black'),panel.grid.minor = element_blank())

