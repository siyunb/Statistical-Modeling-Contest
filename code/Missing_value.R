library(plyr)
library(dplyr)
library(mice)
library(stringr)   #�ַ�������

#######��ȡ����
setwd("D:/bigdatahw/�ϲƶ�����/��������/��������/newdata/newdata")  #���ù���·��
alldata_test = read.csv('alldata_test.csv',head=TRUE)        #��ȡ���Լ�����������
fraud = read.csv('contest_fraud.csv',head=TRUE)     #��ȡ��թ��Ϊ�� 
alldata_test=merge(alldata_test,fraud,by='REPORT_ID',all.x=TRUE) 
md.pattern(alldata_test)
#######ȱʧֵ����
alldata_test$count_org_loan = ifelse(is.na(alldata_test$count_org_loan), 0, alldata_test$count_org_loan)
alldata_test$count_org_card = ifelse(is.na(alldata_test$count_org_card), 0, alldata_test$count_org_card)
alldata_test$amount_mean_loan = ifelse(is.na(alldata_test$amount_mean_loan), 0, alldata_test$amount_mean_loan)
alldata_test$amount_mean_card = ifelse(is.na(alldata_test$amount_mean_card), 0, alldata_test$amount_mean_card)
alldata_test$cyc_max_loan = ifelse(is.na(alldata_test$cyc_max_loan), 0, alldata_test$cyc_max_loan)
alldata_test$cyc_max_card = ifelse(is.na(alldata_test$cyc_max_card), 0, alldata_test$cyc_max_card)
alldata_test$overdue_amount_mean_loan = ifelse(is.na(alldata_test$overdue_amount_mean_loan), 0, alldata_test$overdue_amount_mean_loan)
alldata_test$overdue_amount_mean_card = ifelse(is.na(alldata_test$overdue_amount_mean_card), 0, alldata_test$overdue_amount_mean_card)
alldata_test$yanqimonth_loan = ifelse(is.na(alldata_test$yanqimonth_loan), 0, alldata_test$yanqimonth_loan)
alldata_test$yanqimonth_card = ifelse(is.na(alldata_test$yanqimonth_card), 0, alldata_test$yanqimonth_card)
alldata_test$state_bool_loan = ifelse(is.na(alldata_test$state_bool_loan), 0, alldata_test$state_bool_loan)
alldata_test$state_bool_card = ifelse(is.na(alldata_test$state_bool_card), 0, alldata_test$state_bool_card)
alldata_test$type_bool_loan= ifelse(is.na(alldata_test$type_bool_loan), 0, alldata_test$type_bool_loan)
alldata_test$class5_state_bool_loan = ifelse(is.na(alldata_test$class5_state_bool_loan), 0, alldata_test$class5_state_bool_loan)
###########д��csv�ļ�
write.csv(alldata_test,"D:/bigdatahw/�ϲƶ�����/��������/��������/newdata/newdata/alldata_test.csv",row.names = TRUE)  


###########ʡ�����ݴ���
alldata_test=alldata_test[-6] #ɾ��workprovince����
col<-str_extract_all(alldata_test$ID_CARD,"\\d")
province <- rep(0,length(col))
for (i in 1:length(col))
{
  province[i]=paste(col[[i]][1],col[[i]][2],sep = "")
}
alldata_test$province=province            #����ʡ��
alldata_test=alldata_test[-2]             #ɾ������֤

###########��ȡtrain����
train = read.csv('train.csv',head=TRUE)        #��ȡ���Լ�����������
train=train[-c(3,4,6,9)]
train$Y<-as.factor(train$Y)
train_balance<-SMOTE(Y~.,train,perc.over=600,perc.under=100)
