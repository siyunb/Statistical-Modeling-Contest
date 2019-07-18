#########�������ݰ�
library(ggplot2)
library(gplots)
library(plyr)
library(dplyr)
library(showtext)  #ʹ��ͼ��������ӷḻ
library(RColorBrewer)  #���ӵ�ɫ��
library(dplyr)
library(mice)
library(VIM)
library(DMwR)
library(grid)
library(data.table)
library(stringr)   #�ַ�������

##########��ȡ����
setwd("D:/bigdatahw/�ϲƶ�����/��������")  #���ù���·��
test = read.csv('contest_basic_test.csv',head=TRUE)
train= read.csv('contest_basic_train.csv',head=TRUE)
loan_detail= read.csv('contest_ext_crd_cd_ln.csv',head=TRUE)
spl= read.csv('contest_ext_crd_cd_ln_spl.csv',head=TRUE)
Credit_card= read.csv('contest_ext_crd_cd_lnd.csv',head=TRUE)
Credit_card_ovd= read.csv('contest_ext_crd_cd_lnd_ovd.csv',head=TRUE,encoding = 'UTF-8')
report= read.csv('contest_ext_crd_hd_report.csv',head=TRUE,encoding = 'UTF-8')
creditcue= read.csv('contest_ext_crd_is_creditcue.csv',head=TRUE)
ovdsummary= read.csv('contest_ext_crd_is_ovdsummary.csv',head=TRUE,encoding = 'UTF-8')
sharedebt= read.csv('contest_ext_crd_is_sharedebt.csv',head=TRUE,encoding = 'UTF-8')
recorddtlinfo= read.csv('contest_ext_crd_qr_recorddtlinfo.csv',head=TRUE)
recordsmr= read.csv('contest_ext_crd_qr_recordsmr.csv',head=TRUE,encoding = 'UTF-8')
fraud= read.csv('contest_fraud.csv',head=TRUE)

############��������Сд
names(test)=tolower(names(test))
names(train)=tolower(names(train))
names(loan_detail)=tolower(names(loan_detail))
names(spl)=tolower(names(spl))
names(Credit_card)=tolower(names(Credit_card))
names(Credit_card_ovd)=tolower(names(Credit_card_ovd))
names(report)=tolower(names(report))
names(creditcue)=tolower(names(creditcue))
names(ovdsummary)=tolower(names(ovdsummary))
names(sharedebt)=tolower(names(sharedebt))
names(recorddtlinfo)=tolower(names(recorddtlinfo))
names(recordsmr)=tolower(names(recordsmr))
names(fraud)=tolower(names(fraud))

###########ת����������
str(train)                 #�鿴��������
train$salary = with(train, as.factor(salary))
train$work_province = with(train, as.factor(work_province))
train$has_fund = with(train, as.factor(has_fund))
train$y = with(train, as.factor(y)) #ת����������
str(creditcue)

##########ת������ɾ��������ʹ��ǿ����е������¼
loan_detail_fin<-loan_detail %>%
  mutate(state_bool=as.factor((state =='����'|state=='����') * 1)) %>%
  select(loan_id,report_id,state,finance_org,type_dw,class5_state,payment_state,credit_limit_amount,curr_overdue_cyc,curr_overdue_amount,state_bool)
col1<-str_extract_all(loan_detail_fin$payment_state,"\\d")
yanqimonth <- rep(0,length(col1))
for (i in 1:length(col1))
{
  yanqimonth[i]=max(as.numeric(col1[[i]])) 
}
loan_detail_fin$yanqimonth=yanqimonth            #����24�ںϼ�������
loan_detail_fin$yanqimonth = as.factor(ifelse(loan_detail_fin$yanqimonth == '-Inf', '0',loan_detail_fin$yanqimonth)) 
loan_detail_fin=loan_detail_fin[-7] 
loan_detail_fin=loan_detail_fin[-3] 
loan_detail_fin<- loan_detail_fin %>%
  mutate(class5_state_bool=as.factor((class5_state !='NULL'&class5_state!='����') * 1)) %>%
  mutate(type_bool=as.factor((type_dw =='���˾�Ӫ�Դ���'|type_dw=='������������'|type_dw=='��������') * 1)) %>%
  select(loan_id,report_id,finance_org,credit_limit_amount,curr_overdue_cyc,curr_overdue_amount,state_bool,yanqimonth,class5_state_bool,type_bool)
loan_detail_fin$curr_overdue_cyc = as.numeric(as.character(loan_detail_fin$curr_overdue_cyc))  #ת��Ϊ������
loan_detail_fin$curr_overdue_cyc = ifelse(is.na(loan_detail_fin$curr_overdue_cyc), 0, loan_detail_fin$curr_overdue_cyc)
loan_detail_fin$curr_overdue_amount = as.numeric(as.character(loan_detail_fin$curr_overdue_amount))  #ת��Ϊ������
loan_detail_fin$curr_overdue_amount = ifelse(is.na(loan_detail_fin$curr_overdue_amount), 0, loan_detail_fin$curr_overdue_amount)
###########ת����������
loan_detail_fin$state_bool=as.numeric(as.character(loan_detail_fin$state_bool))
loan_detail_fin$yanqimonth=as.numeric(as.character(loan_detail_fin$yanqimonth))
loan_detail_fin$class5_state_bool=as.numeric(as.character(loan_detail_fin$class5_state_bool))
loan_detail_fin$type_bool=as.numeric(as.character(loan_detail_fin$type_bool))
###########���з���ϲ�
loan_detail_zl<-loan_detail_fin %>%
  select(report_id,finance_org,credit_limit_amount,curr_overdue_cyc,curr_overdue_amount,state_bool,yanqimonth,class5_state_bool,type_bool) %>%
  group_by(report_id) %>%
  summarize(count_org = length(unique(finance_org)),
            amount_mean = mean(credit_limit_amount),
            cyc_max = max(curr_overdue_cyc),
            overdue_amount_mean = mean(curr_overdue_amount),
            state_bool = max(state_bool),
            yanqimonth = max(yanqimonth),
            class5_state_bool = max(class5_state_bool),
            type_bool = max(type_bool)) 
###########д��csv�ļ�
write.csv(loan_detail_zl,"D:/bigdatahw/�ϲƶ�����/��������/loan.csv",row.names = TRUE)  

###########�������ǿ���
Credit_card_fin <-Credit_card %>%
  mutate(state_bool=as.factor((state =='����'|state=='ֹ��'|state=='����') * 1)) %>%
  select(report_id,state,finance_org,credit_limit_amount,curr_overdue_cyc,curr_overdue_amount,payment_state,state_bool)

col2<-str_extract_all(Credit_card_fin$payment_state,"\\d")
card_yanqimonth <- rep(0,length(col2))
for (i in 1:length(col2))
{
  card_yanqimonth[i]=max(as.numeric(col2[[i]])) 
}
Credit_card_fin$card_yanqimonth=card_yanqimonth            #����24�ںϼ�������
Credit_card_fin$card_yanqimonth = as.factor(ifelse(Credit_card_fin$card_yanqimonth == '-Inf', '0',Credit_card_fin$card_yanqimonth)) 
Credit_card_fin=Credit_card_fin[-7] 
Credit_card_fin=Credit_card_fin[-2]

Credit_card_fin$curr_overdue_cyc = as.numeric(as.character(Credit_card_fin$curr_overdue_cyc))  #ת��Ϊ������
Credit_card_fin$curr_overdue_cyc = ifelse(is.na(Credit_card_fin$curr_overdue_cyc), 0, Credit_card_fin$curr_overdue_cyc)
Credit_card_fin$curr_overdue_amount = as.numeric(as.character(Credit_card_fin$curr_overdue_amount))  #ת��Ϊ������
Credit_card_fin$curr_overdue_amount = ifelse(is.na(Credit_card_fin$curr_overdue_amount), 0, Credit_card_fin$curr_overdue_amount)
###########ת����������
Credit_card_fin$curr_overdue_cyc=as.numeric(as.character(Credit_card_fin$curr_overdue_cyc))
Credit_card_fin$curr_overdue_amount=as.numeric(as.character(Credit_card_fin$curr_overdue_amount))
Credit_card_fin$state_bool=as.numeric(as.character(Credit_card_fin$state_bool))
Credit_card_fin$card_yanqimonth=as.numeric(as.character(Credit_card_fin$card_yanqimonth))
###########���з���ϲ�
Credit_card_zl<-Credit_card_fin %>%
  group_by(report_id) %>%
  summarize(count_org = length(unique(finance_org)),
            amount_mean = mean(credit_limit_amount),
            cyc_max = max(curr_overdue_cyc),
            overdue_amount_mean = mean(curr_overdue_amount),
            state_bool = max(state_bool),
            card_yanqimonth = max(card_yanqimonth)) 
###########д��csv�ļ�
write.csv(Credit_card_zl,"D:/bigdatahw/�ϲƶ�����/��������/��������/card.csv",row.names = TRUE)  








###########ƴ�ӱ���
isTRUE(duplicated(train$report_id))  #����report_id�Ƿ�ΪΨһֵ
train_creditcue=merge(train,creditcue,by='report_id',all.x=TRUE)  
train_creditcue=train_creditcue[,c(-15,-17,-19,-20,-21)]
credit_train_creditcue=merge(Credit_card,train_creditcue,by='report_id',all.x=TRUE) 
loan_train_creditcue=merge(loan_detail,train_creditcue,by='report_id',all.x=TRUE)  

#train_loan_detail[train_loan_detail=='NULL']=NA1
#a=as.character(train_loan_detail$edu_level[2]) 
#train_loan_detail[train_loan_detail==a]=NA

##########����24���»���״̬��
state=strsplit(as.character(train_loan_detail$payment_state) ,"",fixed=TRUE)
last=paste("last", order(1:24, decreasing=TRUE), sep = "")
payment_state=matrix(nrow=length(state),ncol=24)
payment_state=as.data.frame(payment_state)

#ȱʧֵģʽ̽��
md.pattern(train_loan_detail)
aggr_plot <- aggr(train_loan_detail, col = c("blue", "red"), numbers = TRUE, prop = TRUE, 
                  sortVars = TRUE, labels = names(train_loan_detail), cex.axis = 0.8, gap = 2, 
                  ylab = c("ȱʧ����ֱ��ͼ","ȱʧ����ģʽͼ"))




##########����Ԥ����

