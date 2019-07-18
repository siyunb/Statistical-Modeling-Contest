#########加载数据包
library(ggplot2)
library(gplots)
library(plyr)
library(dplyr)
library(showtext)  #使作图的字体更加丰富
library(RColorBrewer)  #增加调色板
library(dplyr)
library(mice)
library(VIM)
library(DMwR)
library(grid)
library(data.table)
library(stringr)   #字符串处理

##########读取数据
setwd("D:/bigdatahw/上财东正杯/竞赛数据")  #设置工作路径
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

############将变量名小写
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

###########转换变量类型
str(train)                 #查看变量类型
train$salary = with(train, as.factor(salary))
train$work_province = with(train, as.factor(work_province))
train$has_fund = with(train, as.factor(has_fund))
train$y = with(train, as.factor(y)) #转换变量类型
str(creditcue)

##########转换表格，删除贷款表和贷记卡表中的冗余记录
loan_detail_fin<-loan_detail %>%
  mutate(state_bool=as.factor((state =='呆账'|state=='逾期') * 1)) %>%
  select(loan_id,report_id,state,finance_org,type_dw,class5_state,payment_state,credit_limit_amount,curr_overdue_cyc,curr_overdue_amount,state_bool)
col1<-str_extract_all(loan_detail_fin$payment_state,"\\d")
yanqimonth <- rep(0,length(col1))
for (i in 1:length(col1))
{
  yanqimonth[i]=max(as.numeric(col1[[i]])) 
}
loan_detail_fin$yanqimonth=yanqimonth            #加入24期合计延期数
loan_detail_fin$yanqimonth = as.factor(ifelse(loan_detail_fin$yanqimonth == '-Inf', '0',loan_detail_fin$yanqimonth)) 
loan_detail_fin=loan_detail_fin[-7] 
loan_detail_fin=loan_detail_fin[-3] 
loan_detail_fin<- loan_detail_fin %>%
  mutate(class5_state_bool=as.factor((class5_state !='NULL'&class5_state!='正常') * 1)) %>%
  mutate(type_bool=as.factor((type_dw =='个人经营性贷款'|type_dw=='个人汽车贷款'|type_dw=='其他贷款') * 1)) %>%
  select(loan_id,report_id,finance_org,credit_limit_amount,curr_overdue_cyc,curr_overdue_amount,state_bool,yanqimonth,class5_state_bool,type_bool)
loan_detail_fin$curr_overdue_cyc = as.numeric(as.character(loan_detail_fin$curr_overdue_cyc))  #转化为数字型
loan_detail_fin$curr_overdue_cyc = ifelse(is.na(loan_detail_fin$curr_overdue_cyc), 0, loan_detail_fin$curr_overdue_cyc)
loan_detail_fin$curr_overdue_amount = as.numeric(as.character(loan_detail_fin$curr_overdue_amount))  #转化为数字型
loan_detail_fin$curr_overdue_amount = ifelse(is.na(loan_detail_fin$curr_overdue_amount), 0, loan_detail_fin$curr_overdue_amount)
###########转化变量类型
loan_detail_fin$state_bool=as.numeric(as.character(loan_detail_fin$state_bool))
loan_detail_fin$yanqimonth=as.numeric(as.character(loan_detail_fin$yanqimonth))
loan_detail_fin$class5_state_bool=as.numeric(as.character(loan_detail_fin$class5_state_bool))
loan_detail_fin$type_bool=as.numeric(as.character(loan_detail_fin$type_bool))
###########进行分组合并
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
###########写出csv文件
write.csv(loan_detail_zl,"D:/bigdatahw/上财东正杯/竞赛数据/loan.csv",row.names = TRUE)  

###########整理贷记卡表
Credit_card_fin <-Credit_card %>%
  mutate(state_bool=as.factor((state =='呆账'|state=='止付'|state=='冻结') * 1)) %>%
  select(report_id,state,finance_org,credit_limit_amount,curr_overdue_cyc,curr_overdue_amount,payment_state,state_bool)

col2<-str_extract_all(Credit_card_fin$payment_state,"\\d")
card_yanqimonth <- rep(0,length(col2))
for (i in 1:length(col2))
{
  card_yanqimonth[i]=max(as.numeric(col2[[i]])) 
}
Credit_card_fin$card_yanqimonth=card_yanqimonth            #加入24期合计延期数
Credit_card_fin$card_yanqimonth = as.factor(ifelse(Credit_card_fin$card_yanqimonth == '-Inf', '0',Credit_card_fin$card_yanqimonth)) 
Credit_card_fin=Credit_card_fin[-7] 
Credit_card_fin=Credit_card_fin[-2]

Credit_card_fin$curr_overdue_cyc = as.numeric(as.character(Credit_card_fin$curr_overdue_cyc))  #转化为数字型
Credit_card_fin$curr_overdue_cyc = ifelse(is.na(Credit_card_fin$curr_overdue_cyc), 0, Credit_card_fin$curr_overdue_cyc)
Credit_card_fin$curr_overdue_amount = as.numeric(as.character(Credit_card_fin$curr_overdue_amount))  #转化为数字型
Credit_card_fin$curr_overdue_amount = ifelse(is.na(Credit_card_fin$curr_overdue_amount), 0, Credit_card_fin$curr_overdue_amount)
###########转化变量类型
Credit_card_fin$curr_overdue_cyc=as.numeric(as.character(Credit_card_fin$curr_overdue_cyc))
Credit_card_fin$curr_overdue_amount=as.numeric(as.character(Credit_card_fin$curr_overdue_amount))
Credit_card_fin$state_bool=as.numeric(as.character(Credit_card_fin$state_bool))
Credit_card_fin$card_yanqimonth=as.numeric(as.character(Credit_card_fin$card_yanqimonth))
###########进行分组合并
Credit_card_zl<-Credit_card_fin %>%
  group_by(report_id) %>%
  summarize(count_org = length(unique(finance_org)),
            amount_mean = mean(credit_limit_amount),
            cyc_max = max(curr_overdue_cyc),
            overdue_amount_mean = mean(curr_overdue_amount),
            state_bool = max(state_bool),
            card_yanqimonth = max(card_yanqimonth)) 
###########写出csv文件
write.csv(Credit_card_zl,"D:/bigdatahw/上财东正杯/竞赛数据/整理数据/card.csv",row.names = TRUE)  








###########拼接表格
isTRUE(duplicated(train$report_id))  #检验report_id是否为唯一值
train_creditcue=merge(train,creditcue,by='report_id',all.x=TRUE)  
train_creditcue=train_creditcue[,c(-15,-17,-19,-20,-21)]
credit_train_creditcue=merge(Credit_card,train_creditcue,by='report_id',all.x=TRUE) 
loan_train_creditcue=merge(loan_detail,train_creditcue,by='report_id',all.x=TRUE)  

#train_loan_detail[train_loan_detail=='NULL']=NA1
#a=as.character(train_loan_detail$edu_level[2]) 
#train_loan_detail[train_loan_detail==a]=NA

##########生成24个月还款状态表
state=strsplit(as.character(train_loan_detail$payment_state) ,"",fixed=TRUE)
last=paste("last", order(1:24, decreasing=TRUE), sep = "")
payment_state=matrix(nrow=length(state),ncol=24)
payment_state=as.data.frame(payment_state)

#缺失值模式探索
md.pattern(train_loan_detail)
aggr_plot <- aggr(train_loan_detail, col = c("blue", "red"), numbers = TRUE, prop = TRUE, 
                  sortVars = TRUE, labels = names(train_loan_detail), cex.axis = 0.8, gap = 2, 
                  ylab = c("缺失数据直方图","缺失数据模式图"))




##########数据预处理


