#########币种与贷款金额关系
ggplot(Credit_card,aes(x=currency,y=credit_limit_amount,fill=currency))+
  geom_boxplot(outlier.size=1.5, outlier.shape=15,notch=FALSE,alpha=.35)+
  stat_summary (fun.y="mean",geom="point",shape=23,size=2,fill="white")+
  xlab("币种") + ylab("贷款额度") +ggtitle('币种贷款金额箱线图')+ylim(0,550000)+
  theme(plot.title = element_text(hjust = 0.5,  family="myFont",size=18,color="red"), 
        panel.background=element_rect(fill='aliceblue',color='black')) 
###########人口学信息可视化
#数据预处理
train$has_fund<-as.factor(train$has_fund)         
taiwan$EDUCATION<-as.factor(taiwan$EDUCATION)
taiwan$MARRIAGE<-as.factor(taiwan$MARRIAGE)
taiwan$default.payment.next.month<-as.factor(taiwan$default.payment.next.month)
names(taiwan)[25]="default"              #重命名
levels(taiwan$SEX)=list(M="1",F="2")     #进行重命名等级
levels(taiwan$EDUCATION)=list(others="0",graduate="1",university="2", highschool="3",others="4",others="5",others="6")
levels(taiwan$MARRIAGE)=list(married="1", single="2",others="3",others="0")
levels(taiwan$default)=list(T="1", F="0")

#教育状况与违约占比饼状图，分面的风玫瑰图
label<-c("others","highschool","university","graduate")
train$edu_level<- ordered(taiwan$EDUCATION, levels = label)
ggplot(train,aes(x=y,fill=edu_level))+
  geom_bar()+coord_polar(theta = 'x')+
  scale_fill_brewer(palette='Spectral')+facet_wrap(~has_fund)+theme_bw()+ 
  labs(x="违约状况",y="频数",fill="学历状况",title='分面风玫瑰图')+scale_x_discrete()+coord_polar(theta="x")+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="gold3"),panel.background=element_rect(fill='aliceblue')) 

#违约与年龄分布直方图
label<-c("F","T")
taiwan$default<- ordered(taiwan$default, levels = label)
p<-ggplot(taiwan,aes(x=AGE,fill=default)) 
p+geom_histogram(position="identity",alpha=0.5)+ggtitle('各学历违约状况与年龄分布直方图')+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="red"),panel.background=element_rect(fill='aliceblue')) + xlab("年龄") + 
  ylab("频数")+facet_wrap(~EDUCATION)

#银行给定额度与违约箱线图
ggplot(taiwan,aes(x=default,y=LIMIT_BAL,fill=default))+
  geom_boxplot(outlier.size=1.5, outlier.shape=15,notch=TRUE,alpha=.35)+
  stat_summary (fun.y="mean",geom="point",shape=23,size=2,fill="white")+
  xlab("违约与否") + ylab("给定的信用额度") +ggtitle('给定信用额度与违约箱线图')+ylim(0,550000)+
  theme(plot.title = element_text(hjust = 0.5,  family="myFont",size=18,color="red"), 
        panel.background=element_rect(fill='aliceblue',color='black')) 

#婚姻状况与违约之间关系
ggplot(taiwan,aes(x=factor(1),fill=default))+
  geom_bar(aes(fill=default),position="fill")+coord_polar(theta="y")+
  ggtitle('婚姻状况与违约之间关系')+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="black"),      
        panel.background=element_rect(fill='aliceblue',color='black'))+facet_wrap(~MARRIAGE) 

#教育与违约关系
ggplot(taiwan, aes(EDUCATION))+geom_bar(aes(fill=default),position="fill")+
  coord_polar(theta = "y")+
  ggtitle('教育水平与违约之间关系')+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="black"),      
        panel.background=element_rect(fill='aliceblue',color='black'))

#年龄与给定额度关系
taiwan<-taiwan[which(taiwan$default=='T'),]
p=ggplot(taiwan,aes(x=AGE,y=log(LIMIT_BAL)))
#默认等高线图进行分箱化处理
p+geom_point(alpha=0.2)+stat_bin2d()+scale_fill_gradient(low="lightblue",high="red")+stat_density2d()+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="slateblue2"),panel.background=element_rect(fill='papayawhip'))+
  labs(x='年龄',y='log(给定额度)',title='年龄与给定额度密度关系')
