################数据处理
library(sampling)
library(dplyr) 
setwd("D:/大数据作业/抽样技术/")
summary(mydata)    
mydata<-
  read.csv("LoanStats3c.csv",header = T,skip=1)%>%
  dplyr::select(id,member_id,loan_amnt,annual_inc,grade,issue_d)   #整理实验数据集
write.csv(mydata,"D:/大数据作业/抽样技术/mydata.csv",row.names = TRUE)  #输出实验总体集
any(is.na(mydata))  #检查数据中是否存在缺失值
which(is.na(mydata))     #查看缺失值位置
mydata<-mydata[complete.cases(mydata),]  #删除含缺失值的记录


################简单随机抽样
##简单随机抽样提取loan_amnt和annual_inc两个指标进行样本质量的计算
##首先进行annual_inc指标的样本质量计算
data0<-mydata[,c('loan_amnt','annual_inc')]   #提取所需样本矩阵
attach(data0)
N<-length(annual_inc)
bre1<-c(0,10000*(1:10),150000,100000*(2:5),max(annual_inc))  #进行变量的数据切分分组
PD1<-
  annual_inc %>%
  cut(breaks = bre1) %>%   #划分连续变量分组
  table()/N                #确定总体频率
##简单随机抽样loan_amnt指标
hist(loan_amnt,xlab="贷款金额",ylab="频数",main="贷款金额直方图",col="lightblue") #画出直方图观察分布
bre2<-c(0,5000*(1:7))
PD2<-
  loan_amnt %>%
  cut(breaks = bre2) %>%    #划分连续变量分组
  table()/N                 #确定总体频率

##确定总抽样个数samp
x<-seq(6,20,by=0.1)
y<-2^(x)
samp<-round(y)[-c(1:51)][-c(69:90)]   #确定样本抽取个数
n<-length(samp)
samp<-as.matrix(samp)

##定义函数
###定义抽样函数
fun1<-function(i,datasam){
  sub<-sample(nrow(data0),i)
  if(datasam=="annual_inc")  p<-data0[sub,]$annual_inc
  if(datasam=="loan_amnt")  p<-data0[sub,]$loan_amnt
  p<-c(p,matrix(NA,1,samp[n]-length(p)))
  return(p)
}
###定义信息量计算函数
fun2<-function(datasam1,bre,PD){
  datasam1<-cut(na.omit(datasam1),breaks = bre) 
  PS<-table(datasam1)/length(na.omit(datasam1))+0.0000000001         #防止某个分组概率为零
  J<-sum((PS-PD)*(log(PS/PD)))
  return(J)
}


######实证检验
Q1<-matrix(NA,length(samp),10)
for(i in 1:10){
  ma1<-apply(samp,1,function(x) fun1(x,"annual_inc"))    #抽样指标annual_inc样本矩阵
  J1<-apply(ma1,2,function(x) fun2(x,bre1,PD1))          #计算每个抽样样本下的样本质量
  ma2<-apply(samp,1,function(x) fun1(x,"loan_amnt"))
  J2<-apply(ma2,2,function(x) fun2(x,bre2,PD2))
  Q1[,i]<-exp(-(J1+J2)/2)     #合并两个指标的KL信息量
}                            #每个样本容量下产生10个样本数据集并
Qj<-apply(Q1,1,mean)          #计算每个样本容量下的样本质量均值
plot(samp,Qj)                #画出图像
                                

###################分层随机抽样
#首先抽取出样本矩阵
data3<-
  mydata%>%
  dplyr::select(loan_amnt,annual_inc,grade)
PD3<-table(data3$grade)/N
fcdata<-data3[order(data3[,3]),]#构造分层抽样样本数据集
fun4<-function(s,datasam){
  sub=strata(fcdata,stratanames="grade",size=round(s*PD3[-1]),method="srswor")
  if(datasam=="annual_inc")  p<-data3[sub$ID_unit,]$annual_inc
  if(datasam=="loan_amnt")  p<-data3[sub$ID_unit,]$loan_amnt
  res<-c(p,matrix(NA,1,samp[n]+5-length(p)))
  return(res)
}

######实证检验
mb1<-apply(samp,1,function(x) fun4(x,"annual_inc"))
J1<-apply(mb1,2,function(x) fun2(x,bre1,PD1)) 
mb2<-apply(samp,1,function(x) fun4(x,"loan_amnt"))
J2<-apply(mb2,2,function(x) fun2(x,bre2,PD2))
Q2<-exp(-(J1+J2)/2)
plot(samp,Q2)                 #电脑是在跑不动因此只设置了一层循环


##整群随机抽样
data4<-mydata[,c('loan_amnt','annual_inc','issue_d')]    ##整群抽样数据集
zqdata<-data4[order(data4[,3]),]    #按日期顺序进行排序
xl<-vector(mode="character",length=0) #构造零向量，按日期顺序人为切割成多个分组
YF<-as.vector(table(zqdata$issue_d)[-1])
m=0
label=c('Apr','Aug','Dec','Feb','Jan','Jul','Jun','Mar','May','Nov','Oct','Sep')
for(j in 1:length(label) ){
  a=0
  for(i in 1:YF[j]){
    if(i%%1000==1){
      a=a+1
      x=sprintf("%s-%d",label[j],a)
    }  
    m=m+1
    xl[m]<-x
  }
}               ##认为拆分每个月份，构造新的标签列
zqdata<-cbind(zqdata,xl)
names(zqdata)[4]<-c("riqi")   ##命名新列名日期

#按日期先后顺序划分成200个群
summary(zqdata$riqi)
xulie=seq(from=1, to=228, by=4)    ##构造整群抽样样本数1-228个群
sam_count<-vector(mode="numeric",length=0) 
n=0
fun5<-function(s,datasam){
  sub1<-cluster(data=zqdata,clustername="riqi",size=s,method="srswor",description=FALSE)
  if(datasam=="annual_inc")  p<-data4[sub1$ID_unit,]$annual_inc
  if(datasam=="loan_amnt")  p<-data4[sub1$ID_unit,]$loan_amnt
  res<-c(p,matrix(NA,1,228000-length(p)))
  return(res)
}
######实证检验
Q3<-matrix(NA,57,2)
xulie<-as.matrix(xulie)
for(i in 1:2){
  mc1<-apply(xulie,1,function(x) fun5(x,"annual_inc"))
  J1<-apply(mc1,2,function(x) fun2(x,bre1,PD1))     #annual_inc样本数据的信息量
  mc2<-apply(xulie,1,function(x) fun5(x,"loan_amnt"))
  J2<-apply(mc2,2,function(x) fun2(x,bre2,PD2))     #loan_amnt样本数据的信息量
  Q3[,i]<-exp(-(J1+J2)/2)          ##权重皆为1
}
Qz<-apply(Q3,1,mean)          #计算每个样本容量下的样本质量均值、
plot(xulie,Qz)               #内存16g我都跑不出来循环，放弃了

###########两阶段抽样
label1=c('Apr-20','Dec-11','Feb-16','Jan-16','Aug-19 ','Jul-30','Jun-18','Mar-17','May-20','Nov-26','Sep-11','Oct-39')
xk<-vector(mode="numeric",length=0) 
n=0
for(i in 1:length(xl)){
  if(xl[i] %in% label1){
    n=n+1
    xk[n]=i
  } 
}            ##删除部分数据，这些数据不满足每个群含足够的的样本数量
data5<-mydata[,c('loan_amnt','annual_inc','issue_d','grade')]    ##多阶段抽样数据集
ljddata<-cbind(data5[order(data5[,3]),],xl)   #按日期顺序进行排序并构造两阶段数据集
ljddata<-ljddata[-xk,]
samp<-as.matrix(samp)                
fun6<-function(s,datasam){
  qs<-round((s%/%1000)*2)
  mzgs<-s%/%qs
  yushu<-s-(qs-1)*mzgs      ##取余
  zngs<-c(rep(mzgs,qs-1),yushu)
  sub2<-mstage(data=ljddata,stage = c("cluster","stratified"),varnames = list("xl","grade"),
               size = list(qs,zngs),method = c("srswor","srswor"),description = FALSE)
  if(datasam=="annual_inc")  p<-data5[sub2$`2`$ID_unit,]$annual_inc
  if(datasam=="loan_amnt")  p<-data5[sub2$`2`$ID_unit,]$loan_amnt
  res<-c(p,matrix(NA,1,230000-length(p)))
  return(res)
}
########实证检验
Q4<-matrix(NA,length(samp),2)
samp<-samp[1:50,]
samp<-as.matrix(samp)
for(i in 1:2){
  md1<-apply(samp,1,function(x) fun6(x,"annual_inc"))
  J1<-apply(md1,2,function(x) fun2(x,bre1,PD1))     #annual_inc样本数据的信息量
  md2<-apply(samp,1,function(x) fun6(x,"loan_amnt"))
  J2<-apply(md2,2,function(x) fun2(x,bre2,PD2))     #loan_amnt样本数据的信息量
  Q4[,i]<-exp(-(J1+J2)/2)          ##权重皆为1
}
Ql<-apply(Q4,1,mean)          #计算每个样本容量下的样本质量均值
plot(samp,Ql)

##计算最优样本量，停止增加是样本量的条件
##1、最小样本质量为95%
##2、前后两次增加样本样本质量并没有明显改善，设置为前后两次的取样必须达到样本质量1%的进步
##每次增加样本步长为25个,起始为100
fun7<-function(lab=fun1){
  a<-300
  Q<-vector(mode="numeric",length=0)
  Qm<-0
  while(Qm<0.95||(Qm-Qm1)>0.1){
    for(i in 1:5){
      mr1<-lab(a,"annual_inc")
      j1<-fun2(mr1,bre1,PD1)
      mr2<-lab(a,"loan_amnt")
      j2<-fun2(mr2,bre2,PD2)
      Q[i]<-exp(-(j1+j2)/2)
    }
    Qm1<-Qm
    Qm<-mean(Q)
    a<-a+10
  }
  print(Qm)
  return(a)
}

##给定样本质量95%时，计算样本容量，设定为每次添加样本步长为5，样本质量允许误差为0.0005
###给定样本质量来计算样本容量函数
fun3<-function(pro1,lab=fun1){
  a<-300
  Q<-vector(mode="numeric",length=0)
  Qm<-0
  while(abs(Qm-pro1)>0.0005){
    for(i in 1:2){
      mr1<-lab(a,"annual_inc")
      j1<-fun2(mr1,bre1,PD1)
      mr2<-lab(a,"loan_amnt")
      j2<-fun2(mr2,bre2,PD2)
      Q[i]<-exp(-(j1+j2)/2)
    }
    if(Qm>0.9505) break
    Qm<-mean(Q)
    a<-a+10
  }
  print(Qm)
  return(a)
}








