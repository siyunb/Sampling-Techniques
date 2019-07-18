###读取数据
mydata<-read.csv("D:/大数据作业/大数据统计基础考试/2017《大数据统计基础》考试题/2017《大数据统计基础》考试题/LoanStats3c.csv",header=TRUE,skip=1)
#删除缺失值
mydata0<-mydata[,'loan_amnt']
N<-length(mydata0)
summary(mydata0)
mydata0<-na.omit(mydata0)
#离散化数据
plot<-hist(mydata0,xlab="贷款金额",ylab="频数", main = "贷款量分布频数分布直方图",col="lightblue")
databreaks<-plot$breaks
mydata1<-cut(mydata0,breaks=databreaks)
freq<-table(mydata1)/N
barplot(table(mydata1),xlab = "分组间隔",ylab = "数量",col = "lightblue",
        main = "各分组数量条形图",border = "red")
#重新生成分组间隔

databreaks=c(0,1200*(1:9),14000,18000,23000,2400*(11:14),34500,max(mydata0))
mydata2=cut(mydata0,breaks = databreaks)
barplot(table(mydata2),xlab = "分组间隔",ylab = "数量",col = "lightblue",
        main = "各分组数量条形图",border = "red")
freq1<-table(mydata2)/N    #计算再次分组的分组频率
##确定总抽样个数samp
x<-seq(6,20,by=0.1)
y<-2^(x)
samp<-round(y)[-c(1:51)][-c(69:90)]   #确定样本抽取个数
n<-length(samp)
samp<-as.matrix(samp)
#给定样本量
sam0<-c(100,1000,5000,10000)
n<-length(sam0)

###简单随机抽样
#定义简单随机抽样的抽样函数fun1
fun1<-function(i){
  p<-sample(mydata0,i)
  p<-c(p,matrix(NA,1,samp[n]-length(p)))
  return(p)
}

###定义简单随机抽样样本质量函数
fun2<-function(datasam){
  datasam1<-cut(na.omit(datasam),breaks=databreaks)
  frequ<-table(datasam1)/length(na.omit(datasam))+0.0000000001  #防止某个分组概率为零
  J<-sum((frequ-freq1)*(log(frequ/freq1)))
  q<-exp(-J)
  return(q)
}

###10次循环计算平均样本质量
sam0<-as.matrix(sam0)
Q1<-matrix(NA,length(sam0),10)
for(i in 1:10){
  ma<-apply(sam0,1,fun1)    #抽样指标loan_amnt抽样样本矩阵
  Q1[,i]<-apply(ma,2,fun2)     #计算每个抽样样本下的样本质量
}    
Qj<-apply(Q1,1,mean)
###绘制样本质量图
plot(sam0,Qj,xlab = "样本数量",ylab = "样本质量",main="简单随机抽样结果",col = "red",bg="lightblue",cex = 1.2)
lines(sam0,Qj,col='blue')


###10次循环求68个样本的平均样本质量
samp<-as.matrix(samp)
n<-length(samp)
Q1<-matrix(NA,length(samp),10)
for(i in 1:10){
  ma<-apply(samp,1,fun1)    #抽样指标loan_amnt抽样样本矩阵
  Q1[,i]<-apply(ma,2,fun2)     #计算每个抽样样本下的样本质量
}    
Qj<-apply(Q1,1,mean)
###绘制样本质量图
plot(samp,Qj,xlab = "样本数量",ylab = "样本质量",main="68个样本插值拟合结果",col = "blue",cex = 1.2)
lines(samp,Qj,col='blue')
###绘制预测样本质量图
s<-smooth.spline(samp,Qj)
pr<-predict(s,samp)
points(pr$x,pr$y,pch=4,col='red') 
legend("bottomright", c("Q预测", "Q"), pch = c(4, 1), col = c("red", "blue"), cex = 1)

#选择样本拟合样本量和样本质量变化曲线图
x=seq(6,20,by=0.1)
y=2^(x)
samp=round(y)[-c(1:20)]
samp=samp[-c(40:length(samp))]#选取合适数据
sa=data.frame(y=samp,x=c(1:39))
n1=length(samp)
samp=as.array(samp)
ma<-apply(samp,1,fun1)#利用随机抽样函数抽取一组样本
q1=apply(ma,2,fun2)#计算样本质量
s1=smooth.spline(q1,samp)#拟合曲线并预测样本质量在0.95的情况下的样本量
pr1=predict(s1,0.95)  #预测95%的样本容量
ceiling(pr1[[2]])
pr2=predict(s1,0.96)  #预测96%的样本容量
ceiling(pr1[[2]])pr1=predict(s1,0.95)  #预测95%的样本容量
ceiling(pr1[[2]])
pr3=predict(s1,0.97)  #预测97%的样本容量
ceiling(pr1[[2]])
pr4=predict(s1,0.98)  #预测99%的样本容量
ceiling(pr1[[2]])

#选取最优样本
##计算最优样本量，停止增加是样本量的条件
##1、最小样本质量为95%
##2、前后两次增加样本样本质量并没有明显改善，设置为前后两次的取样必须达到样本质量1%的进步
##每次增加样本步长为25个,起始为100
  a<-300
  Q<-vector(mode="numeric",length=0)
  Qm<-0
  while(Qm<0.95||(Qm-Qm1)>0.001){
    for(i in 1:5){
      ma<-fun1(a)
      Q[i]<-fun2(ma)
    }
    Qm1<-Qm
    Qm<-mean(Q)
    a<-a+25
  }
  print(Qm)
  a
