
################# 第 3 章 R 程序代码  ####################

knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)
knitr::opts_chunk$set(fig.align="center"
                      ## ,out.width="0.9\\textwidth" # latex
                      ,out.width="60%" # for both latex and html
                      ,fig.width=5, fig.height=3
                      )

rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")

###加载包
library(kableExtra)
library(knitr)
library(fBasics)
library(tseries)
library(xts)
library(readxl)
library(urca)
library(FinTS)
library(parallel)
library(rugarch)
library(fUnitRoots)
library(fGarch)
library(highfrequency)


############构建已实现测度函数#########
Rt=function(x) {
    rt=rep(0,length(x)) #x为收盘价向量
    for (i in 2:length(x)) {
      rt[i]=(log(x[i])-log(x[i-1]))*100
    }
    rt
} # 计算日内收益率函数,将收益率扩大了100倍

RV=function(x,y,z) {
    rv=rep(0,length(x)) #x为交易日向量
    m=length(y)/length(x) #y为收盘价向量，m为时间间隔
    for (i in 1:length(x)) {
    for (j in 1:m){   
    rv[i]=rv[i]+z[m*(i-1)+j]^2 #z为收益率向量
}}
rv 
} #计算已实现波动RV

################计算收益率#######################
path="./data/data_raw"
fileNames=dir(path)
Lst1<-list() # 日内收益率数据
for(i in fileNames) {
    Lst1[[i]]<-as.data.frame(read.csv(paste(path,i,sep = "/"))) #读入数据
    Lst1[[i]]$DATE=as.character(Lst1[[i]]$DATE) 
    Lst1[[i]]$TIME=as.character(Lst1[[i]]$TIME)  #转换数据
    Lst1[[i]]$rt=Rt(Lst1[[i]]$Close) #计算日内收益率
    Lst1[[i]]=Lst1[[i]][which(!(Lst1[[i]]$TIME %in% c("9:05","20:05"))),] #剔除异常数据
} 

names(Lst1)    # "Au1.csv"：普通时间； "Au2.csv"：节假日后第一天
########################各种已实现测度############
#### 计算交易日和已实现测度

Lst3=list() #存放各种向量和已实现测度表 
for (i in (1:2)) { 
    Lst3[[i]]=Lst1[[i]][which(Lst1[[i]]$TIME == "15:30"), "DATE"] #计算交易日
    Lst3[[i+2]]=RV(Lst3[[i]],Lst1[[i]]$Close,Lst1[[i]]$rt) #已实现波动RV
    Lst3[[i+4]]=as.data.frame(cbind(Lst3[[i]],Lst3[[i+2]]),stringsAsFactors = F) ##各种已实现测度
    colnames(Lst3[[i+4]])=c("date","RV")
}
names(Lst3)[1:2]=c("Au1date","Au2date") #交易日
names(Lst3)[3:4]=c("Au1_rv","Au2_rv") #RV
names(Lst3)[5:6]=c("Au1real","Au2real") #已实现测度表

Au_dayrt=read_excel('./data/day_rt.xlsx',sheet=1)#读入日收益率
Au_dayrt$date=as.Date(Au_dayrt$date)

Real_Au=rbind(Lst3[[5]],Lst3[[6]]) # Au所有交易日已实现测度
Lst4=list(Real_Au,Au_dayrt) #存放合并后的已实现测度
names(Lst4)[1]=c("Real_Au.csv")
Lst4[[1]]$date=lubridate::ymd(Lst4[[1]]$date) #转为日期形式
Lst4[[1]]=dplyr::arrange(Lst4[[1]], date) # 按日期排序
Lst4[[1]]$RV=as.numeric(Lst4[[1]]$RV)
Lst4[[1]]=merge(Lst4[[1]],Lst4[[2]],sort = FALSE,all = TRUE) #加入日收益率和日收盘价
write.csv(Lst4[[1]],paste('./data/',names(Lst4)[1],sep = ""),row.names = F) #保存

str(Lst4[[1]])
###以上代码只运行一次


#读入数据
AU=read.csv("./data/Real_Au.csv",header=TRUE,sep=',',stringsAsFactors=FALSE)
AU$date=as.Date(AU$date)

par(mfrow=c(2,1))
par(mai=c(0,2,1,1))
plot(AU$close,type='l',xlab='日期',ylab='Au(T+D)每日收盘价',xaxt="n",cex.lab=2.5,cex.axis=2,lwd=2) 
par(mai=c(1,2,0,1))
plot(AU$rt,type='l',xlab='日期',ylab='Au(T+D)收益率',xaxt="n",las=0,cex.lab=2.5,cex.axis=2,lwd=2) 
axis(1,labels=c("19年8月","10月","12月","20年2月","4月","6月","8月","10月","12月"),at=c(23,63,101,139,179,218,261,304,347),cex.axis=2)

Real_Au <-xts(read.zoo("./data/Real_Au.csv",header=TRUE,sep=','))

####Au基本统计特征
Au_rt=basicStats(Real_Au$rt) #rt基本统计特征
Au_RV=basicStats(Real_Au$RV) #RV基本统计特征

Au_rt_JB=jarque.bera.test(Real_Au$rt)#(p<0.05 非正态) #JB检验
Au_RV_JB=jarque.bera.test(Real_Au$RV)

Auchara<-as.data.frame(array(,dim=c(2,7)))
Auchara[1,]<-t(c(Au_rt[c("Mean",'Minimum','Maximum','Stdev','Skewness','Kurtosis'),1],as.numeric(Au_rt_JB[[1]][1])))
Auchara[2,]<-t(c(Au_RV[c(7,3,4,14,15,16),1],as.numeric(Au_RV_JB[[1]][1])))
colnames(Auchara)=c('均值','最小值','最大值','标准差','偏度','峰度','JB统计量')
rownames(Auchara)=c('Au_rt','Au_RV')
kable(Auchara, align = c('c'), caption="Au(T+D)收益率和已实现测度统计特征",longtable = TRUE, booktabs = TRUE, linesep="")%>%
			     kable_styling(latex_options = c("scale_down", "repeat_header", "hold_position"),
                 repeat_header_text = "(续)")

#####Au收益率序列平稳性检验########
AuADF=adf.test(Real_Au$rt) #ADF检验
Auadf_test=data.frame(a=AuADF$statistic[1],b=0.0001)
colnames(Auadf_test)=c("检验统计量","P值")
rownames(Auadf_test)="ADF检验"
kable(Auadf_test, align = c('c'), caption="Au(T+D)收益率序列平稳性检验",longtable = TRUE, booktabs = TRUE, linesep="")

par(mfrow=c(2,1))
par(mar=c(3,2,2,1))
acf(Real_Au$rt,main="",xlab="滞后期",ylab="ACF",lag.max=20,ylim=c(-0.2,1))#画自相关图
title(main="(a)the ACF of Au Return",cex.main=0.95)
pacf(Real_Au$rt,main="",xlab="滞后期",ylab="PACF",lag.max=20,ylim=c(-0.2,0.2))#画偏自相关图
title(main="(b)the PACF of Au Return",cex.main=0.95)

#####Au收益率序列-LB统计量########
Au_box=Box.test(Real_Au$rt[1:241],lag=10,type = "Ljung") #随机性检验
Q_test=data.frame(a=Au_box[1],df=Au_box[2],p=Au_box[3])
colnames(Q_test)=c("LB统计量",'延迟阶数',"P值")
rownames(Q_test)="纯随机性检验"
kable(Q_test, align = c('c'), caption="Au(T+D)收益率序列纯随机性检验",longtable = TRUE, booktabs = TRUE, linesep="")

par(mfrow=c(1,1))
par(mar=c(2,4,1,2))
x=c("201908","10","12","202002","4","6","8","10","12")
xat=c(23,64,103,141,183,222,265,308,351)
fit1=stats::arima(Real_Au$rt[1:241],order = c(0,0,0))
Au_r<-stats::residuals(fit1)
plot(Au_r^2,type="l",
     xlab="日期",ylab="Au(T+D)残差平方",xaxt="n")
axis(1,labels=x,at=xat)

#####Au收益率序列-ARCH检验########
Au_arch=ArchTest(Au_r^2,lag=5) #异方差性检验
arch_test=data.frame(stat=Au_arch[1],df=Au_arch[2],p=0.0000)
colnames(arch_test)=c("LM检验统计量",'自由度',"P值")
rownames(arch_test)="ARCH LM-检验"
kable(arch_test, align = c('c'), caption="Au(T+D)残差平方序列ARCH检验",longtable = TRUE, booktabs = TRUE, linesep="")

Real_Au1 <-xts(read.zoo("./data/Real_Au.csv",header=TRUE,sep=','))
Real_Au=Real_Au1[1:241]  #样本内参数估计

###设定模型分布形式
norm_spec.mod= ugarchspec(
  variance.model = list(model = "realGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
  distribution.model = "norm") # 正态分布
std_spec.mod= ugarchspec(
  variance.model = list(model = "realGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
  distribution.model = "std") # t分布
sstd_spec.mod= ugarchspec(
  variance.model = list(model = "realGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
  distribution.model = "sstd") # 偏t分布
ghyp_spec.mod= ugarchspec(
  variance.model = list(model = "realGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
  distribution.model = "ghyp") # 广义双曲线分布


Lst5=list(norm_spec.mod,std_spec.mod,sstd_spec.mod,ghyp_spec.mod)  #存放模型分布
AuLst=list() #存放Au拟合模型回归结果  1-4是RV

###Au拟合模型参数估计
for (i in 1:4) {
  AuLst[[i]]=ugarchfit(data=Real_Au[,"rt"], spec = Lst5[[i]], 
                        solver = "hybrid", realizedVol = Real_Au[,"RV"])
}
names(AuLst)=c("AuRV_norm_sgarch.mod","AuRV_std_sgarch.mod","AuRV_sstd_sgarch.mod","AuRV_ghyp_sgarch.mod")

AuRVfit1=AuLst[["AuRV_norm_sgarch.mod"]]
AuRVfit1_par=AuRVfit1@fit$matcoef[, 1] #系数
AuRVfit1_p=AuRVfit1@fit$matcoef[, 4] #p值
AuRVfit1_log=sum(-AuRVfit1@fit$log.likelihoods) #极大似然值
AuRVfit1=cbind(AuRVfit1_par,AuRVfit1_p) #合并
AuRVfit1  #全部显著

AuRVfit2=AuLst[["AuRV_std_sgarch.mod"]]
AuRVfit2_par=AuRVfit2@fit$matcoef[, 1] #系数
AuRVfit2_p=AuRVfit2@fit$matcoef[, 4] #p值
AuRVfit2_log=sum(-AuRVfit2@fit$log.likelihoods) #极大似然值
AuRVfit2=cbind(AuRVfit2_par,AuRVfit2_p) #合并
AuRVfit2  #全部显著

AuRVfit3=AuLst[["AuRV_sstd_sgarch.mod"]]
AuRVfit3_par=AuRVfit3@fit$matcoef[, 1] #系数
AuRVfit3_p=AuRVfit3@fit$matcoef[, 4] #p值
AuRVfit3_log=sum(-AuRVfit3@fit$log.likelihoods) #极大似然值
AuRVfit3=cbind(AuRVfit3_par,AuRVfit3_p) #合并
AuRVfit3  #全部显著

AuRVfit4=AuLst[["AuRV_ghyp_sgarch.mod"]]
AuRVfit4_par=AuRVfit4@fit$matcoef[, 1] #系数
AuRVfit4_p=AuRVfit4@fit$matcoef[, 4] #p值
AuRVfit4_log=sum(-AuRVfit4@fit$log.likelihoods) #极大似然值
AuRVfit4=cbind(AuRVfit4_par,AuRVfit4_p) #合并
AuRVfit4  #全部显著

#似然函数值
AuRV_log=c(AuRVfit1_log,AuRVfit2_log,AuRVfit3_log,AuRVfit4_log)
AuRV_log  #AuRV_ghyp值最大

options(digits=3)
Au_estimate <- read.csv('./results/Auestimation.csv',header=T,sep=',',encoding ="ANSI")
colnames(Au_estimate) <- c("模型形式",'$\\omega$',"$\\alpha$","$\\beta$",'$\\tau_1$','$\\tau_2$','$\\delta$','$\\lambda$','$\\xi$','$\\pi$')
kable(Au_estimate, row.names =F, align = c("c"), caption="Au(T+D)不同模型参数估计结果",
             longtable = TRUE, booktabs = TRUE, escape = F,
             linesep  = c(rep("", 7), "\\hline", rep("",7), "\\hline", rep("",7), "\\hline")) %>%
			     kable_styling(latex_options = c("scale_down", "repeat_header", "hold_position"),
                 repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1:10, width = c("1.8cm", "1.1cm","1.1cm","1.1cm","1.1cm","1.1cm","1.1cm","1.1cm","1.1cm","1.1cm"))%>%
    footnote(general ="每个参数估计结果的第一行为参数值大小，第二行为每个估计结果的p值。",
             general_title = "注:",footnote_as_chunk = T,threeparttable=T,fixed_small_size=T) 

AULOG=as.data.frame(t(AuRV_log))
colnames(AULOG)=c('正态分布','t分布','偏t分布','广义双曲线分布')
rownames(AULOG)=c('对数似然函数值')
kable(AULOG, align = c('c'), caption="Au(T+D)不同分布下的模型极大似然函数值",longtable = TRUE, booktabs = TRUE, linesep="")

Auvar=read.csv("./results/AuVaR.csv",header=T,sep=',',encoding ="ANSI") 
colnames(Auvar)=c("VaR显著性水平","模型形式","LR统计量","失败率","失败天数","P值")
Auvar[2,1]="$\\alpha$=0.01"
Auvar[-2,1]=""
kable(Auvar, row.names =F, align = c("c"), caption="不同Realized Garch模型样本外VaR一步预测",
             longtable = TRUE, booktabs = TRUE, escape = F,
             linesep  = "") %>%
			     kable_styling(latex_options = c("scale_down", "repeat_header", "hold_position"),
                 repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1:6, width = c("2cm","3cm", "1.9cm", "1.9cm","1.9cm", "1.9cm"))

total_Au = xts(read.zoo("./data/Real_Au.csv",header=TRUE,sep=','))
best_model = ugarchfit(data=total_Au[,"rt"], spec = Lst5[[4]], 
                      solver = "hybrid", realizedVol = total_Au[,"RV"])
Auvol = best_model@fit[["sigma"]]^2
fit1=stats::arima(total_Au$rt,order = c(0,0,0))
Au_r=stats::residuals(fit1)

x=c("201908","10","12","202002","4","6","8","10","12")
xat=c(23,64,103,141,183,222,265,308,351)
plot(Auvol,type='l',xlab='日期',ylab='波动率',xaxt = "n")
lines(Au_r^2,type="l",xlab="日期",ylab="波动率",xaxt="n",col='grey')
axis(1,labels=x,at=xat)
legend("topleft",c("拟合波动率","实际波动率"),lty=c(1,1),col=c("black","grey"),cex=1)
