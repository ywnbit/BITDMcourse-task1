#读取数据
mydata <- read.table('Analysis.txt',
          header=F,
          dec='.',
          col.names=c('season','size','speed','mxPH','mnO2','Cl',
          'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
          'a5','a6','a7'),
          na.strings=c('XXXXXXX'))

#分析摘要
summary(mydata)


#数据可视化
library(car)

#绘制直方图
hist(mydata$mxPH)

#绘制QQ图
qqPlot(mydata$mxPH,main='Norm QQ Plot of mxPH')

#绘制箱形图
boxplot(mydata$mxPH,ylab='mxPH')
rug(mydata$mxPH,side=4)
abline(h=mean(mydata$mxPH,na.rm=T),lty=2)

hist(mydata$mnO2)
qqPlot(mydata$mnO2,main='Norm QQ Plot of mnO2')
boxplot(mydata$mnO2,ylab='mnO2')
rug(mydata$mnO2,side=4)
abline(h=mean(mydata$mnO2,na.rm=T),lty=2)

hist(mydata$Cl)
qqPlot(mydata$Cl,main='Norm QQ Plot of Cl')
boxplot(mydata$Cl,ylab='Cl')
rug(mydata$Cl,side=4)
abline(h=mean(mydata$Cl,na.rm=T),lty=2)

hist(mydata$NO3)
qqPlot(mydata$NO3,main='Norm QQ Plot of NO3')
boxplot(mydata$NO3,ylab='NO3')
rug(mydata$NO3,side=4)
abline(h=mean(mydata$NO3,na.rm=T),lty=2)

hist(mydata$NH4)
qqPlot(mydata$NH4,main='Norm QQ Plot of NH4')
boxplot(mydata$NH4,ylab='NH4')
rug(mydata$NH4,side=4)
abline(h=mean(mydata$NH4,na.rm=T),lty=2)

hist(mydata$oPO4)
qqPlot(mydata$oPO4,main='Norm QQ Plot of oPO4')
boxplot(mydata$oPO4,ylab='oPO4')
rug(mydata$oPO4,side=4)
abline(h=mean(mydata$oPO4,na.rm=T),lty=2)

hist(mydata$PO4)
qqPlot(mydata$PO4,main='Norm QQ Plot of PO4')
boxplot(mydata$PO4,ylab='PO4')
rug(mydata$PO4,side=4)
abline(h=mean(mydata$PO4,na.rm=T),lty=2)

hist(mydata$Chla)
qqPlot(mydata$Chla,main='Norm QQ Plot of Chla')
boxplot(mydata$Chla,ylab='Chla')
rug(mydata$Chla,side=4)
abline(h=mean(mydata$Chla,na.rm=T),lty=2)

#绘制条件盒图
library(lattice)

bwplot(size~a1,data=mydata,ylab='River Size',xlab='a1')
bwplot(size~a2,data=mydata,ylab='River Size',xlab='a2')
bwplot(size~a3,data=mydata,ylab='River Size',xlab='a3')
bwplot(size~a4,data=mydata,ylab='River Size',xlab='a4')
bwplot(size~a5,data=mydata,ylab='River Size',xlab='a5')
bwplot(size~a6,data=mydata,ylab='River Size',xlab='a6')
bwplot(size~a7,data=mydata,ylab='River Size',xlab='a7')

#缺失数据处理
#剔除缺失数据
omiteddata = na.omit(mydata)
write.table(omitdata,'D:/DataMining/OmitedData.txt',col.names = F,row.names = F, quote = F)

#使用高频数据替换
library(DMwR)
preprocess2 = mydata[-manyNAs(mydata),]
preprocess2 = centralImputation(Preprocess2)
write.table(preprocess2,'D:/DataMining/CentralImputationData.txt',
col.names = F,row.names = F, quote = F)

#通过变量相关性填补缺失值
symnum(cor(mydata[,4:18],use='complete.obs'))
lm(formula=PO4~oPO4, data=mydata)
preprocess3 = mydata[-manyNAs(mydata),]
fillPO4 <- function(oP){
	if(is.na(oP))
		return(NA)
	else return (42.897 + 1.293 * oP)
}
preprocess3[is.na(preprocess3$PO4),'PO4'] <- sapply(preprocess3[is.na(preprocess3$PO4),'oPO4'],fillPO4)
write.table(preprocess3,'D:/DataMining/linearDefaultData.txt',
col.names = F,row.names = F, quote = F)

#通过案例的相关性填补缺失值
preprocess4 = knnImputation(mydata,k=10)
write.table(preprocess4,'D:/DataMining/knnImputationData.txt',
col.names = F,row.names = F, quote = F)