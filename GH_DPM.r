
library(R2jags)
library(coda)
library(lattice)

# 2. Read observed data

# 1. Read observed data
### Change the directory
GH<-read.table("D://dandan//short course//examples//GH//GH.txt", header=T, sep=' ')  # data frame containing the observed GH data
N<-nrow(GH)  							# N: total number of observations
yall<-as.matrix(GH[,1:3])						# N by 3 matrix each comlumn of which corresponds to one
# of the three observation times
trtall<-as.integer(GH[,4])						# numeric vector indicating the assigned treatment to each subject

# 2. Reorder the data for treatment 1
yall2<-yall[trtall==1,]
# standadize the data
yall1<-(yall2-mean(c(yall2),na.rm=TRUE))/sd(c(yall2),na.rm=TRUE)*sqrt(0.5)
ymean<-mean(c(yall2),na.rm=TRUE)
ysd<-sd(c(yall2),na.rm=TRUE)/sqrt(0.5)

y1<-yall1[is.na(yall1[,2])==TRUE&is.na(yall1[,3])==TRUE,]
y2<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==TRUE,]
y3<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==FALSE,]
y<-rbind(y1,y2,y3)
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))
N<-nrow(y)
K<-50
g1<-(26.2/ysd)^2
g2<-(23.4/ysd)^2
g3<-(15.3/ysd)^2

# 3. run the model for treatment 1
param<-c('cluster','mmu1','mmu2','mmu3','logli')  		
data=list(y=y,N=N,K=K,N1=N1,N2=N2,g1=g1,g2=g2,g3=g3,ymean=ymean,ysd=ysd)
fits1<-jags(data=data,  parameters.to.save=param, 
            model.file="D://dandan//short course//examples//GH//DPM//DPMmod12.txt",
            n.chains=1, n.iter=55000, n.burnin=5000,n.thin=1)

# 4. Reorder the data for treatment 2
yall2<-yall[trtall==2,]
# standadize the data
yall1<-(yall2-mean(c(yall2),na.rm=TRUE))/sd(c(yall2),na.rm=TRUE)*sqrt(0.5)
ymean<-mean(c(yall2),na.rm=TRUE)
ysd<-sd(c(yall2),na.rm=TRUE)/sqrt(0.5)

y1<-yall1[is.na(yall1[,2])==TRUE&is.na(yall1[,3])==TRUE,]
y2<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==TRUE,]
y3<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==FALSE,]
y<-rbind(y1,y2,y3)
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))
N<-nrow(y)
K<-50
g1<-(23.3/ysd)^2
g2<-(13.2/ysd)^2
g3<-(9.4/ysd)^2

# 5. run the model for treatment 2
data=list(y=y,N=N,K=K,N1=N1,N2=N2,g1=g1,g2=g2,g3=g3,ymean=ymean,ysd=ysd)
fits2<-jags(data=data,  parameters.to.save=param, 
            model.file="D://dandan//short course//examples//GH//DPM//DPMmod12.txt",
            n.chains=1, n.iter=55000, n.burnin=5000,n.thin=1)

# 6. Reorder the data for treatment 3
yall2<-yall[trtall==3,]
# standadize the data
yall1<-(yall2-mean(c(yall2),na.rm=TRUE))/sd(c(yall2),na.rm=TRUE)*sqrt(0.5)
ymean<-mean(c(yall2),na.rm=TRUE)
ysd<-sd(c(yall2),na.rm=TRUE)/sqrt(0.5)

y1<-yall1[is.na(yall1[,2])==TRUE&is.na(yall1[,3])==TRUE,]
y2<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==TRUE,]
y3<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==FALSE,]
y<-rbind(y1,y2,y3)
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))
N<-nrow(y)
K<-50
g1<-(27.1/ysd)^2
g2<-(13.9/ysd)^2
g3<-(8.9/ysd)^2


# 7. run the model for treatment 3
data=list(y=y,N=N,K=K,N1=N1,N2=N2,g1=g1,g2=g2,g3=g3,ymean=ymean,ysd=ysd)
fits3<-jags(data=data,  parameters.to.save=param, 
            model.file="D://dandan//short course//examples//GH//DPM//DPMmod12.txt",
            n.chains=1, n.iter=55000, n.burnin=5000,n.thin=1)


# 8. Reorder the data for treatment 4
yall2<-yall[trtall==4,]
# standadize the data
yall1<-(yall2-mean(c(yall2),na.rm=TRUE))/sd(c(yall2),na.rm=TRUE)*sqrt(0.5)
ymean<-mean(c(yall2),na.rm=TRUE)
ysd<-sd(c(yall2),na.rm=TRUE)/sqrt(0.5)

y1<-yall1[is.na(yall1[,2])==TRUE&is.na(yall1[,3])==TRUE,]
y2<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==TRUE,]
y3<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==FALSE,]
y<-rbind(y1,y2,y3)
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))
N<-nrow(y)
K<-50
g1<-(24.8/ysd)^2
g2<-(14.1/ysd)^2
g3<-(12.2/ysd)^2


# 9. run the model for treatment 4
data=list(y=y,N=N,K=K,N1=N1,N2=N2,g1=g1,g2=g2,g3=g3,ymean=ymean,ysd=ysd)
fits4<-jags(data=data,  parameters.to.save=param, 
            model.file="D://dandan//short course//examples//GH//DPM//DPMmod12.txt",
            n.chains=1, n.iter=55000, n.burnin=5000,n.thin=1)

LPML4<-sum(-log(colMeans(1/exp(fits1$BUGSoutput$sims.list$logli))))+
  sum(-log(colMeans(1/exp(fits2$BUGSoutput$sims.list$logli))))+
  sum(-log(colMeans(1/exp(fits3$BUGSoutput$sims.list$logli))))+
  sum(-log(colMeans(1/exp(fits4$BUGSoutput$sims.list$logli))))







