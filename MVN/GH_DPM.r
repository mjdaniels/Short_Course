
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
yall1<-yall[trtall==1,] # for treatment 1 		
y1<-yall1[is.na(yall1[,2])==TRUE&is.na(yall1[,3])==TRUE,]
y2<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==TRUE,]
y3<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==FALSE,]
y<-rbind(y1,y2,y3)
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))
N<-nrow(y)
K<-50

# 3. run the model for treatment 1
param<-c('cluster','mmu1','mmu2','mmu3','logli')  		
data=list(y=y,N=N,K=K,N1=N1,N2=N2)
fits1<-jags(data=data,  parameters.to.save=param, 
            model.file="D://dandan//short course//examples//GH//DPM//DPMmod1.txt",
            n.chains=1, n.iter=55000, n.burnin=5000,n.thin=1)

# 4. Reorder the data for treatment 2
yall1<-yall[trtall==2,]    	
y1<-yall1[is.na(yall1[,2])==TRUE&is.na(yall1[,3])==TRUE,]
y2<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==TRUE,]
y3<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==FALSE,]
y<-rbind(y1,y2,y3)
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))
N<-nrow(y)

# 5. run the model for treatment 2
fits2<-jags(data=data,  parameters.to.save=param, 
            model.file="D://dandan//short course//examples//GH//DPM//DPMmod1.txt",
            n.chains=1, n.iter=55000, n.burnin=5000,n.thin=1)

# 6. Reorder the data for treatment 3
yall1<-yall[trtall==3,]    	
y1<-yall1[is.na(yall1[,2])==TRUE&is.na(yall1[,3])==TRUE,]
y2<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==TRUE,]
y3<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==FALSE,]
y<-rbind(y1,y2,y3)
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))
N<-nrow(y)

# 7. run the model for treatment 3
fits3<-jags(data=data,  parameters.to.save=param, 
            model.file="D://dandan//short course//examples//GH//DPM//DPMmod1.txt",
            n.chains=1, n.iter=55000, n.burnin=5000,n.thin=1)


# 8. Reorder the data for treatment 4
yall1<-yall[trtall==4,]      
y1<-yall1[is.na(yall1[,2])==TRUE&is.na(yall1[,3])==TRUE,]
y2<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==TRUE,]
y3<-yall1[is.na(yall1[,2])==FALSE&is.na(yall1[,3])==FALSE,]
y<-rbind(y1,y2,y3)
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))
N<-nrow(y)

# 9. run the model for treatment 4
fits4<-jags(data=data,  parameters.to.save=param, 
            model.file="D://dandan//short course//examples//GH//DPM//DPMmod1.txt",
            n.chains=1, n.iter=55000, n.burnin=5000,n.thin=1)

LPML4<-sum(-log(colMeans(1/exp(fits1$BUGSoutput$sims.list$logli))))+
  sum(-log(colMeans(1/exp(fits2$BUGSoutput$sims.list$logli))))+
  sum(-log(colMeans(1/exp(fits3$BUGSoutput$sims.list$logli))))+
  sum(-log(colMeans(1/exp(fits4$BUGSoutput$sims.list$logli))))







