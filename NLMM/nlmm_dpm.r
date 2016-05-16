unlink(".RData")
rm(list=ls())

library(R2jags)
library(coda)
library(lattice)

################# read dataset
data<-read.table("D:\\dandan\\short course\\examples\\dmd.dir\\dmd.csv",sep=",",header=T)
y1<-data[order(as.numeric(data$id),data$time),]

id<-sort(unique(data$id))
time<-sort(unique(data$time))

n<-length(id)
A<-length(time)

y2<-data.frame(id[rep(1:n,each=A)],time[rep(1:A,n)])
colnames(y2)<-c("id","time")

y3<-merge(y2,y1,by=c("id","time"),all.x=T)
for (i in 2:(dim(y3)[1])){
  if (is.na(y3$age[i])==T) {
    y3$age[i]<-y3$age[i-1]+(y3$time[i]-y3$time[i-1])/12
  }
}

y4<-y3[order(y3$time,y3$id),]
y<-array(y4$y,dim=c(n,A))
age<-array(y4$age,dim=c(n,A))

######################### Call jags run MCMC

K<-50
agegrid<-seq(5,18,length=50)
data=list(y=y,age=age,n=n,T=A,K=K,agegrid=agegrid)



##### parameters  
para<-c("A", "tau.y","alpha", "theta0","xi0","nu0","omega0","mu.y","mu.ypred","mupop")


data=list(y=y,age=age,n=n,T=A,K=K)
#para<-c("A", "tau.y","alpha", "theta0","xi0","nu0","omega0")

nlmefits4<-jags(data=data, inits=NULL, parameters.to.save=para, model.file="D:\\dandan\\short course\\examples\\dmd.dir\\nlmeA1.txt",
            n.chains=1, n.iter=15000, n.burnin=10000,n.thin=1)


### plot for subject-level and population-level trajectories
mu<-NULL
for (i in 1:10)
{mu<-rbind(mu,apply(nlmefits4$BUGSoutput$sims.list$mu.ypred[,i,],2,mean))}

par(mfrow=c(1,1))
plot(agegrid,mu[1,],lty=1,type="l",ylab="Measure",xlab="age",xaxt='n',ylim=c(0,1))
axis(1,at=seq(5,18,by=1),labels=seq(5,18,by=1))
for (i in 2:10) {
  lines(agegrid,mu[i,],lty=i,type="l")}

lines(agegrid,colMeans(nlmefits4$BUGSoutput$sims.list$mupop),col="red",lty=1,type="l",lwd=2)
legend(5,1, c("subject-level","population-level    "), lty=c(1,1),lwd=c(1,2),col=c("black","red"))