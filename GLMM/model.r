library(R2jags)
library(coda)
library(lattice)

source("D:\\dandan\\short course\\examples\\CTQ\\writeDatafileR.txt")
cqt.data<-read.csv("D:\\dandan\\short course\\examples\\CTQ\\CTQ.csv", header = TRUE, sep = ",")
y<-t(matrix(cqt.data$quit,ncol=281))
t<-t(matrix(cqt.data$week,ncol=281))
trt<-t(matrix(cqt.data$trtgroup,ncol=281))
id<-matrix(rep(1:nrow(y),12),ncol=12)

yy<-c(y)
y.1<-yy[is.na(yy)==FALSE]

tt<-c(t)
t.1<-tt[is.na(yy)==FALSE]

x1<-rep(0,length(yy))
x1[tt<5]<-1
x.1<-x1[is.na(yy)==FALSE]

x2<-rep(0,length(yy))
x2[tt>=5]<-1
x.2<-x2[is.na(yy)==FALSE]

idd<-c(id)
id.1<-id[is.na(yy)==FALSE]

trt11<-c(trt)
trt.1<-trt11[is.na(yy)==FALSE]

N=length(trt.1)
N1<-nrow(y)


## glmm
data=list(y=y.1,x1=x.1,x2=x.2,trt=trt.1,id=id.1,N=N,N1=N1)

para<-c("mu.alpha0","beta","tau.alpha0","tau.theta0","phi","theta","mutheta0")

glmmfit2<-jags(data=data, inits=NULL, parameters.to.save=para, model.file="D:\\dandan\\short course\\examples\\CTQ\\glmm.txt",
               n.chains=1, n.iter=20000, n.burnin=10000,n.thin=1)
print(glmmfit2)
glmm2<-colMeans(glmmfit2$BUGSoutput$sims.matrix)


## DPM glmm
K=50
data=list(y=y.1,x1=x.1,x2=x.2,trt=trt.1,id=id.1,N=N,N1=N1,K=K)

para<-c("mu.mu.alpha0","tau.mu.alpha0",
        "mu.beta","tau.beta",
        "mu.phi","tau.phi",
        "atau.alpha0","btau.alpha0",
        "atau.theta0","btau.theta0",
        "theta","cluster")

glmmfit3<-jags(data=data, inits=NULL, parameters.to.save=para, model.file="D:\\dandan\\short course\\examples\\CTQ\\DPMglmm.txt",
               n.chains=1, n.iter=10000, n.burnin=5000,n.thin=1)
print(glmmfit3)
glmm3<-colMeans(glmmfit3$BUGSoutput$sims.matrix)


