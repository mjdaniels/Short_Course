library(R2jags)
library(coda)
library(lattice)

source("D:\\dandan\\short course\\examples\\CTQ\\writeDatafileR.txt")
cqt.data<-read.csv("D:\\dandan\\short course\\examples\\CTQ\\CTQ.csv", header = TRUE, sep = ",")
yy<-t(matrix(cqt.data$quit,ncol=281))
y<-yy[rowSums(is.na(yy))<12,]
tt<-t(matrix(cqt.data$week,ncol=281))
t<-tt[rowSums(is.na(yy))<12,]
trtt<-t(matrix(cqt.data$trtgroup,ncol=281))
trt<-trtt[rowSums(is.na(yy))<12,]
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



logli<-NULL

for (i in 1:N1) {
  y.3<-y.1[id.1!=i]
  x.11<-x.1[id.1!=i]
  x.22<-x.2[id.1!=i]
  id.4<-id.1[id.1!=i]
  id.3<-id.4
  id.3[id.4>i]<-id.4[id.4>i]-1
  trt.3<-trt.1[id.1!=i]
  nrec.3<-length(y.3)
  nsub.3<-N1-1
  ynew<-y.1[id.1==i]
  xnew1<-x.1[id.1==i]
  xnew2<-x.2[id.1==i]
  nobs<-length(ynew)
  trtnew<-trt.1[id.1==i]
  K=20
    
  data=list(y=y.3,x1=x.11,x2=x.22,trt=trt.3,id=id.3,N=nrec.3,N1=nsub.3,K=K,
            ynew=ynew,xnew1=xnew1,xnew2=xnew2,nobs=nobs,trtnew=trtnew)
  
  para<-c("ynewlogli")
  
  glmmcpo<-jags(data=data, inits=NULL, parameters.to.save=para, model.file="D:\\dandan\\short course\\examples\\CTQ\\DPMglmm_cpo.txt",
                n.chains=1, n.iter=10000, n.burnin=5000,n.thin=1)
  
  
  logli<-cbind(logli,glmmcpo$BUGSoutput$sims.list$ynewlogli)
}

cpo<-colMeans(logli)
lpml<-sum(cpo)

######CPO

