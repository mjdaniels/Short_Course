
cqt.data<-read.csv("D:\\dandan\\short course\\examples\\CTQ\\CTQ.csv", header = TRUE, sep = ",")
trt<-cqt.data$trtgroup[seq(1,3372,by=12)]
y1<-cqt.data$quit
y<-t(matrix(cqt.data$quit,ncol=281))
t<-t(matrix(cqt.data$week,ncol=281))
trt1<-t(matrix(cqt.data$trtgroup,ncol=281))


## MVP -- trt==1

# z=y if y observed, z=2 if y missing
z.1<-c(y[trt1[,1]==1,5:12])
z.1[is.na(z.1)==TRUE]<-2

y.1<-c(y[trt1[,1]==1,5:12])
y.2<-c(y[trt1[,1]==1,5:12])

# fill initial value for missing y
y.1[is.na(y.2)==TRUE]<-rbern(length(y.1[is.na(y.2)==TRUE]),0.5)

p=8
n=length(z.1)/8

X<-matrix(0,length(z.1),8)
for (i in 1:8) {
  X[((i-1)*n+1):(i*n),i]<-1
}


Data1=list(p=p,y=y.1,X=X,z=z.1)
Mcmc1=list(R=25000,keep=1)

out1=rmvpGibbs(Data=Data1,Mcmc=Mcmc1)

k<-8
ind=seq(from=0,by=p,length=k)
inda=1:8
ind=ind+inda
betatilde1=matrix(out1$betadraw/sqrt(out1$sigmadraw[,ind]),nrow=25000)
# the posterior of the mean for the latent multivariate normal
betatilde1<-betatilde1[5001:25000,]

## MVP2 -- trt==2

z.1<-c(y[trt1[,1]==2,5:12])
z.1[is.na(z.1)==TRUE]<-2


y.1<-c(y[trt1[,1]==2,5:12])
y.2<-c(y[trt1[,1]==2,5:12])
y.1[is.na(y.2)==TRUE]<-rbern(length(y.1[is.na(y.2)==TRUE]),0.5)

p=8
n=length(z.1)/8

X<-matrix(0,length(z.1),8)
for (i in 1:8) {
  X[((i-1)*n+1):(i*n),i]<-1
}


Data1=list(p=p,y=y.1,X=X,z=z.1)
Mcmc1=list(R=25000,keep=1)

out2=rmvpGibbs(Data=Data1,Mcmc=Mcmc1)


k<-8
ind=seq(from=0,by=p,length=k)
inda=1:8
ind=ind+inda
betatilde2=matrix(out2$betadraw/sqrt(out2$sigmadraw[,ind]),nrow=25000)
# the posterior of the mean for the latent multivariate normal
betatilde2<-betatilde2[5001:25000,]





