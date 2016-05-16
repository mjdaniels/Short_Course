
# read the data
sch <- read.table("D://dandan//short course//examples//schizo//schizo.dat",header=F)


t<-c(0,1,2,3,4,6)
## covariates: quadratic orthogonal polynomial
x1<-t-mean(t)
x2<-(t-mean(t))^2-sum((t-mean(t))^3)/sum((t-mean(t))^2)*(t-mean(t))-sum((t-mean(t))^2)/6

x<-cbind(1,x1,x2)

id<-unique(as.matrix(sch[,1:3]))

sch1<-data.frame(cbind(rep(id[,1],each=6),rep(id[,2],each=6),rep(id[,3],each=6),
                       rep(c(0,1,2,3,4,6),245)))


colnames(sch1) <- c("V1", "V2","V3","V4")

schall<-merge(sch1,sch,by=c("V1", "V2","V3","V4"),all=TRUE)

y1<-t(matrix(as.numeric(schall[,5]),6,245))
trt1<-as.integer(id[,1])
Nsub<-nrow(y1)
y2<-c(y1)
trt2<-rep(trt1,6)
pid2<-rep(1:245,6)
t2<-rep(1:6,each=245)

y<-y2[is.na(y2)==FALSE]
trt<-trt2[is.na(y2)==FALSE]
pid<-pid2[is.na(y2)==FALSE]
t<-t2[is.na(y2)==FALSE]

Nrec<-length(y)



R1<-diag(c(120,2,2))
bmu0<-rep(0,3)

grp<-matrix(0,length(y),245)
for (i in 1:245) {
  grp[pid==i,i]<-1
}


dat<-list("y","trt", "pid","t", "R1","bmu0","Nsub","Nrec","x","grp")
 

schmodel1<-function() 
{  for (i in 1:Nrec)
{ for (k in 1:3)
{beta1[i,k]<-beta[trt[i],k]+b[pid[i],k]} 
mu[i]<-inprod(x[t[i],1:3],beta1[i,1:3])
y[i]~dnorm(mu[i],tau)
chi.obs[i]<-pow((y[i]-mu[i]),2)*tau
ypdf[i]<-log(1/sqrt(2*3.14)*sqrt(tau))-1/2*tau*pow((y[i]-mu[i]),2)
}  

for (i in 1:Nrec)
{ 
  yrep[i]~dnorm(mu[i],tau)
  chi.rep[i]<-pow((yrep[i]-mu[i]),2)*tau
} 

chiobs<-sum(chi.obs[])
chirep<-sum(chi.rep[])
chi.diff<-chirep-chiobs
chi.diff1<-step(chirep-chiobs)

for (k in 1:4)
{ for(j in 1:3)       
{ beta[k,j]~dnorm(0, 0.000001) } 
}

for (i in 1:Nsub) {
  b[i,1:3]~dmnorm(bmu0[1:3],btau[1:3,1:3])
}

for (i in 1:Nsub) {
  logli[i]<-inprod(ypdf[1:Nrec],grp[1:Nrec,i])
}

btau[1:3,1:3]~dwish(R1[,],3)    			# prior for precision matrix
tau<-1/pow(sigma,2)
sigma~dunif(0,100)

for (i in 1:4) {
  for (j in 1:6) {
    mu0[i,j]<-inprod(x[j,1:3],beta[i,1:3])
  }
  change[i]<-mu0[i,6]-mu0[i,1]
}
for (k in 2:4)
{
  diff[k-1]<-change[k]-change[1] 
  diff1[k-1]<-step(diff[k-1])
}
for (k in 3:4)
{
  diff[k+1]<-change[k]-change[2] 
  diff1[k+1]<-step(diff[k+1])
}
diff[6]<-change[4]-change[3] 
diff1[6]<-step(diff[6])

}  


write.model(schmodel1, "D://dandan//short course//examples//schizo//model1//schmodel1.txt")				# Writes the compiled "MARmodel" WinBUGS model 


param<-c('mu0','beta','change','diff','diff1','btau',"chi.diff","chi.diff1","tau","logli")					# A character vector with the names of the parameters to be updated

LMM<-bugs(  data  = dat,
            inits = NULL,
            parameters = param,
            model.file = "schmodel1.txt",
            n.chains = 2,
            n.iter = 10000,
            n.burnin = 5000,
            n.thin=1,
            bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
            working.directory = "D:/dandan/short course/examples/schizo/model1/",
            clearWD = FALSE,
            debug=TRUE
)

print(LMM)



####### model check
hist(LMM$sims.list$chi.diff,xlab=expression(paste("difference in ",chi^2," discrepancy",sep="")),main="",breaks=seq(-300,400,by=20))
mean(LMM$sims.list$chi.diff1)

##### CPO
cpoi<-function(yi,trti,x,beta,btau,tau) {
  nm<-1:6
  nm<-nm[is.na(yi)==FALSE]
  xx<-matrix(x[nm,],ncol=3)
  mu<-xx%*%t(matrix(beta[trti,],ncol=3))
  sigma<-xx%*%solve(btau)%*%t(xx)+1/tau*diag(1,length(nm))
  cpoi<-dmvnorm(yi[nm],mu,sigma)
  return(cpoi)
}

nsave<-length(LMM$sims.list$btau)
cpoi1<-rep(NA,nsave)
cpoi2<-rep(NA,nrow(y1))
for (i in 1:nrow(y1)) {
  yi<-y1[i,]
  trti<-trt1[i]
  for (j in 1:nsave) {
    beta<-matrix(LMM$sims.list$beta[j,,],ncol=3)
    btau<-matrix(LMM$sims.list$btau[j,,],ncol=3)
    tau<-LMM$sims.list$btau[j]
    cpoi1[j]<-1/cpoi(yi,trti,x,beta,btau,tau)
  }
  cpoi2[i]<-1/mean(cpoi1)
}


### WAIC
logli<-as.matrix(LMM$sims.list$logli)
lpd<-sum(log(colMeans(exp(logli))))
pwaic<-sum((apply(logli,2,sd))^2)
elpd<-lpd-pwaic
waic<- -2*elpd




