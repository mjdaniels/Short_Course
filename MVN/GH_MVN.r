
library(R2WinBUGS)

# 1. Read observed data
### Change the directory
GH<-read.table("D://dandan//short course//examples//GH//GH.txt", header=T, sep=' ')	# data frame containing the observed GH data
N<-nrow(GH)  							# N: total number of observations
yall<-as.matrix(GH[,1:3])						# N by 3 matrix each comlumn of which corresponds to one
# of the three observation times
trtall<-as.integer(GH[,4])						# numeric vector indicating the assigned treatment to each subject

# 2. Reorder the data 
y1<-yall[is.na(yall[,2])==TRUE&is.na(yall[,3])==TRUE,] # only observed at time 1
y2<-yall[is.na(yall[,2])==FALSE&is.na(yall[,3])==TRUE,] # observed at time 1,2
y3<-yall[is.na(yall[,2])==FALSE&is.na(yall[,3])==FALSE,] # all observed
y<-rbind(y1,y2,y3)
trt<-c(trtall[is.na(yall[,2])==TRUE&is.na(yall[,3])==TRUE],trtall[is.na(yall[,2])==FALSE&is.na(yall[,3])==TRUE],
       trtall[is.na(yall[,2])==FALSE&is.na(yall[,3])==FALSE])
N1<-nrow(y1)
N2<-nrow(rbind(y1,y2))

# --------------------------------------------------
# 3. Specify the parameters of the model

dat<-list("y","trt","N","N1","N2")				# A character vector with the names of the parameters included in the model
# (Information to be pased to WinBUGS package)

# --------------------------------------------------
# 4. Specify the MAR model 1

MARmodel1<-function() 
{  for (i in 1:N1)
{ y[i,1]~dnorm(mu1[trt[i]],tau1[trt[i]])
  logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1[trt[i]]))-1/2*tau1[trt[i]]*pow((y[i,1]-mu1[trt[i]]),2)
} 
for (i in (N1+1):N2)
{ y[i,1]~dnorm(mu1[trt[i]],tau1[trt[i]])
  mu22[i]<-beta0[trt[i]]+phi21[trt[i]]*y[i,1]
  y[i,2]~dnorm(mu22[i],tau2[trt[i]])
  logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1[trt[i]]))-1/2*tau1[trt[i]]*pow((y[i,1]-mu1[trt[i]]),2)+
    log(1/sqrt(2*3.14)*sqrt(tau2[trt[i]]))-1/2*tau2[trt[i]]*pow((y[i,2]-mu22[i]),2)
} 
for (i in (N2+1):N)
{ y[i,1]~dnorm(mu1[trt[i]],tau1[trt[i]])
  mu22[i]<-beta0[trt[i]]+phi21[trt[i]]*y[i,1]
  mu33[i]<-beta1[trt[i]]+phi31[trt[i]]*y[i,1]+phi32[trt[i]]*y[i,2]
  y[i,2]~dnorm(mu22[i],tau2[trt[i]])
  y[i,3]~dnorm(mu33[i],tau3[trt[i]])
  logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1[trt[i]]))-1/2*tau1[trt[i]]*pow((y[i,1]-mu1[trt[i]]),2)+
    log(1/sqrt(2*3.14)*sqrt(tau2[trt[i]]))-1/2*tau2[trt[i]]*pow((y[i,2]-mu22[i]),2)+
    log(1/sqrt(2*3.14)*sqrt(tau3[trt[i]]))-1/2*tau3[trt[i]]*pow((y[i,3]-mu33[i]),2)
} 
for (k in 1:4)
{     
mu1[k]~dnorm(0, 0.000001)
beta0[k]~dnorm(0, 0.000001)
beta1[k]~dnorm(0, 0.000001)
phi21[k]~dnorm(0, 0.000001)
phi31[k]~dnorm(0, 0.000001)
phi32[k]~dnorm(0, 0.000001)
sigma1[k]~dunif(0,100)
sigma2[k]~dunif(0,100)
sigma3[k]~dunif(0,100)
tau1[k]<-1/pow(sigma1[k],2)
tau2[k]<-1/pow(sigma2[k],2)
tau3[k]<-1/pow(sigma3[k],2)
}
for (k in 1:4)
{
mu2[k]<-beta0[k]+phi21[k]*mu1[k]
mu3[k]<-beta1[k]+phi31[k]*mu1[k]+phi32[k]*mu2[k]
}
for (k in 2:4)
{
diff[k-1]<-mu3[k]-mu3[1] 
diff1[k-1]<-step(diff[k-1])
}
for (k in 3:4)
{
  diff[k+1]<-mu3[k]-mu3[2] 
  diff1[k+1]<-step(diff[k+1])
}
diff[6]<-mu3[4]-mu3[3] 
diff1[6]<-step(diff[6])
}  
write.model(MARmodel1, "D:/dandan/short course/examples/GH/model1/MARmod1.txt")	

# model 2

MARmodel2<-function() 
{  
  for (i in 1:N1)
  { y[i,1]~dnorm(mu1[trt[i]],tau1)
    logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1))-1/2*tau1*pow((y[i,1]-mu1[trt[i]]),2)
    
  } 
  for (i in (N1+1):N2)
  { y[i,1]~dnorm(mu1[trt[i]],tau1)
    mu22[i]<-beta0[trt[i]]+phi21*y[i,1]
    y[i,2]~dnorm(mu22[i],tau2)
    logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1))-1/2*tau1*pow((y[i,1]-mu1[trt[i]]),2)+
      log(1/sqrt(2*3.14)*sqrt(tau2))-1/2*tau2*pow((y[i,2]-mu22[i]),2)
    
  } 
  for (i in (N2+1):N)
{ y[i,1]~dnorm(mu1[trt[i]],tau1)
  mu22[i]<-beta0[trt[i]]+phi21*y[i,1]
  mu33[i]<-beta1[trt[i]]+phi31*y[i,1]+phi32*y[i,2]
  y[i,2]~dnorm(mu22[i],tau2)
  y[i,3]~dnorm(mu33[i],tau3)
  logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1))-0.5*tau1*pow((y[i,1]-mu1[trt[i]]),2)+
    log(1/sqrt(2*3.14)*sqrt(tau2))-1/2*tau2*pow((y[i,2]-mu22[i]),2)+
    log(1/sqrt(2*3.14)*sqrt(tau3))-1/2*tau3*pow((y[i,3]-mu33[i]),2)
} 
phi21~dnorm(0, 0.000001)
phi31~dnorm(0, 0.000001)
phi32~dnorm(0, 0.000001)
sigma1~dunif(0,100)
sigma2~dunif(0,100)
sigma3~dunif(0,100)
tau1<-1/pow(sigma1,2)
tau2<-1/pow(sigma2,2)
tau3<-1/pow(sigma3,2)

for (k in 1:4)
{     
  mu1[k]~dnorm(0, 0.000001)
  beta0[k]~dnorm(0, 0.000001)
  beta1[k]~dnorm(0, 0.000001)
  mu2[k]<-beta0[k]+phi21*mu1[k]
  mu3[k]<-beta1[k]+phi31*mu1[k]+phi32*mu2[k]
}
for (k in 2:4)
{
  diff[k-1]<-mu3[k]-mu3[1] 
  diff1[k-1]<-step(diff[k-1])
}
for (k in 3:4)
{
  diff[k+1]<-mu3[k]-mu3[2] 
  diff1[k+1]<-step(diff[k+1])
}
diff[6]<-mu3[4]-mu3[3] 
diff1[6]<-step(diff[6])

}  
write.model(MARmodel2, "D:/dandan/short course/examples/GH/model2/MARmod2.txt")

## model 3

MARmodel3<-function() 
{ 
  for (i in 1:N1)
  { y[i,1]~dnorm(mu1[trt[i]],tau1)
    logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1))-0.5*tau1*pow((y[i,1]-mu1[trt[i]]),2)
    
  } 
  for (i in (N1+1):N2)
{ y[i,1]~dnorm(mu1[trt[i]],tau1)
  mu22[i]<-beta0[trt[i]]+phi21[trt[i]]*y[i,1]
  y[i,2]~dnorm(mu22[i],tau2[trt[i]])
  logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1))-0.5*tau1*pow((y[i,1]-mu1[trt[i]]),2)+
    log(1/sqrt(2*3.14)*sqrt(tau2[trt[i]]))-0.5*tau2[trt[i]]*pow((y[i,2]-mu22[i]),2)
  
}  
  for (i in (N2+1):N)
{ y[i,1]~dnorm(mu1[trt[i]],tau1)
  mu22[i]<-beta0[trt[i]]+phi21[trt[i]]*y[i,1]
  mu33[i]<-beta1[trt[i]]+phi31*y[i,1]+phi32*y[i,2]
  y[i,2]~dnorm(mu22[i],tau2[trt[i]])
  y[i,3]~dnorm(mu33[i],tau3[trt[i]])
  logli[i]<-log(1/sqrt(2*3.14)*sqrt(tau1))-0.5*tau1*pow((y[i,1]-mu1[trt[i]]),2)+
    log(1/sqrt(2*3.14)*sqrt(tau2[trt[i]]))-0.5*tau2[trt[i]]*pow((y[i,2]-mu22[i]),2)+
    log(1/sqrt(2*3.14)*sqrt(tau3[trt[i]]))-0.5*tau3[trt[i]]*pow((y[i,3]-mu33[i]),2)
  
} 
phi31~dnorm(0, 0.000001)
phi32~dnorm(0, 0.000001)
sigma1~dunif(0,100)
sigma2[1]~dunif(0,100)
sigma3[1]~dunif(0,100)
sigma2[2]~dunif(0,100)
sigma3[2]~dunif(0,100)
tau1<-1/pow(sigma1,2)
tau2[1]<-1/pow(sigma2[1],2)
tau3[1]<-1/pow(sigma3[1],2)
tau2[2]<-1/pow(sigma2[2],2)
tau3[2]<-1/pow(sigma3[2],2)
tau2[3]<-tau2[2]
tau2[4]<-tau2[2]
tau3[3]<-tau3[2]
tau3[4]<-tau3[2]
phi21[1]~dnorm(0, 0.000001)
phi21[4]~dnorm(0, 0.000001)
phi21[2]<-phi21[1]
phi21[3]<-phi21[1]

for (k in 1:4)
{     
  mu1[k]~dnorm(0, 0.000001)
  beta0[k]~dnorm(0, 0.000001)
  beta1[k]~dnorm(0, 0.000001)

  mu2[k]<-beta0[k]+phi21[k]*mu1[k]
  mu3[k]<-beta1[k]+phi31*mu1[k]+phi32*mu2[k]
}
for (k in 2:4)
{
  diff[k-1]<-mu3[k]-mu3[1] 
  diff1[k-1]<-step(diff[k-1])
}
for (k in 3:4)
{
  diff[k+1]<-mu3[k]-mu3[2] 
  diff1[k+1]<-step(diff[k+1])
}
diff[6]<-mu3[4]-mu3[3] 
diff1[6]<-step(diff[6])

}  
write.model(MARmodel3, "D:/dandan/short course/examples/GH/model3/MARmod3.txt")

# --------------------------------------------------
# 5. Specify the parameters to update.

param<-c('mu1','mu2','mu3','diff','diff1',
         'phi21','phi31','phi32','beta0','beta1',
         'sigma1','sigma2','sigma3','logli')					# A character vector with the names of the parameters to be updated



# --------------------------------------------------
# 8. Run the model

GHLM1<-bugs(  data  = dat,
            inits = NULL,
             parameters = param,
             model.file = "MARmod1.txt",
             n.chains = 3,
             n.iter = 15000,
             n.burnin = 5000,
             n.thin=1,
             bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
             working.directory = "D:/dandan/short course/examples/GH/model1",
             clearWD = FALSE,
             debug=TRUE,
            
)


GHLM2<-bugs(  data  = dat,
              inits = NULL,
              parameters = param,
              model.file = "MARmod2.txt",
              n.chains = 2,
              n.iter = 15000,
              n.burnin = 5000,
              n.thin=1,
              bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
              working.directory = "D:/dandan/short course/examples/GH/model2",
              clearWD = FALSE,
              debug=TRUE
)

GHLM3<-bugs(  data  = dat,
              inits = NULL,
              parameters = param,
              model.file = "MARmod3.txt",
              n.chains = 2,
              n.iter = 15000,
              n.burnin = 5000,
              n.thin=1,
              bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
              working.directory = "D:/dandan/short course/examples/GH/model3",
              clearWD = FALSE,
              debug=TRUE
)


# 9. Print the results

print(GHLM1)
print(GHLM2)
print(GHLM3)

# 10. Calculate LPML
LPML1<-sum(-log(colMeans(1/exp(GHLM1$sims.list$logli))))
LPML2<-sum(-log(colMeans(1/exp(GHLM2$sims.list$logli))))
LPML3<-sum(-log(colMeans(1/exp(GHLM3$sims.list$logli))))
