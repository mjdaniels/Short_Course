
# 1. Load R2WinBugs library

	library(R2WinBUGS)		


# --------------------------------------------------
# 2. Read observed data
	setwd("D:\\dandan\\short course\\examples\\GH")

	GH  <- read.table('GHRbugs.txt', header=T, sep='\t')	# data frame containing the observed GH data
	N   <- nrow(GH)							# N: total number of observations
	y   <- as.matrix(GH[,1:3])					# N by 3 matrix each comlumn of which corresponds to one
										# of the three observation times
	trt <- as.numeric(GH[,4])					# numeric vector indicating the assigned treatment to each subject
	a   <- matrix(1,2,3) 						 
	N1  <- 19 								# total number of subjects with only one observation (obs: time_1)
	N2  <- 25 								# total number of subjects with only one or two observations (obs: time_1 &/or time_2)


# --------------------------------------------------
# 3. Specify the parameters of the model

	dat <- list("y","trt","a","N1","N2","N")			# A character vector with the names of the parameters included in the model
										# (Information to be pased to WinBUGS package)


# --------------------------------------------------
# 4. Specify the MNAR model	


	MNARmodel <- function() 
{ 
      ####  Model for observable responses  ####
	  for (i in 1:N1)   # pattern_1, one observation      
	   {
	    y[i,1] ~ dnorm(mu1[trt[i]],tau1[trt[i]])  
          u[i] <- 1    
	   }
        for (i in (N1+1):N2)  # pattern_2, two observations
         { 
	    y[i,1] ~ dnorm(mu2[trt[i]],tau2[trt[i]]) 
          y[i,2] ~ dnorm(mu2_1[i], tau2_1[trt[i]])   
          mu2_1[i] <- beta0[trt[i]]+beta1[trt[i]]*y[i,1]            
   	    u[i] <- 2 
         }
        for (i in (N2+1):N)  # pattern_3 (completers), three observations 
	   {
          y[i,1] ~ dnorm( mu3[trt[i]], tau3[trt[i]] )
          y[i,2] ~ dnorm( mu2_1[i],    tau2_1[trt[i]] ) 
          mu2_1[i] <- beta0[trt[i]] + beta1[trt[i]]*y[i,1]    
          y[i,3] ~ dnorm( mu3_2_1[i],  tau3_2_1[trt[i]] )    
          mu3_2_1[i] <- beta2[trt[i]] + beta3[trt[i]] * y[i,1] + beta4[trt[i]] * y[i,2] 
          u[i] <- 3                               
         }

      ####### Dropout probabilities #######
	  for (i in 1:N) 
	   { u[i] ~ dcat(alpha[trt[i],1:3]) } 

      ####### priors #######
	alpha[1,1:3] ~ ddirch(a[1,1:3])	
      alpha[2,1:3] ~ ddirch(a[2,1:3])  	
        for (k in 1:2)   
	   {
            mu1[k] ~ dnorm(0, 0.000001)
            mu2[k] ~ dnorm(0, 0.000001)
            mu3[k] ~ dnorm(0, 0.000001)
            beta0[k] ~ dnorm(0, 0.0001)
            beta1[k] ~ dnorm(0, 0.0001)
            beta2[k] ~ dnorm(0, 0.0001)
            beta3[k] ~ dnorm(0, 0.0001)
            beta4[k] ~ dnorm(0, 0.0001) 
            tau1[k] <- pow(sigma1[k],-2)
            tau2[k] <- pow(sigma2[k],-2)
            tau3[k] <- pow(sigma3[k],-2)
            tau2_1[k] <- pow(sigma2_1[k],-2)
            tau3_2_1[k] <- pow(sigma3_2_1[k],-2)
		sigma1[k] ~ dunif(0,100)
		sigma2[k] ~ dunif(0,100)
		sigma3[k] ~ dunif(0,50)
		sigma2_1[k] ~ dunif(0,50)
		sigma3_2_1[k] ~ dunif(0,50)
         } 

	### --------------------------------- ###
      ### Priors for Sensitivity Parameters ###
	### --------------------------------- ###
	

	      for (k in 1:2)
		 {  
	      	 delta0[k] ~ dunif(- 20,0)
	      	 delta1[k] ~ dunif(-15,0)  

			 beta0ss[k] <- beta0[k] + delta0[k]  # delta0: sensitivity parameter on intercept y2|y1,    U=1    
			 beta2ss[k] <- beta2[k] + delta1[k]  # delta1: sensitivity parameter on intercept y3|y1,y2, U=1,2
			 mean1[k]   <- alpha[k,1] * mu1[k] + alpha[k,2] * mu2[k] + alpha[k,3] * mu3[k]
			 mean2[k]   <- alpha[k,1] * (beta0ss[k] + beta1[k] * mu1[k]) + 
					   alpha[k,2] * (beta0[k]   + beta1[k] * mu2[k]) +
				 	   alpha[k,3] * (beta0[k]   + beta1[k] * mu3[k])
			 mean3[k]   <- alpha[k,1] * (beta2ss[k] + beta3[k] * mu1[k] + beta4[k] * (beta0ss[k] + beta1[k] * mu1[k]))+
					   alpha[k,2] * (beta2ss[k] + beta3[k] * mu2[k] + beta4[k] * (beta0[k]   + beta1[k] * mu2[k]))+
					   alpha[k,3] * (beta2[k]   + beta3[k] * mu3[k] + beta4[k] * (beta0[k]   + beta1[k] * mu3[k]))
		 
	      
	    ## E(y_3) for non-future dependence

       for (k1 in 1:10000) {
         yy1[k1,k] ~ dnorm(mu1[k],tau1[k])
         num2[k1,k]<-1/sigma2[k]*exp(-pow(yy1[k1,k]-mu2[k],2)*tau2[k]/2)
         num3[k1,k]<-1/sigma3[k]*exp(-pow(yy1[k1,k]-mu3[k],2)*tau3[k]/2)
         prob11[k1,k]<-alpha[k,2] *num2[k1,k]/(alpha[k,2] *num2[k1,k]+alpha[k,3] *num3[k1,k])
       }
       prob1[k]<-mean(prob11[,k])
       
	     mean32[k]<-alpha[k,1] * (prob1[k]*beta2ss[k]+(1-prob1[k])*beta2[k] + beta3[k] * mu1[k] + beta4[k] * (beta0ss[k] + beta1[k] * mu1[k]))+
			   alpha[k,2] * (beta2ss[k] + beta3[k] * mu2[k] + beta4[k] * (beta0[k]   + beta1[k] * mu2[k]))+
			   alpha[k,3] * (beta2[k]   + beta3[k] * mu3[k] + beta4[k] * (beta0[k]   + beta1[k] * mu3[k]))
			 
	      }




	####### treatment effect at time 3 (week 12) #######
diff <- mean3[2] - mean3[1]     
diff1<-mean32[2] - mean32[1]

prob0 <- step(diff - 0.0000000001)	
prob1 <- step(diff1 - 0.0000000001)  
      
}  

write.model(MNARmodel, "D:\\dandan\\short course\\examples\\GH\\mnar\\MNARmod.txt")			# Writes the compiled "MNARmodel" WinBUGS model 
	

MNARmodel1 <- function() 
{ 
  ####  Model for observable responses  ####
  for (i in 1:N1)   # pattern_1, one observation      
  {
    y[i,1] ~ dnorm(mu1[trt[i]],tau1[trt[i]])  
    u[i] <- 1    
  }
  for (i in (N1+1):N2)  # pattern_2, two observations
  { 
    y[i,1] ~ dnorm(mu2[trt[i]],tau2[trt[i]]) 
    y[i,2] ~ dnorm(mu2_1[i], tau2_1[trt[i]])   
    mu2_1[i] <- beta0[trt[i]]+beta1[trt[i]]*y[i,1]            
    u[i] <- 2 
  }
  for (i in (N2+1):N)  # pattern_3 (completers), three observations 
  {
    y[i,1] ~ dnorm( mu3[trt[i]], tau3[trt[i]] )
    y[i,2] ~ dnorm( mu2_1[i],    tau2_1[trt[i]] ) 
    mu2_1[i] <- beta0[trt[i]] + beta1[trt[i]]*y[i,1]    
    y[i,3] ~ dnorm( mu3_2_1[i],  tau3_2_1[trt[i]] )    
    mu3_2_1[i] <- beta2[trt[i]] + beta3[trt[i]] * y[i,1] + beta4[trt[i]] * y[i,2] 
    u[i] <- 3                               
  }
  
  ####### Dropout probabilities #######
  for (i in 1:N) 
  { u[i] ~ dcat(alpha[trt[i],1:3]) } 
  
  ####### priors #######
  alpha[1,1:3] ~ ddirch(a[1,1:3])	
  alpha[2,1:3] ~ ddirch(a[2,1:3])  	
  for (k in 1:2)   
  {
    mu1[k] ~ dnorm(0, 0.000001)
    mu2[k] ~ dnorm(0, 0.000001)
    mu3[k] ~ dnorm(0, 0.000001)
    beta0[k] ~ dnorm(0, 0.0001)
    beta1[k] ~ dnorm(0, 0.0001)
    beta2[k] ~ dnorm(0, 0.0001)
    beta3[k] ~ dnorm(0, 0.0001)
    beta4[k] ~ dnorm(0, 0.0001) 
    tau1[k] <- pow(sigma1[k],-2)
    tau2[k] <- pow(sigma2[k],-2)
    tau3[k] <- pow(sigma3[k],-2)
    tau2_1[k] <- pow(sigma2_1[k],-2)
    tau3_2_1[k] <- pow(sigma3_2_1[k],-2)
    sigma1[k] ~ dunif(0,100)
    sigma2[k] ~ dunif(0,100)
    sigma3[k] ~ dunif(0,50)
    sigma2_1[k] ~ dunif(0,50)
    sigma3_2_1[k] ~ dunif(0,50)
  } 

  for (k in 1:2)
  {  
    
    
    beta0ss[k] <- beta0[k] + delta0[k]  # delta0: sensitivity parameter on intercept y2|y1,    U=1    
    beta2ss[k] <- beta2[k] + delta1[k]  # delta1: sensitivity parameter on intercept y3|y1,y2, U=1,2
    mean1[k]   <- alpha[k,1] * mu1[k] + alpha[k,2] * mu2[k] + alpha[k,3] * mu3[k]
    mean2[k]   <- alpha[k,1] * (beta0ss[k] + beta1[k] * mu1[k]) + 
      alpha[k,2] * (beta0[k]   + beta1[k] * mu2[k]) +
      alpha[k,3] * (beta0[k]   + beta1[k] * mu3[k])
    mean3[k]   <- alpha[k,1] * (beta2ss[k] + beta3[k] * mu1[k] + beta4[k] * (beta0ss[k] + beta1[k] * mu1[k]))+
      alpha[k,2] * (beta2ss[k] + beta3[k] * mu2[k] + beta4[k] * (beta0[k]   + beta1[k] * mu2[k]))+
      alpha[k,3] * (beta2[k]   + beta3[k] * mu3[k] + beta4[k] * (beta0[k]   + beta1[k] * mu3[k]))
    
    
    ## E(y_3) for non-future dependence
    
    for (k1 in 1:10000) {
      yy1[k1,k] ~ dnorm(mu1[k],tau1[k])
      num2[k1,k]<-1/sigma2[k]*exp(-pow(yy1[k1,k]-mu2[k],2)*tau2[k]/2)
      num3[k1,k]<-1/sigma3[k]*exp(-pow(yy1[k1,k]-mu3[k],2)*tau3[k]/2)
      prob11[k1,k]<-alpha[k,2] *num2[k1,k]/(alpha[k,2] *num2[k1,k]+alpha[k,3] *num3[k1,k])
    }
    prob1[k]<-mean(prob11[,k])
    
    mean32[k]<-alpha[k,1] * (prob1[k]*beta2ss[k]+(1-prob1[k])*beta2[k] + beta3[k] * mu1[k] + beta4[k] * (beta0ss[k] + beta1[k] * mu1[k]))+
      alpha[k,2] * (beta2ss[k] + beta3[k] * mu2[k] + beta4[k] * (beta0[k]   + beta1[k] * mu2[k]))+
      alpha[k,3] * (beta2[k]   + beta3[k] * mu3[k] + beta4[k] * (beta0[k]   + beta1[k] * mu3[k]))
    
  }
 
  ####### treatment effect at time 3 (week 12) #######
  diff <- mean3[2] - mean3[1]     
  
  diff1<-mean32[2] - mean32[1]
  
  prob0 <- step(diff - 0.0000000001)
  prob1 <- step(diff1 - 0.0000000001)
  
}  
write.model(MNARmodel1, "D:\\dandan\\short course\\examples\\GH\\mnar\\MNARmod1.txt")			# Writes the compiled "MNARmodel" WinBUGS model 

# --------------------------------------------------
# 5. Specify the parameters to update.

param <- c('mean1','mean2','mean3','diff','prob0','sigma1','sigma2','sigma3','sigma2_1','sigma3_2_1',"prob1","mean32","diff1")		# A character vector with the names of the parameters to be updated





# --------------------------------------------------
# 7. Run the model

### uniform on the sensitivity parameters
GHPMM_MNAR1 <- bugs( data  = dat, 
			   inits = initial_MNAR2,
			   parameters = param,
			   model.file = "MNARmod.txt",
			   n.chains = 2,
		         n.iter = 25000,
			   n.burnin = 5000,
			   n.thin=1,
		         bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
			   working.directory = "D:/dandan/short course/examples/GH/mnar",
            	   clearWD = FALSE,
         debug=TRUE
		       )

### MAR with delta=0
delta0<-c(0,0)
delta1<-c(0,0)
dat1 <- list("y","trt","a","N1","N2","N","delta0","delta1")
GHPMM_MNAR2 <- bugs( data  = dat1, 
                     inits = initial_MNAR3,
                     parameters = param,
                     model.file = "MNARmod1.txt",
                     n.chains = 2,
                     n.iter = 25000,
                     n.burnin = 5000,
                     n.thin=1,
                     bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
                     working.directory = "D:/dandan/short course/examples/GH/mnar",
                     clearWD = FALSE,
                     debug=TRUE
)


# --------------------------------------------------
# 8. Print the results

print(GHPMM_MNAR1)	

print(GHPMM_MNAR2)  

