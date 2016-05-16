
library(R2WinBUGS)


#number of subjects on ET
N1 <- 149
#total number of subjects
 N <- 298

#read in data
data.o <- read.table("D://dandan//short course//examples//oasis-data.dir//oasis.data.txt")

#prepare response and missing data indicator matrix
Y <- as.matrix(data.o[,1:4])
r <- as.matrix(data.o[,5:8])


data<-list("Y","N1","N")
#MAR - list of parameters under ignorability
parameters<-c("betae","betas","thetae","thetas","gammae","gammas","mpie","mpis","OR","diff","diff1")


# MAR
fit<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/mar.txt",
              n.chains = 2, n.iter = 25000, n.burnin = 5000,n.thin=1, 
              bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
              working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
              clearWD = FALSE,DIC=FALSE,debug=TRUE)

print(fit)