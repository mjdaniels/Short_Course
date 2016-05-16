

library(R2WinBUGS)

#number of subjects on ET
N1 <- 149
#total number of subjects
N <- 298
P <- c(0.25,0.25,0.25,0.25)

#read in data
data.o <- read.table("D://dandan//short course//examples//oasis-data.dir//oasis.data.txt")

#prepare response and missing data indicator matrix
Y <- as.matrix(data.o[,1:4])
r <- as.matrix(data.o[,5:8])

Ye<-Y[1:N1,]
Ys<-Y[(N1+1):N,]
re<-r[1:N1,]
rs<-r[(N1+1):N,]

Y1<-Ye[re[,1]==0&re[,2]==1&re[,3]==1&re[,4]==1,]
r1<-re[re[,1]==0&re[,2]==1&re[,3]==1&re[,4]==1,]
Y2<-Ye[re[,1]==0&re[,2]==0&re[,3]==1&re[,4]==1,]
r2<-re[re[,1]==0&re[,2]==0&re[,3]==1&re[,4]==1,]
Y3<-Ye[re[,1]==0&re[,2]==0&re[,3]==0&re[,4]==1,]
r3<-re[re[,1]==0&re[,2]==0&re[,3]==0&re[,4]==1,]
Y4<-Ye[re[,1]==0&re[,2]==0&re[,3]==0&re[,4]==0,]
r4<-re[re[,1]==0&re[,2]==0&re[,3]==0&re[,4]==0,]

Y5<-Ys[rs[,1]==0&rs[,2]==1&rs[,3]==1&rs[,4]==1,]
r5<-rs[rs[,1]==0&rs[,2]==1&rs[,3]==1&rs[,4]==1,]
Y6<-Ys[rs[,1]==0&rs[,2]==0&rs[,3]==1&rs[,4]==1,]
r6<-rs[rs[,1]==0&rs[,2]==0&rs[,3]==1&rs[,4]==1,]
Y7<-Ys[rs[,1]==0&rs[,2]==0&rs[,3]==0&rs[,4]==1,]
r7<-rs[rs[,1]==0&rs[,2]==0&rs[,3]==0&rs[,4]==1,]
Y8<-Ys[rs[,1]==0&rs[,2]==0&rs[,3]==0&rs[,4]==0,]
r8<-rs[rs[,1]==0&rs[,2]==0&rs[,3]==0&rs[,4]==0,]

N1<-nrow(r1)
N2<-nrow(r2)+N1
N3<-nrow(r3)+N2
N4<-nrow(r4)+N3
N5<-nrow(r5)+N4
N6<-nrow(r6)+N5
N7<-nrow(r7)+N6
N8<-nrow(r8)+N7

Y<-rbind(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
r<-rbind(r1,r2,r3,r4,r5,r6,r7,r8)
a<-matrix(1,2,4)
p<-c(0.25,0.25,0.25,0.25)

data<-list("Y","N1","N2","N3","N4","N5","N6","N7","N8","a","p")


parameters<-c("mpie","mpis","OR","diff","diff1","mpie1","mpis1","mpie2","mpis2",
              "mpie3","mpis3","mpie4","mpis4","gammae1","gammae2","gammae3","gammae4",
              "thetae1","thetae2","thetae3","thetae4")

#parameters<-c("mpie","mpis","OR","diff","diff1")

## prior for sensitivity parameters - expert 1
MNAR1<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/mnar1.txt",
              n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
              bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
              working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
              clearWD = FALSE,DIC=FALSE,debug=TRUE)
## prior for sensitivity parameters - expert 2
MNAR2<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/mnar2.txt",
            n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
            bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
            working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
            clearWD = FALSE,DIC=FALSE,debug=TRUE)
## prior for sensitivity parameters - expert 3
MNAR3<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/mnar3.txt",
            n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
            bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
            working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
            clearWD = FALSE,DIC=FALSE,debug=TRUE)
## prior for sensitivity parameters - expert 4
MNAR4<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/mnar4.txt",
            n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
            bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
            working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
            clearWD = FALSE,DIC=FALSE,debug=TRUE)
## prior for sensitivity parameters - combined
MNAR5<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/mnar5.txt",
            n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
            bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
            working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
            clearWD = FALSE,DIC=FALSE,debug=TRUE)

## prior for sensitivity parameters - expert 1
NFD1<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/nfd1.txt",
                n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
                bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
                working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
                clearWD = FALSE,DIC=FALSE,debug=TRUE)
## prior for sensitivity parameters - expert 2
NFD2<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/nfd2.txt",
           n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
           bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
           working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
           clearWD = FALSE,DIC=FALSE,debug=TRUE)
## prior for sensitivity parameters - expert 3
NFD3<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/nfd3.txt",
           n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
           bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
           working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
           clearWD = FALSE,DIC=FALSE,debug=TRUE)
## prior for sensitivity parameters - expert 4
NFD4<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/nfd4.txt",
           n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
           bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
           working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
           clearWD = FALSE,DIC=FALSE,debug=TRUE)
## prior for sensitivity parameters - combined
NFD5<-bugs(data, inits=NULL, parameters, model.file = "D:/dandan/short course/examples/oasis-data.dir/nfd5.txt",
           n.chains = 2, n.iter = 10000, n.burnin = 5000,n.thin=1, 
           bugs.directory = "D:/Downloads/winbugs14/WinBUGS14/",
           working.directory = "D:/dandan/short course/examples/oasis-data.dir/", 
           clearWD = FALSE,DIC=FALSE,debug=TRUE)




