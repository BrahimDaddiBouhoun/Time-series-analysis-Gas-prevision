# power of the test:
require("MASS")
#====================================
gc()
# 0 correlation:
par1=c(0,0.1,0.2,0.3,0.4,0.5)
pcor0=c()
for (j in 1:6) {
  r=c()
  for (i in 1:2000) {
    at = arima.sim(list(),n = 200)
    bt = arima.sim(list(ar=c(par1[j])),n = 200)  
    r=c(r,test(at,bt,1,1,0.05)[1])
  }
  pcor0=c(pcor0,mean(r))
}

#====================================
gc()
#correlation 0.5
pcor0.5=c()
for (j in 1:6) {
  r=c()
  for (i in 1:2000) {
    q12=0.5*(1-par1[j]*par1[j])/sqrt((1-par1[j]^2)*(1-par1[j]^2))
    f=mvrnorm(200,mu=c(0,0),Sigma=cbind(c(1,q12),c(q12,1)))
    at = arima.sim(list(),n = 200,innov =f[,1])
    bt = arima.sim(list(ar=par1[j]),n = 200,innov =f[,2])  
    r=c(r,test(at,bt,1,1,0.05)[1])
  }
  pcor0.5=c(pcor0.5,mean(r))
}

#====================================
gc()
#correlation 0.9
pcor0.9=c()
for (j in 1:6) {
  r=c()
  for (i in 1:2000) {
    q12=0.9*(1-par1[j]*par1[j])/sqrt((1-par1[j]^2)*(1-par1[j]^2))
    f=mvrnorm(200,mu=c(0,0),Sigma=cbind(c(1,q12),c(q12,1)))
    at = arima.sim(list(),n = 200,innov =f[,1])
    bt = arima.sim(list(ar=par1[j]),n = 200,innov =f[,2])  
    r=c(r,test(at,bt,1,1,0.05)[1])
  }
  pcor0.9=c(pcor0.9,mean(r))
}

#====================================

power= data.frame("cor 0"=c("======",pcor0),
                    "cor 5"=c("======",pcor0.5),
                    "cor 9"=c("======",pcor0.9),
                    
                    row.names =c(" ==========  ",
                                 "     Q1=0    ",
                                 "    Q1=0.1   ",
                                 "    Q1=0.2   ",
                                 "    Q1=0.3   ",
                                 "    Q1=0.4   ",
                                 "    Q1=0.5   "))
power