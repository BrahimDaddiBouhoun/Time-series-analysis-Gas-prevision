# overall estimates
require("MASS")
#====================================
gc()
# 0 correlation:
par=c(0,0.1,0.5,0.9)
cor0=c()
for (j in 1:4) {
  r=c()
  for (i in 1:2000) {
    at = arima.sim(list(ar=c(par[j])),n = 200)
    bt = arima.sim(list(ar=c(par[j])),n = 200)  
    r=c(r,test(at,bt,1,1,0.05)[1])
  }
  cor0=c(cor0,mean(r))
}

#===================================
gc()
#correlation 0.5
par=c(0,0.1,0.5,0.9)
cor0.5=c()
for (j in 1:4) {
  r=c()
  for (i in 1:2000) {
    q12=0.5*(1-par[j]*par[j])/sqrt((1-par[j]^2)*(1-par[j]^2))
    f=mvrnorm(200,mu=c(0,0),Sigma=cbind(c(1,q12),c(q12,1)))
    at = arima.sim(list(ar=par[j]),n = 200,innov =f[,1])
    bt = arima.sim(list(ar=par[j]),n = 200,innov =f[,2])  
    r=c(r,test(at,bt,1,1,0.05)[1])
  }
  cor0.5=c(cor0.5,mean(r))
}

#===================================
gc()
#correlation 0.9
par=c(0,0.1,0.5,0.9)
cor0.9=c()
for (j in 1:4) {
  r=c()
  for (i in 1:2000) {
    q12=0.9*(1-par[j]*par[j])/sqrt((1-par[j]^2)*(1-par[j]^2))
    f=mvrnorm(200,mu=c(0,0),Sigma=cbind(c(1,q12),c(q12,1)))
    at = arima.sim(list(ar=par[j]),n = 200,innov =f[,1])
    bt = arima.sim(list(ar=par[j]),n = 200,innov =f[,2])  
    r=c(r,test(at,bt,1,1,0.05)[1])
  }
  cor0.9=c(cor0.9,mean(r))
}


#=========AR2====================
gc()
# AR2 cor 0:
ar2cor0=c()
r=c()
for (i in 1:2000) {
  at = arima.sim(list(ar=c(0.6,0.2)),n = 200)
  bt = arima.sim(list(ar=c(0.6,0.2)),n = 200)  
  r=c(r,test(at,bt,2,2,0.05)[1])
}
ar2cor0=c(mean(r))  


#================================
gc()
# AR2 correlation 0.5
ar2cor0.5=c()
r=c()
for (i in 1:2000) {
  f=mvrnorm(200,mu=c(0,0),Sigma=cbind(c(1,0.5),c(0.5,1)))
  at = arima.sim(list(ar=c(0.6,0.2)),n = 200,innov = f[,1])
  bt = arima.sim(list(ar=c(0.6,0.2)),n = 200,innov = f[,2]) 
  r=c(r,test(at,bt,2,2,0.05)[1])
}
ar2cor0.5=c(mean(r))


#===============================
gc()
# AR2 correlation 0.9
ar2cor0.9=c()
r=c()
for (i in 1:2000) {
  f=mvrnorm(200,mu=c(0,0),Sigma=cbind(c(1,0.9),c(0.9,1)))
  at = arima.sim(list(ar=c(0.6,0.2)),n = 200,innov = f[,1])
  bt = arima.sim(list(ar=c(0.6,0.2)),n = 200,innov = f[,2])  
  r=c(r,test(at,bt,2,2,0.05)[1])
}
ar2cor0.9=c(mean(r))



overall= data.frame("cor 0"=c("======",cor0,"======",ar2cor0),
                    "cor 5"=c("======",cor0.5,"======",ar2cor0.5),
                    "cor 9"=c("======",cor0.9,"======",ar2cor0.9),
                    
                    row.names =c(" ==========  ",
                                 "     Q1=0    ",
                                 "    Q1=0.1   ",
                                 "    Q1=0.5   ",
                                 "    Q1=0.9   ",
                                 " =========== ",
                                 "Q1=0.6 Q2=0.2"))
overall
