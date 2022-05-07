# not correlated series:
series=matrix(0,nrow = 40,ncol = 200)
for (i in 1:10) {
    gc()
    series[i,]=arima.sim(list(),n = 200)
    series[10+i,]=arima.sim(list(ar=c(0.5)),n = 200)
    series[20+i,]=arima.sim(list(ma=c(0.9)),n = 200)
    series[30+i,]=arima.sim(list(ar=c(-0.6),ma=c(0.3)),n = 200)
  }

# ===========================
m_test=matrix(0,nrow = 40,ncol = 40)

for (i in 1:39) {
  for (j in (i+1):40) {
    m_test[j,i]=1-test(series[i,],series[j,],1,1,0.01)[2]
  }
}

# ===========================
dist1 = as.dist(m_test)
hc = hclust(dist1)
plot(hc)

# setting the number of  clusters
cut=cutree(hc,h=0.99 )
rect.hclust(hc , k = 4, border = 2:6)

# coloring the clusters
require('dendextend')
dend_obj = as.dendrogram(hc)
col_dend = color_branches(dend_obj,k=4,col = c(1,2,3,4))
plot(col_dend)

# correlated series:
require("MASS")

series_cor=matrix(0,nrow = 40,ncol = 200)
f=mvrnorm(200,mu=diag(diag(0,40)),Sigma=(diag(0.5,40)+0.5))
for (i in 0:9) {
  series_cor[1+(i*4),]=arima.sim(list(),n = 200,innov =f[,1+(i*4)])
  series_cor[2+(i*4),]=arima.sim(list(ar=c(0.5)),n = 200,innov =f[,2+(i*4)])
  series_cor[3+(i*4),]=arima.sim(list(ma=c(0.9)),n = 200,innov =f[,3+(i*4)])
  series_cor[4+(i*4),]=arima.sim(list(ar=c(-0.6),ma=c(0.3)),n = 200,innov =f[,4+(i*4)])
}

# ===========================
m_test=matrix(0,nrow = 40,ncol = 40)

for (i in 1:39) {
  for (j in (i+1):40) {
    m_test[j,i]=1-test(series_cor[i,],series_cor[j,],1,1,0.01)[2]
  }
}

# ===========================
dist1 = as.dist(m_test)
hc = hclust(dist1)
plot(hc)

# sonelgaz



