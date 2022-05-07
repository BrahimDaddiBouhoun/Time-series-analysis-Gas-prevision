library(readxl)
require(stats)
Classeur1 = read_excel("Classeur1.xlsx", 
                       sheet = "for R")

df = data.frame(Classeur1)
head(df)

Classeur2 = read_excel("Classeur1.xlsx")

df2 = data.frame(Classeur2)
head(df)


# preparation des series
xt=ts(df["AIN.BENIAN"],start = c(2000,1),frequency =12)
yt=ts(df["AIN.BESSAM"],start = c(2000,1),frequency =12)
zt=ts(df["BOUIRA"],start = c(2000,1),frequency =12)
wt=ts(df["GDYEL"],start = c(2000,1),frequency =12)


# get the best ARMA order for sonelgaz series

require('forecast')
order=c()
for (i in 2:250) {
  order = c(order,auto.arima(diff(ts(df[i],12)),max.q = 0)$arma[1])
}

for (i in 1:249) {
  if (order[i]==0) {
    order[i]=1
  }
}

# p values matrix
m_test_sonelgaz=matrix(0,nrow = 249,ncol = 249)

for (i in 1:248) {
  for (j in (i+1):249) {
    m_test_sonelgaz[j,i]=test(diff(ts(df[i+1]),12),diff(ts(df[j+1]),12),order[i],order[j],0.01)[2]
    m_test_sonelgaz[i,j]=m_test_sonelgaz[j,i]
  }
}

# clustring
sonel0.05=cluster(248,m_test_sonelgaz,0.05)
sonel0.01=cluster(248,m_test_sonelgaz,0.01)

# sort groupe member
for (i in 1:ncol(sonel0.05)) {
  sonel0.05[,i]=sort(sonel0.05[,i])
}

sonel0.01=sonel0.01[193:249,]
sonel0.05=sonel0.05[227:249,]

rownames(m_test_sonelgaz)=colnames(df)[2:250]
colnames(m_test_sonelgaz)=colnames(df)[2:250]

# ===========================
dist1_sonelgaz = as.dist(1-m_test_sonelgaz)
hc_sonelgaz = hclust(dist1_sonelgaz,method = 'complet')
plot(hc_sonelgaz)

# setting the number of  clusters
cut_sonelgaz=cutree(hc_sonelgaz,k = 14 )
rect.hclust(hc_sonelgaz , k = 14, border = 2:6)

# coloring the clusters
require('dendextend')
dend_obj_sonelgaz = as.dendrogram(hc_sonelgaz)
col_dend_sonelgaz = color_branches(dend_obj_sonelgaz,k=14,col = c(1,2,3,4,8,7,6,5,1,2,3,4,8,7))
plot(col_dend_sonelgaz)

# counting the groupes
require("dplyr")
df_cl_sonelgaz = mutate(df2, cluster = cut_sonelgaz)
count(df_cl_sonelgaz,cluster)

