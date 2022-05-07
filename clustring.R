cluster = function(a,pmat,alpha){
  idmat=diag(1,a+1)
  pm=max(pmat)
  z=idmat
  pr=((a+1)*a)/2
  aa=a+1
  aaa=aa
  
  if (pm<=alpha) {
    k=pr+1
  }
  
  k=1
  while (k<=pr) {
    i=1
    while(i<=aaa){
      j=1
      while (j<=aaa) {
        if (pm==pmat[i,j]) {
          w1=i
          w2=j
          i=aaa
          j=aaa
        }
        j=j+1
      }
      i=i+1
    }
    for (i in 1:aa) {
      if (z[i,w2]==1) {
        jr=i
      }
      if (z[i,w1]==1) {
        ir=i
      }
    } 
    if (ir!=jr) {
      i=1
      while (i<=aaa) {
        j=1
        while (j<=aaa) {
          if (z[ir,i]==1 & z[jr,j]==1 ) {
            if (pmat[i,j]<=alpha) {
              ir=1
              jr=1
            }
          }
          j=j+1
        }
        i=i+1
      }
    }
    if (ir!=jr) {
      z[jr,]=z[jr,]+z[ir,]
      z1=numeric(aaa)
      z2=numeric(aaa)
      if (ir==1) {
        z=z[2:aa,]
        }
      else if (ir==aa) {
        z=z[1:(aa-1),]
      }
      else {
        for (r in 1:(ir-1)) {
          z1=rbind(z1,z[r,])
        }
        rr=ir+1
        for (r in rr:aa) {
          z2=rbind(z2,z[r,])
        }
        lz1=length(z1[,1]);
        lz2=length(z2[,1]);
        z1=z1[2:lz1,];
        z2=z2[2:lz2,];
        z=rbind(z1,z2);
      }
      aa=aa-1
    }
    pmat[w1,w2]=0
    pm=max(pmat)
    if (pm<=alpha) {
      k=pr
    }
    k=k+1
  }
  
  for (i in 1:aa) {
    for (j in 1:aaa) {
      if (z[i,j]==1) {
        z[i,j]=j
      }
    }
  }
  z=t(z)
  return(z)
}

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
    m_test[j,i]=test(series[i,],series[j,],1,1,0.01)[2]
    m_test[i,j]=m_test[j,i]
  }
}

cluster(39,m_test,0.01)
