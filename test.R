# fonction du test
test = function(X,Y,k1,k2,alpha){
  k=max(k1,k2)
  T = length(X)
  
  # la matrice W
  w=matrix(0,nrow=2*(T-k),ncol=2*k)
  
  # Wx
  for(i in 1:(T-k)){
    j=k
    d=i
    while(j>0){
      w[i,j]=X[d]
      j=j-1
      d=d+1
    }
    
  }
  
  #Wy
  for( i in (T-k+1):(2*(T-k)) ){
    j=2*k
    d=i-(T-k)
    while(j>k){
      w[i,j]=Y[d]
      j=j-1
      d=d+1
    }
  }
  
  # le vecteur Z:
  z=c(X[(k+1):T],Y[(k+1):T])
  
  # estimer les paramatres ols 
  pi=solve(t(w)%*%w)%*%t(w)%*%z
  
  # residuelle ols
  res=z- (w%*%pi)
  resx=res[1:(T-k)]
  resy=res[(T-k+1):(2*(T-k))]
  sigma1=t(resx)%*%resx/ length(resx)
  sigma2=t(resy)%*%resy/ length(resy)
  sigma12=t(resx)%*%resy/ length(resx) 
  
  # la matrice V: ols
  sigma = matrix(c(sigma1,sigma12,sigma12,sigma2),nrow = 2,ncol = 2,byrow = T)
  v=kronecker(sigma,diag(1,T-k))
  
  # l'estimateur phi: fgls
  phi=solve(t(w)%*%solve(v)%*%w)%*%(t(w)%*%solve(v)%*%z)
  
  # residuelle  fgls
  res=z- (w%*%phi)
  resx=res[1:(T-k)]
  resy=res[(T-k+1):(2*(T-k))]
  sigma1=t(resx)%*%resx/ length(resx)
  sigma2=t(resy)%*%resy/ length(resy)
  sigma12=t(resx)%*%resy/ length(resx)
  
  # la matrice V: fgls
  sigma = matrix(c(sigma1,sigma12,sigma12,sigma2),nrow = 2,ncol = 2,byrow = T)
  v=kronecker(sigma,diag(1,T-k))
  
  # la statistique du test: 
  R=cbind(diag(1,k),diag(-1,k))
  
  D=t(R%*%phi)%*%solve(R%*%solve(t(w)%*%solve(v)%*%w)%*%t(R))%*%(R%*%phi)
  
  # P-value
  # p= 2*min(1-pchisq(D,k),pchisq(D,k))
  p= 1-pchisq(D,k)
  
  # resultas du test avec la p-value:
  if (p<alpha) {
  
    return(c(1,p,D))
  }
  else {
    return(c(0,p,D))
  }
  
  # resultat du test avec la valeur tabulee
  #g= qchisq(alpha/2,k)
  #d= qchisq(1-alpha/2,k)
  #if (D<g || D>d) {
  #  return(1)
  #}
  #else {
  #  return(0)
  #}
}