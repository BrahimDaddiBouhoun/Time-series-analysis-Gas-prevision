del_zeros = function(x){
  n=length(x)
  r=c()
  for (i in 1:n) {
    if (x[i]!= 0) {
      r=c(r,x[i])
    }
  }
  return(r)
}
