A1=function(lambda,p,n,k)
{
  w=seq(0,n,1) # numerele dintre 0 si n cu pas 1
  po = dpois(0:n, lambda)
  geo = dgeom(0:n, p)
  b=dbinom(w,n,p)
  hist(po[k:n], col='red')
  hist(geo[k:n],col='blue', add=TRUE)
  hist(b[0:n],col='green', add=TRUE)
}
A1(4,0.25,5,4)
A1(3,0.2,5,3)
###########
A2_a = function(x) {
  x=scan("fisier.txt")
  w = vector() # facem un vector
  w[1] = median(x) 
  w[2] = mean(x) # media arit vect
  w[3] = sd(x)# deviatia
  w[4] = as.vector(quantile(x))[1 + 1]
  w[5] = as.vector(quantile(x))[1 + 2]
  w[6] = as.vector(quantile(x))[1 + 3]
  return (w)
}
A2_a(x)
#########
A2_b = function(x) {
  x=scan("fisier.txt")
  n = length(x)
  medie_aritmetica = sum(x) / n
  s = sd(x) 
  st = medie_aritmetica - s * 2
  dr = medie_aritmetica + s * 2
  v = vector()
  t = 0
  for(i in 1:n) {
    if(x[i] > st || x[i] <dr) {
      t = t + 1
      v[t] = x[i]
    }
  }
  return(v)
}
A2_b(x)
#########
A2_c = function(x) {
  x=scan("fisier.txt")
  y = A2_b(x) 
  interval = seq(0, 100, 10)
  hist(y, breaks = interval,freq = T ,right = F)
  
}
A2_c(x)
##########
