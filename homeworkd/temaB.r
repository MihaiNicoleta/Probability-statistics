B1 = function(a,b,c,h,n) {
  PE = (pi * a * b * (h^2)) / (2 * c)
  count = 0
  for(i in 1:n) {
    x = runif(1, (-a * sqrt(h/c)), (a * sqrt(h/c)))#1-> generam un nr, si capetele intervalului
    y = runif(1, (-b * sqrt(h/c)), (b * sqrt(h/c)))
    z = runif(1, 0, h)

    if(((x^2)/(a^2) + (y^2)/(b^2)) <= (z/c)) {
      count = count+1;
    } 
  }
  val = ((4 * (a * (b * (h^2)))) *count) / n
  val_relativa = abs( (PE - val) / val)
  cat("\nAria aproximativa: ", val)
  cat("\nDiferenta: ",abs( PE - val))
  cat("\nEroarea relativa: ", val_relativa)
}
a=4
b=3
c=4
h=4

x1 = B1(a, b, c, h,20000)
x2 = B1(a, b, c, h,50000)
x3 = B1(a, b, c, h,100000)
#########################################

B2 = function(n, a, b, c, d) {
  count = 0
  for(i in 1:n) {
    x = runif(1, a, b)
    y = runif(1, c, d)
    if(x >= 1 & y>=0 & y <= 2 & y <= x-1 & y <= 7-x) {
      count = count + 1
    }
  }
  aria= (  abs( (b - a) * (d - c) ) * count / n)
  cat("\nAria aproximativa a trapezului: ", aria)
}

B2(20000, 1, 4, 1, 4)

######################################


B3_a = function(n, a, b) {
  sum = 0
  for(i in 1:n) {
    x = runif(1, a, b)
    sum = sum + (x/((x^2+2)^3))
  }
  integrala= ( (b - a) * sum / n )
  cat("\n\nAria aproximativa intre", a,"si",b, ": ", integrala)
  cat("\nValoarea exacta intre", a, "si", b,": ", 1/48)
}

B3_a(10000, 1, 2)

############################################

B3_b = function(n, a, b) {
  sum = 0
  for(i in 1:n) {
    x = runif(1, a, b)
    sum = sum + (1/(x^2+9))
  }
  integrala = ( (b - a) * sum / n )
  cat("\nAria aproximativa intre", a,"si",b, ": ", integrala)
  cat("\nValoarea exacta intre", a, "si", b,": ", pi/6)
}

B3_b(10000, -3, 3)

############################################

#
B_3c=function(n)
{
  s=0
  for(i in 1:n)
  {
    x=rexp(1,1)
    s=s+(x*(exp(-(x^2))/exp(-x)))
  }
  integrala=s/n
  cat("\nVal aprox a integralei este: ", integrala)
  cat("\nVal exacta a integralei este: ", 1/2) 
}
B_3c(100000)

################# B4 ######################

ex_B4 = function(n)
{
  t= 0;
  for(i in 1:n)
  {
    p = runif(1, 0, 1)
    if(p <= 0.25) t = t+ rgamma(1, shape = 4, scale = 3)
    if(p > 0.25 & p <= 0.5) t = t + rgamma(1, shape = 4, scale = 2)
    if(p > 0.5 & p <= 0.8) t = t+ rgamma(1, shape = 5, scale = 2)
    if(p > 0.8) t = t + rgamma(1, shape = 5, scale = 3)
    
    t = t + rexp(1, 4)
  }
  
  return(t / n)
}

ex_B4(100000)


### B5 ###############################################

vector_initial=function(n){
  vect=vector(mode="logical",length = n)
  i=sample(n,1)
  vect[i]=TRUE
  return(vect)
}
#vector_initial(50)


#functie care numara computerele infectate

Nr_comp_inf=function(vec,N){
  c=0
  for(i in 1:N){
    if(vec[i]==TRUE){
      c=c+1
    }
  }
  return(c)
}
# Nr_comp_inf(vector_initial(50),50)

#functie de curatare pentru k calculatoare

curatare=function(vec,k){
  curatat=vector()
  j=1
  for(i in 1:length(vec)){
    if(vec[i]==TRUE){
      curatat[j]=i#pastram pozitiile unde avem infectat
      j=j+1
    }
  }
  #in curatat pun pozitiile exacte pe care se gasesc calculatoarele infectate
  # print(curatat)
  l=length(curatat)

  if(k>l)#daca k este mai mare decat lungimea vect cu pozitii infectate, curatam toate pozitiile
  { 
    h=l
  }else 
     if(l>0){
    h=k
  }#h este nr de conturi de curatat
  index=sample(l,h,replace = F) # generam nr l-numere pana la l, h-cate numere
  # print(index)
  for(i in 1:length(curatat)){
    vec[curatat[index[i]]]=FALSE
  }
  return(vec)
}

#curatare(vector_initial(50),8)


#infectam cu probabilitate 0.2
infectam=function(vec,p){
  l=length(vec)
  for(i in 1:length(vec)){
    if(vec[i]==TRUE){
      for(j in 1:length(vec)){
        if(vec[j]==FALSE){
          x=runif(1,0,1)
          #print(x)
          if(x<=p){
            vec[j]=TRUE
          }
        }
      }
    }
  }
  return(vec)
}

infectam(vector_initial(50),0.2) # val posibile pt p 0.05, 0.1, 0.2



#simularea functiei 
B5_a=function(n,k,p)
{
  count=0
  for(i in 1:n)
  {
    calculatoare=vector_initial(50)
    repetare=calculatoare
    
    m=1
    while(Nr_comp_inf(calculatoare,50)!=0)
    {
      calculatoare=infectam(calculatoare,p)
      len = length(calculatoare)
      for(i in 1:len)
        if(calculatoare[i] == TRUE) repetare[i] = TRUE
      m = Nr_comp_inf(repetare,50)
      calculatoare=curatare(calculatoare,k)
      if(m == 50) 
      {
        count=count+1 
        break
      }
      
    }
  }
  return(count*100/n)
  
}
B5_a(1000,8, 0.2)
B5_a(1000,8, 0.1)
B5_a(1000,8, 0.05)
## B5 b
B5_b=function(n,k,p)
{
  count=0
  for(i in 1:n)
  {
    calculatoare=vector_initial(50)
    nr_zile=1
    
    while(nr_zile<=7)
    {
      calculatoare=infectam(calculatoare,p)
      calculatoare=curatare(calculatoare,k)
      nr_zile=nr_zile+1
      if(Nr_comp_inf(calculatoare,50) != 0)
      {
        count = count + 1
      }
      
    }
  }
  return(count*100/n)
  
}
B5_b(1000,8, 0.2)
B5_b(1000,8, 0.1)
B5_b(1000,8, 0.05)

####B5 c

