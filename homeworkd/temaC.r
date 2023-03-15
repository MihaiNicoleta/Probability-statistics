#nodurile de pe penultimul rand au toate valoarea OR
### C1 ###
tree = function(i, frunze) {
  lungime = length(frunze)
  nr_noduri=lungime+lungime/2;
  
  nod=4  #numarul celui mai mic nod care e capat de rand si e mai mare decat i 
  putere=3 # fiecare nod are 3 fii
  while(i>nod)
  {
    putere=putere*3
    nod=nod+putere
  }
  nivel=log(putere,3)  #distanta de la radacina la nodul i =log in baza 3
  
  if(i==1)  #nivelul radacinii este 0
    nivel=0
  
  if(i*9>nr_noduri) #copiii nodului dat sunt frunze
  { 
    
    if(frunze[3*i-nod-1] == 0) # stim  ca avem or, deci verificam daca avem cel putin un 1
    {
      if(frunze[3*i-nod]==0)
        return(frunze[3*i-nod+1])
      else
        return(1)
    }
    else
      return(1)
  }
  
  
  if(nivel%%2==0)# nodul i e la dist para de radacina, deci e de tip AND
  { 
    
    if(tree(3*i-1, frunze) == 0)
      return(0)
    if(tree (3*i,  frunze) == 0)
      return(0)
    if(tree(3*i+1,  frunze) == 0)
      return(0)
    return(1)
    
  }
  
  if(nivel%% 2 == 1)# nodul i e la dist impara de radacina, deci e de tip OR
  {
    if(tree(3*i-1,  frunze) == 0)
    {
      if(tree(3*i,  frunze)==0)
        return(tree(3*i+1,  frunze))     
      else
        return(1)
    }
    else
      return(1)
  }
}

game_tree= function( frunze) 
{
  return(tree(1,  frunze))
}
frunze_1 = c(0,0,0,0,1,0,1,1,1) 
frunze_2 = c(1,1,0,0,1,0,1,1,1) 
game_tree( frunze_1 ) 
game_tree(frunze_2)

########### C2 ##########


########  C3  ##

library(numbers)

number=function(v){
  suma=0
  n=length(v)
  for(i in 1:n)
  {
    suma=suma+v[i]*(2^(i-1))
  }
  return(suma)
}

ex_C3 = function(u, U,n,m) {
  a = vector()
  p = sample(Primes(2, n*n), 1) 
  r = number(u)%%p
  for(j in 1:m)
  {
    rj = vector(length = n)
    for(i in 1:n)
      rj[i] = U[j,i]
    a[j] = number(rj)%%p
  }
  x=length(a)
  for(i in 1:x)
  {
    if(a[i]==r) return ("u apartine U")
  }
 return ("u nu apartine U")
}
n = 6
m = 4
u = c(1,0,0,0,1,0)
u1 = c(0,1,0,0,1,1)
u2 = c(1,1,1,1,0,0)
u3 = c(0,0,0,1,1,1)
u4=u
U = matrix(c(u1,u2,u3,u4),ncol=n,nrow= m,byrow=TRUE)
#print(U)

ex_C3(u, U, n, m) 

  

