## D1 ###
confidence_interval=function(n,sample_mean,alfa,s)
{
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  interval = c(a,b)
  print("Interval de incredere: ")
  return (interval)
}
confidence_interval(20,300,0.05,30)

### D2 ###


D_2=function(incredere)
{
  alfa=(100-incredere)/100
  aparitii=0
  for(i in 1:100)
  {
    x=sample(0:9,40,replace=T) # 40 de valori intre 0 si 9
    dev=sd(x)
    sample_mean=mean(x)
    se=dev/sqrt(40)
    critical_t = qt(1 - alfa/2, 40 - 1)
    a = sample_mean - critical_t*se
    b = sample_mean + critical_t*se
    if(4.5>=a && 4.5<=b)
      aparitii=aparitii+1
  }
  return(aparitii)
}
D_2(95)
D_2(99)


#### D3 ######

test= function(n, p0, succese, alfa, ipoteza){
  #as. la stanga = 'stg'
  #as. la dreapta = 'drp' 
  #simetrica = 'sim'
  p_prim = succese/n;
  z_score = (p_prim - p0)/(sqrt(p0*(1 - p0)/n));
  if(ipoteza=='stg'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  if(ipoteza=='drp'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}
test(1250, 0.72, 852, 0.01, "stg")
test(1250, 0.72, 852, 0.05, "stg")
## => fals proportie este mai mica decat 72%
### D4 ###

test(1020, 0.60, 623, 0.01, "drp")
test(1020, 0.60, 623, 0.05, "drp")
## => adevarat 
#############