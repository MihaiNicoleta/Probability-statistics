E1 = function(n, pop_mean, select_mean, sigma, alfa, ipoteza){
  
  z_score = (select_mean - pop_mean)/(sigma/sqrt(n));
  if(ipoteza=='stanga'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='dreapta'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='simetric'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}
E1(125,420,418,2.75,0.01,"stanga")

######### E2 #########
E2 = function(n, pop_mean, select_mean, sigma, alfa, ipoteza){
  
  z_score = (select_mean - pop_mean)/(sigma/sqrt(n));
  if(ipoteza=='stanga'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='dreapta'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='simetric'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}
E2(25, 4.9,5.17,0.35,0.01,"dreapta");






##### E3 ### 
E3= function(n1,n2,m0,sample_mean1,sample_mean2,sigma1,sigma2, alfa, ipoteza){
  #as. la standa = 'left', 
  #as. la dreapta = 'right' 
  #simetrica = 'sim'
  combined_sigma=sqrt(sigma1^2/n1+sigma2^2/n2)
  z_score = ((sample_mean1 - sample_mean2)-m0)/sqrt((sigma1^2)/n1+(sigma2^2)/n2);
  if(ipoteza=='left'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}

#### E3
# sim, pentru ca se cere daca media difera
E3(25, 28, 0, 5.48, 6.12, 1.31, 0.93, 0.01, 'sim')

E3(21, 22, 0, 76.56, 72.23, 2.95, 3.12, 0.05, 'left')

########  E4 #########
E4= function(n1,n2,s1,s2, alfa, ipoteza){
  #as. la dreapta = 'right' 
  #simetrica = 'sim'
  F_score = s1^2/s2^2
  if(ipoteza=='right'){
    critical_F = qf(1 - alfa,n1-1,n2-1);
    if(F_score > critical_F){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
    cat("Scorul:", F_score," Valoari critica: ", critical_F, "\n");
  }
  if(ipoteza=='sim'){
    critical_Fs = qf(alfa/2,n1-1,n2-1);
    critical_Fd = qf(1-alfa/2,n1-1,n2-1);
    if((F_score<critical_Fs )||(F_score>critical_Fd)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
    cat("Scorul:", F_score," Valori critice: ", critical_Fs, critical_Fd,  "\n");
  }
}
E4(25, 28, 1.83, 1.24 ,0.01, 'right')

