library("parallel")
library("readr")


# 1 observation
f_maxSPRT = function (Sa,Sb,Ta,Tb) {
  library ("Brobdingnag") # LLH1 and LLHO becames too small when the iteration increases, we need this package to avoid underfow
  # number of cumulative success-trial for bucket A, bucket B and sum of buckets, at time i
  # St and Tt will be used of P0 calculation  
  St = Sa+Sb
  Tt = Ta+Tb
  
  # Maximum likelyhood estimeates Pa,Pb,P0 -- Round values for 3 decimal points, Do we need to Round for more ????
  Pa = round(Sa/Ta,6)
  Pb = round(Sb/Tb,6)  
  P0 = round(St/Tt,6)
  
  # Likelyhood under H1 and H0 at time i
  #LLH1 = (Pa^Sa)  * ((1-Pa)^(Ta-Sa))    *   (Pb^Sb) * ((1-Pb)^(Tb-Sb))
  #LLH0 = (P0^Sa)  * ((1-P0)^ (Ta-Sa))   *   (P0^Sb) * ((1-P0)^(Tb-Sb))
  
  
  LLH1 = ((as.brob(Pa))^Sa)  * ((as.brob(1-Pa))^ (Ta-Sa))   *   ((as.brob(Pb))^Sb) * ((as.brob(1-Pb))^(Tb-Sb))
  LLH0 = ((as.brob(P0))^Sa)  * ((as.brob(1-P0))^ (Ta-Sa))   *   ((as.brob(P0))^Sb) * (as.brob((1-P0))^(Tb-Sb))
  
  
  # LLR in maxSPRT at time i
  
  LLR = log (LLH1/LLH0)   ## be sure about the log base !!!
  return (ifelse(is.nan(LLR),0,LLR))
  #return (LLR)  
}


# 1 simulation
f_maxSPRT_1simulation = function(N =25000,DF) {
  
  #N =25000 #options(digits=5)
  sequence = seq(100,N, 100)
  
  LLR_vector            = vector()
  LLR_vector_index      = 1
  
  for (i in sequence) {
    
    LLR_vector[LLR_vector_index] = f_maxSPRT( Sa = DF[i,]$Arm1_cum_suc
                                              ,Sb = DF[i,]$Arm2_cum_suc 
                                              ,Ta = DF[i,]$Arm1_cum_trial
                                              ,Tb = DF[i,]$Arm2_cum_trial) 
    
    LLR_vector_index             = LLR_vector_index+1
    
  }
  return (LLR_vector)
}


# All Simulations on paralel
maxSPRT_MAB_MC = function (runID = 1,DF_ , N=25000 ){
  
  
  DF       = DF_ [(DF_$numSim == runID),] 
  temp= f_maxSPRT_1simulation (N =N,DF =DF) 
  return (temp)
}



# Calculates Wald boundries if it is required
f_Calculate_Boundries = function (Type1=0.05,Type2=0.2,NumIte,BoundryType="Wald") {
  
  if (BoundryType == "Wald") {
    
    return (log((1-Type2)/ Type1))
  }
  
  if(BoundryType == "Ramesh"){
    
    res =Type1 * sqrt( log(NumIte)/NumIte )
    
    return ( ifelse(is.nan(res),0,res)  )
  }
  
}



if (0) {
# Calculate LLR for all p1-delta values
# Suggested p1 values 0.2, 0.3, 0.5
p1 = 0.5
r  = c(0,2,3,5,10)

p2 = p1 *(1+r/100)

N = vector ()
## No need to divide 2
for (i in 1:length(r) ){
  if (r[i]==0) {N[i]= round (power.prop.test(p1=p1, p2=(1+r[i+1]/100)*p1, power=0.8)$n,0) }
  
  else N[i]= round (power.prop.test(p1=p1, p2=(1+r[i]/100)*p1, power=0.8)$n,0)
  
}
N=N*2



for (i in 1:length(p2)) {
  print (p1)
  print (p2[i])
  print (N[i])
  
  DF=read.csv(paste0("[", p1 ,  ", " , p2[i] ,"]- it",N[i],"-sim1000_Arranged.csv"),stringsAsFactors=FALSE)
  DF=read.csv(paste0("[", p1 ,  ", " , p2[i] ,"]- it",N[i],"-sim500_Arranged.csv"))
  colnames(DF)= c ("X","numSim","numIte","Arm1_cum_suc","Arm2_cum_suc","Arm1_cum_trial","Arm2_cum_trial") 

  yx  = mclapply(1:500, maxSPRT_MAB_MC , DF_=DF ,N =N[i] , mc.cores = 10)
  x   = do.call(rbind.data.frame, yx)
  #names (x) = 1:length(x)
  write.csv(x, paste0("[", p1 ,  ", " , p2[i] ,"]- it",N[i],"-500-LLR.csv"))

  
  }



}




if (0) {
# Investigation for the Giant LLR boundary AA for MAB
# Read 1 simulation from arranged csv file look at the end of the simulation.
p1 = 0.2
N = 316300

DF          =read.csv(paste0("[", p1 ,  ", " , p1 ,"]- it",N,"-sim500_Arranged.csv"))
colnames(DF)= c ("X","numSim","numIte","Arm1_cum_suc","Arm2_cum_suc","Arm1_cum_trial","Arm2_cum_trial") 

DF_         = DF [(DF$numSim == 1),]
tail (DF)


}


