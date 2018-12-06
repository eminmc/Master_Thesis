
# Calculate LLR  


# 1 observation
# All values are must so there is no default for Sa,Sb,Ta,Tb, 

f_maxSPRT = function (Sa,Sb,Ta,Tb) {
  library ("Brobdingnag") # LLH1 and LLHO becames too small when the iteration increases, we need this package to avoid underflow
  # number of cumulative success-trial for bucket A, bucket B and sum of buckets, at time i
  # St and Tt will be used of P0 calculation  
  St = Sa+Sb
  Tt = Ta+Tb
  
  # Maximum likelyhood estimeates Pa,Pb,P0 
  Pa = round(Sa/Ta,6)
  Pb = round(Sb/Tb,6)  
  P0 = round(St/Tt,6)
  
  LLH1 = ((as.brob(Pa))^Sa)  * ((as.brob(1-Pa))^ (Ta-Sa))   *   ((as.brob(Pb))^Sb) * ((as.brob(1-Pb))^(Tb-Sb))
  LLH0 = ((as.brob(P0))^Sa)  * ((as.brob(1-P0))^ (Ta-Sa))   *   ((as.brob(P0))^Sb) * (as.brob((1-P0))^(Tb-Sb))
  
  LLR = log (LLH1/LLH0)   
  return (ifelse(is.nan(LLR),0,LLR))
  #return (LLR)  
}




# 1 simulation 
# Function returns the all LLR vector
f_maxSPRT_1simulation = function(N =25000,DF) {
  
  #N =25000 #options(digits=5)
  sequence = seq(100,N, 100)
  
  LLR_vector            = vector()
  LLR_vector_index      = 1
  
  for (i in sequence) {
    
    LLR_vector[LLR_vector_index] = f_maxSPRT( Sa = DF[i,]$Arm1_cum_suc
                                              ,Sb = DF[i,]$Arm2_cum_suc 
                                              ,Ta = DF[i,]$numIte
                                              ,Tb = DF[i,]$numIte) 
    
    LLR_vector_index             = LLR_vector_index+1
    
  }
  return (LLR_vector)
}


# All simulations for one p1-p2

maxSPRT_MAB_MC = function (runID = 1,DF_ ,N = 25000){
  
  DF       = DF_ [(DF_$numSim == runID),] 
  temp= f_maxSPRT_1simulation (N =N,DF =DF) 
  return (temp)
}



# Test for p1 - p2 pairs

if (0) {
  
  library("parallel")
  NumIte= 39239
  p1=0.5
  p2=0.51
  
  DF         = read.csv(gzfile(paste0("AB_",p1,"-",p2 ,"-",NumIte,".gz")))
  names (DF) = c("numSim","numIte","Arm1_suc","Arm2_suc","Arm1_cum_suc","Arm2_cum_suc")
  
  y_AB_LLR   = mclapply(1:1000, maxSPRT_MAB_MC , DF_=DF ,N =NumIte , mc.cores = 10)
  x_AB_LLR   = do.call(rbind.data.frame, y_AB_LLR)
  
  write.csv(x_AB_LLR, paste0("_AB_",p1,"-",p2 ,"_LLR.csv"))
  
  
  }




# Calculate for one p1 but all deltas
if (0) {

library("parallel")
library("readr")
# Suggested p1 values 0.1, 0.2, 0.3, 0.5
# r  = c(0,2,3,5,10)

p1 = 0.1
r  = c(0,2,3,5,10)
p2 = p1 *(1+r/100)

N = vector ()


for (i in 1:length(r) ){
  if (r[i]==0) {N[i]= round (power.prop.test(p1=p1, p2=(1+r[i+1]/100)*p1, power=0.8)$n,0) }
  
  else N[i]= round (power.prop.test(p1=p1, p2=(1+r[i]/100)*p1, power=0.8)$n,0)
  
}

for (i in 1:length(p2)) {
  print (p1)
  print (p2[i])

  DF = read.csv(gzfile(paste0("AB_",p1,"-",p2[i] ,"-",N[i] ,".gz")))
  
  names (DF) = c("numSim","numIte","Arm1_suc","Arm2_suc","Arm1_cum_suc","Arm2_cum_suc")
  
  y_AB_LLR  = mclapply(1:1000, maxSPRT_MAB_MC , DF_=DF ,N =N[i] , mc.cores = 15)
  x_AB_LLR = do.call(rbind.data.frame, y_AB_LLR)
  
  write.csv(x_AB_LLR, paste0("AB_",p1,"-",p2[i] ,"_LLR.csv"))
}

}














