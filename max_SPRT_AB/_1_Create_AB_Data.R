
# Create 1 simulation data for A/B 
f_create_1AB_data_faster = function (runID=1,NumIte,p1,p2) {
  
  i = 1:NumIte
  s1 =sample(0:1, NumIte, p= c(1-p1, p1), replace=TRUE)
  s2 =sample(0:1, NumIte, p= c(1-p2, p2), replace=TRUE)
  
  df=data.frame(j=runID,i=i,p1=s1,p2=s2, s1_cum = cumsum(s1) , s2_cum =cumsum(s2), stringsAsFactors=FALSE)
  return (df)
}

#Test: Create 1 simulation with 1000 Observation
if (0){

  f_create_1AB_data_faster (runID=1,NumIte= 1000,p1=0.05,p2=0.05)
  
}
  

#Test: Create 2 simulations parallel with 1000 Observations each
if (0){
  library("parallel")
  NumIte= 1000
  p1=0.05
  p2=0.05
  y  = mclapply(1:2, f_create_1AB_data_faster  ,NumIte=NumIte   ,p1=p1 ,p2=p2  , mc.cores = 2)
  x  = do.call(rbind.data.frame, y)
  
}




# Create all the p1-p2 cases with the required number of observations.
# Write them into gz files use p1,p2 and number of samples on naming
# File name look like AB_0.5-0.525-6274.gz

if (0) {
  
library("parallel")

x = c (0.1 , 0.2, 0.3, 0.5)  # p1 values

for (j in 1:length(x)) {
  
  p1 = x[j]
  r = c(0,2,3,5,10)          # delta values
  p2 = p1 *(1+r/100)
  
  N = vector ()
  
  # Required Sample size for each case
  for (i in 1:length(r) ){
    if (r[i]==0) {N[i]= round (power.prop.test(p1=p1, p2=(1+r[i+1]/100)*p1, power=0.8)$n,0) }
    
    else N[i]= round (power.prop.test(p1=p1, p2=(1+r[i]/100)*p1, power=0.8)$n,0)
    
                          }
  
  # Data Creation
  for (i in 1:length(p2)) {
    print (p1)
    print (p2[i])
    print (N[i])
    y   = mclapply(1:1000, f_create_1AB_data_faster ,NumIte=N[i]   ,p1=p1 ,p2=p2[i]  , mc.cores = 20)
    yx  = do.call(rbind.data.frame, y)
    write.csv(yx, gzfile(paste0("AB_",p1,"-",p2[i] ,"-",N[i] ,".gz")), row.names=F)
    
    
  }
  
  remove (N)
  remove (y)
  
}


}


# Investigating Why AA boundary works on A/B ?
# Tail of one AB data to show that how close the cumulative sum values for each arm 

if (0) {
  
  p1 = 0.2
  N = 158150
  DF=f_create_1AB_data_faster (runID=1,NumIte= N,p1=p1 ,p2=p1 )
  tail (DF)
  
}




