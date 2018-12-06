library("parallel")
library("readr")


## Learning the Upper Boundary 


# One observation
# Calculate LLR value with AA learning algorithm

# Since ti =0 for A / B testing
# wai used only to calculate total number of success!

AA_LLR = function (wai,wbi){
  
  library ("Brobdingnag")
  Ti = wai+ wbi 
  
  P0 = 0.5 
  P1 =  wbi / Ti  # wbi can be 0 at the begining of the test, so return 0 instead of NaN if wbi is 0 
  
  H0 = (as.brob(P0) ^ wbi) * (as.brob((1-P0)) ^ (Ti-wbi))
  H1 = (as.brob(P1) ^ wbi) * (as.brob((1-P1)) ^ (Ti-wbi))
  
  LLR = log (H1/H0)
  
  #return (LLR)
  return (ifelse(is.nan(LLR),0,LLR)) # return 0 instead of NaN when wbi = 0
}


# 
# # Returns the max LLR value
# AA_LLR_1simulation_max = function(N =25000,DF) {
#   
#   #N =25000 #options(digits=5)
#   sequence = seq(100,N, 100)
#   
#   LLR_vector            = vector()
#   LLR_vector_index      = 1
#   
#   for (i in sequence) {
#     
#     LLR_vector[LLR_vector_index] = AA_LLR (wai = DF[i,]$Arm1_cum_suc  , wbi = DF[i,]$Arm2_cum_suc )  
#     
#     LLR_vector_index             = LLR_vector_index+1
#     
#   }
#   return (max (LLR_vector))
# }
# 
# 
# AA_LLR_AB_AllSimulation_max = function (runID = 1,DF_ ){
#   
#   
#   DF       = DF_ [(DF_$numSim == runID),] 
#   temp= AA_LLR_1simulation_max (N =25000,DF =DF) 
#   return (temp)
# }




# Only one Simulation
# Calculate LLR value with AA learning algorithm 
# Do not return the max 
# We will slice the data later becasue experiment length differs for delta values
AA_LLR_1simulation_vector = function(N =25000,DF) {
  
  #N =25000 #options(digits=5)
  sequence = seq(100,N, 100)
  
  LLR_vector            = vector()
  LLR_vector_index      = 1
  
  for (i in sequence) {
    
    LLR_vector[LLR_vector_index] = AA_LLR (wai = DF[i,]$Arm1_cum_suc  , wbi = DF[i,]$Arm2_cum_suc )  
    
    LLR_vector_index             = LLR_vector_index+1
    
  }
  return (LLR_vector)
}




# All Simulations paralel
AA_LLR_AB_AllSimulation_vector = function (runID = 1,DF_ , N=2500){
  
  
  DF       = DF_ [(DF_$numSim == runID),] 
  temp= AA_LLR_1simulation_vector (N =N,DF =DF) 
  return (temp)
}




if (0) {

# The lenght of the simulations are same for delta =2 and delta =0
# Find the sample size to read the file p1=p2
# Set the p1 value only.

p1 = 0.2
r  = c(0,2)
p2 = p1 *(1+r/100)

N = vector ()
## No need to divide 2
for (i in 1:length(r) ){
  if (r[i]==0) {N[i]= round (power.prop.test(p1=p1, p2=(1+r[i+1]/100)*p1, power=0.8)$n,0)*2 }
  
  else N[i]= round (power.prop.test(p1=p1, p2=(1+r[i]/100)*p1, power=0.8)$n,0)*2
  
}


print (p1)
print (N[1])


DF=read.csv(paste0("[", p1 ,  ", " , p1 ,"]- it",N[1],"-sim500_Arranged.csv"))

colnames(DF)= c ("X","numSim","numIte","Arm1_cum_suc","Arm2_cum_suc","Arm1_cum_trial","Arm2_cum_trial") 

yx  = mclapply(1:500, AA_LLR_AB_AllSimulation_vector , DF_=DF ,N =N[1] , mc.cores = 10)
x   = do.call(rbind.data.frame, yx)
names (x) = 1:length(x)
write.csv(x, paste0("MAB_",p1,"-",p1,"-","_LLR_AA_Boundry-.csv"))

}


if (0) {
# Learning Boundries for delta values
# Calculate the batch that we need to stop
# Slice the data for corresponding sample size for delta
# Print p1 p2 boundary recursively
p1 = 0.2
Nx = 316300
r  = c(0,2,3,5,10)
p2 = p1 *(1+r/100)

N     = vector ()
batch = vector ()
q     = vector ()
#df = read.csv(paste0("MAB_",p1,"-",p1,"-","_LLR_AA_Boundry-.csv"))       # use from AA algorihm values are too big
df= read.csv(paste0("[", p1 ,  ", " , p1 ,"]- it",Nx,"-500-LLR.csv"))     # use from p1=p2 csvs

for (i in 1:length(r) ){
  if (r[i]==0) {N[i]= round (power.prop.test(p1=p1, p2=(1+r[i+1]/100)*p1, power=0.8)$n,0) }
  
  else {N[i]= round (power.prop.test(p1=p1, p2=(1+r[i]/100)*p1, power=0.8)$n,0)}
  
  batch[i] = length ( seq(100,N[i], 100))  
  
  
  df[, "max"] <- apply(df[, 2:batch[i]], 1, max)
  sorted= sort(df$max, decreasing = FALSE)
  q[i] =round(quantile (sorted,0.95),2)
  print (p1)
  print (p2)
  print (q) 
  
}


}