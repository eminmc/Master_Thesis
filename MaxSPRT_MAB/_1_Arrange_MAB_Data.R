
# The raw data is created with thompson sampling on jupyhter hub.
# The columns are not cumulative in the created MAB data with TS .
# This function adds the cumulative succsess and trials columns for max SPRT

library("parallel")
library("readr")


arrangecsv_MAB =function (rundID=1,DF_) {

  DF = DF_ [(DF_$numSim == rundID  ),] #&X500_525$numIte <= 25000
  
  #### You will create cumulatives for each simulation NOT the entore csv!!!!!!!!
  # Create success column for each arm at time i,( not cumulative)
  DF$Arm1_success = ifelse(DF$chosen_arm ==0 & DF$reward ==1 , 1, 0)
  DF$Arm2_success = ifelse(DF$chosen_arm ==1 & DF$reward ==1 , 1, 0)
  
  # Create trail column for each arm at time i,( not cumulative)
  DF$Arm1_trial = ifelse(DF$chosen_arm ==0  , 1, 0)
  DF$Arm2_trial = ifelse(DF$chosen_arm ==1  , 1, 0)
  
  
  # Create cumulative success column for each arm up to time i
  DF$Arm1_cum_suc = cumsum (DF$Arm1_success)
  DF$Arm2_cum_suc = cumsum (DF$Arm2_success)
  
  # Create cumulative trial column for each arm up to time i
  DF$Arm1_cum_trial = cumsum (DF$Arm1_trial)
  DF$Arm2_cum_trial = cumsum (DF$Arm2_trial)
  
  DF = data.frame(DF$numSim,DF$numIte, DF$Arm1_cum_suc, DF$Arm2_cum_suc ,DF$Arm1_cum_trial, DF$Arm2_cum_trial)
  
  return (DF)
}

# Test for 2 smulations
if (0) {


y_AB_LLR  = mclapply(1:2, arrangecsv_MAB , DF_=DF , mc.cores = 2)
x_AB_LLR = do.call(rbind.data.frame, y_AB_LLR)
write.csv(x_AB_LLR, paste0("AB_",p1,"-",p2[i] ,"_LLR.csv"))
  
}


# Arrange csv for p1-delta pairs
# Suggested p1 values 0.2, 0.3, 0.5

if (0) {



p1 = 0.2
r  = c(0,2,3,5,10)
p2 = p1 *(1+r/100)

N = vector ()

  for (i in 1:length(r) ){
  if (r[i]==0) {N[i]= round (power.prop.test(p1=p1, p2=(1+r[i+1]/100)*p1, power=0.8)$n,0) }
  
  else N[i]= round (power.prop.test(p1=p1, p2=(1+r[i]/100)*p1, power=0.8)$n,0)
  
  }
N=N*2


  for (i in 1:length(p2)) {
  print (p1)
  print (p2[i])
  print (N[i])


  DF = read_csv(paste0("[", p1 ,  ", " , p2[i] ,"]- it",N[i],"-sim500.csv"),col_names = FALSE)
  names (DF) = c("numSim","numIte","chosen_arm","reward","cum_reward")

  yx  = mclapply(1:500, arrangecsv_MAB , DF_=DF , mc.cores = 10)
  x = do.call(rbind.data.frame, yx)

  write.csv(x, paste0("[", p1 ,  ", " , p2[i] ,"]- it",N[i],"-sim500_Arranged.csv"))

  remove (yx)
  remove (x)
  }


}


# ###### Do it once
# print (paste0("[", p1 ,  ", " , p1 ,"]- it",N[1],"-sim500.csv"))
# DF = read_csv(paste0("[", p1 ,  ", " , p1 ,"]- it",N[1],"-sim500.csv"),col_names = FALSE)
# names (DF) = c("numSim","numIte","chosen_arm","reward","cum_reward")
# 
# yx  = mclapply(1:500, arrangecsv_MAB , DF_=DF , mc.cores = 10)
# x = do.call(rbind.data.frame, yx)
# 
# write.csv(x, paste0("[", p1 ,  ", " , p1 ,"]- it",N[1],"-sim500_Arranged.csv"))
# 
# remove (yx)
# remove (x)












