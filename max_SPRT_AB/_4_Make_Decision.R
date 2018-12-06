AB_Boundries_1row = function (runID = 1,DF_ ,threshold){
  
  
  
  LLR            = DF_[runID,]  
  tempthreshold = vector()
  tmph          = FALSE
  N=ncol(LLR)
  for (i in 1:N)
    
    if (threshold < LLR[i] & tmph==FALSE ){
      tempthreshold= c (round(i,2),round(LLR[i],2), "AcceptH1")
      tmph=TRUE
      
    }
  
  if (i==N & tmph==FALSE ) {  tempthreshold= c (round(i,2),round(LLR[i],2), "AcceptH0") }
  
  
  
  names (tempthreshold) = c("i","LLR","Decision")
  return (tempthreshold)
  
}


if (0){
###### Test for 1
library("parallel")
NumIte= 39239
p1=0.5
p2=0.51

DF = read_csv(paste0("AB_",p1,"-",p2 ,"_LLR.csv"))

DF = DF[,(2:ncol(DF)),drop=FALSE]  # still a data.frame
colnames(DF) = 1:ncol(DF)

y_AB_LLR_Boundry  = mclapply(1:1000, AB_Boundries_1row , DF_=DF  ,threshold=2.34, mc.cores = 10)
x_AB_LLR_Boundry = do.call(rbind.data.frame, y_AB_LLR_Boundry)
write.csv(x_AB_LLR_Boundry, paste0("_AB_",p1,"-",p2 ,"_LLR_Boundry.csv"))

}

######






if (0){
library("parallel")
library(readr)
# Suggested p1 values 0.05, 0.1 , 0.2, 0.3, 0.5
#0.5 threshold  = c (2.34,2.34,2.27,2.03,1.66)
#0.3 threshold  = c (3.24,3.24,3.07,2.83,2.6)
#0.2 threshold  = c (3.65,3.65,3.52,3.33,3.04)
#0.1 threshold  = c (4.39,4.39,4.27,4.04,3.69)
threshold  = c (4.39,4.39,4.27,4.04,3.69)
p1         = 0.1
r          = c(0,2,3,5,10)
p2         = p1 *(1+r/100)


for (i in 1:length(p2)) {
  print (p1)
  print (p2[i])
  DF = read_csv(paste0("AB_",p1,"-",p2[i] ,"_LLR.csv"))
  
  ncol(DF)
  DF = DF[,(2:ncol(DF)),drop=FALSE]  # still a data.frame
  colnames(DF) = 1:ncol(DF)
  
  y_AB_LLR_Boundry  = mclapply(1:1000, AB_Boundries_1row , DF_=DF  ,threshold=threshold[i], mc.cores = 10)
  
  x_AB_LLR_Boundry = do.call(rbind.data.frame, y_AB_LLR_Boundry)
  
  write.csv(x_AB_LLR_Boundry, paste0("AB_",p1,"-",p2[i] ,"_LLR_Boundry.csv"))
}




DF = read_csv(paste0("AB_",0.5,"-",0.55 ,"_LLR.csv"))
#x= read_csv(paste0("AB_",0.5,"-",0.55 ,"_LLR.csv"))

ncol(DF)
DF = DF[,(2:ncol(DF)),drop=FALSE]  # still a data.frame
colnames(DF) = 1:ncol(DF)

y_AB_LLR_Boundry  = mclapply(1:1, AB_Boundries_1row , DF_=DF  ,threshold=threshold, mc.cores = 1)

x_AB_LLR_Boundry = do.call(rbind.data.frame, y_AB_LLR_Boundry)

write.csv(x_AB_LLR_Boundry, paste0("AB_",p1,"-",p2[i] ,"_LLR_Boundry.csv"))


}


# ######
# p1=0.05
# p2=0.05
# DF = read_csv(paste0("AB_",p1,"-",p2 ,"_LLR.csv"))
# colnames(DF) = 1:251
# DF = DF[,(2:251),drop=FALSE]  # still a data.frame
# colnames(DF) = 1:250
# 
# y_AB_LLR_Boundry  = mclapply(1:1000, AB_Boundries_1row , DF_=DF  ,threshold=threshold, mc.cores = 10)
# 
# x_AB_LLR_Boundry = do.call(rbind.data.frame, y_AB_LLR_Boundry)
# 
# #names (x_AB_LLR) = 1:250
# 
# write.csv(x_AB_LLR_Boundry, paste0("AB_",p1,"-",p2 ,"_LLR_Boundry.csv"))