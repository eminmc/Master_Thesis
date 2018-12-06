
# Create A/B data 

source("_1_Create_AB_Data.R")



library("parallel")
p1=0.5
p2=0.55
round (power.prop.test(p1=p1, p2=p2, power=0.8)$n,0)

NumIte10= 1565
NumSim= 10
y10  = mclapply(1:NumSim, f_create_1AB_data_faster  ,NumIte=NumIte10   ,p1=p1 ,p2=p2  , mc.cores = 10)
x10  = do.call(rbind.data.frame, y10)
names (x10) = c("numSim","numIte","Arm1_suc","Arm2_suc","Arm1_cum_suc","Arm2_cum_suc")


# Create AA Data 
p1=0.5
p2=0.51 # to calculate length
# The length delta 0 = delta =2

round (power.prop.test(p1=p1, p2=p2, power=0.8)$n,0)

NumIte0= 39239
NumSim= 10
y0  = mclapply(1:NumSim, f_create_1AB_data_faster  ,NumIte=NumIte0   ,p1=p1 ,p2=p1  , mc.cores = 10)
x0  = do.call(rbind.data.frame, y0)
names (x0) = c("numSim","numIte","Arm1_suc","Arm2_suc","Arm1_cum_suc","Arm2_cum_suc")


source("_2_Learn_AA_Upper_Boundary.R")
## use A/A data

  
  yAA  = mclapply(1:NumSim, AA_LLR_AB_AllSimulation_vector , DF_= x0 ,N =NumIte0 , mc.cores = 10)
  xAA   = do.call(rbind.data.frame, yAA)
  names (xAA) = 1:length(xAA)
  
  
#slice it for A/A and A/B
    
    df = xAA
    
    batch0 = length ( seq(100,NumIte0, 100))  
    df[, "max"] <- apply(df[, 2:batch0], 1, max)
    
    sorted= sort(df$max, decreasing = FALSE)
    q0 =round(quantile (sorted,0.95),2)
    print (q0) 
    
    df = xAA
    batch10 = length ( seq(100,NumIte10, 100))  
    df[, "max"] <- apply(df[, 2:batch10], 1, max)
    
    sorted= sort(df$max, decreasing = FALSE)
    q10 =round(quantile (sorted,0.95),2)
    print (q10) 
    
  
source ("_3_Calculate_LLR.R")
## on A/B data  

  
  y10_LLR  = mclapply(1:NumSim, maxSPRT_MAB_MC , DF_=x10 ,N =NumIte10 , mc.cores = 10)
  x10_LLR = do.call(rbind.data.frame, y10_LLR)
  
  
## on A/A  

  
  y0_LLR  = mclapply(1:NumSim, maxSPRT_MAB_MC , DF_=x0 ,N =NumIte0 , mc.cores = 10)
  x0_LLR = do.call(rbind.data.frame, y0_LLR)
  
  
  # plot avg LLR with boundary
  p1         = 0.5
  r          = c(0,10)
  
  
  plot (colMeans (x10_LLR)   ,  type = "l", col="blue", lwd =2, ylab = "LLR" , xlab = "Number of Batch" , main = paste0("Max SPRT-I-AA: A/B LLR & Boundary\n p1= ",p1," vs Delta") ,ylim = c(0,5))
  lines(colMeans (x0_LLR)    , col="red"   , lwd=4 )
  abline(h=q10 , col="blue"  , lwd=2, lty=2 )
  abline(h=q0  , col="red"   , lwd=2, lty=4 )
  grid (NULL,NULL, lty = 6, col = "cornsilk2") 
  legend("bottomright",legend=c("0","10"), col=c("blue","red"),lty = 1, ncol=1)
  
  
  
source ("_4_Make_Decision.R")
  
  y10_Decision  = mclapply(1:NumSim, AB_Boundries_1row , DF_= x10_LLR  ,threshold= q10, mc.cores = 10)
  
  x10_Decision = do.call(rbind.data.frame, y10_Decision)
  
  table (x10_Decision$Decision)
  
  
  y0_Decision  = mclapply(1:NumSim, AB_Boundries_1row , DF_= x0_LLR  ,threshold= q0, mc.cores = 10)
  
  x0_Decision = do.call(rbind.data.frame, y0_Decision)
  table (x0_Decision$Decision)
  
  
  
  # How quickly H1 is accepted
  resultH1 = subset(x10_Decision, Decision=="AcceptH1")
  
  nmean =round (mean(resultH1$i),0)
  RequiredBatch = NumIte10/100
  saved = (RequiredBatch-nmean)/RequiredBatch  
  
  print (round (saved,2))
  
  
  hist(resultH1$i,  main = paste0("Early Stopping") , xlab = "Number of Batch" , col="bisque" ,xlim=c(0,RequiredBatch))
  grid()
  abline(v=RequiredBatch, col="red", lwd=2)
  abline(v=nmean, col="blue", lwd=2)
  
  
  legend(x = "topright", inset=.05, 
         c( paste0(RequiredBatch), paste0(nmean)),
         col = c("red", "blue"),
         lwd = c(1, 1),
         cex=1,
         title="Calculated - Average Batch "
  )
  
  
