

## Read Arranged files
library("sqldf")
library("parallel")

## change the condition in sql query as well
NumSim= 10 

# times 2
p1=0.5
p2=0.55
print (round (power.prop.test(p1=p1, p2=p2, power=0.8)$n,0)*2)
NumIte10= round (power.prop.test(p1=p1, p2=p2, power=0.8)$n,0)*2
x10 = read.csv.sql("[0.5, 0.55]- it3130-sim500_Arranged.csv", sql = "select * from file where `DF.numSim` < 11", eol = "\n")
colnames(x10)= c ("X","numSim","numIte","Arm1_cum_suc","Arm2_cum_suc","Arm1_cum_trial","Arm2_cum_trial") 
tail (x10)



p1=0.5
p2=0.51 # to calculate length
print (round (power.prop.test(p1=p1, p2=p2, power=0.8)$n,0)*2)
NumIte0= round (power.prop.test(p1=p1, p2=p2, power=0.8)$n,0)*2
x0  = read.csv.sql("[0.5, 0.5]- it78478-sim500_Arranged.csv", sql = "select * from file where `DF.numSim` < 11", eol = "\n")
colnames(x0)= c ("X","numSim","numIte","Arm1_cum_suc","Arm2_cum_suc","Arm1_cum_trial","Arm2_cum_trial") 
tail (x0)




#Learning the upper boundary
source("_2_Learn_AA_Upper_Boundary.R")

yAA  = mclapply(1:NumSim, AA_LLR_AB_AllSimulation_vector , DF_= x0 ,N =NumIte0 , mc.cores = 10)
xAA   = do.call(rbind.data.frame, yAA)
names (xAA) = 1:length(xAA)



# slice it twice # boundaries are extremely large
df = xAA
batch0 = length ( seq(100,NumIte0, 100))  
df[, "max"] <- apply(df[, 2:batch0], 1, max)
sorted= sort(df$max, decreasing = FALSE)
q0 =round(quantile (sorted,0.95),2)
print ( paste0("Boundary for p1=p2: ",q0)) 


df = xAA
batch10 = length ( seq(100,NumIte10, 100))  
df[, "max"] <- apply(df[, 2:batch10], 1, max)
sorted= sort(df$max, decreasing = FALSE)
q10 =round(quantile (sorted,0.95),2)
print ( paste0("Boundary for p1!=p2: ",q10)) 


#Calculate LLR
source ("_3_Calculate_LLR.R")

## on p1!=p2
y10_LLR  = mclapply(1:NumSim, maxSPRT_MAB_MC , DF_=x10 ,N =NumIte10 , mc.cores = 10)
x10_LLR = do.call(rbind.data.frame, y10_LLR)

## on p1=p2
y0_LLR  = mclapply(1:NumSim, maxSPRT_MAB_MC , DF_=x0 ,N =NumIte0 , mc.cores = 10)
x0_LLR = do.call(rbind.data.frame, y0_LLR)

p1         = 0.5
r          = c(0,10)
plot (colMeans (x10_LLR)   ,  type = "l", col="blue", lwd =2, ylab = "LLR" , xlab = "Number of Batch" , main = paste0("Max SPRT-I-AA: A/B LLR & Boundary\n p1= ",p1," vs Delta")     ,ylim = c(0,5))
lines(colMeans (x0_LLR)    , col="red"   , lwd=4 )
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
legend("bottomright",legend=c("10","0"), col=c("blue","red"),lty = 1, ncol=1)

### LLR calculation looks fine



## investigating why we get gian upper boundaries
tail (x0,2)


# Create 1 simulation data for A/B 
f_create_1AB_data_faster = function (runID=1,NumIte,p1,p2) {
  
  i = 1:NumIte
  s1 =sample(0:1, NumIte, p= c(1-p1, p1), replace=TRUE)
  s2 =sample(0:1, NumIte, p= c(1-p2, p2), replace=TRUE)
  
  df=data.frame(j=runID,i=i,p1=s1,p2=s2, s1_cum = cumsum(s1) , s2_cum =cumsum(s2), stringsAsFactors=FALSE)
  return (df)
}

  p1 = 0.5
  N = NumIte0
  DF=f_create_1AB_data_faster (runID=1,NumIte= N,p1=p1 ,p2=p1 )
  tail (DF,2)
  

# Alternative can be useing p1-p2 as baseline


  df = x0_LLR
  batch0 = length ( seq(100,NumIte0, 100))  
  df[, "max"] <- apply(df[, 2:batch0], 1, max)
  sorted= sort(df$max, decreasing = FALSE)
  q0 =round(quantile (sorted,0.95),2)
  print ( paste0("Boundary for p1=p2: ",q0)) 
  
  
  df = x0_LLR
  batch10 = length ( seq(100,NumIte10, 100))  
  df[, "max"] <- apply(df[, 2:batch10], 1, max)
  sorted= sort(df$max, decreasing = FALSE)
  q10 =round(quantile (sorted,0.95),2)
  print ( paste0("Boundary for p1!=p2: ",q10)) 

#  Those values are better but still high..
#  The boundariese which are learned form A/B experiment tested as well but they were high as well.
  
  