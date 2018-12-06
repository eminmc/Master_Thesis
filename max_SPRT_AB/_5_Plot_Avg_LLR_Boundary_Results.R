

# Suggested p1 values 0.1 , 0.2, 0.3, 0.5
threshold  = c (2.34,2.34,2.27,2.03,1.66)   #0.5 
#threshold  = c (3.24,3.24,3.07,2.83,2.6)    #0.3
# threshold  = c (3.65,3.65,3.52,3.33,3.04)   #0.2
# threshold  = c (4.39,4.39,4.27,4.04,3.69)      #0.1 

p1         = 0.5
r          = c(0,2,3,5,10)
p2         = p1 *(1+r/100)


t0 =read.csv(paste0("AB_",p1,"-",p2[1] ,"_LLR.csv"))
t2 =read.csv(paste0("AB_",p1,"-",p2[2] ,"_LLR.csv"))
t3 =read.csv(paste0("AB_",p1,"-",p2[3] ,"_LLR.csv"))
t5 =read.csv(paste0("AB_",p1,"-",p2[4] ,"_LLR.csv"))
t10 =read.csv(paste0("AB_",p1,"-",p2[5] ,"_LLR.csv"))

t0 = t0 [,2:ncol(t0)]
t2 = t2 [,2:ncol(t2)]
t3 = t3 [,2:ncol(t3)]
t5 = t5 [,2:ncol(t5)]
t10 = t10 [,2:ncol(t10)]


plot (colMeans (t0)   ,  type = "l", col="red", lwd =2, ylab = "LLR" , xlab = "Number of Batch" , main = paste0("Max SPRT-I-AA: A/B LLR & Boundary\n p1= ",p1," vs Delta") ,ylim = c(0,5))
lines(colMeans (t2) , col="blue"   , lwd=4 )
lines(colMeans (t3) , col="gray"   , lwd=4 )
lines(colMeans (t5) , col="green"  , lwd=4 )
lines(colMeans (t10), col="yellow" , lwd=4 )
abline(h=threshold[1], col="red", lwd=2, lty=2 )
abline(h=threshold[2], col="blue", lwd=2, lty=4 )
abline(h=threshold[3], col="gray", lwd=2, lty=4 )
abline(h=threshold[4], col="green", lwd=2, lty=4 )
abline(h=threshold[5], col="yellow", lwd=2, lty=4 )
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
legend("bottomright",legend=c("0","2","3","5","10"), col=c("red","blue","gray","green","yellow"),lty = 1, ncol=1)