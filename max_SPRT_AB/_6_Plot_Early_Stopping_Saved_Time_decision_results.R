# See Type 2 error
# Saved time
# How qucikly h1 is accepted
p1         = 0.5
r          = c(2,3,5)
p2         = p1 *(1+r/100)

#result = data.frame()

for (i in 1:length(p2)) {
  print (p1)
  print (p2[i])
  
  result= read.csv(paste0("AB_",p1,"-",p2[i] ,"_LLR_Boundry.csv"))
  print (table (result$Decision))
  
  RequiredBatch = round (power.prop.test(p1=p1, p2=(1+2/100)*p1, power=0.8)$n /100,0)
  
  resultH1 = subset(result, Decision=="AcceptH1")
  
  nmean =round (mean(resultH1$i),0)
  saved = (RequiredBatch-nmean)/RequiredBatch  
  
  print (round (saved,2))
  print ("****")
  hist(resultH1$i,  main = paste0("Early Stopping : ", p1," - ",p1) , xlab = "Number of Batch" ,xlim=c(0,RequiredBatch), col="bisque")
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

}







# See Type 1 error
# H0 is accepted at the end of the experiment as it should be
p1         = 0.5
r          = c(0)
p2         = p1 *(1+r/100)

#result = data.frame()

for (i in 1:length(p2)) {
  print (p1)
  print (p2[i])
  
  result= read.csv(paste0("AB_",p1,"-",p2[i] ,"_LLR_Boundry.csv"))
  print (table (result$Decision))
  
  RequiredBatch = round (power.prop.test(p1=p1, p2=(1+2/100)*p1, power=0.8)$n /100,0)
  
  resultH1 = subset(result, Decision=="AcceptH0")
  
  nmean =round (mean(resultH1$i),0)
  saved = (RequiredBatch-nmean)/RequiredBatch  
  
  print (round (saved,2))
  print ("****")
  hist(resultH1$i,  main = paste0("Early Stopping : ", p1," - ",p1) , xlab = "Number of Batch" ,xlim=c(0,RequiredBatch), col="bisque")
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
  
}