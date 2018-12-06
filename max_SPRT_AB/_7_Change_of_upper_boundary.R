
final =read.csv("upper_threshold_by_p1_p2.csv")

library(ggplot2)
ggplot(final, aes(x = delta, y = q, colour = p1)) +
  geom_point() +
  
  #ylim(0, 1) +
  xlab("P2 Value") +
  ylab("Boundary Value") +
  ggtitle("Change of Upper Boundary")
#ggtitle("Change of Upper Boundary for different P1-P2")



final =read.csv("upper_threshold_by_p1_p2_2.csv")
final
library(ggplot2)
ggplot(final, aes(x = delta, y =q , group=p1)) +
  
  geom_point(aes(color = factor(p1)), size = 4) +
  #geom_line(aes(group = p1))+
  geom_line(aes(color = factor(p1)))+
  #ylim(0, 1) +
  #ylim(0, 1) +
  xlab("Delta Value") +
  ylab("Boundary Value") +
  ggtitle("Change of Upper Boundary")
#ggtitle("Change of Upper Boundary for different P1-P2")


# 
# library(lattice)
# # Each group in a separate mini plot
# xyplot(p2 ~ q | p1, data = final)
# # All groups in one plot, different colors for each group
# #   Not at all interesting with the example data you've provided
# xyplot(p2 ~ q, groups=final$p1, data = final)
# 
# 
# 
# plot (final$p2,final$q, type = "l", lwd = 0.5, ylab = "Upper Threshold" , xlab = "P2 value" , main = "Max SPRT Threshold by P1-P2" )
# axis(side = 2, at = -2:18)
# grid (NULL,NULL, lty = 6, col = "cornsilk2") 
# lines(x,mean__5, col="blue" , lwd=1 )
# lines(x,mean__4, col="green" , lwd=1 )
# lines(x,mean__3, col="yellow" , lwd=1 )
# lines(x,mean__2, col="gray" , lwd=1 )
# lines(x,mean__1, col="brown" , lwd=1 )
# lines(x,mean__0, col="orange" , lwd=1 )
# 
# abline(h=test__1$wald.A , lwd=3, lty= 4)
# abline(h=test__1$wald.B,lwd=3 , lty= 4)
# legend(1,15,legend=c("20","10","5","3","2","1","0"), col=c("red","blue","green","yellow","gray", "brown","orange"),lty = 1, ncol=1)
