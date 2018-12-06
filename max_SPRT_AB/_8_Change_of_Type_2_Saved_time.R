



final =read.csv("all_type_2.csv")
final
library(ggplot2)
ggplot(final, aes(x = delta, y =type2 , group=p1)) +
  
  geom_point(aes(color = factor(p1)), size = 4) +
  #geom_line(aes(group = p1))+
  geom_line(aes(color = factor(p1)))+
  #ylim(0, 1) +
  xlab("Delta Value") +
  ylab("Type 2 Error Rate") +
  
  ggtitle("Change of Type 2 Error Rate")
#ggtitle("Change of Upper Boundary for different P1-P2")













final =read.csv("all_type_2.csv")
final
library(ggplot2)
ggplot(final, aes(x = delta, y =saved , group=p1)) +
  
  geom_point(aes(color = factor(p1)), size = 4) +
  #geom_line(aes(group = p1))+
  geom_line(aes(color = factor(p1)))+
  #ylim(0, 1) +
  xlab("Delta Value") +
  ylab("Saved Time %") +
  
  ggtitle("Change of Saved Time")
#ggtitle("Change of Upper Boundary for different P1-P2")

