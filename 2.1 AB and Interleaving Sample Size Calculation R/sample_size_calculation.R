
#p1    = 0.1
#p1    = 0.2
#p1    = 0.3
p1    = 0.5
p2    = vector ()
delta = c(2,3,5,10)

for (i in 1:length (delta))  {
  p2[i]  =(1+delta[i]/100)*p1  
  print (p1)
  print (p2[i])
  print(power.prop.test(p1=p1, p2=p2[i], power=0.8)$n)
}

  