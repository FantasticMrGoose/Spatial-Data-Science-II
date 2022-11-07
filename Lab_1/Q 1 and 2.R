plot(x = mpg$displ, y = mpg$hwy)
plot(
  x = mpg$displ,
  y = mpg$hwy,
  xlab = "Engine Displacement", 
  ylab = "Miles per Gallon (Highway)")

help(mpg)
library(ggplot2)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))+
  xlab("Engine Displacement")+
  ylab("Miles per Gallon (Highway)")+
  ggtitle("Highway Fuel Efficiency vs. Engine Displacement")
