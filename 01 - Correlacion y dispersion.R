x = c(145,141,145,155,161,155,150)
y = c(52,59,49,62,72,75,68)
z = c(12,12,13,13,14,13,13)
D = data.frame(x,y,z)

cor.test(x,y)

library(ppcor)
pcor.test(x,y,z)

plot(D$x,D$y,pch=18,xlab="Estatura (cm)",ylab="Puntaje")

library(dplyr)
library(ggplot2)
D %>% 
  ggplot(aes(x=x,y=y))+
  geom_point(size=3,colour="forestgreen")+
  labs(x="Estatura (cm)",
       y="Puntaje")+
  theme_minimal() + 
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=15))

library(corrplot)
D %>% cor %>% corrplot 
D %>% cor %>% corrplot(method="number",type="lower")

library(PerformanceAnalytics)
D %>% chart.Correlation

library(dichromat)
D %>% cor %>% heatmap()
D %>% cor %>% heatmap(col = colorRampPalette(c("red", "orange", "gold"))(20))
D %>% cor %>% heatmap(col = colorRampPalette(c("blue", "skyblue", "white"))(20))
