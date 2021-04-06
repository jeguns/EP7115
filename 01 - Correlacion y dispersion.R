x = c(145,141,145,155,161,155,150)
y = c(52,59,49,62,72,75,68)
z = c(12,12,13,13,14,13,13)
D = data.frame(x,y,z)

cor.test(x,y)

library(ppcor)
# H0: correl. parcial = 0
# H1: correl. parcial != 0
# alpha = 0.10
pcor.test(x,y,z)

# Correlación parcial = corr(x,y|z) ~ corr(x,y)/(corr(x,z),corr(y,z))

# Edad ~ Estatura ←
# Edad ~ Puntaje en la prueba de conocimientos ←
# Estatura ~ Puntaje

plot(D$x,D$y,pch=18,xlab="Estatura (cm)",ylab="Puntaje")

library(dplyr)
library(ggplot2)

### objeto %>% funcion es lo mismo que funcion(objeto)

### objeto %>% funcion1 %>% funcion2 %>% funcion3 es lo mismo que
### funcion3(funcion2(funcion1(objeto)))

# ggplot(D,aes(x=x,y=y))

D %>% 
  ggplot(aes(x=x,y=y))+
  geom_point(size=3,colour="forestgreen")+
  labs(x="Estatura (cm)",y="Puntaje")+
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

library(skimr)
skim(D)
library(psych)
describe(D)
