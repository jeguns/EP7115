

# Lectura de datos --------------------------------------------------------

read.table('01_Reg_01.txt',T) -> datos
attach(datos)
lm(Rendimiento ~ Ozono) -> modelo

# Estimación puntual ------------------------------------------------------

modelo
modelo$coefficients
library(dplyr)
modelo %>% coef()
coef(modelo) # esta línea ejecuta lo mismo que la línea previa

# Estimación intervalar ---------------------------------------------------

modelo %>% summary
253.43-qt(0.95,4-2)*10.77
253.43+qt(0.95,4-2)*10.77

library(broom)
modelo %>% tidy
modelo %>% confint
modelo %>% confint(level = 0.90)
modelo %>% confint(level = 0.95)

modelo %>% vcov
modelo %>% vcov %>% diag %>% sqrt

# Análisis de varianza ----------------------------------------------------

modelo %>% aov
modelo %>% aov %>% summary

# Estimación de la variabilidad -------------------------------------------

summary(modelo)$sigma
summary(modelo)$sigma**2

(LI.var = sum(modelo$residuals^2)/qchisq(0.95,df=2))
(LS.var = sum(modelo$residuals^2)/qchisq(0.05,df=2))
(LI.sd = sqrt(LI.var))
(LS.sd = sqrt(LS.var))


# Prueba de hipótesis -----------------------------------------------------

(modelo %>% tidy -> resultados)
((resultados[2,2]-(-300))/resultados[2,3] -> tcalc)
(qt(0.05,2) -> tcrit)


# Ajuste del modelo -------------------------------------------------------

summary(modelo)$r.squared
summary(modelo)$adj.r.squared

(summary(modelo)$r.squared -> r2)
(4:100 -> n)
(1-((1-r2)*(n-1))/(n-1-1) -> r2aj)
plot(n,r2aj,pch=18)
abline(h=r2,col="red",lwd=2)

modelo %>% AIC
library(qpcR) # AICc
library(minpack.lm)
modelo %>% AICc()

(modelo %>% AIC -> aic)
(4:100 -> n)
(aic + (2*2^2+2*2)/(n-2-1) -> aicc)
plot(n,aicc,pch=18)
abline(h=aic,col="red",lwd=2)

# Estimación --------------------------------------------------------------

modelo %>% fitted
modelo %>% predict
modelo %>% predict(interval = "confidence", # IC para la media estimada
                   level = 0.99)
modelo %>% predict(interval = "confidence", # IC para la media estimada
                   level = 0.99) %>% 
  data.frame() %>% 
  mutate(dif=upr-lwr)

library(ggplot2)
datos %>% 
  ggplot(aes(Ozono, Rendimiento)) +
  geom_point(size=5) +
  geom_smooth(method = "lm", se = T, fullrange = T)+
  scale_x_continuous(limits=c(0,0.3))+
  theme_minimal()

modelo %>% predict(data.frame(Ozono=0.13),
                   interval = "confidence",
                   level = 0.99)
modelo %>% predict(data.frame(Ozono=c(0.05,0.15,0.25)),
                   interval = "confidence",
                   level = 0.95)



# Predicción --------------------------------------------------------------

modelo %>% predict(data.frame(Ozono=0.13))
modelo %>% predict(data.frame(Ozono=c(0.05,0.15,0.25)))
modelo %>% predict(data.frame(Ozono=0.13),
                   interval = "prediction",
                   level = 0.99)
modelo %>% predict(data.frame(Ozono=c(0.05,0.15,0.25)),
                   interval = "prediction",
                   level = 0.95)

# Residuales --------------------------------------------------------------

modelo %>% residuals
modelo %>% residuals %>% mean
modelo %>% residuals %>% plot(type="b",pch=18)
abline(h=0, col="red")

# Lectura de datos --------------------------------------------------------

library(readxl)
read_xlsx('01_Reg_02.xlsx') -> datos2
lm(Rendimiento ~ Ozono, data = datos2) -> modelo2


