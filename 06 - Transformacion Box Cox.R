

# Paquetes ----------------------------------------------------------------

library(datasets)
library(dplyr)
library(ggplot2)
library(skimr)
library(broom)
library(car) # función spreadLevelPlot, ncvTest
library(lmtest) # función bptest
library(olsrr) # función ols_test_breusch_pagan
library(MASS)
library(forecast)


# Datos -------------------------------------------------------------------

warpbreaks -> datos
datos %>% skim()

boxplot(datos$breaks~datos$wool)

boxplot(datos$breaks~datos$tension)

# Modelo inicial ----------------------------------------------------------

lm(breaks ~ wool + tension, data = datos) -> modelo_0

modelo_0 %>% plot(which=1)

modelo_0 %>% 
  augment %>% 
  ggplot(aes(x=wool,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "Wool",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 1")+
  theme_minimal()

modelo_0 %>% spreadLevelPlot
modelo_0 %>% residuals %>% shapiro.test
modelo_0 %>% ols_test_breusch_pagan
modelo_0 %>% bptest()


# Transformación Box Cox --------------------------------------------------

modelo_0 %>% boxcox()
modelo_0 %>% boxcox(lambda = seq(-1,1, by = 0.1))
modelo_0 %>% boxcox(plotit = F)
(data.frame(x=boxcox(modelo_0,plotit = F)$x,
            y=boxcox(modelo_0,plotit = F)$y) %>% 
    arrange(-y) %>% 
    top_n((1)) %>% 
    dplyr::select(x) %>% 
    as.numeric() -> lambda)

# Modelo propuesto 1 ------------------------------------------------------

BoxCox(datos$breaks, lambda) -> ynueva

(datos$breaks^lambda - 1)/lambda

lm(ynueva ~ wool + tension, data = datos) -> modelo_1

modelo_1 %>% plot(which=1)

modelo_1 %>% 
  augment %>% 
  ggplot(aes(x=wool,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "Wool",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 1")+
  theme_minimal()

modelo_1 %>% spreadLevelPlot
modelo_1 %>% residuals %>% shapiro.test
modelo_1 %>% ols_test_breusch_pagan
modelo_1 %>% bptest()

# Modelo propuesto 2 ------------------------------------------------------

log(datos$breaks) -> ynueva2

lm(ynueva2 ~ wool + tension, data = datos) -> modelo_2

modelo_2 %>% plot(which=1)

modelo_2 %>% 
  augment %>% 
  ggplot(aes(x=wool,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "Wool",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 1")+
  theme_minimal()

modelo_2 %>% spreadLevelPlot
modelo_2 %>% residuals %>% shapiro.test
modelo_2 %>% ols_test_breusch_pagan
modelo_2 %>% bptest()

# Comparando --------------------------------------------------------------

modelo_0 %>% summary
modelo_1 %>% summary
modelo_2 %>% summary

ypred0 = modelo_0 %>% predict
ypred1 = ((modelo_1 %>% predict)*lambda+1)^(1/lambda)
ypred2 = exp(modelo_2 %>% predict)

plot(datos$breaks,ypred0,pch=18)
plot(datos$breaks,ypred1,pch=18)
plot(datos$breaks,ypred2,pch=18)

modelo_0 %>% AIC
modelo_1 %>% AIC
modelo_2 %>% AIC

modelo_0 %>% BIC
modelo_1 %>% BIC
modelo_2 %>% BIC



# Modelo adecuado ---------------------------------------------------------

glm(breaks ~ wool + tension, family = poisson(link="log"), data = datos) -> modelo_g
summary(modelo_g)
