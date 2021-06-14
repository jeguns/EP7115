
# Paquetes ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(skimr)

# Datos -------------------------------------------------------------------

read_excel("06_scores.xlsx") -> datos

datos %>% skim()
datos %>% ggplot(aes(x=horas,y=score))+geom_point()+theme_minimal()

# Modelo sin pesos --------------------------------------------------------

modelo0 = lm(score ~ horas, data = datos)

x11();par(mfrow=c(2,2));plot(modelo0)
modelo0 %>% residuals %>% shapiro.test
modelo0 %>% spreadLevelPlot
modelo0 %>% bptest()

modelo0 %>% 
  augment %>% 
  ggplot(aes(x=horas,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "Dias transcurridos",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 0")+
  theme_minimal()

1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2 -> peso1
lm(score ~ horas, data = datos, weights=peso1) -> modelo1

datos$horas -> peso2
lm(score ~ horas, data = datos, weights=peso2) -> modelo2

# Comparación -------------------------------------------------------------

modelo0 %>% summary
modelo1 %>% summary
modelo2 %>% summary

modelo0 %>% AIC
modelo1 %>% AIC
modelo2 %>% AIC

modelo0 %>% BIC
modelo1 %>% BIC
modelo2 %>% BIC

modelo0 %>% residuals %>% shapiro.test
modelo1 %>% residuals %>% shapiro.test
modelo2 %>% residuals %>% shapiro.test

modelo0 %>% bptest()
modelo1 %>% bptest()
modelo2 %>% bptest()

modelo0 %>% plot(which=1)
modelo1 %>% plot(which=1)
modelo2 %>% plot(which=1)

modelo0 %>% plot(which=3)
modelo1 %>% plot(which=3)
modelo2 %>% plot(which=3)

data.frame(y = rstandard(modelo0),
           x = modelo1$fitted.values) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")

data.frame(y = rstandard(modelo1),
           x = modelo2$fitted.values) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")

data.frame(y = rstandard(modelo2),
           x = modelo2$fitted.values) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")







