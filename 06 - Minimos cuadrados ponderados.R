
# Paquetes ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)

# Datos -------------------------------------------------------------------

read_excel("06_casos.xlsx") -> datos

datos %>% skim()
datos %>% ggplot(aes(x=Dias,y=Estudiantes))+geom_point()+theme_minimal()
  
# Modelo sin pesos --------------------------------------------------------

lm(Estudiantes ~ Dias, data = datos) -> modelo0

x11();par(mfrow=c(2,2));plot(modelo0)
modelo0 %>% residuals %>% shapiro.test
modelo0 %>% spreadLevelPlot
modelo0 %>% bptest()

modelo0 %>% 
  augment %>% 
    ggplot(aes(x=Dias,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "Dias transcurridos",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 0")+
  theme_minimal()


# Modelos con pesos -------------------------------------------------------

# Caso 1:
1 / lm(abs(modelo0$residuals) ~ modelo0$fitted.values)$fitted.values^2 -> peso1
lm(Estudiantes ~ Dias, data = datos, weights = peso1) -> modeloA

# Caso 2: var(e) = sigma2/dias = V, pero nosotros definimos W=V^-1 
datos$Dias -> peso2
lm(Estudiantes ~ Dias, data = datos, weights = peso2) -> modeloB

# Comparacion -------------------------------------------------------------

modelo0 %>% summary
modeloA %>% summary
modeloB %>% summary

modelo0 %>% AIC
modeloA %>% AIC
modeloB %>% AIC

modelo0 %>% BIC
modeloA %>% BIC
modeloB %>% BIC

modelo0 %>% residuals %>% shapiro.test
modeloA %>% residuals %>% shapiro.test
modeloB %>% residuals %>% shapiro.test

modelo0 %>% bptest()
modeloA %>% bptest()
modeloB %>% bptest()

modelo0 %>% ols_test_breusch_pagan
modeloA %>% ols_test_breusch_pagan
modeloB %>% ols_test_breusch_pagan

modelo0 %>% plot(which=1)
modelo1 %>% plot(which=1)
modelo2 %>% plot(which=1)

modelo0 %>% plot(which=3)
modelo1 %>% plot(which=3)
modelo2 %>% plot(which=3)

modelo0 %>% residuals %>% TSA::acf()
modelo1 %>% residuals %>% TSA::acf()
modelo2 %>% residuals %>% TSA::acf()

data.frame(y = rstandard(modelo0),
           x = modelo1$fitted.values) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  #geom_line()+
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")

data.frame(y = rstandard(modelo1),
           x = modelo2$fitted.values) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  #geom_line()+
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")

data.frame(y = rstandard(modelo2),
           x = modelo2$fitted.values) %>%
  ggplot(aes(x = x, y = y)) + 
  #geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")



