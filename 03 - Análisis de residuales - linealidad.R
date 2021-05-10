
# Paquetes ----------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(broom)
library(car)
library(lmtest)
library(olsrr)

# Datos y modelos ---------------------------------------------------------

datos1  = read_excel("03_Residuales.xlsx")
modelo1 = lm(Alquiler~Area, data = datos1)
modelo1 %>% summary

datos2  = read_excel("03_Residuales_02.xlsx")
modelo2 = lm(Demanda~Precio, data = datos2)
modelo2 %>% summary

datos3  = read.delim("03_Residuales_03.txt")
modelo3 = lm(Gastos~Vasos, data = datos3)
modelo3 %>% summary

# Gráficos de dispersion --------------------------------------------------

plot(datos1$Area,datos1$Alquiler,pch=18)

plot(datos2$Precio,datos2$Demanda,pch=18)

plot(datos3$Vasos,datos3$Gastos,pch=18)


# Linealidad 1 ------------------------------------------------------------

modelo1 %>% plot(which=1)

modelo1 %>% 
  augment %>% 
  with(lowess(x = .fitted, y = .resid)) %>% 
  as.data.frame -> smoothed
modelo1 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  geom_path(data = smoothed, aes(x = x, y = y), col = "red")+
  labs(x = "Valor ajustado",
       y = "Residual", 
       title = "Evaluación de linealidad",
       subtitle = "Modelo 1")+
  theme_minimal()

modelo1 %>% residualPlots
modelo1 %>% crPlots

# Linealidad 2 ------------------------------------------------------------

modelo2 %>% plot(which=1)

modelo2 %>% 
  augment %>% 
  with(lowess(x = .fitted, y = .resid)) %>% 
  as.data.frame -> smoothed
modelo2 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  geom_path(data = smoothed, aes(x = x, y = y), col = "red")+
  labs(x = "Valor ajustado",
       y = "Residual", 
       title = "Evaluación de linealidad",
       subtitle = "Modelo 2")+
  theme_minimal()

modelo2 %>% residualPlots
modelo2 %>% crPlots


# Linealidad 3 ------------------------------------------------------------

modelo3%>% 
  augment %>% 
  with(lowess(x = .fitted, y = .resid)) %>% 
  as.data.frame -> smoothed
modelo3 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  geom_path(data = smoothed, aes(x = x, y = y), col = "red")+
  labs(x = "Valor ajustado",
       y = "Residual", 
       title = "Evaluación de linealidad",
       subtitle = "Modelo 3")+
  theme_minimal()

modelo3 %>% residualPlots
modelo3 %>% crPlots


library(gvlma)
gvmodel <- gvlma(modelo1)
summary(gvmodel)

modelo1 %>% gvlma 
modelo2 %>% gvlma
modelo3 %>% gvlma

modelo3x = lm(Gastos[1:97]~Vasos[1:97], data = datos3)
modelo3x %>% gvlma
