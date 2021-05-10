
# Paquetes ----------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(broom)
library(ggfortify)
library(lmtest)
library(car)

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

# Independencia 1 ---------------------------------------------------------

modelo1 %>% 
  augment %>% 
  ggplot(aes(x=1:nrow(datos1),y=.resid))+
  geom_point(size = 3) +
  geom_line()+
  geom_hline(yintercept=0)+
  labs(x = "Orden",
       y = "Residual", 
       title = "Evaluación de independencia",
       subtitle = "Modelo 1")+
  theme_minimal()

modelo1 %>% 
  augment %>% 
  select(.resid) %>% 
  TSA::acf(lag = 15, plot=F) %>% 
  autoplot +
  labs(x = "Desfase",
       y = "Autocorrelación") + 
  theme_minimal()

modelo1 %>% dwtest(alternative = "two.sided")

modelo1 %>% durbinWatsonTest(alternative = "two.sided",max.lag=10)

# Independencia 2 ---------------------------------------------------------

modelo2 %>% 
  augment %>% 
  ggplot(aes(x=1:nrow(datos2),y=.resid))+
  geom_point(size = 3) +
  geom_line()+
  geom_hline(yintercept=0)+
  labs(x = "Orden",
       y = "Residual", 
       title = "Evaluación de independencia",
       subtitle = "Modelo 2")+
  theme_minimal()

modelo2 %>% 
  augment %>% 
  select(.resid) %>% 
  TSA::acf(lag = 15, plot=F) %>% 
  autoplot +
  labs(x = "Desfase",
       y = "Autocorrelación") + 
  theme_minimal()

modelo2 %>% dwtest(alternative = "two.sided")

modelo2 %>% durbinWatsonTest(alternative = "two.sided",max.lag=10)

# Independencia 3 ---------------------------------------------------------

modelo3 %>% 
  augment %>% 
  ggplot(aes(x=1:nrow(datos3),y=.resid))+
  geom_point(size = 3) +
  geom_line()+
  geom_hline(yintercept=0)+
  labs(x = "Orden",
       y = "Residual", 
       title = "Evaluación de independencia",
       subtitle = "Modelo 2")+
  theme_minimal()

modelo3 %>% 
  augment %>% 
  select(.resid) %>% 
  TSA::acf(lag = 15, plot=F) %>% 
  autoplot +
  labs(x = "Desfase",
       y = "Autocorrelación") + 
  theme_minimal()

modelo3 %>% dwtest(alternative = "two.sided")

modelo3 %>% durbinWatsonTest(alternative = "two.sided",max.lag=10)

