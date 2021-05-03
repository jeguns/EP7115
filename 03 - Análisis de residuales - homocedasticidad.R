

# Paquetes ----------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
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


# Residuales --------------------------------------------------------------

modelo1 %>% residuals %>% data.frame -> residuales1

modelo2 %>% residuals %>% data.frame -> residuales2

modelo3 %>% residuals %>% data.frame -> residuales3

# Homocedasticidad 1 ------------------------------------------------------

modelo1 %>% plot(which=1)

library(broom)
modelo1 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "valor ajustado",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 1")+
  theme_minimal()

modelo1 %>% 
  augment %>% 
  ggplot(aes(x=Area,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "Área del local",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 1")+
  theme_minimal()

modelo1 %>% ncvTest
modelo1 %>% ols_test_breusch_pagan
modelo1 %>% ols_test_score
modelo1 %>% ols_test_f

# Homocedasticidad 2 ------------------------------------------------------

modelo2 %>% plot(which=1)

modelo2 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "valor ajustado",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 2")+
  theme_minimal()

modelo2 %>% 
  augment %>% 
  ggplot(aes(x=Precio,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "Precio del producto",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 2")+
  theme_minimal()

modelo2 %>% ncvTest
modelo2 %>% ols_test_breusch_pagan
modelo2 %>% ols_test_score
modelo2 %>% ols_test_f

# Homocedasticidad 3 ------------------------------------------------------

modelo3 %>% plot(which=1)

modelo3 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "valor ajustado",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 3")+
  theme_minimal()

modelo3 %>% 
  augment %>% 
  ggplot(aes(x=Vasos,y=.resid))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0)+
  labs(x = "Gasto",
       y = "Residual", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 3")+
  theme_minimal()

modelo3 %>% ncvTest
modelo3 %>% ols_test_breusch_pagan
modelo3 %>% ols_test_score
modelo3 %>% ols_test_f




