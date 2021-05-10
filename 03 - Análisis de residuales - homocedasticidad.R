
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

datos1 %>% 
  ggplot(aes(x = Area, y = Alquiler)) +
  geom_point(size = 4) +
  geom_smooth(size = 3, method = "lm", se = F)+
  theme_minimal()

datos2 %>% 
  ggplot(aes(x = Precio, y = Demanda)) +
  geom_point(size = 4) +
  geom_smooth(size = 3, method = "lm", se = F)+
  theme_minimal()

datos3 %>% 
  ggplot(aes(x = Vasos, y = Gastos)) +
  geom_point(size = 4) +
  geom_smooth(size = 3, method = "lm", se = F)+
  theme_minimal()

# Residuales --------------------------------------------------------------

modelo1 %>% residuals %>% data.frame -> residuales1

modelo2 %>% residuals %>% data.frame -> residuales2

modelo3 %>% residuals %>% data.frame -> residuales3

# Homocedasticidad 1 ------------------------------------------------------

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

modelo1 %>% plot(which=3)

modelo1 %>% spreadLevelPlot

modelo1 %>% 
  augment %>% 
  with(lowess(x = .fitted, y = sqrt(abs(.std.resid)))) %>% 
  as.data.frame -> smoothed
modelo1 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=sqrt(abs(.std.resid))))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0, linetype = "dotted", col = "grey")+
  geom_path(data = smoothed, aes(x = x, y = y), col = "red")+
  labs(x = "valor ajustado",
       y = "raíz cuadrada del valor absoluto del residual estudentizado", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 1")+
  theme_minimal()

modelo1 %>% ncvTest
modelo1 %>% ols_test_breusch_pagan
modelo1 %>% bptest
modelo1 %>% bptest(studentize = F)

# Homocedasticidad 2 ------------------------------------------------------

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

modelo2 %>% plot(which=3)

modelo2 %>% spreadLevelPlot

modelo2 %>% 
  augment %>% 
  with(lowess(x = .fitted, y = sqrt(abs(.std.resid)))) %>% 
  as.data.frame -> smoothed
modelo2 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=sqrt(abs(.std.resid))))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0, linetype = "dotted", col = "grey")+
  geom_path(data = smoothed, aes(x = x, y = y), col = "red")+
  labs(x = "valor ajustado",
       y = "raíz cuadrada del valor absoluto del residual estudentizado", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 2")+
  theme_minimal()

modelo2 %>% ncvTest
modelo2 %>% ols_test_breusch_pagan
modelo2 %>% bptest
modelo2 %>% bptest(studentize = F)

# Homocedasticidad 3 ------------------------------------------------------

modelo3 %>% plot(which=1)

modelo3 %>% 
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

modelo3 %>% plot(which=3)

modelo3 %>% spreadLevelPlot

modelo3 %>% 
  augment %>% 
  with(lowess(x = .fitted, y = sqrt(abs(.std.resid)))) %>% 
  as.data.frame -> smoothed

modelo3 %>% 
  augment %>% 
  ggplot(aes(x=.fitted,y=sqrt(abs(.std.resid))))+
  geom_point(size = 3) + 
  geom_hline(yintercept=0, linetype = "dotted", col = "grey")+
  geom_path(data = smoothed, aes(x = x, y = y), col = "red")+
  labs(x = "valor ajustado",
       y = "raíz cuadrada del valor absoluto del residual estudentizado", 
       title = "Evaluación de homocedasticidad",
       subtitle = "Modelo 2")+
  theme_minimal()

modelo3 %>% ncvTest
modelo3 %>% ols_test_breusch_pagan
modelo3 %>% bptest(studentize = F)
modelo3 %>% bptest(studentize = T)

modelo3 %>% residuals -> e
lm(e^2 ~ Vasos, data = datos3) -> modbp
nrow(datos3) -> n
n/(2*(n-1)) * var(e^2)/(var(e))^2 -> lambda

(Studentized_bp = n * summary(modbp)$r.squared)
(Original_bp = Studentized_bp * lambda)

