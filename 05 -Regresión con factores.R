
# Use alpha = 0.10 para todas las pruebas de hipótesis

# Paquetes ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(forcats)
library(gmodels)
library(lmtest)
library(car)

# Un factor dicotómico ----------------------------------------------------

read_excel("05_Reg_01.xlsx") -> datos1

datos1 %>% 
  mutate(Turno = factor(Turno, levels = c("Mañana","Noche"))) -> datos1

lm(Nota ~ PC1 + Turno, data = datos1) -> modelo1

modelo1 %>% model.matrix()
modelo1 %>% summary
modelo1 %>% vif

# Un factor dicotómico ----------------------------------------------------

read_excel("05_Reg_02.xlsx") -> datos2

datos2 %>% 
  mutate(Turno = factor(Turno, levels = c("Mañana","Tarde","Noche"))) -> datos2

lm(Nota ~ PC1 + Turno, data = datos2) -> modelo2

modelo2 %>% model.matrix

lm(Nota ~ PC1, data = datos2) -> modelo2_
anova(modelo2_,modelo2)
modelo2 %>% lrtest("Turno")

modelo2 %>% summary
cons = rbind(c(0,0,1,-1))
ret  = glh.test(modelo2, cons)
ret
ret %>% summary

modelo2 %>% vif

datos2 %>% 
  mutate(Turno = fct_collapse(Turno, 
                              "MañanaTarde" = c("Mañana","Tarde"))) -> datos2a
lm(Nota ~ PC1 + Turno, data = datos2a) -> modelo2a
modelo2a %>% summary

modelo2a %>% vif

# Interacción -------------------------------------------------------------

read_excel("05_Reg_03.xlsx") -> datos3

datos3 %>% 
  mutate(Turno = factor(Turno, levels = c("MT","Noche"))) -> datos3

lm(Nota ~ PC1 * Turno, data = datos3) -> modelo3
modelo3 %>% summary
modelo3 %>% vif

lm(Nota ~ PC1 + Turno, data = datos3) -> modelo3a
modelo3a %>% summary
modelo3a %>% vif

