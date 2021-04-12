

# Lectura de datos --------------------------------------------------------

read.table('01_Reg_01.txt',T) -> datos
attach(datos)
lm(Rendimiento ~ Ozono) -> modelo


# Estimación puntual ------------------------------------------------------

modelo
modelo$coefficients
library(dplyr)
modelo %>% coef()

# Estimación intervalar ---------------------------------------------------

modelo %>% summary
library(broom)
modelo %>% tidy
modelo %>% confint
modelo %>% confint(level = 0.99)

# Análisis de varianza ----------------------------------------------------

modelo %>% aov
modelo %>% aov %>% summary

# Lectura de datos --------------------------------------------------------

library(readxl)
read_xlsx('01_Reg_02.xlsx') -> datos2
lm(Rendimiento ~ Ozono, data = datos2) -> modelo2

