

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

# Lectura de datos --------------------------------------------------------

library(readxl)
read_xlsx('01_Reg_02.xlsx') -> datos2
lm(Rendimiento ~ Ozono, data = datos2) -> modelo2


