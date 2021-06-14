

# Paquetes ----------------------------------------------------------------

library(readxl)
library(ggplot2)


# Datos -------------------------------------------------------------------

read_excel("05_Reg_04.xlsx") -> datos4

modelo1 = lm(y~x1+x2+I(x2^2), data = datos4)

# Modelo 1 ----------------------------------------------------------------

datos4 %>% 
  ggplot(aes(x1,y))+
  geom_point()+
  geom_smooth()

datos4 %>% 
  ggplot(aes(x2,y))+
  geom_point()+
  geom_smooth()


# Modelo 2 ----------------------------------------------------------------

datos5 = data.frame(y  = log(datos4$y), 
                    x1 = datos4$x1, 
                    x2 = log(datos4$x2))

modelo2 = lm(y ~ x1 + x2, data = datos5)

# Comparacion -------------------------------------------------------------

modelo1 %>% AIC()
modelo2 %>% AIC()

modelo1 %>% summary
modelo2 %>% summary

# y = 1604.71 + 11.64x1 - 107.91x2 + 6.21x²
# log(y) = 2.447147 + 0.004x1 + 1.706874log(x2)

modelo1 %>% predict
modelo2 %>% predict %>% exp 

plot(datos4$y,modelo1 %>% predict)
plot(datos4$y,modelo2 %>% predict %>% exp)

modelo1 %>% predict(data.frame(x1=15,x2=20))
modelo2 %>% predict(data.frame(x1=15,x2=log(20))) %>% exp 

modelo1 %>% predict(data.frame(x1=15,x2=20), interval = "confidence")
modelo2 %>% predict(data.frame(x1=15,x2=log(20)), interval = "confidence") %>% exp 
