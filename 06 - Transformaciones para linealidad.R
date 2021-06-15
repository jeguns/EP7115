

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

datos6 = data.frame(y  = log(datos4$y), 
                    x1 = datos4$x1, 
                    x2 = datos4$x2)

modelo3 = lm(y ~ x1 + x2, data = datos6)


# Comparacion -------------------------------------------------------------

modelo1 %>% AIC()
modelo2 %>% AIC()
modelo3 %>% AIC()

modelo1 %>% summary # R²AJ = 0.948
# y = 1604.71 + 11.64x1 - 107.91x2 + 6.21x²
# yobs vs ypred → R²pred

modelo2 %>% summary # R²AJ = 0.942
# log(y) = 2.447147 + 0.004x1 + 1.706874log(x2)
# yobs vs exp(log(ypred)) → R²pred

modelo3 %>% summary # R²AJ = 0.945
# log(y) = 6.209853 + 0.004x1 + 0.068764x2
# yobs vs exp(log(ypred)) → R²pred

x11();par(mfrow=c(2,2));modelo1 %>% plot
x11();par(mfrow=c(2,2));modelo2 %>% plot
x11();par(mfrow=c(2,2));modelo3 %>% plot ### ELEGIRÍA

modelo1 %>% predict
modelo2 %>% predict %>% exp 
modelo3 %>% predict %>% exp

modelo1 %>% predict(interval="confidence")
modelo2 %>% predict(interval="confidence") %>% exp 
modelo3 %>% predict(interval="confidence") %>% exp

plot(datos4$y,modelo1 %>% predict)
plot(datos4$y,modelo2 %>% predict %>% exp)
plot(datos4$y,modelo3 %>% predict %>% exp)

modelo1 %>% predict(data.frame(x1=15,x2=20))
modelo2 %>% predict(data.frame(x1=15,x2=log(20))) %>% exp 
modelo3 %>% predict(data.frame(x1=15,x2=20)) %>% exp

modelo3 %>% predict(data.frame(x1=15,x2=20),
                    interval = "confidence",
                    level = 0.90) %>% exp

modelo2 %>% confint
