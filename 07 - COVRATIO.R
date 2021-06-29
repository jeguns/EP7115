library(dplyr)
x1 = c(1,4,3,2.5,8,5,3,5.8,6,4,14)
y1 = c(3.1,14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
x1_ = c(4,3,2.5,8,5,3,5.8,6,4,14)
y1_ = c(14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
modelo1 = lm(y1~x1)
modelo1_sin = lm(y1_~x1_)

(modelo1 %>% summary)$sigma^2 -> cme
(modelo1_sin %>% summary)$sigma^2 -> cme_sin
modelo1 %>% hatvalues -> h
(cme_sin/cme)^2*(1/(1-h[1]))

modelo1 %>% covratio()

2 -> k
length(x1) -> n
n > 3*k
modelo1 %>% covratio() > 1+3*k/n 
modelo1 %>% covratio() < 1-3*k/n
modelo1 %>% covratio() > 1+3*k/n | modelo1 %>% covratio() < 1-3*k/n

modelo1 %>% influence.measures()
