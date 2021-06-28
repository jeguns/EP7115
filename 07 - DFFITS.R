library(dplyr)
x1 = c(1,4,3,2.5,8,5,3,5.8,6,4,14)
y1 = c(3.1,14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
x1_ = c(4,3,2.5,8,5,3,5.8,6,4,14)
y1_ = c(14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
modelo1 = lm(y1~x1)
modelo1_sin = lm(y1_~x1_)

(modelo1 %>% predict(data.frame(x1=1)))[1] -> y_hat
(modelo1_sin %>% predict(data.frame(x1_=1)))[1] -> y_hat_sin
(modelo1_sin %>% summary)$sigma^2 -> cme_sin
modelo1 %>% hatvalues -> h
(y_hat-y_hat_sin)/(sqrt(cme_sin*(h[1])))

modelo1 %>% rstudent -> rstud
sqrt(h[1]/(1-h[1]))*rstud[1]

modelo1 %>% dffits
2 -> k
length(x1) -> n
modelo1 %>% dffits > 2*sqrt(k/n)
