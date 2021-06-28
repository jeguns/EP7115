library(dplyr)
x1 = c(1,4,3,2.5,8,5,3,5.8,6,4,14)
y1 = c(3.1,14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
x1_ = c(4,3,2.5,8,5,3,5.8,6,4,14)
y1_ = c(14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
modelo1 = lm(y1~x1)
modelo1_sin = lm(y1_~x1_)

(modelo1 %>% coef)[1] -> beta0
(modelo1_sin %>% coef)[1] -> beta0_sin
(modelo1 %>% coef)[2] -> beta1
(modelo1_sin %>% coef)[2] -> beta1_sin

c(1,1) -> x
residuals(modelo) -> r
(solve(t(X)%*%X)%*%x%*%r[1])/(1-as.numeric(x%*%solve(t(X)%*%X)%*%x))
beta0 - beta0_sin
beta1 - beta1_sin
modelo1 %>% dfbeta

(modelo1_sin %>% summary)$sigma^2 -> cme_sin
modelo1 %>% model.matrix -> X
solve(t(X)%*%X) -> C
(beta0-beta0_sin)/sqrt(cme_sin*C[1,1])
(beta1-beta1_sin)/sqrt(cme_sin*C[2,2])

modelo1 %>% dfbetas
modelo1 %>% influence.measures()

modelo1 %>% dfbetas %>% data.frame %>% 
  rename(dfbeta0=1,dfbeta1=2) %>% abs
modelo1 %>% dfbetas %>% data.frame %>% 
  rename(dfbeta0=1,dfbeta1=2) %>% abs > 2/sqrt(length(x1))

