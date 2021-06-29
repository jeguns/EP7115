library(dplyr)
x1 = c(1,4,3,2.5,8,5,3,5.8,6,4,14)
y1 = c(3.1,14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
x1_ = c(4,3,2.5,8,5,3,5.8,6,4,14)
y1_ = c(14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
plot(x1,y1,pch=18)
plot(x1_,y1_,pch=18)
modelo1 = lm(y1~x1)
modelo1_sin = lm(y1_~x1_)
modelo1 %>% coef -> beta
modelo1_sin %>% coef -> beta_sin
modelo1 %>% model.matrix -> X
(modelo1 %>% sigma)**2 -> cme
2 -> k
(t(beta-beta_sin)%*%t(X)%*%X%*%(beta-beta_sin))/(k*cme)

modelo1 %>% hatvalues -> h1
modelo1 %>% rstandard -> rsta1
(rsta1[1]**2*h1[1])/(k1*(1-h1[1]))
(rsta1**2*h1)/(k1*(1-h1))

modelo1 %>% cooks.distance()

modelo1 %>% influence.measures()

modelo1 %>% plot(which=4)
modelo1 %>% plot(which=5)

pf(3.55,6,19)
modelo1 %>% cooks.distance %>% pf(2,11-2)

x2 = c(1,4,3,2.5,8,5,3,5.8,6,4,14)
y2 = c(3.1,14,8.8,10,10,15.4,9.6,15,19.1,10,15)
plot(x2,y2,pch=18)
options(scipen=999)
modelo2 %>% cooks.distance()
modelo2 %>% influence.measures()
modelo2 %>% plot(which=4)
modelo2 %>% plot(which=5)
modelo2 %>% cooks.distance %>% pf(2,11-2)

