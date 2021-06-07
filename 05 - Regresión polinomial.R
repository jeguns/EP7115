


# Paquetes ----------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(broom)
library(car)
library(lmtest)
library(olsrr)
library(gvlma)
library(MuMIn)
library(DAAG)
library(npreg)

# Regresión polinómica ----------------------------------------------------

x = c(4,3.1,5.5,6,7.6,8,3.3,5,2,7,4.5)
y = c(24.74,16.83,40.52,47.56,73.15,80.37,16.55,35.82,8.35,62.56,29.86)
lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9)+I(x^10)-1) %>% summary
lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9)+I(x^10)-1) %>% model.matrix -> X
solve(t(x)%*%X)
lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9)+I(x^10)-1) %>% vif


data.frame(x,y) %>% 
  ggplot(aes(x,y))+
  geom_point()+
  geom_smooth(method="lm", se=F, color = "darkorange",
              formula = y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9)+I(x^10)-1,) +
  geom_smooth(method="lm", se=F, color = "forestgreen",
              formula = y~x) +
  theme_minimal()

lm(y~x) -> modelo1
lm(y~x+I(x^2)) -> modelo2
lm(y~I(x^2)) -> modelo3

modelo1 %>% residualPlots
modelo1 %>% crPlots

###   

read_excel("05_Reg_04.xlsx") -> datos4

lm(y~.,data=datos4) -> modelo0
summary(modelo0)
plot(datos4$x1,datos4$y,pch=18)
plot(datos4$x2,datos4$y,pch=18)
modelo0 %>% plot(which=1)
modelo0 %>% residualPlots
modelo0 %>% crPlots
modelo0 %>% gvlma() 
modelo0 %>% AICc
summary(modelo0)$adj.r.squared

lm(y~x1+x2+I(x2^2),data=datos4) -> modelo1
summary(modelo1)
modelo1 %>% plot(which=1)
modelo1 %>% residualPlots
modelo1 %>% crPlots
modelo1 %>% gvlma() 
modelo1 %>% AICc
summary(modelo1)$adj.r.squared

lm(y~x1+I(x2^2),data=datos4) -> modelo2
summary(modelo2)
modelo2 %>% plot(which=1)
modelo2 %>% residualPlots
modelo2 %>% crPlots
modelo2 %>% gvlma() 
modelo2 %>% AICc
summary(modelo2)$adj.r.squared

lm(y~I(x2^2),data=datos4) -> modelo3
modelo3 %>% summary
modelo3 %>% plot(which=1)
modelo3 %>% residualPlots
modelo3 %>% crPlots
modelo3 %>% gvlma() 
modelo3 %>% AICc
summary(modelo3)$adj.r.squared


lm(y~x2,data=datos4) -> modelo4
modelo4 %>% summary
modelo4 %>% plot(which=1)
modelo4 %>% residualPlots
modelo4 %>% crPlots
modelo4 %>% gvlma() 
modelo4 %>% AICc
summary(modelo4)$adj.r.squared

lm(y~x1,data=datos4) -> modelo5
modelo5 %>% summary
modelo5 %>% plot(which=1)
modelo5 %>% residualPlots
modelo5 %>% crPlots
modelo5 %>% gvlma() 
modelo5 %>% AICc
summary(modelo5)$adj.r.squared

modelo0 %>% formula
modelo1 %>% formula
modelo2 %>% formula
modelo3 %>% formula
modelo4 %>% formula
modelo5 %>% formula

summary(modelo1)$adj.r.squared
summary(modelo2)$adj.r.squared
summary(modelo3)$adj.r.squared
summary(modelo4)$adj.r.squared
summary(modelo5)$adj.r.squared

datos4 %>% 
  mutate(xx = x2**2)->datos5

DAAG::cv.lm(data=data.frame(datos4), form.lm = formula(y ~ x1+x2), m=3) -> vc.modelo0
DAAG::cv.lm(data=data.frame(datos4), form.lm = formula(y ~ x1+x2+I(x2^2)), m=3)-> vc.modelo1
DAAG::cv.lm(data=data.frame(datos4), form.lm = formula(y ~ x1+I(x2^2)), m=3)-> vc.modelo2a
DAAG::cv.lm(data=data.frame(datos5), form.lm = formula(y ~ x1+xx), m=3)-> vc.modelo2b
DAAG::cv.lm(data=data.frame(datos4), form.lm = formula(y ~ I(x2^2)), m=3)-> vc.modelo3a
DAAG::cv.lm(data=data.frame(datos5), form.lm = formula(y ~ xx), m=3)-> vc.modelo3b
DAAG::cv.lm(data=data.frame(datos4), form.lm = formula(y ~ x2), m=3)-> vc.modelo4
DAAG::cv.lm(data=data.frame(datos4), form.lm = formula(y ~ x1), m=3)-> vc.modelo5

mean((vc.modelo0$y-vc.modelo0$cvpred)^2)
mean((vc.modelo1$y-vc.modelo1$cvpred)^2)
mean((vc.modelo2a$y-vc.modelo2a$cvpred)^2)
mean((vc.modelo2b$y-vc.modelo2b$cvpred)^2)
mean((vc.modelo3b$y-vc.modelo3b$cvpred)^2)
mean((vc.modelo4$y-vc.modelo4$cvpred)^2)
mean((vc.modelo5$y-vc.modelo5$cvpred)^2)


# Splines -----------------------------------------------------------------

read_excel("05_Reg_05.xlsx") -> datos5
datos5$x -> x
datos5$y -> y

plot(x,y,pch=18)

mod.sp1 <- ss(x,y, all.knots = TRUE, lambda = 1e-10)
mod.sp1
mod.sp1$fit$knot
mod.sp1$fit$coef
mod.sp1$sigma

mod.sp2 <- ss(x,y, all.knots = TRUE)
mod.sp2

mod.sp3 <- ss(x,y, all.knots = TRUE, lambda = 50)
mod.sp3

mod.sp4 <- ss(x,y, nknots = 3)
mod.sp4

mod.sp5 <- ss(x,y, nknots = 10)
mod.sp5

mod.sp6 <- ss(x,y, nknots = 50)
mod.sp6

mod.sp7 <- ss(x,y, m = 1)
mod.sp7

mod.sp8 <- ss(x,y)
mod.sp8

mod.sp9 <- ss(x,y, m = 3)
mod.sp9

par(mfrow=c(3,3))
plot(mod.sp1, ylim = range(y))
points(x,y,pch=18)
plot(mod.sp2, ylim = range(y))
points(x,y,pch=18)
plot(mod.sp3, ylim = range(y))
points(x,y,pch=18)
plot(mod.sp4, ylim = range(y))
points(x,y,pch=18)
plot(mod.sp5, ylim = range(y))
points(x,y,pch=18)
plot(mod.sp6, ylim = range(y))
points(x,y,pch=18)
plot(mod.sp7, ylim = range(y))
points(x,y,pch=18)
plot(mod.sp8, ylim = range(y))
points(x,y,pch=18)
plot(mod.sp9, ylim = range(y))
points(x,y,pch=18)

summary(mod.sp1)$adj.r.squared
summary(mod.sp2)$adj.r.squared
summary(mod.sp3)$adj.r.squared
summary(mod.sp4)$adj.r.squared
summary(mod.sp5)$adj.r.squared
summary(mod.sp6)$adj.r.squared
summary(mod.sp7)$adj.r.squared
summary(mod.sp8)$adj.r.squared
summary(mod.sp9)$adj.r.squared

predict.ss(mod.sp6,x=c(15.2,23.2,8))

###

modelo_sp1 = lm(y~x1+bs(x2, degree = 1), data = datos4)
modelo_sp2 = lm(y~x1+bs(x2, degree = 2), data = datos4)
modelo_sp3 = lm(y~x1+bs(x2, degree = 3), data = datos4)
modelo_sp4 = lm(y~x1+bs(x2, degree = 4), data = datos4)
modelo_sp5 = lm(y~x1+bs(x2, degree = 5), data = datos4)


