


# Paquetes ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(psych) # pairs.panels
library(GGally) # ggpairs
library(scatterplot3d)
library(rgl)
library(car) # avPlots # ellipse # confidenceEllipse
library(broom)
library(ellipse)
library(lmtest)
library(gmodels) # glh.test
library(ggplot2)
library(moments)
library(nortest)
library(lmtest)
library(olsrr) 
library(gvlma)
library(qpcR) 

# Lectura de datos --------------------------------------------------------

datos = read_excel("04_Reg_01.xlsx")

# Gráficas exploratorias --------------------------------------------------

datos %>% 
  select_if(is.numeric) %>% 
  pairs

datos %>% 
  select_if(is.numeric) %>% 
  pairs.panels

datos %>% 
  ggpairs + 
  theme_bw()

datos %>% 
  ggpairs(
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet")) +
  theme_bw()

(scatterplot3d(datos$Educacion,datos$X4, datos$Sueldo, pch=16, highlight.3d=TRUE,
               type="h", main="3D Scatterplot") -> graf3d)

(scatterplot3d(datos$Educacion,datos$Edad, datos$Sueldo, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot") -> graf3d)

fit <- lm(Sueldo ~ Educacion+Edad, data = datos)
graf3d$plane3d(fit)

plot3d(datos$Educacion,datos$Edad, datos$Sueldo, col="darkblue", size=8)

plot3d(datos$Educacion,datos$X4, datos$Sueldo, col="darkblue", size=8)

lm(Sueldo ~ ., data = datos) %>% avPlots

# Gráfico de regresión parcial para la EDAD
modelo_sueldo = lm(Sueldo ~ Educacion + Sexo + X4, data = datos)
modelo_edad   = lm(Edad ~ Educacion + Sexo + X4, data = datos)
data.frame(res_edad = residuals(modelo_edad),
           res_sueldo = residuals(modelo_sueldo))%>% 
  ggplot(aes(x=res_edad,y=res_sueldo))+
  geom_point()+
  geom_smooth(method='lm',se = F)+
  labs(x="Edad | otros",y="Sueldo | otros")+
  theme_minimal()

# Modelo ------------------------------------------------------------------

lm(Sueldo ~ ., data = datos) -> modelo

# Estimación puntual ------------------------------------------------------

modelo %>% summary
(modelo %>% tidy -> betas)
(summary(modelo)$sigma -> sigma)
(sigma**2 -> sigma2)


# Estimación intervalar ---------------------------------------------------

modelo %>% confint(level = 0.95)
vcov(modelo) 
X = model.matrix(Sueldo~Educacion+Edad+Sexo+X4,data=datos)
C = solve(t(X)%*%X)
(LI = betas[1,2] - qt(0.975,19)*sigma*sqrt(C[1,1]))
(LS = betas[1,2] + qt(0.975,19)*sigma*sqrt(C[1,1]))

(LI = betas[1,2] - qt(0.975,19)*sqrt(vcov(modelo)[1,1]))
(LS = betas[1,2] + qt(0.975,19)*sqrt(vcov(modelo)[1,1]))


X = model.matrix(Sueldo~Educacion+Edad+Sexo+X4,data=datos)
k = 5
n = 24
CME = 1.29 # summary(aov(modelo))
betaest = betas$estimate
valorF  = qf(0.95,k,n-k)

betap   = c(-0.5,0.2,0.8,0.15,0.05) # ¿estará en la región de confianza?
(t(betaest - betap)%*%t(X)%*%X%*%(betaest - betap))/(p*CME) < valorF

modelo %>% 
  ellipse(which = c(1,4)) %>% 
  plot(type = "l",
       col  = "forestgreen")
points(modelo$coefficients[1], modelo$coefficients[4], col  = "forestgreen", pch = 18)

modelo %>% confidenceEllipse(fill       = T,
                             lwd        = 1,
                             which.coef = c(2,4),
                             Scheffe    = F,
                             col        = "forestgreen")

# Prueba de hipótesis -----------------------------------------------------

summary(modelo)
summary(aov(modelo))

modelo2 = lm(Sueldo~Educacion+Edad+Sexo, data = datos)
anova(modelo2,modelo)
anova(modelo,modelo2)

lrtest(modelo,4)
lrtest(modelo,"X4")
lrtest(modelo2,modelo)
lrtest(modelo,modelo2)

cons = rbind(c(0,-1,0,1,0))
ret  = glh.test(modelo, cons)
ret
ret %>% summary

cons = rbind(c(0,-1,0,1,0),c(0,1,2,1,0))
ret  = glh.test(modelo, cons, d = c(0,5))
ret
ret %>% summary

# Estimación de la media --------------------------------------------------

modelo$fitted.values

(modelo %>% 
    broom::augment() %>% 
    dplyr::select(.fitted) -> yest)

modelo %>% 
  augment %>% 
  ggplot(aes(x=Sueldo,y=.fitted)) +
  geom_point() +
  geom_abline(intercept=0, col="blue")+
  labs(x="Sueldo observado", y = "Sueldo estimado")+
  scale_x_continuous(limits=c(0,10))+
  scale_y_continuous(limits=c(0,10))+
  theme_minimal()

modelo %>% 
  predict(data.frame(Educacion = c(10,12),
                     Sexo = c("M","F"),
                     Edad = c(42,53),
                     X4   = c(-5,1)))

modelo %>% 
  predict(data.frame(Educacion = c(10,12),
                     Sexo = c("M","F"),
                     Edad = c(42,53),
                     X4   = c(-5,1)),
          interval = "confidence",
          level    = 0.95)

(X = model.matrix(Sueldo~Educacion+Edad+Sexo+X4,data=datos))
(x = c(1,10,42,1,-5))
(H = X%*%solve(t(X)%*%X)%*%t(X));dim(H)
(h = H %>% diag %>% max)
t(x)%*%solve(t(X)%*%X)%*%x
t(x)%*%solve(t(X)%*%X)%*%x > h

(x = c(1,60,-15,1,35))
t(x)%*%solve(t(X)%*%X)%*%x
t(x)%*%solve(t(X)%*%X)%*%x > h


# Predicción individual ---------------------------------------------------

modelo %>% 
  predict(data.frame(Educacion = c(10,12),
                     Sexo = c("M","F"),
                     Edad = c(42,53),
                     X4   = c(-5,1)),
          interval = "prediction",
          level    = 0.95)

# Supuestos ---------------------------------------------------------------

par(mfrow=c(2,2))
modelo %>% plot

modelo %>% residuals -> residuales

residuales %>% kurtosis
residuales %>% skewness
residuales %>% shapiro.test
residuales %>% ad.test
residuales %>% ks.test("pnorm")
residuales %>% TSA::acf()
modelo %>% ncvTest
modelo %>% dwtest(alternative = "two.sided")
modelo %>% spreadLevelPlot
modelo %>% ols_test_breusch_pagan
modelo %>% residualPlots
modelo %>% crPlots
modelo %>% durbinWatsonTest(alternative = "two.sided",
                            max.lag=10,
                            reps=1e5)
modelo %>% gvlma

modelo %>% 
  augment %>% 
  ggplot(aes(sample=.resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

hist(modelo$residuals)

# Adecuación --------------------------------------------------------------

(summary(modelo)$r.squared -> r2)
(summary(modelo)$adj.r.squared -> r2a)
n <- nrow(datos) 
p <- 4
1-(1-r2)*(n-1)/(n-p-1)

lm(Sueldo ~ Educacion + Sexo + Edad, data = datos) -> modeloprop
(summary(modeloprop)$r.squared -> r2.prop)
(summary(modeloprop)$adj.r.squared -> r2a.prop)
cbind(r2,r2.prop,r2-r2.prop)
cbind(r2a,r2a.prop,r2a-r2a.prop)

lm(Sueldo ~ Educacion + Sexo + X4, data = datos) -> modeloprop2
(summary(modeloprop2)$r.squared -> r2.prop2)
(summary(modeloprop2)$adj.r.squared -> r2a.prop2)
cbind(r2,r2.prop2,r2-r2.prop2)
cbind(r2a,r2a.prop2,r2a-r2a.prop2)

modelo %>% AIC
(2*6-2*logLik(modelo) -> aic)
modelo %>% AICc
(aic + (2*5^2+2*5)/(n-5-1) -> aicc)

cbind(modelo %>% AIC,modeloprop %>% AIC,modeloprop2 %>% AIC)
cbind(qpcR::AICc(modelo), qpcR::AICc(modeloprop), qpcR::AICc(modeloprop2))

# Multicolinealidad ------------------------------------------------------------

modelo %>% vif

modedu = lm(Educacion ~ Sexo+Edad+X4, data=datos)
summary(modedu)$r.squared
1/(1-summary(modedu)$r.squared)

datos2 = datos %>% mutate(Sex = ifelse(Sexo=="M",1,0))
mosexo = lm(Sex ~ Educacion+Edad+X4, data=datos2)
summary(mosexo)$r.squared
1/(1-summary(mosexo)$r.squared)
