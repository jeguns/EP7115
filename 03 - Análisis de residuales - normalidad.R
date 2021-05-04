

# Paquetes ----------------------------------------------------------------

library(dplyr)
library(nortest)
library(normtest)
library(moments)
library(readxl)
library(ggplot2)
library(broom)

# Introducción: Análisis de normalidad  -----------------------------------

y = pvsw = pvad = pvks = asi = cur = pvjb =  NULL
for(i in 1:1000){
  rnorm(1000) -> y[[i]]
  skewness(y[[i]]) -> asi[i]
  kurtosis(y[[i]]) -> cur[i]
  shapiro.test(y[[i]])$p.value -> pvsw[i]
  ad.test(y[[i]])$p.value -> pvad[i]
  jb.norm.test(y[[i]])$p.value -> pvjb[i]
  ks.test(y[[i]],"pnorm")$p.value -> pvks[i]
}

data.frame(A = asi,
           K = cur) -> estad
colMeans(estad)
plot(estad$A, type="l")
plot(estad$K, type="l")

# H0: Los valores siguen una distribución normal
# H1: Los valores no siguen una distriución normal

# H0: e_i ~ N(0,sigma^2)
# H1: e_i no~ N(0,sigma^2)

# e_1 ~ N(0,1)
# e_2 ~ N(0,3)
# e_3 ~ N(0,0.2)

data.frame(sw = pvsw,
           ad = pvad,
           ks = pvks,
           jb = pvjb) -> pv
pv > 0.05 -> pv1
colMeans(pv1)

y[[29]] %>% hist
y[[29]] %>% density %>% plot
y[[29]] %>% qqnorm; y[[29]] %>% qqline

yt = pvswt = pvadt = pvkst = asit = curt = NULL

for(i in 1:1000){
  rt(500,10) -> yt[[i]]
  skewness(yt[[i]]) -> asit[i]
  kurtosis(yt[[i]]) -> curt[i]
  shapiro.test(yt[[i]])$p.value -> pvswt[i]
  ad.test(yt[[i]])$p.value -> pvadt[i]
  ks.test(yt[[i]],"pnorm")$p.value -> pvkst[i]
}

data.frame(A = asit,
           K = curt) -> estadt
colMeans(estadt)
plot(estadt$A, type="l")
plot(estadt$K, type="l")
data.frame(sw = pvswt,
           ad = pvadt,
           ks = pvkst) -> pvt
pvt > 0.05 -> pv1t
colMeans(pv1t)

yg = pvswg = pvadg = pvksg = asig = curg = NULL

for(i in 1:1000){
  rgamma(5000,1,4) -> yg[[i]]
  skewness(yg[[i]]) -> asig[i]
  kurtosis(yg[[i]]) -> curg[i]
  shapiro.test(yg[[i]])$p.value -> pvswg[i]
  ad.test(yg[[i]])$p.value -> pvadg[i]
  ks.test(yg[[i]],"pnorm")$p.value -> pvksg[i]
}

data.frame(A = asig,
           K = curg) -> estadg
colMeans(estadg)
plot(estadg$A, type="l")
plot(estadg$K, type="l")
data.frame(sw = pvswg,
           ad = pvadg,
           ks = pvksg) -> pvg
pvg > 0.05 -> pv1g
colMeans(pv1g)

hist(yg[[6]])

mediag = NULL
for(i in 1:1000){
  mean(yg[[i]]) -> mediag[i]
}
hist(mediag)

# H0: mu_e = 0
# H1: mu_e dist 0

# H0: e ~ N()
# H1: e no ~ N()

rgamma(1000,2,3) -> asim
hist(asim)
hist(sqrt(asim))

plot(table(rpois(1000,2)))
plot(table(rpois(1000,320)))

# Datos y modelos ---------------------------------------------------------

datos1  = read_excel("03_Residuales.xlsx")
modelo1 = lm(Alquiler~Area, data = datos1)
modelo1 %>% summary

datos2  = read_excel("03_Residuales_02.xlsx")
modelo2 = lm(Demanda~Precio, data = datos2)
modelo2 %>% summary

datos3  = read.delim("03_Residuales_03.txt")
modelo3 = lm(Gastos~Vasos, data = datos3)
modelo3 %>% summary

# Residuales --------------------------------------------------------------

modelo1 %>% residuals  -> residuales1
residuals(modelo1) -> residuales1

modelo2 %>% residuals  -> residuales2

modelo3 %>% residuals  -> residuales3

# Normalidad 1 ------------------------------------------------------------

modelo1 %>% plot(which = 2)

modelo1 %>% 
  augment %>% 
  ggplot(aes(sample=.resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

hist(modelo1$residuals)

modelo1 %>% 
  augment %>% 
  ggplot(aes(x=.resid))+
  geom_histogram(bins = 6)+
  labs(x = "residuales",
       y = "frecuencia") +
  theme_minimal()

modelo1 %>% 
  augment %>% 
  ggplot(aes(x=.resid))+
  geom_density()+
  labs(x = "residuales",
       y = "densidad")+
  theme_minimal()

modelo1 %>% residuals() %>% skewness()
modelo1 %>% residuals() %>% kurtosis()

modelo1 %>% residuals %>% shapiro.test()
modelo1 %>% residuals %>% ad.test()
modelo1 %>% residuals %>% ks.test("pnorm")

# Normalidad 2 ------------------------------------------------------------

modelo2 %>% plot(which = 2)

modelo2 %>% 
  augment() %>% 
  ggplot(aes(sample=.resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

hist(modelo2$residuals)

modelo2 %>% 
  augment %>% 
  ggplot(aes(x=.resid))+
  geom_histogram(bins = 7)+
  labs(x = "residuales",
       y = "frecuencia") +
  theme_minimal()

modelo2 %>% 
  augment %>% 
  ggplot(aes(x=.resid))+
  geom_density()+
  labs(x = "residuales",
       y = "densidad")+
  theme_minimal()

modelo2 %>% residuals() %>% skewness()
modelo2 %>% residuals() %>% kurtosis()

modelo2 %>% residuals() %>% shapiro.test()
modelo2 %>% residuals() %>% ad.test()
modelo2 %>% residuals() %>% ks.test("pnorm")

# Normalidad 3 ------------------------------------------------------------

modelo3 %>% plot(which = 2)

modelo3 %>% 
  augment %>% 
  slice_head(n=97) %>% 
  ggplot(aes(sample=.resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

modelo3 %>% 
  augment() %>% 
  #slice_head(n=97) %>% 
  ggplot(aes(x=.resid))+
  geom_histogram(binwidth = 7)+
  labs(x = "residuales",
       y = "frecuencia") +
  theme_minimal()

modelo3 %>% 
  augment() %>% 
  slice_head(n=97) %>% 
  ggplot(aes(x=.resid))+
  geom_density()+
  labs(x = "residuales",
       y = "densidad")+
  theme_minimal()

(modelo3 %>% residuals())[-c(98,99)] %>% skewness()
(modelo3 %>% residuals())[-c(98,99)] %>% kurtosis()

(modelo3 %>% residuals())[-c(98,99)] %>% shapiro.test()
(modelo3 %>% residuals())[-c(98,99)] %>% ad.test()
(modelo3 %>% residuals())[-c(98,99)] %>% ks.test("pnorm")

