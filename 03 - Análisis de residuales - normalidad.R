

# Paquetes ----------------------------------------------------------------

library(dplyr)
library(nortest)
library(moments)
library(readxl)
library(ggplot2)

# Introducción: Análisis de normalidad  -----------------------------------

y = pvsw = pvad = pvks = asi = cur = NULL
for(i in 1:1000){
  rnorm(1000) -> y[[i]]
  skewness(y[[i]]) -> asi[[i]]
  kurtosis(y[[i]]) -> cur[[i]]
  shapiro.test(y[[i]])$p.value -> pvsw[[i]]
  ad.test(y[[i]])$p.value -> pvad[[i]]
  ks.test(y[[i]],"pnorm")$p.value -> pvks[[i]]
}

data.frame(A = asi %>% unlist,
           K = cur %>% unlist) -> estad
colMeans(estad)
plot(estad$A, type="l")
plot(estad$K, type="l")
data.frame(sw = pvsw %>% unlist,
           ad = pvad %>% unlist,
           ks = pvks %>% unlist) -> pv
pv > 0.05 -> pv1
colMeans(pv1)

y[[45]] %>% hist
y[[45]] %>% density %>% plot
y[[45]] %>% qqnorm; y[[45]] %>% qqline

yt = pvswt = pvadt = pvkst = asit = curt = NULL

for(i in 1:1000){
  rt(1000,10) -> yt[[i]]
  skewness(yt[[i]]) -> asit[[i]]
  kurtosis(yt[[i]]) -> curt[[i]]
  shapiro.test(yt[[i]])$p.value -> pvswt[[i]]
  ad.test(yt[[i]])$p.value -> pvadt[[i]]
  ks.test(yt[[i]],"pnorm")$p.value -> pvkst[[i]]
}

data.frame(A = asit %>% unlist,
           K = curt %>% unlist) -> estadt
colMeans(estadt)
plot(estadt$A, type="l")
plot(estadt$K, type="l")
data.frame(sw = pvswt %>% unlist,
           ad = pvadt %>% unlist,
           ks = pvkst %>% unlist) -> pvt
pvt > 0.05 -> pv1t
colMeans(pv1t)

yg = pvswg = pvadg = pvksg = asig = curg = NULL

for(i in 1:1000){
  rgamma(1000,100,40) -> yg[[i]]
  skewness(yg[[i]]) -> asig[[i]]
  kurtosis(yg[[i]]) -> curg[[i]]
  shapiro.test(yg[[i]])$p.value -> pvswg[[i]]
  ad.test(yg[[i]])$p.value -> pvadg[[i]]
  ks.test(yg[[i]],"pnorm")$p.value -> pvksg[[i]]
}


data.frame(A = asig %>% unlist,
           K = curg %>% unlist) -> estadg
colMeans(estadg)
plot(estadg$A, type="l")
plot(estadg$K, type="l")
data.frame(sw = pvswg %>% unlist,
           ad = pvadg %>% unlist,
           ks = pvksg %>% unlist) -> pvg
pvg > 0.05 -> pv1g
colMeans(pv1g)

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

modelo1 %>% residuals %>% data.frame -> residuales1

modelo2 %>% residuals %>% data.frame -> residuales2

modelo3 %>% residuals %>% data.frame -> residuales3

# Normalidad 1 ------------------------------------------------------------

modelo1 %>% plot(which = 2)

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

modelo1 %>% 
  augment %>% 
  ggplot(aes(sample=.resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

modelo1 %>% residuals %>% shapiro.test()
modelo1 %>% residuals %>% ad.test()
modelo1 %>% residuals %>% ks.test("pnorm")

# Normalidad 2 ------------------------------------------------------------

modelo2 %>% plot(which = 2)

modelo2 %>% 
  augment %>% 
  ggplot(aes(x=.resid))+
  geom_histogram(bins = 5)+
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

modelo2 %>% 
  augment() %>% 
  ggplot(aes(sample=.resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

modelo2 %>% residuals() %>% shapiro.test()
modelo2 %>% residuals() %>% ad.test()
modelo2 %>% residuals() %>% ks.test("pnorm")

# Normalidad 3 ------------------------------------------------------------

modelo3 %>% plot(which = 2)

modelo3 %>% 
  augment() %>% 
  ggplot(aes(x=.resid))+
  geom_histogram(bins = 5)+
  labs(x = "residuales",
       y = "frecuencia") +
  theme_minimal()

modelo3 %>% 
  augment() %>% 
  ggplot(aes(x=.resid))+
  geom_density()+
  labs(x = "residuales",
       y = "densidad")+
  theme_minimal()

modelo3 %>% residuals() %>% skewness()
modelo3 %>% residuals() %>% kurtosis()

modelo3 %>% 
  augment %>% 
  ggplot(aes(sample=.resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

modelo3 %>% residuals() %>% shapiro.test()
modelo3 %>% residuals() %>% ad.test()
modelo3 %>% residuals() %>% ks.test("pnorm")

