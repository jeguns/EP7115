library(dplyr)

x = c(3,5,10,4,7,6,8,6,5,11,6.5,10.7)
y = c(7,11,17,8.5,12,12.8,16,60,11.2,20,16,19.9)
lm(y~x) -> modelo

modelo %>% resid -> r
summary(modelo)$sigma -> s
modelo %>% model.matrix -> X
X %*% solve(t(X) %*% X) %*% t(X) -> H
(r/(s*sqrt(1-diag(H))) -> res_stand)
modelo %>% rstandard() 


lm(y[-1]~x[-1]) -> modelo_1
summary(modelo_1)$sigma -> s_1
r[1]/(s_1*sqrt(1-H[1,1]))

lm(y[-2]~x[-2]) -> modelo_2
summary(modelo_2)$sigma -> s_2
r[2]/(s_2*sqrt(1-H[2,2]))

length(x) -> n
1 -> k
res_stand*sqrt((n-k-2)/(n-k-1-res_stand^2))

modelo %>% rstudent
library(MASS)
modelo %>% studres

