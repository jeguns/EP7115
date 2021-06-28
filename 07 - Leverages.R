library(dplyr)
x1 = c(1,4,3,2.5,8,5,3,5.8,6,4,14)
y1 = c(3.1,14,8.8,10,10,15.4,9.6,15,19.1,10,45.4)
plot(x1,y1,pch=18)
modelo1 = lm(y1~x1)
modelo1 %>% summary
modelo1 %>% model.matrix -> X1
X1 %*% solve(t(X1)%*%X1) %*% t(X1) -> H1
H1 %>% diag
modelo1 %>% hatvalues
2 -> k1
length(x1) -> n1
diag(H1)>2*k1/n1

x2 = c(1,4,3,2.5,8,5,3,5.8,6,4,14)
y2 = c(3.1,14,8.8,10,10,15.4,9.6,15,19.1,10,15)
plot(x2,y2,type="p",pch=18)
modelo2 = lm(y2~x2)
modelo2 %>% summary
modelo2 %>% model.matrix -> X2
X2 %*% solve(t(X2)%*%X2) %*% t(X2) -> H2
diag(H2)
2 -> k2
length(x2) -> n2
diag(H2)>2*k2/n2

modelo1 %>% rstudent
modelo2 %>% rstudent

data.frame(leverage=diag(H1)>2*k1/n1,
           influencial=abs(modelo1 %>% rstudent)>2)

data.frame(leverage=diag(H2)>2*k2/n2,
           influencial=abs(modelo2 %>% rstudent)>2)

modelo1 %>% influence.measures()
modelo2 %>% influence.measures()

modelo1 %>% plot(which=5)

modelo2 %>% plot(which=5)

k = seq(2:10)
n = seq(10:100)
expand.grid(k,n) %>% 
  rename(k=1,n=2) %>% 
  mutate(umbral = 2*k/n) %>% 
  View()

