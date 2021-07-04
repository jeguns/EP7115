

# Paquetes ----------------------------------------------------------------

library(readxl)
library(faraway)
library(skimr)
library(ggcorrplot)
library(broom)
library(dplyr)
library(olsrr)

# Caso introductorio ------------------------------------------------------

read_excel("datos_seleccion.xlsx") -> datos
lm(y~.,datos) -> modelo_completo

modelo_completo%>% summary

modelo_completo %>% 
  tidy %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_col() +
  labs(title = "Coeficientes del modelo completo") +
  theme_minimal()

modelo_completo %>% vif

modelo_completo %>% logLik%>% as.numeric -> logvero
modelo_completo %>% coef %>% length -> p
2*(p+1)-2*logvero
modelo_completo %>% AIC

modelo_completo %>% logLik%>% as.numeric -> logvero
modelo_completo %>% coef %>% length -> p
modelo_completo %>% fitted.values %>% length -> n
log(n)*(p+1)-2*logvero
modelo_completo %>% BIC

modelo_completo %>% 
  ols_step_all_possible %>% 
  data.frame %>% 
  arrange(-adjr)

modelo_completo %>% 
  ols_step_best_subset

step(object    = lm(formula = y ~ ., datos),
     direction = "backward",
     scope     = list(upper = ~., lower = ~1),
     trace     = T) -> modelo_backward

modelo_backward %>% summary
modelo_backward %>% vif

step(object    = lm(formula = y ~ 1, data = datos),
     direction = "forward",
     scope     = formula(lm(y~.,data=datos)),
     trace     = T) -> modelo_forward

modelo_forward %>% summary
modelo_forward %>% vif

step(object    = lm(formula = y ~ 1, data = datos),
     direction = "both",
     scope     = formula(lm(y~.,data=datos)),
     trace     = T) -> modelo_step

modelo_step %>% summary
modelo_step %>% vif


# Ejemplo  ----------------------------------------------------------------

data("meatspec")
datos <- meatspec
datos %>% skim
datos %>% cor %>% ggcorrplot(method = "circle",type="lower") -> correlacion
c
ggsave("correlacion.jpg",correlacion,width = 100,height = 100,units = "cm")

datos[,1:100] %>% scale -> x
datos[,101] -> y
data.frame(x,y) -> datos

set.seed(1235)
id_train <- sample(1:nrow(datos), size = 0.7*nrow(x), replace = FALSE)
datos[id_train, ] -> datos_train
datos[-id_train, ] -> datos_test

datos_train %>% str
datos_test %>% str

lm(y ~ ., data = datos_train) -> modelo_completo

modelo_completo %>% summary

modelo_completo %>% 
  tidy %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 90))

options(scipen=999)

modelo_completo %>% vif
modelo_completo %>% vif %>% min
modelo_completo %>% vif %>% max

datos_train[,1:100] -> x_train 
datos_train[,101] -> y_train
modelo_completo %>% predict(newdata = data.frame(x_train)) -> predicciones_train 
(mean((predicciones_train - y_train)^2) -> training_mse)

datos_test[,1:100] -> x_test
datos_test[,101] -> y_test
modelo_completo %>% predict(newdata = data.frame(x_test)) -> predicciones_test
(mean((predicciones_test - y_test)^2) -> testing_mse)

# BACKWARD

step(object    = lm(formula = y ~ ., data = datos_train),
     direction = "backward",
     scope     = list(upper = ~., lower = ~1),
     trace     = F) -> modelo_backward

modelo_backward %>% summary

modelo_backward %>% 
  tidy %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 90))

modelo_backward %>% vif
modelo_backward %>% vif %>% min
modelo_backward %>% vif %>% max

modelo_backward %>% predict(newdata = data.frame(x_train)) -> predicciones_train 
(mean((predicciones_train - y_train)^2) -> training_mse)

modelo_backward %>% predict(newdata = data.frame(x_test)) -> predicciones_test
(mean((predicciones_test - y_test)^2) -> testing_mse)

# FORWARD

step(object    = lm(formula = y ~ 1, data = datos_train),
     direction = "forward",
     scope     = formula(lm(y~.,data=datos)),
     trace     = T) -> modelo_forward

modelo_forward %>% summary

modelo_forward %>% 
  tidy %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 90))

modelo_forward %>% vif
modelo_forward %>% vif %>% min
modelo_forward %>% vif %>% max

modelo_forward %>% predict(newdata = data.frame(x_train)) -> predicciones_train 
(mean((predicciones_train - y_train)^2) -> training_mse)

modelo_forward %>% predict(newdata = data.frame(x_test)) -> predicciones_test
(mean((predicciones_test - y_test)^2) -> testing_mse)

# STEPWISE

step(object    = lm(formula = y ~ 1, data = datos_train),
     direction = "both",
     scope     = formula(lm(y~.,data=datos)),
     trace     = T) -> modelo_step

modelo_step %>% summary

modelo_step %>% 
  tidy %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 90))

modelo_step %>% vif
modelo_step %>% vif %>% min
modelo_step %>% vif %>% max

modelo_step %>% predict(newdata = data.frame(x_train)) -> predicciones_train 
(mean((predicciones_train - y_train)^2) -> training_mse)

modelo_step %>% predict(newdata = data.frame(x_test)) -> predicciones_test
(mean((predicciones_test - y_test)^2) -> testing_mse)


