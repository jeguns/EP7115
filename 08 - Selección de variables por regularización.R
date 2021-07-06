
# Paquetes ----------------------------------------------------------------

library(glmnet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(tidyr)

# Caso introductorio ------------------------------------------------------

x1 = c(5,9,12,4,18) %>% scale
x2 = c(9,8,7,5,2) %>% scale
x3 = c(0,0,0,1,1) %>% as.factor
x4 = c(0,1,0,0,1) %>% as.factor
x5 = c(1,2,3,2,3) %>% scale
x6 = c(9,8,5,6,3) %>% scale
x  = cbind(x1,x2,x3,x4,x5,x6)
y = c(25.93,29.02,25.28,20.04,31.10)

lm(y~x) -> modelo
summary(modelo)

glmnet(x           = x,
       y           = y,
       alpha       = 0,
       nlambda     = 200,
       standardize = TRUE) -> modelo_ridge

modelo_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_ridge$lambda) %>% 
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes") %>% 
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

cv_error <- cv.glmnet(x      = x,
                      y      = y,
                      alpha  = 0,
                      nfolds = 10,
                      type.measure = "mse",
                      standardize  = TRUE)
cv_error

cv_error %>% plot()

library(plotmo)
plot_glmnet(cv_error$glmnet.fit)

cv_error$lambda.min
cv_error$lambda.min %>% log
cv_error$lambda.1se
cv_error$lambda.1se %>% log

glmnet(x           = x,
       y           = y,
       alpha       = 0,
       lambda      = cv_error$lambda.1se,
       standardize = TRUE) -> modelo_ridge_final

modelo_ridge_final %>%
  coef() %>% 
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = 2) %>%
  filter(predictor != "(Intercept)") %>%
  mutate(predictor = c("x1","x2","x3","x4","x5","x6")) %>% 
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col(fill="dodgerblue2") +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_minimal()

lm(y~x4) -> modelo_final_propuesto
modelo_final_propuesto %>% summary()

x_nuevo = t(as.matrix(c(1,-0.5,0,1,0.5,0.75)))
predict(modelo_ridge_final, newx = x_nuevo)
predict(modelo_final_propuesto, data.frame(x4="1"))

x_nuevo = t(as.matrix(c(1,-0.5,0,0,0.5,0.75)))
predict(modelo_ridge_final, newx = x_nuevo)
predict(modelo_final_propuesto, data.frame(x4="0"))


# Ejemplo RIDGE -----------------------------------------------------------

data("meatspec")
datos <- meatspec

datos[,1:100] -> x
datos[,101] -> y

set.seed(1235)
id_train <- sample(1:nrow(datos), size = 0.7*nrow(x), replace = FALSE)
x[id_train, ] %>% as.matrix -> x_train
x[-id_train, ] %>% as.matrix -> x_test
y[id_train] %>% as.matrix -> y_train
y[-id_train] %>% as.matrix -> y_test

x_train %>% str
x_test %>% str
y_train %>% str
y_test %>% str

glmnet(x           = x_train,
       y           = y_train,
       alpha       = 0,
       nlambda     = 200,
       standardize = T) -> modelo_ridge

x11();modelo_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_ridge$lambda) %>% 
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes") %>% 
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes utilizando modelo RIDGE") +
  theme_bw() +
  theme(legend.position = "none")

cv_error <- cv.glmnet(x      = x_train,
                      y      = y_train,
                      alpha  = 0,
                      nfolds = 10,
                      type.measure = "mse",
                      standardize  = TRUE)
cv_error

cv_error %>% plot()

cv_error$lambda.min
cv_error$lambda.min %>% log
cv_error$lambda.1se
cv_error$lambda.1se %>% log

glmnet(x           = x_train,
       y           = y_train,
       alpha       = 0,
       lambda      = cv_error$lambda.1se,
       standardize = TRUE) -> modelo_ridge_final

modelo_ridge_final %>%
  coef() %>% 
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = 2) %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col(fill="dodgerblue2") +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 6, angle = 90))

predict(modelo_ridge_final, newx = x_train) -> predicciones_train_ridge
(mean((predicciones_train_ridge - y_train)^2) -> training_mse_ridge)

predict(modelo_ridge_final, newx = x_test) -> predicciones_test_ridge
(mean((predicciones_test_ridge - y_test)^2) -> testing_mse_ridge)

# Ejemplo LASSO -----------------------------------------------------------

glmnet(x           = x_train,
       y           = y_train,
       alpha       = 1,
       nlambda     = 200,
       standardize = TRUE) -> modelo_lasso

modelo_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso$lambda) %>% 
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes") %>% 
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes utilizando modelos LASSO") +
  theme_bw() +
  theme(legend.position = "none")

cv_error <- cv.glmnet(x      = x_train,
                      y      = y_train,
                      alpha  = 1,
                      nfolds = 10,
                      type.measure = "mse",
                      standardize  = TRUE)
cv_error

cv_error %>% plot()

cv_error$lambda.min
cv_error$lambda.min %>% log
cv_error$lambda.1se
cv_error$lambda.1se %>% log

glmnet(x           = x_train,
       y           = y_train,
       alpha       = 1,
       lambda      = cv_error$lambda.1se,
       standardize = TRUE) -> modelo_lasso_final

modelo_lasso_final %>%
  coef() %>% 
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = 2) %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col(fill="dodgerblue2") +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 6, angle = 90))

predict(modelo_lasso_final, newx = x_train) -> predicciones_train_lasso
(mean((predicciones_train_lasso - y_train)^2) -> training_mse_lasso)

predict(modelo_lasso_final, newx = x_test) -> predicciones_test_lasso
(mean((predicciones_test_lasso - y_test)^2) -> testing_mse_lasso)

# Ejemplo ELASTIC NET -----------------------------------------------------


glmnet(x           = x_train,
       y           = y_train,
       alpha       = 0.85, # hiperparámetro a mejorar
       nlambda     = 200,
       standardize = TRUE) -> modelo_elastic

modelo_elastic$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_elastic$lambda) %>% 
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes") %>% 
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes utilizando modelos LASSO") +
  theme_bw() +
  theme(legend.position = "none")

cv_error <- cv.glmnet(x      = x_train,
                      y      = y_train,
                      alpha  = 0.85,
                      nfolds = 10,
                      type.measure = "mse",
                      standardize  = TRUE)
cv_error

cv_error %>% plot()

cv_error$lambda.1se
cv_error$lambda.1se %>% log

glmnet(x           = x_train,
       y           = y_train,
       alpha       = 0.85,
       lambda      = cv_error$lambda.1se,
       standardize = TRUE) -> modelo_elastic_final

modelo_elastic_final %>%
  coef() %>% 
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = 2) %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col(fill="dodgerblue2") +
  labs(title = "Coeficientes del modelo Elastic Net") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 6, angle = 90))

predict(modelo_elastic_final, newx = x_train) -> predicciones_train_elastic
(mean((predicciones_train_elastic - y_train)^2) -> training_mse_elastic)

predict(modelo_elastic_final, newx = x_test) -> predicciones_test_elastic
(mean((predicciones_test_elastic - y_test)^2) -> testing_mse_elastic)

