
# Tidymodels: https://www.tidymodels.org/
# https://campus.datacamp.com/courses/modeling-with-tidymodels-in-r/machine-learning-with-tidymodels
# install.packages("tidymodels")

# Lectura de datos --------------------------------------------------------

library(readxl)
read_xlsx('01_Reg_02.xlsx') -> datos

# Partición en train y test -----------------------------------------------

library(tidymodels)
datos %>% 
  initial_split(prop = 0.75,
                strata = Rendimiento) -> datos_div

datos_div %>% training -> datos_train
datos_div %>% testing  -> datos_test

datos_train %>% nrow
datos_test %>% nrow
datos %>% nrow

# Modelamiento - Forma 1 --------------------------------------------------

(linear_reg() %>% 
   set_engine('lm') %>% 
   set_mode('regression') %>% 
   fit(Rendimiento ~ Ozono, data = datos_train) -> modelo_ajustado)

(modelo_ajustado %>% 
    tidy -> coeficientes)

(modelo_ajustado %>% 
    predict(new_data = datos_test) -> predicciones)

(datos_test %>% 
    bind_cols(predicciones) -> resultados)

(resultados %>% 
  rsq(truth = Rendimiento, estimate = .pred) -> r2)


# Modelamiento - Forma 2 --------------------------------------------------

(linear_reg() %>% 
    set_engine('lm') %>% 
    set_mode('regression') %>% 
    last_fit(Rendimiento ~ Ozono, split = datos_div) -> modelo_ajustado2)

modelo_ajustado2 %>% 
  collect_metrics()

modelo_ajustado2 %>% 
  collect_predictions() 

