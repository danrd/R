#‘ункци€ строит логистическую модель, где y Ч зависима€ переменна€, а x Ч независима€, 
#и возвращает вектор со значением экспоненты коэффициентов модели. 
get_coefficients<-function(df) {
  logit <- glm(y ~ x, data=df, family = binomial())
  return(exp(logit$coefficients))
}
 