#Функция строит регрессионную модель, используя переменные набора данных, 
#а затем для каждой  независимой переменной рассчитывает показатель vif
VIF <-  function(data){
  vars <- data[-1] #независимые переменные 
  custom_fun <- function(x, data){1/(1-summary(lm(x, data=data))$r.squared)} #считает VIF для конкретногопризнака
  cols <- colnames(vars)
  formulas <- sapply(paste(cols, ".", sep="~"), as.formula) #модели для оценки VIF
  res <- sapply(formulas, custom_fun, data=vars)
  names(res) <- cols
  return(res)
}