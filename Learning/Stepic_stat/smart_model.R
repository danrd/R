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

#Функция строит регрессию с этими переменными и проверяет есть ли в модели 
#переменные с показателем vif больше 10. Если хотя бы у одной переменной vif > 10, 
#то из регрессионной модели удаляется переменная с максимальным показателем vif, 
#если после этого в новой модели все еще остались переменные с vif больше 10, 
#то они таким же образом последовательно исключаются
smart_model <-  function(data){
  while(max(VIF(data))>10){
    vif<-VIF(data)
    m<-which.max(vif)[1]
    n<-names(vif)[m]
    indices<-which(colnames(data)==n)
    data<-data[-indices]
    if (dim(data)[2]==2){
      break
    }
  }
  model<-lm(data[, 1]~as.matrix(data[, -1]), data=data)
  return(model$coef)
}