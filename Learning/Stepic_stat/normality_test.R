#Поверяет нормальность распределения количественных переменных. 
#Функция возвращает вектор значений p-уровней значимости теста shapiro.test 
#для каждой количественной переменной.
normality_test <- function(x){
  data <- x[lapply(x, typeof)=="double"]
  res <- sapply(data, shapiro.test)
  p <- unlist(res[2,])
  return(p)
}