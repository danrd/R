#получает на вход dataframe с двумя переменными x и y произвольной длины и 
#возвращает вектор из двух значений - верхней и нижней границы доверительного 
#интервала для коэффициента наклона в модели y ~ x
slope_cl_boot <- function(y){
  slope <- lm(dist~speed, data=y)$coefficients[2]
  boot <- c(1:1000)
  fr <- floor(dim(y)[1]*0.66)  
  res <- sort(sapply(boot, function(x) lm(dist~speed, data=y[sample(seq(1, dim(y)[1], 1), fr, replace = T), ])$coefficients[2]-slope))+slope
  return (c(res[25], res[975]))
}