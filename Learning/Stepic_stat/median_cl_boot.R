#Получает на вход числовой вектор произвольной длины и возвращает вектор из 
#двух значений - верхней и нижней границы доверительного интервала для медиан
median_cl_boot <- function(){
  v <- rnorm(10000, 10000)
  med <- median(v)
  boot <- c(1:1000)
  res <- sort(sapply(boot, function(x) median(sample(v, 1000, replace = T)-med)))+med
  return (c(res[50], res[950]))
}