#‘ункци€ рассчитывает, какое минимальное число главных компонент объ€сн€ет 
#больше 90% изменчивости в исходных данных и добавл€ет значени€ этих компонент 
#в исходный dataframe в виде новых переменных.
get_pca2 <- function(df){
  pc <- prcomp(df)
  pca <- prcomp(df)$x
  summary <- summary(pc)$importance
  indices <- which(summary[3,]>0.9)[1]
  add <- pca[, 0:indices]
  new <- cbind(df, add)
  return (new)
}