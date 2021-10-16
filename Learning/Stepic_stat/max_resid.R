#‘ункци€ находит €чейку таблицы сопр€женности с максимальным  значением 
#стандартизированного остатка и возвращает вектор из двух элементов: 
#название строчки и столбца этой €чейки.
max_resid <- function(x){
  res <- table(x)
  chi <- chisq.test(res)
  pos <- which(chi$stdres==max(chi$stdres), arr.ind = T)
  row <- rownames(chi$stdres)[pos[1]]
  col <- colnames(chi$stdres)[pos[2]]
  return (c(row, col))
}