#мода вектора
stat_mode <- function(vector){
  res <- table(vector)
  return (as.numeric(names(res)[which(res==max(res))]))
}
