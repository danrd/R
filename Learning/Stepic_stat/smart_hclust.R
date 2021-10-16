#Функция в исходный набор данных добавляет новую переменную фактор - 
#cluster  -- номер кластера, к которому отнесено каждое из наблюдений.
smart_hclust <- function(df, n){
  dist_matrix <- dist(df, method = "euclidean", diag = FALSE, upper = FALSE, p = 2) 
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  cluster <- cutree(fit, n)
  df["cluster"] = factor(cluster)
  return(df)
}