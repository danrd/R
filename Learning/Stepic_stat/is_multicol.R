#‘ункци€ провер€ет существование строгой мультиколлинеарности, 
#а именно наличие линейной комбинации между предикторами и возвращает имена 
#переменных, между которыми есть линейна€ зависимость или 
#cобщение "There is no collinearity in the data"
is_multicol <- function(df){
  names <- names(df)
  c <- cor(df)
  d <- diag(x = 1, dim(c)) #единична€ матрица дл€ обнулени€ диагоналей корелл€ционной
  c <- abs(c-d)
  indices <- which(c>0.999, arr.ind = T)
  if (length(indices)==0){
    return ("There is no collinearity in the data")
  }
  else{
    return(row.names(indices))
  }
}