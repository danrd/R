#Функция возвращает dataframe с новой переменной - фактором important_cases.
#Переменная  important_cases принимает значение Yes, если для данного 
#наблюдения больше половины количественных переменных имеют значения больше среднего. 
#В противном случае переменная important_cases принимает значение No.
get_important_cases <- function(df){
  means<-apply(df, 2, mean) #средние значения признаков
  m<-dim(x)[2] #кол-во признаков в dataframe
  res<-apply(df, 1, function(x) x>means) #наблюдения со значениями выше среднего
  indices<-sapply(res, function(x) sum(x)>m/2)
  df$important_cases<-factor(indices, levels=c("FALSE", "TRUE"), labels=c("No", "Yes"))
  return(df)
}