# функция создает переменную important_cases - фактор с двумя градациями 
#("No" и "Yes"). Переменная должна принимать значение Yes, если для данного 
#цветка значения хотя бы трех количественных переменных выше среднего. 
#В противном случае переменная important_cases  будет принимать значение No.
important_cases<-function(){
  data("iris")
  data<-iris[1:4]
  means<-apply(data, 2, mean) #средние значения признаков 
  res<-apply(data, 1, function(x) x>means) #наблюдения со значениями выше среднего
  indices<-apply(res, 2, function(x) sum(x)>=3)
  iris$important_cases<-factor(indices, levels=c("FALSE", "TRUE"), labels=c("No", "Yes"))
  return(iris)
}