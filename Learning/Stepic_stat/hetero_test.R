#функция строит вспомогательную регрессию, в которой зависимая переменная - 
#это квадраты остатков исходной модели, а независимые переменные - это предикторы 
#из исходной модели и возвращает значение R квадрат этой вспомогательной модели.
hetero_test <-  function(test_data){
  model<-lm(X1 ~ ., data=test_data)
  residuals<-(model$residuals)**2
  homo_test<-lm(residuals ~ ., data=test_data[-1])
  return(as.numeric(summary(homo_test)$r.squared))
}