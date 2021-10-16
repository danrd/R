#функция принимает два набора данных и возвращает имя пассажира с наиболее 
#подозрительным багажом. Если несколько пассажиров получили максимальное 
#значение вероятности, то верните вектор с несколькими именами. 
most_suspicious <- function(train, test){
  train[, 1] <-  factor(train[, 1], labels = c(1, 0)) 
  train[, "type"] <-  factor(train[, "type"], labels = c(2, 1)) 
  fit <- glm(train$is_prohibited ~ ., data = train, family = "binomial")
  test[, "type"] <-  factor(test[, "type"], labels = c(2, 1))
  pred <- predict.glm(fit, newdata = test[, 1:4], type = "response")
  indices <- which.max(pred)
  persons <- test[indices, 5]
  return(persons)
}