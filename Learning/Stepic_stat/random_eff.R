#пример смешанной регрессионной модели
random_eff <- function(x){
  fit_1 <- lmer(frequency ~  attitude + (attitude | subject) + (attitude | scenario), data=x)
  return (fit_1)
}