#—троит логистическую регрессию и возвращает вектор с названи€ми статистически 
#значимых переменных (p < 0.05). ≈сли в данных нет значимых предикторов, 
#функци€ возвращает строку с сообщением  "Prediction makes no sense"
get_features <- function(df){
  df[, 1] <-  factor(df[, 1], labels = c(1, 0)) 
  df[, "type"] <-  factor(df[, "type"], labels = c(2, 1)) 
  fit <- glm(df$is_prohibited ~ ., data = df, family = "binomial")
  pi <- anova(fit, test = "Chisq")$`Pr(>Chi)`
  indices <- which(pi<0.05)
  if (length(indices)==0){
    return("Prediction makes no sense")
  }
  else {
    return(names(df)[ind]) 
  }
}