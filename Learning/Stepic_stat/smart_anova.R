#≈сли распределени€ во всех группах значимо не отличаютс€ от нормального, 
#а дисперсии в группах гомогенны, функци€ сравнивает три группы при помощи 
#дисперсионного анализа и вернуть именованный вектор со значением p-value, им€ элемента Ч "ANOVA".
#≈сли хот€ бы в одной группе распределение значимо отличаетс€ от нормального 
#или дисперсии негомогенны, функци€ сравнивает группы при помощи 
#критери€  раскела Ч ”оллиса и возвращает именованный вектор со значением 
#p-value, им€ вектора  Ч "KW".
smart_anova <- function(x){
  x[, 2] <- factor(x[, 2])
  group1<-x[x[,2]=="A", ]
  group2<-x[x[,2]=="B", ]
  group3<-x[x[,2]=="C", ]
  data <- as.data.frame(cbind(group1[, 1], group2[, 1], group3[, 1]))
  norm <- unlist(sapply(data, shapiro.test)[2, ])
  bart <- bartlett.test(x[, 1], x[, 2])$p.value
  stat <- c(norm, bart)
  if (any(stat<0.05)){
    KW <- c(KW=kruskal.test(x[, 1], x[, 2])$p.value)
    return(KW)
  }
  else {
    fit <- aov(x ~ y, x)
    ANOVA <- c(ANOVA=summary(fit)[[1]]$'Pr(>F)'[1])
    return(ANOVA)
  }
}