#������� ���������� �������� ����������, �� ������� ���� ���������� �������� 
#�������� ����� ����������� ���������� (p < 0.05). 
get_difference <- function(df, n){
  dist_matrix <- dist(df, method = "euclidean", diag = FALSE, upper = FALSE, p = 2) 
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  cluster <- cutree(fit, n)
  names <- names(df)
  formulas <- sapply(paste(names, "cluster", sep="~"), as.formula) #�������, ��� ��������� ���������� - �������, � ����������� - ����� ��������
  df["cluster"] = factor(cluster)
  custom_fun <- function(x){anova(aov(x, data=df))$P[1]} #������������� ������ �� ������� ��������
  res <- sapply(formulas, custom_fun)
  return (names[which(res<0.05)])
}