#������� ������������, ����� ����������� ����� ������� ��������� ��������� 
#������ 90% ������������ � �������� ������ � ��������� �������� ���� ��������� 
#� �������� dataframe � ���� ����� ����������.
get_pca2 <- function(df){
  pc <- prcomp(df)
  pca <- prcomp(df)$x
  summary <- summary(pc)$importance
  indices <- which(summary[3,]>0.9)[1]
  add <- pca[, 0:indices]
  new <- cbind(df, add)
  return (new)
}