#�������� ������������ ������������� �������������� ����������. 
#������� ���������� ������ �������� p-������� ���������� ����� shapiro.test 
#��� ������ �������������� ����������.
normality_test <- function(x){
  data <- x[lapply(x, typeof)=="double"]
  res <- sapply(data, shapiro.test)
  p <- unlist(res[2,])
  return(p)
}