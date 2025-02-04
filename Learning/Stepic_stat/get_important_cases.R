#������� ���������� dataframe � ����� ���������� - �������� important_cases.
#����������  important_cases ��������� �������� Yes, ���� ��� ������� 
#���������� ������ �������� �������������� ���������� ����� �������� ������ ��������. 
#� ��������� ������ ���������� important_cases ��������� �������� No.
get_important_cases <- function(df){
  means<-apply(df, 2, mean) #������� �������� ���������
  m<-dim(x)[2] #���-�� ��������� � dataframe
  res<-apply(df, 1, function(x) x>means) #���������� �� ���������� ���� ��������
  indices<-sapply(res, function(x) sum(x)>m/2)
  df$important_cases<-factor(indices, levels=c("FALSE", "TRUE"), labels=c("No", "Yes"))
  return(df)
}