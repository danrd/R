# ������� ������� ���������� important_cases - ������ � ����� ���������� 
#("No" � "Yes"). ���������� ������ ��������� �������� Yes, ���� ��� ������� 
#������ �������� ���� �� ���� �������������� ���������� ���� ��������. 
#� ��������� ������ ���������� important_cases  ����� ��������� �������� No.
important_cases<-function(){
  data("iris")
  data<-iris[1:4]
  means<-apply(data, 2, mean) #������� �������� ��������� 
  res<-apply(data, 1, function(x) x>means) #���������� �� ���������� ���� ��������
  indices<-apply(res, 2, function(x) sum(x)>=3)
  iris$important_cases<-factor(indices, levels=c("FALSE", "TRUE"), labels=c("No", "Yes"))
  return(iris)
}