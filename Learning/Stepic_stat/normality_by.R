#������� ��������� ������������� �� ������������ � ������ ������ 
#� ���������� dataframe � ������������ ���������� ����� shapiro.test
normality_by <- function(x){
  g <- x %>% group_by(y,z)
  return(g %>% summarise(p_value=shapiro.test(x)$p.value))
}