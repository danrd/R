#������� ������ ������������� ������, ��� y � ��������� ����������, � x � �����������, 
#� ���������� ������ �� ��������� ���������� ������������� ������. 
get_coefficients<-function(df) {
  logit <- glm(y ~ x, data=df, family = binomial())
  return(exp(logit$coefficients))
}
 