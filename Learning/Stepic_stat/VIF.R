#������� ������ ������������� ������, ��������� ���������� ������ ������, 
#� ����� ��� ������  ����������� ���������� ������������ ���������� vif
VIF <-  function(data){
  vars <- data[-1] #����������� ���������� 
  custom_fun <- function(x, data){1/(1-summary(lm(x, data=data))$r.squared)} #������� VIF ��� �������������������
  cols <- colnames(vars)
  formulas <- sapply(paste(cols, ".", sep="~"), as.formula) #������ ��� ������ VIF
  res <- sapply(formulas, custom_fun, data=vars)
  names(res) <- cols
  return(res)
}