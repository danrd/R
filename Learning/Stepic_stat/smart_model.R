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

#������� ������ ��������� � ����� ����������� � ��������� ���� �� � ������ 
#���������� � ����������� vif ������ 10. ���� ���� �� � ����� ���������� vif > 10, 
#�� �� ������������� ������ ��������� ���������� � ������������ ����������� vif, 
#���� ����� ����� � ����� ������ ��� ��� �������� ���������� � vif ������ 10, 
#�� ��� ����� �� ������� ��������������� �����������
smart_model <-  function(data){
  while(max(VIF(data))>10){
    vif<-VIF(data)
    m<-which.max(vif)[1]
    n<-names(vif)[m]
    indices<-which(colnames(data)==n)
    data<-data[-indices]
    if (dim(data)[2]==2){
      break
    }
  }
  model<-lm(data[, 1]~as.matrix(data[, -1]), data=data)
  return(model$coef)
}