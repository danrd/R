#�������� �� ���� ����� ������ �� ���� �������, ������ ������� y � ������ x. 
#������� ������ ����� ����� ���������� ������� ��� ������������� x, 
#��� ������� ����� x � y ����� ������������ ���������� �������� ����������
transform_x <-function(x){
  transform <-function(x, lambda){
    if (lambda>0){
      return (x^(lambda))
    }
    else if (lambda==0){
      return (log(x))
    }
    else {
      return (-(x^(lambda)))
    }
  }
  lambdas <- seq(-2, 2, 0.1)
  res <- lapply(lambdas, transform, x=x[, 2])
  cors <- sapply(res, cor, y=x[, 1])
  best <- abs(sapply(cors, function (x) max(x[x<1])))
  indices <- which(best==max(best))
  return (unlist(res[indices]))
}