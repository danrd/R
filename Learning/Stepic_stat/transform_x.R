transform_x <-  function(test_data){
  x<-test_data[2]
  lambda<-seq(-2, 2, 0.1)
  temp<-0
  temp$x1<-(x**lambda[1])
  for (i in 2:length(lambda)){
    if (lambda[i]<0){
      temp$xi<--(x**lambda[i])
    }
    else if (lambda[i]>0){
      temp$xi<x**lambda[i]
    }
    else if (lambda[i]==0){
      temp$xi<log(x)
    }
    return(cor(test_data[1], temp))
  }
}