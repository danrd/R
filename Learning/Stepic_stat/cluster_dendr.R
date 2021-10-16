library(ggplot2) 
library(ggrepel) # для симпатичной подписи точек на графике

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data)
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 2) # укажите желаемое число кластеров, сейчас стоит 2