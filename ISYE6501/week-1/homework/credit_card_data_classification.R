setwd("/Users/z002r1g/Documents/Masters/EDxMicromasters/FA_SP_hw1/data 2.2")
data <- read.csv("credit_card_data-headers.txt", header = TRUE, sep = "\t")
x <- as.matrix(data[,1:10])
y <-as.factor(data[,11])  

C = c(0.0001, 0.01, 0.1, 10, 100, 1000, 10000000)
acc <- rep(NA, times = length(C))

for(i in 1:length(C)){
  model <- ksvm(x, y, type="C-svc", kernel="vanilladot", C=C[i], scaled=TRUE)
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  a0 <- model@b
  pred <- predict(model, x)
  acc[i] <- sum(pred == y) / nrow(x)
}

kernels <- c("rbfdot", "polydot", "vanilladot", "tanhdot", "laplacedot", "besseldot", "splinedot")

acc_kernel <- rep(NA, length(kernels))
for(i in 1:length(kernels)){
  model <- ksvm(x, y, type="C-svc", kernel=kernels[i], C=C[i], scaled=TRUE)
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  a0 <- model@b
  pred <- predict(model, x)
  acc_kernel[i] <- sum(pred == y) / nrow(x)
}

