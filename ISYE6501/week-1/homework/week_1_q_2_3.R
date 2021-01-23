setwd("/Users/z002r1g/Documents/Masters/EDxMicromasters/FA_SP_hw1/data 2.2")
data <- read.csv("credit_card_data-headers.txt", header = TRUE, sep = "\t")
x <- as.matrix(data[,1:10])
y_actual <- as.factor(data[,11])
y_pred <- rep(NA, times = nrow(data))

num_neighbors = 2:10
acc <- rep(NA, times = length(num_neighbors))

for(k in 1:length(num_neighbors)){
  cat("Running knn with ", num_neighbors[k], " neighbors\n")
  for(i in  1:nrow(data)){
    data.train <- data[-i,]
    data.test <- data[i,]
    data.kknn <- kknn(as.factor(R1)~., data.train, data.test, k = num_neighbors[k], kernel = "rectangular", scale = TRUE)
    y_pred[i] <- predict(data.kknn)
  }
  acc[k] <- sum(as.numeric(y_actual)  == as.numeric(y_pred)) / nrow(data)
}