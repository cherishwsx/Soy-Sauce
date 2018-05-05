Iris_result <- data.frame(bags = rep(NA, times = 20), error = rep(NA, times = 20))
Iris <- iris
s <- sample(1:150, 75)
j <- 1
for (i in c(1:10, 101:110)) {
  Forest <- get_Forest(feature = Iris[s,1:4], class = Iris[s,5], NumofBag = i, NumofObs = 75)
  rr <- randforePredict(ForestList = Forest, myData = Iris[-s,])
  Iris_result$bags[j] <- i 
  Iris_result$error[j] <- 1-(mean(rr==as.character(Iris[-s,5])))
  j <- j+1
}
a1 <- table(rr, as.character(Iris[-s,5]))


Credit_result <- data.frame(bags = rep(NA, times = 20), error = rep(NA, times = 20))
s <- sample(1:1000, 500)
j <- 1
for (i in 1:20) {
  Forest <- get_Forest(feature = credit[s,1:20], class = credit[s,21], NumofBag = i, NumofObs = 500)
  rr <- randforePredict(ForestList = Forest, myData = credit[-s,])
  Credit_result$bags[j] <- i 
  Credit_result$error[j] <- 1-(mean(rr==as.character(credit[-s,21])))
  j <- j+1
}
a2 <- table(rr, as.character(credit[-s,21]))


bCredit_result <- data.frame(bags = rep(NA, times = 20), error = rep(NA, times = 20))
b1 <- credit[credit$Score=="bad",]
b2 <- credit[credit$Score=="good",]
b2 <- b2[sample(1:700, size = 300),]
bCredit <- rbind(b1, b2)
s <- sample(1:600, 500)
j <- 1
for (i in 1:20) {
  Forest <- get_Forest(feature = bCredit[s,1:20], class = bCredit[s,21], NumofBag = i, NumofObs = 500)
  rr <- randforePredict(ForestList = Forest, myData = bCredit[-s,])
  bCredit_result$bags[j] <- i 
  bCredit_result$error[j] <- 1-mean(rr==as.character(bCredit[-s,21]))
  j <- j+1
}
a3 <- table(rr, as.character(bCredit[-s,21]))


ggplot() + geom_line(data = Iris_result[1:10,], aes(x = bags, y = error)) + 
  geom_line(data = Iris_result[11:20,], aes(x = bags, y = error)) + geom_hline(yintercept = mean(Iris_result$error), color = "red") + 
  labs(title = "Number of trees Versus Error rate for Iris", x = "number of trees", y = "error rate") + theme(plot.title = element_text(hjust = 0.5))

ggplot() + geom_line(data = Credit_result, aes(x = bags, y = error)) + 
  labs(title = "Number of trees Versus Error rate for Credit", x = "number of trees", y = "error rate") + theme(plot.title = element_text(hjust = 0.5))

ggplot() + geom_line(data = bCredit_result, aes(x = bags, y = error)) + 
  labs(title = "Trees Versus Error for balanced Credit", x = "number of trees", y = "error rate") + theme(plot.title = element_text(hjust = 0.5))

b_result <- data.frame(obs = rep(NA, times = 28), error = rep(NA, times = 28))
s <- sample(1:600, 300)
j <- 1
for (i in c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,125,150,175,200,225,250,275,300)) {
  Forest <- get_Forest(feature = bCredit[s,1:20], class = bCredit[s,21], NumofBag = 10, NumofObs = i)
  rr <- randforePredict(ForestList = Forest, myData = bCredit[-s,])
  b_result$obs[j] <- i 
  b_result$error[j] <- 1-mean(rr==as.character(bCredit[-s,21]))
  j <- j+1
}

ggplot() + geom_line(data = b_result, aes(x = obs, y = error)) + 
  labs(title = "Observations Versus Error for balanced Credit", x = "number of observations", y = "error rate") + theme(plot.title = element_text(hjust = 0.5))
