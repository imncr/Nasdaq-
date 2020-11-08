#import data
Nasdaq.Prices.Final <- read.csv("C:/Users/Nicolas/Desktop/Nick/School/Grad School (19-20)/Fall 19/ST 635/Group Project/ST635 Group Project/Data/Nasdaq Prices Final.csv")
stocks <- Nasdaq.Prices.Final

#look at summary stats
stocks <- transform(stocks, Direction = as.factor(Direction))
stocks <- stocks[,9:15]
head(stocks)
summary(stocks)
contrasts(stocks$Direction)

#plots
par(mfrow = c(1, 3))

plot(stocks$Lag1 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag1")
plot(stocks$Lag2 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag2")
plot(stocks$Lag3 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag3")

plot(stocks$Vol_Lag1 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Vol_Lag1")
plot(stocks$Vol_Lag2 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Vol_Lag2")
plot(stocks$Vol_Lag3 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Vol_Lag3")

plot(stocks$Vol_Lag1*stocks$Lag1 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag1:Vol_Lag1")
plot(stocks$Vol_Lag2*stocks$Lag2 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag2:Vol_Lag2")
plot(stocks$Vol_Lag3*stocks$Lag3 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag3:Vol_Lag3")

#create train and test set
set.seed(2)
train_index <- sample(1:nrow(stocks), size = 200)
train <- stocks[train_index, ]
test <- stocks[-train_index, ]

#logit
logit.full <- glm(Direction ~. + Lag1:Vol_Lag1 + Lag2:Vol_Lag2 + Lag3:Vol_Lag3, data = train, family = "binomial")
summary(logit.full)

logit.empty <- glm(Direction ~ 1, data = train, family = "binomial")
summary(logit.empty)

logit.bwd <- step(logit.full, scope = list(lower = logit.empty$formula, upper = logit.full$formula), direction = "backward", k = 2, trace = 0)
summary(logit.bwd)

logit.fwd <- step(logit.empty, scope = list(lower = logit.empty$formula, upper = logit.full$formula), direction = "forward", k = 2, trace = 0)
summary(logit.fwd)

#loocv
library(boot)
set.seed(2)
cv.glm(train, logit.bwd)$delta[1]
cv.glm(train, logit.fwd)$delta[1]

#k fold cv
set.seed(2)
cv.glm(train, logit.bwd, K = 10)$delta[1]
cv.glm(train, logit.fwd, K = 10)$delta[1]

#bootstrap
#Error in glm.control(index = 1:936) : unused argument (index = 1:936)
#still need to create confidence intervals
betas.bwd <- function(data, index){
  return(coef(glm(Direction~Lag1+Vol_Lag1+Lag1:Vol_Lag1, data = data, index = index)))
}
set.seed(2)
bwd_boot_est <- boot(data = stocks, statistic = betas.bwd, R = 1000)

betas.fwd <- function(data, index){
  return(coef(glm(Direction~Lag1:Vol_Lag1, data = data, index = index)))
}
set.seed(2)
fwd_boot_est <- boot(data = stocks, statistic = betas.fwd, R = 1000)

logit.bwd$coefficients
print(bwd_boot_est)

logit.fwd$coefficients
print(fwd_boot_est)

#trees
par(mfrow = c(1, 1))

library(tree)
tree.stocks <- tree(Direction~., split = "gini", data = train)
summary(tree.stocks)

plot(tree.stocks)
text(tree.stocks, pretty = 0)

tree.stocks.pred <- predict(tree.stocks, newdata = test, type = 'class')
table(tree.stocks.pred, test$Direction)
mean(tree.stocks.pred == test$Direction)
mean(tree.stocks.pred != test$Direction)

set.seed(2)
(cv.stocks <- cv.tree(tree.stocks, FUN = prune.misclass, K = 10))
(bs.stocks <- cv.stocks$size[cv.stocks$dev == min(cv.stocks$dev)])

prune.stocks <- prune.tree(tree.stocks, best = min(bs.stocks), method = "misclass")
plot(prune.stocks)
text(prune.stocks, pretty = 0)

tree.stocks.prune.pred <- predict(prune.stocks, newdata = test, type = 'class')
table(tree.stocks.prune.pred, test$Direction)
mean(tree.stocks.prune.pred == test$Direction)
mean(tree.stocks.prune.pred != test$Direction)
