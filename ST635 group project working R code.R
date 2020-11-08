#import data
Nasdaq.Prices.Final <- read.csv("C:/Users/Nicolas/Desktop/Nick/School/Grad School (19-20)/Fall 19/ST 635/Group Project/ST635 Group Project/Data/Nasdaq Prices Final.csv")
stocks <- Nasdaq.Prices.Final
mean(Nasdaq.Prices.Final$Vol_Lag1)
min(Nasdaq.Prices.Final$Return)
max(Nasdaq.Prices.Final$Return)
beta0 <- 0.243195
beta1 <- -0.005992
x <- 2.479015
prob_x <- exp(beta0 + beta1*x) / (1 + exp(beta0 + beta1*x))
prob_x

#look at summary stats
stocks <- transform(stocks, Direction = as.factor(Direction))
stocks <- stocks[,9:15]
head(stocks)
summary(stocks)
contrasts(stocks$Direction)
#Direction 0 = Down
#Direction 1 = Up

#plots
par(mfrow = c(1, 3))

#Boxplots of Direction and Lags
plot(stocks$Lag1 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag1")
plot(stocks$Lag2 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag2")
plot(stocks$Lag3 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag3")

#Boxplots of Direction and Volume Lags
plot(stocks$Vol_Lag1 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Vol_Lag1")
plot(stocks$Vol_Lag2 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Vol_Lag2")
plot(stocks$Vol_Lag3 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Vol_Lag3")

#Boxplots of Direction and Lags*VolumeLags
plot(stocks$Vol_Lag1*stocks$Lag1 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag1:Vol_Lag1")
plot(stocks$Vol_Lag2*stocks$Lag2 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag2:Vol_Lag2")
plot(stocks$Vol_Lag3*stocks$Lag3 ~ stocks$Direction, col = c(4, 2), xlab = "Direction", ylab = "Lag3:Vol_Lag3")

#create train and test set
set.seed(2)
train_index <- sample(1:nrow(stocks), size = nrow(stocks)-200)
train <- stocks[train_index, ]
test <- stocks[-train_index, ]

#logit
#creates full model for forward and backward iteration
logit.full <- glm(Direction ~. + Lag1:Vol_Lag1 + Lag2:Vol_Lag2 + Lag3:Vol_Lag3, data = train, family = "binomial")
summary(logit.full)

#creates empty model for forward and backward iteration
logit.empty <- glm(Direction ~ 1, data = train, family = "binomial")
summary(logit.empty)

#creates backward iteration model
logit.bwd <- step(logit.full, scope = list(lower = logit.empty$formula, upper = logit.full$formula), direction = "backward", k = 2, trace = 0)
summary(logit.bwd)
#Includes Lag3, Vol_Lag1, Vol_Lag3, and interaction of Lag3 and Vol_Lag3
#AIC = 1013.8

#creates forward iteration model
logit.fwd <- step(logit.empty, scope = list(lower = logit.empty$formula, upper = logit.full$formula), direction = "forward", k = 2, trace = 0)
summary(logit.fwd)
#Includes only Lag3 and Vol_Lag3 interaction
#AIC = 1011.8

logit.final <- glm(Direction~Lag3+Vol_Lag3+Lag3:Vol_Lag3, data = train, family = "binomial")
summary(logit.final)

#loocv
library(boot)
cv.glm(train, logit.bwd, cost = function(r, pi = 0) mean(abs(r-pi) > 0.5))$delta[1]
#Backward iteration = 0.4605978

cv.glm(train, logit.fwd, cost = function(r, pi = 0) mean(abs(r-pi) > 0.5))$delta[1]
#Forward iteration = 0.4538043

#k fold cv
set.seed(2)
cv.glm(train, logit.bwd, K = 10, cost = function(r, pi = 0) mean(abs(r-pi) > 0.5))$delta[1]
#Backward iteration = 0.451087

cv.glm(train, logit.fwd, K = 10, cost = function(r, pi = 0) mean(abs(r-pi) > 0.5))$delta[1]
#Forward iteration = 0.4483696

#trees
par(mfrow = c(1, 1))

library(tree)
#fits an unpruned tree model
tree.stocks <- tree(Direction~., split = "gini", data = train)
summary(tree.stocks)
#Has 104 terminal nodes
#Train misclassification error rate = 0.1943

#plots tree
plot(tree.stocks)
text(tree.stocks, pretty = 0)

#Calculate test misclassification error rate
tree.stocks.pred <- predict(tree.stocks, newdata = test, type = 'class')
table(tree.stocks.pred, test$Direction)
mean(tree.stocks.pred == test$Direction)
mean(tree.stocks.pred != test$Direction)
#Test misclassification error rate = 0.545

set.seed(2)
#finding optimal terminal nodes
(cv.stocks <- cv.tree(tree.stocks, FUN = prune.misclass, K = 10))
(bs.stocks <- cv.stocks$size[cv.stocks$dev == min(cv.stocks$dev)])

#prunes tree to optimal terminal nodes
prune.stocks <- prune.tree(tree.stocks, best = min(bs.stocks), method = "misclass")

#plots tree
plot(prune.stocks)
text(prune.stocks, pretty = 0)

#Calculates test misclassification error rate
tree.stocks.prune.pred <- predict(prune.stocks, newdata = test, type = 'class')
table(tree.stocks.prune.pred, test$Direction)
mean(tree.stocks.prune.pred == test$Direction)
mean(tree.stocks.prune.pred != test$Direction)
