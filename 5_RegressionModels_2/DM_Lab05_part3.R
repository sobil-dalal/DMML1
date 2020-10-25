remove(list = ls())
library(ISLR)
library(MASS)
data("Boston")

X_Boston = Boston["lstat"]
Y_Boston = Boston$medv

# create a sequence for the range of Boston lstat column
lstat_grid <- data.frame(lstat = (seq(from = range(X_Boston$lstat)[1], to = range(X_Boston$lstat)[2], by = 0.01)))

# predicting with different K values
pred_001 <- FNN::knn.reg(train = X_Boston, test = lstat_grid, y = Y_Boston, k = 1)
pred_005 <- FNN::knn.reg(train = X_Boston, test = lstat_grid, y = Y_Boston, k = 5)
pred_010 <- FNN::knn.reg(train = X_Boston, test = lstat_grid, y = Y_Boston, k = 10)
pred_050 <- FNN::knn.reg(train = X_Boston, test = lstat_grid, y = Y_Boston, k = 50)
pred_100 <- FNN::knn.reg(train = X_Boston, test = lstat_grid, y = Y_Boston, k = 100)
pred_506 <- FNN::knn.reg(train = X_Boston, test = lstat_grid, y = Y_Boston, k = 506)

# visulaizing
par(mfrow = c(3,2))

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 1")
lines(lstat_grid$lstat, pred_001$pred, col = "darkorange", lwd = 0.25)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 5")
lines(lstat_grid$lstat, pred_005$pred, col = "darkorange", lwd = 0.75)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 10")
lines(lstat_grid$lstat, pred_010$pred, col = "darkorange", lwd = 1)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 25")
lines(lstat_grid$lstat, pred_050$pred, col = "darkorange", lwd = 1.5)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 50")
lines(lstat_grid$lstat, pred_100$pred, col = "darkorange", lwd = 2)

plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", main = "k = 506")
lines(lstat_grid$lstat, pred_506$pred, col = "darkorange", lwd = 2)
