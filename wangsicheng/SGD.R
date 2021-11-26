setwd('~/Desktop/贝叶斯统计与计算/预习R代码/L10')

# 随机梯度下降法(Stochastic Gradient Descent)
# 广泛用于训练机器学习的各种模型
# 所有有监督机器学习算法的目标都是最好地估计一个将输入数据X映射到输出变量的目标函数
# 需要进行优化以找到一组系数，从而得到目标函数的最佳估计
# 损失函数(Cost function)

# 梯度下降法(Gradient descent)
# 梯度下降法是一种迭代优化算法
# 当尝试最小化函数f时，明显应选择下降最快的方向去优化函数

# Gradient descent algorithm in R
f <- function(x, y) {
  x^2 + y^2
}
dx <- function(x, y) {
  2 * x
}
dy <- function(x, y) {
  2 * y
}

num_iter <- 10
learning_rate <- 0.2 # 学习率/步长
x_val <- 6
y_val <- 6
updates_x <- c(x_val, rep(NA, num_iter))
updates_y <- c(y_val, rep(NA, num_iter))
updates_z <- c(f(x_val, y_val), rep(NA, num_iter))

# iterate
for (i in 1:num_iter) {
  dx_val = dx(x_val, y_val)
  dy_val = dy(x_val, y_val)
  x_val <- x_val - learning_rate * dx_val
  y_val <- y_val - learning_rate * dy_val
  z_val <- f(x_val, y_val)
  updates_x[i + 1] <- x_val
  updates_y[i + 1] <- y_val
  updates_z[i + 1] <- z_val
}

# plotting
m <- data.frame(x = updates_x, y = updates_y)
x <- seq(-6, 6, length = 100)
y <- x
g <- expand.grid(x, y)
z <- f(g[, 1], g[, 2])
f_long <- data.frame(x = g[, 1], y = g[, 2], z = z)
library(ggplot2)
# geom_contour_filled: 在2D图像中呈现3D可视化效果
# geom_path(): 将观测结果按照它们在数据中出现的顺序连接起来
ggplot(f_long, aes(x, y, z = z)) + geom_contour_filled(aes(fill = stat(level)), bins = 50) + 
  guides(fill = FALSE) + geom_path(data = m, aes(x, y, z = 0), col = 2, arrow = arrow()) + 
  geom_point(data = m, aes(x, y, z = 0), size = 3, col = 2) + xlab(expression(x[1])) + 
  ylab(expression(x[2])) + ggtitle(parse(text = paste0("~ f(x[1],x[2]) == ~ x[1]^2 + x[2]^2")))

# Gradient descent for linear regression in R
# Generate some data
beta0 <- 1
beta1 <- 3
sigma <- 1
n <- 10000
x <- rnorm(n, 3, 1)
y <- beta0 + x * beta1 + rnorm(n, mean = 0, sd = sigma)
X <- cbind(1, x)
# 生成一个n*2的矩阵，第一列均为1，第二列为x

# Make the cost function
lm.cost <- function(X, y, beta) {
  n <- length(y)
  loss <- sum((X %*% beta - y)^2) / (2 * n)
  return(loss)
}

# Calculate the gradient
lm.cost.grad <- function(X, y, beta) {
  n <- length(y)
  gradient <- (1/n) * (t(X) %*% (X %*% beta - y))
  # t(): 转置矩阵
  return(gradient)
}

graddesc.lm <- function(X, y, beta.init, alpha = 0.1, tol = 1e-05, max.iter = 100) {
  beta.old <- beta.init
  J <- betas <- list()
  # J: 用来存储每次迭代中损失函数值的列表
  # betas: 用来存储每次迭代中系数值的列表
  betas[[1]] <- beta.old
  J[[1]] <- lm.cost(X, y, beta.old)
  beta.new <- beta.old - alpha * lm.cost.grad(X, y, beta.old)
  betas[[2]] <- beta.new
  J[[2]] <- lm.cost(X, y, beta.new)
  iter <- 1
  while ((abs(lm.cost(X, y, beta.new) - lm.cost(X, y, beta.old)) > tol) & (iter < max.iter)) {
    beta.old <- beta.new
    beta.new <- beta.old - alpha * lm.cost.grad(X, y, beta.old)
    iter <- iter + 1
    betas[[iter + 1]] <- beta.new
    J[[iter + 1]] <- lm.cost(X, y, beta.new)
  }
  if (abs(lm.cost(X, y, beta.new) - lm.cost(X, y, beta.old)) > tol) {
    cat("Did not converge \n")
  } else {
    cat("Converged \n")
    cat("Iterated", iter, "times", "\n")
    cat("Coef: ", beta.new, "\n")
    return(list(coef = betas, cost = J, niter = iter))
  }
}

gd1 <- graddesc.lm(X, y, beta.init = c(0, 0))
gd2 <- graddesc.lm(X, y, beta.init = c(0, 0), alpha = 0.05)
gd3 <- graddesc.lm(X, y, beta.init = c(0, 0), alpha = 0.01)
gd4 <- graddesc.lm(X, y, beta.init = c(0, 0), alpha = 0.15)
gd5 <- graddesc.lm(X, y, beta.init = c(0, 0), alpha = 0.2)

# Compare with lm()
myLM <- lm(y ~ x)
myLM$coefficients

# 随机梯度下降法(Stochastic gradient descent)
# 在非常大的数据集上，梯度下降法可能会运行得很慢
# 梯度下降法的一次循环需要处理整个数据集
# 算法的一次迭代称为一次批量处理(one batch)，这种形式的梯度下降法被称为批量梯度下降法
# SGD对于每次训练的样本会更新参数

# SGD for linear regression
# 1.随机排列所有的训练数据
# 2.与批量梯度下降法中每次更新时用所有样本相对，随机梯度下降法中每次更新时用1个样本，随机也就是说用样本中的一个个体来近似所有的样本

# 当训练时间出现瓶颈时，使用随机梯度下降法(SGD)
