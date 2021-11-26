setwd('~/Desktop/贝叶斯统计与计算/预习R代码/L14')

# MCMC算法非常重要，但也很晦涩，可以参照如下两篇文章来理解
# https://zhuanlan.zhihu.com/p/37121528
# https://zhuanlan.zhihu.com/p/109978580

# 独立蒙特卡洛方法的局限性
# 在贝叶斯统计的实践中，从后验分布中采样的能力是很重要的
# 直接采样(Direct sampling)
# 有时很难找到反函数
# 拒绝采样和重要性采样
# 如果建议分布(proposal density)和目标分布有很大的不同, 方法效率就会很低
# 然而构建一个与目标分布类似的建议分布很困难
# MCMC(Markov chain Monte Carlo)
# 使用适应性的建议分布而非固定的建议分布

# Markov chain(马尔可夫链)
# Markov chain是一类随机过程，随机过程即一个依赖于时间参数的随机变量序列
# Markov chain的当前状态只与前一个状态有关，而与其他状态无关

# MCMC算法成功的关键不是因为马尔可夫链的马尔可夫性
# 而是因为从任意概率分布样本开始，代入马尔科夫链的状态转移矩阵
# 经过一系列转换，最终可以收敛到服从稳定概率分布的样本
# 在马尔可夫链收敛后，继续沿着马尔可夫链采样，就可以得到一系列服从平稳分布的样本

# The Metropolis algorithm
# 下面要采样的目标分布是两个正态分布的加权和，使用Metropolis算法采样，并绘制轨迹

# target distribution: f(x)
p <- 0.4
mu <- c(-1, 2)
sd <- c(0.5, 2)
f <- function(x) {
  p * dnorm(x, mu[1], sd[1]) + (1 - p) * dnorm(x, mu[2], sd[2])
}
curve(f(x), -4, 8, n = 301, las = 1)

# define proposal density q(x)
q <- function(x) {
  return (rnorm(1, x, 4))
}

# define the step
step <- function(x, f, q) {
  # Pick new point
  xp <- q(x)
  # Acceptance probability
  alpha <- min(1, f(xp) / f(x))
  # Accept new point with probability alpha
  if (runif(1) < alpha) {
    x <- xp
  }
  # Returning the point
  return (x)
}

run <- function(x, f, q, nsteps) {
  res <- matrix(NA, nsteps, length(x))
  for (i in seq_len(nsteps)) {
    # seq_len(x): 生成1:x的向量序列
    res[i, ] <- x <- step(x, f, q)
  }
  return(res)
}
res <- run(-10, f, q, 1100)

# Trace plot(1100 samples, with the first 100 burn-in)
plot(res, type = 'l', xlab = 'Sample', ylab = 'Parameter', ylim = c(-10, 8))

# 采样结果和目标分布相似吗?
hist(res[101:1100], 50, freq=FALSE, main="", ylim=c(0, 0.4), las=1,
     xlab="x", ylab="Probability density")
z <- integrate(f, -Inf, Inf)$value
curve(f(x) / z, add=TRUE, col="red", n=200)

# Exercise
# 1.Try 10000 samples
res1 <- run(-10, f, q, 10000)
hist(res1[1000:10000], 50, freq=FALSE, main="", ylim=c(0, 0.4), las=1,
     xlab="x", ylab="Probability density")
curve(f(x), add=TRUE, col="red", n=200)

# 2.Try different initial values (even very unrealistic ones)
res2 <- run(-50, f, q, 10000)
hist(res2[1000:10000], 50, freq=FALSE, main="", ylim=c(0, 0.4), las=1,
     xlab="x", ylab="Probability density")
curve(f(x), add=TRUE, col="red", n=200)

# 3.Try different values of standard deviation for q(e.g.: 50, 0.1)
# Plot their trace plots together with the previous one. Calculate their acceptance rates
n <- 0
q1 <- function(x) {
  return (rnorm(1, x, 50))
}

step <- function(x, f, q) {
  # Pick new point
  xp <- q(x)
  # Acceptance probability
  alpha <- min(1, f(xp) / f(x))
  # Accept new point with probability alpha
  if (runif(1) < alpha) {
    x <- xp
    n <<- n + 1
  }
  # Returning the point
  return (x)
}

run <- function(x, f, q, nsteps) {
  res <- matrix(NA, nsteps, length(x))
  for (i in seq_len(nsteps))
    # seq_len(x): 生成1:x的向量序列
    res[i, ] <- x <- step(x, f, q)
  return(res)
}

res3_1 <- run(-10, f, q1, 1100)
cat('Accept rate: ', n/1100, "\n")

q2 <- function(x) {
  return (rnorm(1, x, 0.1))
}

n <- 0 
res3_2 <- run(-10, f, q2, 1100)
cat('Accept rate: ', n/1100, "\n")

plot(res, type = 'l', xlab = 'Sample', ylab = 'Parameter', ylim = c(-10, 8), col = 'red')
par(new = TRUE)  # 是否叠加新图形，每运行一次该命令叠加一次
plot(res3_1, type = 'l', xlab = '', ylab = '', ylim = c(-10, 8), col = 'blue')
par(new = TRUE)  # 是否叠加新图形，每运行一次该命令叠加一次
plot(res3_2, type = 'l', xlab = '', ylab = '', ylim = c(-10, 8), col = 'green')

# 4.Based on 3, plot the autocorrelation plots of the three traces
acf(res)
acf(res3_1)
acf(res3_2)

# Findings
# 对于随机游走建议分布，接受率过高或过低都是不好的
# 到底什么是过高和过低?
# 这取决于很多因素，包括目标分布和建议分布
# 一个粗略的规则是将接受率设定在20%到70%之间
# 如果接受率超出这个范围，可以将建议分布的方差翻倍或减半
# 在马尔可夫链中相邻两个状态的相关性越低越好
# 马尔可夫链的效率可以用有效样本量(effective sample size)来衡量
# 有效样本量可以用R包coda的函数effectiveSize来计算
library('coda')
effectiveSize(res)
effectiveSize(res1)
effectiveSize(res2)
effectiveSize(res3_1)
effectiveSize(res3_2)

# The Metropolis Hastings algorithm
# 2-dimension example
# make.mvn: 输入均值向量和协方差矩阵，输出多元正态分布的密度函数
make.mvn <- function(mean, vcv) {
  logdet <- as.numeric(determinant(vcv, TRUE)$modulus)
  # determinant(x, logarithm = TRUE): 如果逻辑表达式为TRUE, 则返回矩阵行列式的对数
  tmp <- length(mean) * log(2 * pi) + logdet
  vcv.i <- solve(vcv)
  # solve(A, b): 解形如AX=b的矩阵方程
  # 如果没有输入b的值，b将被视为单位矩阵，solve(A)将返回A的逆矩阵
  function(x) {
    dx <- x - mean
    exp(-(tmp + rowSums((dx %*% vcv.i) * dx))/2)
    # rowSums(x) = apply(x, 1, sum)
    # rowSums: 对矩阵按行求和
  }
}

mu1 <- c(-1, 1) # f1的均值向量
mu2 <- c(2, -2) # f2的均值向量
vcv1 <- matrix(c(1, 0.25, 0.25, 1.5), 2, 2) # f1的协方差矩阵
vcv2 <- matrix(c(2, -0.5, -0.5, 2), 2, 2) # f2的协方差矩阵
f1 <- make.mvn(mu1, vcv1) # 二元正态分布密度函数f1
f2 <- make.mvn(mu2, vcv2) # 二元正态分布密度函数f2
f <- function(x) {
  f1(x) + f2(x)
} # Target density(目标分布): f
x <- seq(-5, 6, length = 71)
y <- seq(-7, 6, length = 61)
xy <- expand.grid(x = x, y = y)
# expand.grid: 用于生成表格式的数据框，对应每个y，都会把所有的x都重复一遍
# xy数据框的每一行为二维平面的一个点，共有length(x)*length(y)行
z <- matrix(apply(as.matrix(xy), 1, f), length(x), length(y))
# 对xy的每一行应用密度函数f，并将结果储存为length(x)*length(y)的矩阵
# z矩阵的每一个元素为对应点(x, y)的概率密度
image(x, y, z, las = 1)
# image: 创建一个彩色或灰度矩形网格，其颜色与z中的值相对应
contour(x, y, z, add = TRUE)
# contour: 创建等高线图，或者向现有的图中添加等高线

# Metropolis sampling
# define proposal density q(x)
q <- function(x, d = 8) {
  x + runif(length(x), -d/2, d/2)
}
x0 <- c(-4, -4)
set.seed(1)
# set.seed(): 设定生成随机数的种子，使生成的随机数保持不变
samples <- run(x0, f, q, 1000)
image(x, y, z, xlim = range(x, samples[, 1]), ylim = range(x, samples[, 2]))
# image: 创建一个彩色或灰度矩形网格，其颜色与z中的值相对应
contour(x, y, z, add = TRUE)
# contour: 创建等高线图，或者向现有的图中添加等高线
lines(samples[, 1], samples[, 2], col = "#00000088")
# 生成MCMC采样的轨迹

# More samples
samples <- run(x0, f, q, 100000)
smoothScatter(samples)
# smoothScatter: 生成一个带有平滑颜色密度的散点图
contour(x, y, z, add=TRUE)
# contour: 创建等高线图，或者向现有的图中添加等高线

# Indirect method v.s. Metropolis Hastings
# 相同点:
# 1.都需要一个建议分布
# 2.建议分布与目标分布的近似程度越好时，两者都更有效
# 3.都涉及某种形式的接受/拒绝
# 不同点:
# Indirect method产生独立样本，MH样本存在相关性
# Indirect method要求p(x)/q(x)对所有x都是有限的
# 当x是高维向量时, MH更有效

# Gibbs sampling
# 标准的Metropolis算法在每一步都更新整个参数向量
# 因此一个p维的参数向量必须有一个p维的建议分布q
# Gibbs sampling的巧妙之处在于它把复杂的高维问题分解成简单的低维问题，从而简化了高维问题

# Advantages of Gibbs sampling:
# 1.不需要固定的建议分布
# 2.接受率为1，每一个采样点都会被接受

# Disadvantages of Gibbs sampling:
# 1.需要推导出条件概率分布
# 2.需要较为容易地从条件概率分布中获得随机样本
# 3.如果参数是相关的，算法可能会收敛的非常慢

# 在Gibbs sampling中应用Metropolis
# 即使单个条件分布不容易模拟，Metropolis Hastings也可以在Gibbs sampling的每一步中使用
# 这种方法非常有效，因为它将一个多变量问题分解成更小的单变量问题来求解

# The Bayesian linear regression model(贝叶斯线性回归)
# 频率学派的观点是，模型参数是固定的、未知的常量
# 贝叶斯学派的观点是，模型参数是未知的变量，它自身也是服从某个概率分布的
# 贝叶斯学派使用概率分布而非点估计来构建线性回归
# 我们只知道模型参数的先验分布，需要通过观察到的数据来进行调整，得到后验分布
# 贝叶斯线性回归的目的不是找到模型参数的单一最佳值，而是确定模型参数的后验分布
