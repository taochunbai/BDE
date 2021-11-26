setwd('~/Desktop/贝叶斯统计与计算/预习R代码/L9')

# Derivative Free Methods(无梯度优化方法)
# 不连续函数(Discontinuous Functions)
# 牛顿迭代法要求一阶导数和二阶导数
# 如果导数很难计算，可以用拟牛顿法来近似
# 那导数不存在的情况怎么办呢?如果函数中存在不连续点，就可能发生这种情况

# 商业上的例子
# 假设目标是通过确定工人的数量来优化企业的收入
# 一开始增加更多的工人会给企业带来更多的收入
# 如果雇佣的工人太多，他们的效率就会降低，企业的收入就会下降
# 现在假设公司必须支付一种税，员工少于50人的公司不纳税，员工超过50人的公司纳税
# 这将会使目标函数由连续型变为不连续型

# Nelder Mead Algorithm
# 即使函数是不连续的，Nelder Mead算法也是稳健的
# Nelder Mead算法的思路是基于在一个n维单纯形的顶点上对函数求值，其中n是函数的输入变量的数目
# 对于二维问题，n维单纯形只是一个三角形
# 一般来说n维单纯形有n+1个顶点

# Nelder Mead in optim()
# Nelder Mead是R函数optim()中的默认算法
# Nelder Mead算法一般比牛顿法和拟牛顿法慢，但对于非光滑函数更稳定
# 在optim()中包含参数control=list(trace = TRUE, REPORT = 1)将打印出算法每一步的详细信息
# 在optim()中使用的术语稍有不同，例如“expansion”被称为“extension”
