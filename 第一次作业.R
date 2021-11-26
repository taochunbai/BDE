newton<-function(func,point=rnorm(1,0,10)){
  #求一阶导
  dfunc=deriv(func,"x",function.arg = TRUE)#deriv函数是数值求导
  repeat{
    #求一阶导函数值
    under<-attr(dfunc(point),"gradient")
    step<-dfunc(point)[1]/under[1]#步长
    #判断退出条件
    if(abs(step)<1e-5){
      cat("函数零点为:",as.character(point))
      break
    }
    #零点的迭代
    pnew<-point-step
    point<-pnew
    
  }
}

#body函数，将function函数类型转为expression表达式类型

newton1<-function(func,point=rnorm(1,0,10)){
  #求一阶导
  dfunc1=deriv(func[1],"x",function.arg = TRUE)
  dfunc2=deriv(func[2],"x",function.arg = TRUE)
  repeat{
    if(point>=0){
      #求一阶导函数值
      under<-attr(dfunc1(point),"gradient")
      step<-dfunc1(point)[1]/under[1]#步长
    }else{
      #求一阶导函数值
      under<-attr(dfunc2(point),"gradient")
      step<-dfunc2(point)[1]/under[1]#步长
    }
    #判断退出条件
    if(abs(step)<1e-5){
      cat("函数零点为:",as.character(point))
      break
    }
    #零点的迭代
    pnew<-point-step
    point<-pnew
    print(point)
  }
}
newton1(c(expression(y=sqrt(x)),expression(y=sqrt(-x))),0.1)

find_max<-function(func,point=rnorm(1,0,10)){
  #求二阶导
  d2func=deriv3(func,"x",function.arg = TRUE)
  repeat{
    #二阶导函数值
    under<-attr(d2func(point),"hessian")
    #一阶导函数值
    top<-attr(d2func(point),"gradient")
    step=top[1]/under[1]#步长
    #判断退出条件
    if(abs(step)<1e-9){
      cat("最大值点为:",as.character(point))
      cat("\n对应最大值为:",as.character(d2func(point)[1]))
      break
    }
    #零点的迭代
    pnew<-point-step
    point<-pnew
    
  }
}

library(numDeriv)
multfind_max<-function(func,n){
  #在n维空间随机取点
  point<-rnorm(n,0,10)
  repeat{
    #求一阶导函数值向量
    dfunc<-grad(func, x = point)
    #求二阶导的hessian矩阵
    d2func<-hessian(func,x = point)
    d2func<-solve(d2func)#求逆
    step<-d2func%*%dfunc#步长
    #此步可以直接写为solve(d2func,dfunc),相当于解线性方程
    #判断退出条件
    if(sum(step^2)<1e-5){
      cat("最大值点为:",as.character(point))
      cat("\n对应最大值为:",as.character(func(point)))
      break
    }
    #零点的迭代
    pnew<-point-t(step)
    point<-pnew
    
  }
}

p<-function(x){
  x[1]^2-x[1]*x[2]+x[2]^2+exp(x[2])
}
#debug(multfind_max(p,2))
multfind_max(p,c(100,100))

#用牛顿迭代法实现极大似然估计
## Generate some data 目标数据生成
beta0 <- 1
beta1 <- 3
sigma <- 1
n <- 1000
x <- rnorm(n, 3, 1)
y <- beta0 + x * beta1 + rnorm(n, mean = 0, sd = sigma)
## Make the log normal likelihood function
#极大似然估计等价的目标函数
func = function(beta) {
  sum((y - beta[1] - beta[2] * x)^2)
}
grad = function(beta) {
  matrix(c(sum(-2 * (y - beta[1] - beta[2] * x)), sum(-2 * x * (y - beta[1] - beta[2] * x))), 2, 1)
}
hess = function(beta) {
  matrix(c(2 * length(x), 2 * sum(x), 2 * sum(x), 2 * sum(x^2)), 2, 2)
}

#针对极大似然函数参数求解的修改
new_multfind_max<-function(func,n){
  #在n维空间随机取点
  point<-rnorm(n,10,50)
  
  repeat{
    func_new<-func(point)
    #求一阶导函数值向量
    dfunc<-func_new[[2]]
    #求二阶导的hessian矩阵
    d2func<-func_new[[3]]
    
    step<-solve(d2func,dfunc)
    #此步可以直接写为solve(d2func,dfunc),相当于解线性方程
    #判断退出条件
    if(sum(step^2)<1e-5){
      cat("最大值点为:",as.character(point))
      cat("\n对应最大值为:",as.character(func(point)))
      return(t(point))
      break
    }
    #零点的迭代
    pnew<-point-t(step)
    point<-pnew
    
  }
}

optimOut <- new_multfind_max(function(beta){list(func(beta), grad(beta), hess(beta))},2)  
(beta0Hat <- optimOut[1,])
(beta1Hat <- optimOut[2,])

myLM <- lm(y ~ x)
(myLMCoef <- myLM$coefficients)
coefs <- data.frame(Method = c("Newton", "LM"), intercept = c(beta0Hat, myLMCoef[1]), slope = c(beta1Hat, myLMCoef[2]))
library(ggplot2)
qplot(x, y) + geom_abline(aes(slope = myLMCoef[2], intercept = myLMCoef[1], color = "LM"), size = 3) + geom_abline(aes(slope = beta1Hat, 
                                                                                                                       intercept = beta0Hat, color = "Newton"), linetype = "dashed", size = 2) + labs(color = "Method")

