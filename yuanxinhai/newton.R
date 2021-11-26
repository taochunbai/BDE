setwd("E:/REnvironment")                                      #初始化
#Lab5-1
rm(list = ls())                                             
root<-function(x0,f0){                                        #设函数root(x0,f0)，x0为迭代初始点，f0为所求函数表达式
  f1<-D(f0,"x")                                               #求一阶导数f1
  x<-x0                                                       #把初始值赋给x
  while(abs(eval(f0))>=10^(-6)){x<-x-eval(f0)/eval(f1)}       #用牛顿法进行迭代，设置停止标准为f(x)<10^(-6)
  return(x)                                                   #返回计算结果
}
fx<-expression(x^2-5)                                         #带入f(x)=x^2-5以及不同的初始值进行计算
root(-3,fx)                                                   #得到两个根-2.236068和2.236068
root(-2,fx)
root(-1,fx)
root(0,fx)                                                    #发现初始值为0时报错，因为0处导数为0，作分母没有意义
root(1,fx)
root(2,fx)
root(3,fx)


#Lab5-2 由于此题原函数在0处没有导数，所以只好人工输入原函数和导函数:
rm(list = ls())                                             
f0<-function(x){(abs(x))^(0.5)}
f1<-function(x){(abs(x))^(0.5)/2/x}
x0<-0.25
while(abs(f0(x0))>=10^(-6)){x0<-x0-f0(x0)/f1(x0)}
x0
#发现出现死循环
#经过计算发现x0-f0(x0)/f1(x0)的值等于-x0
#说明迭代点只会在初始点和其相反数之间跳动
#无法收敛，Newton法失效
#通过简单计算易知零点为0


#Lab5-3
rm(list = ls())                                             
root<-function(x0,f0){                                        #设函数root(x0,f0)，x0为迭代初始点，f0为所求函数表达式
  f1<-D(f0,"x")                                               #求一阶导数f1
  x<-x0                                                       #把初始值赋给x
  while(abs(eval(f0))>=10^(-6)){x<-x-eval(f0)/eval(f1)}       #用牛顿法进行迭代，设置停止标准为f(x)<10^(-6)
  return(x)                                                   #返回计算结果
}

fx<-expression(x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2)           #带入f(x)以及初始值0.5,0.6进行计算
root(0.5,fx)                                                  #得到相同结果x=0.4303876
root(0.6,fx)




#Lab5-continued
rm(list = ls())                                             
opt<-function(x0,f0){                                         #设函数opt(x0,f0)，x0为迭代初始点，f0为所求函数表达式
  f1<-D(f0,"x")                                               #求一阶导数f1
  f2<-D(f1,"x")                                               #求二阶导数f2
  x<-x0                                                       #把初始值赋给x
  while(abs(eval(f1))>=10^(-6)){x<-x-eval(f1)/eval(f2)}       #用牛顿法进行迭代，设置停止标准为f'(x)<10^(-6)
  if(eval(f2)>0){return(x)}                                   #用二阶导大于0的条件筛选出极小值
  if(eval(f2)<0){return("no minimum")}
  }
fx<-expression(x^4)                                           #代入函数进行计算
opt(1,fx)





#P19 编写一个优化函数：
rm(list = ls())                                             
opt<-function(x0,f0){                                         #设函数opt(x0,f0)，x0为迭代初始点，f0为所求函数表达式
  f1<-D(f0,"x")                                               #求一阶导数f1
  f2<-D(f1,"x")                                               #求二阶导数f2
  x<-x0                                                       #把初始值赋给x
  while(abs(eval(f1))>=10^(-6)){x<-x-eval(f1)/eval(f2)}       #用牛顿法进行迭代，设置停止标准为f'(x)<10^(-6)
  return(x)                                                   #返回计算结果
}
fx<-expression(x^2+2*x+1)                                     #代入一个简单的例子进行测试f(x)=x^2+2*x+1
opt(2,fx)                                                     #得出最优点为-1




#lab 6
rm(list = ls())
fx<-deriv(y ~ x1^2-x1*x2+x2^2+exp(x2),c("x1","x2"),hessian=TRUE,function.arg = TRUE)  #构造fx函数，其中包含了原函数的一、二阶导数信息
x01<-1                                                                                #设置初始点(1,1)
x02<-1
x0<-c(x01,x02)
f0<-expression(x01^2-x01*x02+x02^2+exp(x02))
dfdx1<-D(f0,"x01")
dfdx2<-D(f0,"x02")
while(((eval(dfdx1))^2 + (eval(dfdx2))^2)^(0.5) >= 10^(-6)){                          #设置迭代终止条件
  a<-fx(x0[1],x0[2])
  gradient<-attr(a,"gradient")                                                        #计算梯度向量
  gradient<-as.vector(gradient)
  hessian<-attr(a,"hessian")                                                          #计算Hessian矩阵
  h<-matrix(hessian,nrow=2,ncol=2,byrow=TRUE)
  x0<-x0-solve(h)%*%gradient                                                          #递推计算
  x01<-x0[1]
  x02<-x0[2]
}
x0                                                                                    #得到最优点(-0.2162814,-0.4325628)





