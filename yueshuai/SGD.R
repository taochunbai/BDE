# 设置工作路径
setwd("E:/Sulfonamides_of_Shenzhen/code")

# 数据读取
RPF <- read.csv("../data/RPF_Sulfonamides.CSV", header = TRUE)
LOD <- read.csv("../data/LOD_Sulfonamides.CSV", header = TRUE)
Livestock <- read.csv("../data/Livestock_2016-2020.CSV",header = TRUE)
Aquatic <- read.csv("../data/Aquatic_2016-2020.CSV", header = TRUE)
Consumption <- read.csv("../data/consumption.CSV", header = TRUE)
MRL_Livestock <- read.csv("../data/MRL_Livestock_2020.CSV", header = TRUE)
MRL_Aquatic <- read.csv("../data/MRL_Aquatic_2020.CSV", header = TRUE)

save.image(file = ".Rdata")

# 检出率、超标率、最大值、平均值
test.summary <- function(file_Livestock, file_Aquatic, file_LOD, class_std = "total") {
  test_drug <- LOD$Name_cn    # 待检测药品
  if (class_std == "total") {
    Res <- NULL
    for (item in test_drug) {
      if (item %in% colnames(file_Livestock)) {
        temp <- file_Livestock[, which(colnames(file_Livestock) == item)]
        n <- length(temp)    # 样本数
        out <- sum(temp != 0) / n    # 检出率
        over <- sum(temp > 100) / n    #超标率
        for (i in 1: length(temp)) {if (temp[i] == 0) {temp[i] <- 0.5 * file_LOD[file_LOD$Name_cn == item, 2]}}
        Max <- round(max(temp), 3)    # 最大值
        Mean <- round(mean(temp), 3)    # 均值
        Median <- round(median(temp), 3)    # 中位数
        res <- c("畜禽类", item, n, out, over, Max, Mean, Median)
        Res <- rbind(Res, res)
      }
    }
    for (item in test_drug)  {
      if (item %in% colnames(file_Aquatic)) {
        temp <- file_Aquatic[, which(colnames(file_Aquatic) == item)]
        n <- length(temp)    # 样本数
        out <- sum(temp != 0) / n    # 检出率
        over <- sum(temp > 100) / n    #超标率
        for (i in 1: length(temp)) {if (temp[i] == 0) {temp[i] <- 0.5 * file_LOD[file_LOD$Name_cn == item, 2]}}
        Max <- round(max(temp), 3)    # 最大值
        Mean <- round(mean(temp), 3)    # 均值
        Median <- round(median(temp), 3)    # 中位数
        res <- c("水产类", item, n, out, over, Max, Mean, Median)
        Res <- rbind(Res, res) 
      }
    }      
    colnames(Res) <- c("产品类别", "药物", "样本数", "检出率", 
                   "超标率", "最大值(μg/kg)", "均值(μg/kg)", "中位数(μg/kg)")
    write.csv(Res, file = "../result/Summary_Total.CSV", row.names = FALSE)
  }
  if (class_std == "year") {
    Res <- NULL
    for (item in test_drug) {
      for (year in sort(unique(file_Livestock$Year))) {
        if (item %in% colnames(file_Livestock)) {
          temp <- file_Livestock[file_Livestock$Year == year,
                                 which(colnames(file_Livestock) == item)]
          n <- length(temp)    # 样本数
          out <- sum(temp != 0) / n    # 检出率
          over <- sum(temp > 100) / n    #超标率
          for (i in 1: length(temp)) {if (temp[i] == 0) {temp[i] <- 0.5 * file_LOD[file_LOD$Name_cn == item, 2]}}
          Max <- round(max(temp), 3)    # 最大值
          Mean <- round(mean(temp), 3)    # 均值
          Median <- round(median(temp), 3)    # 中位数
          res <- c("畜禽类", item, year, n, out, over, Max, Mean, Median)
          Res <- rbind(Res, res)
        }
      }
    }
    for (item in test_drug) {
      for (year in sort(unique(file_Aquatic$Year))) {
        if (item %in% colnames(file_Aquatic)) {
          temp <- file_Aquatic[file_Aquatic$Year == year, which(colnames(file_Aquatic) == item)]
          n <- length(temp)    # 样本数
          out <- sum(temp != 0) / n    # 检出率
          over <- sum(temp > 100) / n    #超标率
          for (i in 1: length(temp)) {if (temp[i] == 0) {temp[i] <- 0.5 * file_LOD[file_LOD$Name_cn == item, 2]}}
          Max <- round(max(temp), 3)    # 最大值
          Mean <- round(mean(temp), 3)    # 均值
          Median <- round(median(temp), 3)    # 中位数
          res <- c("水产类", item, year, n, out, over, Max, Mean, Median)
          Res <- rbind(Res, res)
        }
      }
    }
    colnames(Res) <- c("产品类别", "药物", "年份", "样本数", "检出率", 
                       "超标率", "最大值(μg/kg)", "均值(μg/kg)", "中位数(μg/kg)")
    write.csv(Res, file = "../result/Summary_Year.CSV", row.names = FALSE)
  }
  if (class_std == "product") {
    Res <- NULL
    test_Livestock <- unique(file_Livestock$Product)    # 检测的畜禽种类
    test_Aquatic <- unique(file_Aquatic$Product)    # 检测的水产种类
    for (product in test_Livestock) {
      for (item in test_drug) {
        if (item %in% colnames(file_Livestock)) {
          temp <- file_Livestock[file_Livestock$Product == product, which(colnames(file_Livestock) == item)]
          n <- length(temp)    # 样本数
          out <- sum(temp != 0) / n    # 检出率
          over <- sum(temp > 100) / n    #超标率
          for (i in 1: length(temp)) {if (temp[i] == 0) {temp[i] <- 0.5 * file_LOD[file_LOD$Name_cn == item, 2]}}
          Max <- round(max(temp), 3)    # 最大值
          Mean <- round(mean(temp), 3)    # 均值
          Median <- round(median(temp), 3)    # 中位数
          res <- c("畜禽类", product, item, n, out, over, Max, Mean, Median)
          Res <- rbind(Res, res)
        }
      }
    }
    for (product in test_Aquatic) {
      for (item in test_drug) {
        if (item %in% colnames(file_Aquatic)) {
          temp <- file_Aquatic[file_Aquatic$Product == product, which(colnames(file_Aquatic) == item)]
          n <- length(temp)    # 样本数
          out <- sum(temp != 0) / n    # 检出率
          over <- sum(temp > 100) / n    #超标率
          for (i in 1: length(temp)) {if (temp[i] == 0) {temp[i] <- 0.5 * file_LOD[file_LOD$Name_cn == item, 2]}}
          Max <- round(max(temp), 3)    # 最大值
          Mean <- round(mean(temp), 3)    # 均值
          Median <- round(median(temp), 3)    # 中位数
          res <- c("水产类", product, item, n, out, over, Max, Mean, Median)
          Res <- rbind(Res, res)
        }
      }
    }
    colnames(Res) <- c("产品类别", "产品", "药物", "样本数", "检出率", 
                       "超标率", "最大值(μg/kg)", "均值(μg/kg)", "中位数(μg/kg)")
    write.csv(Res, file = "../result/Summary_Product.CSV", row.names = FALSE)    
  }
}

test.summary(Livestock, Aquatic, LOD)
test.summary(Livestock, Aquatic, LOD, class_std = "year")
test.summary(Livestock, Aquatic, LOD, class_std = "product")

# 替换
Replace <- function(file_origin, file_LOD, file_RPF, method = "optimistic") {
  test_drug <- colnames(file_origin[, 5: ncol(file_origin)])
  for (item in test_drug) {
    if (method == "optimistic") {
      test_out <- file_origin[, colnames(file_origin) == item]    # 提取对应列
        for (i in 1: length(test_out)) {
          if (test_out[i] == 0) {
            # 用0.5*LOD替换未检出数据
            # 同时用RPF换算相对毒性
            relative_tox <- 0.5*file_LOD[file_LOD$Name_cn == item, 2]*
              file_RPF[file_RPF$Name_cn == item, 5]
            file_origin[i, colnames(file_origin) == item] <- relative_tox
          }
          else {
            relative_tox <- file_origin[i, colnames(file_origin) == item]*
              file_RPF[file_RPF$Name_cn == item, 5]
            file_origin[i, colnames(file_origin) == item] <- relative_tox
          }
        }
    }
    if (method == "pessimistic") {
      test_out <- file_origin[, colnames(file_origin) == item]    # 提取对应列
        for (i in 1: length(test_out)) {
          if (test_out[i] == 0) {
            relative_tox <- file_LOD[file_LOD$Name_cn == item, 2]*
              file_RPF[file_RPF$Name_cn == item, 5]
            file_origin[i, colnames(file_origin) == item] <- relative_tox
          }
          else {
            relative_tox <- file_origin[i, colnames(file_origin) == item]*
              file_RPF[file_RPF$Name_cn == item, 5]
            file_origin[i, colnames(file_origin) == item] <- relative_tox
          }
        }
    }    
  }
  return(file_origin)
}

Livestock_optimistic <- Replace(Livestock, LOD, RPF, method = "optimistic")
Aquatic_optimistic <- Replace(Aquatic, LOD, RPF, method = "optimistic")
Livestock_pessimistic <- Replace(Livestock, LOD, RPF, method = "pessimistic")
Aquatic_pessimistic <- Replace(Aquatic, LOD, RPF, method = "pessimistic")

# 暴露量
Exposure <- function(file_replace, file_consumption) {
  # 计算每种产品毒性
  Tox <- NULL
  for (i in 1: nrow(file_replace)) {
    tox <- sum(file_replace[i, 5: ncol(file_replace)])
    Tox <- c(Tox, tox)
  }
  Exposure_res <- as.data.frame(matrix(nrow = nrow(file_replace), ncol = 15))
  j <- 0
  for (item in colnames(Exposure_res)) {
    j <- j + 1
    names(Exposure_res)[names(Exposure_res) == item] <- paste0(file_consumption[j, 2],
                                                               " ", file_consumption[j, 1])
  }
  for (k in 1: 15) {
    exposure_res <- Tox*file_consumption[k, 4]/(file_consumption[k, 3]*1000)
    Exposure_res[, k] <- exposure_res
  }
  return(Exposure_res)
}

exp_ls_opti <- Exposure(Livestock_optimistic, Consumption)
exp_aq_opti <- Exposure(Aquatic_optimistic, Consumption)
exp_ls_pessi <- Exposure(Livestock_pessimistic, Consumption)
exp_aq_pessi <- Exposure(Aquatic_pessimistic, Consumption)

# 联合暴露量
exp.unit <- function(exp_ls, exp_aq, n) {
  # exp_ls,exp_aq传入一列数据, n为抽样次数
  Exp_unit <- NULL
  for (i in 1: n) {
    exp1 <- sample(exp_ls, 1)
    exp2 <- sample(exp_aq, 1)
    Exp_unit<-c(Exp_unit, exp1 + exp2)    
  }
  return(Exp_unit)    # 暴露量的联合分布
}

# Bootstrap方法
Bootstrap <- function(exp, m, B, method){
  # exp为传入的联合分布,m为重抽样的样本量
  # B为重抽样次数, method为计算方法
  TBoot<-NULL    # 存储特定方法下的50%,2.5%和97.5%
  # SD.val<-NULL    # 存储标准差,可用于枢轴量法计算置信区间
  for (b in 1: B){
    xsample <- sample(exp, m, replace=TRUE)
    if (method == "mean") Tboot <- mean(xsample)
    else if (method == "P50") Tboot <- quantile(xsample, 0.5)
    else if (method == "P97.5") Tboot <- quantile(xsample, 0.975)
    else if (method == "P99") Tboot <- quantile(xsample, 0.99)
    else if (method == "P99.9") Tboot <- quantile(xsample, 0.999)
    TBoot <- c(TBoot, Tboot)
    # SD.val<-c(SD.val, sd(TBoot))
  }
  quantile(TBoot, c(0.5, 0.025, 0.975))
}

# 计算分位数的点估计和置信区间
exp.quantile<-function(exp_ls, exp_aq, n = 2000, m = 500, B = 2000){
  l <- ncol(exp_ls)
  method <- c("mean", "P50", "P97.5", "P99", "P99.9")
  quant.lis <- list()
  for (i in 1: l){
      quant <- NULL
      exp.unit <- exp.unit(exp_ls[, i], exp_aq[, i], n)    # 特定人群的暴露量联合分布
      for (j in 1: length(method)){
        quant <- rbind(quant, Bootstrap(exp.unit, m, B, method[j]))
      }
      rownames(quant) <- method
      quant.lis <- c(quant.lis, list(quant))
      cat(i, "finished\n")
    }
  names(quant.lis) <- colnames(exp_ls)
  quant.lis
}

# 计算并保存输出结果
res_pessi <- exp.quantile(exp_ls_pessi, exp_aq_pessi, 100000, 500, 2000)    # 悲观预计
res_optim <- exp.quantile(exp_ls_opti, exp_aq_opti, 100000, 500, 2000)    #乐观预计

# 格式化输出
output <- function(res, name){
  result <- c()
  method <- c("mean", "P50", "P97.5", "P99", "P99.9")
  for (i in 1: length(res)){
    temp <- cbind(method, res[[i]])
    temp <- cbind(names(res)[i], temp)
    result <- rbind(result, temp)
  }
  write.csv(result, paste(name,"result.csv", sep = "_"), row.names = FALSE)
}

output(res_pessi, "Pessimistic")
output(res_optim, "Optimistic")
