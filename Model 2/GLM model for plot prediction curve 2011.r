##########

library(nnet)
library(randomForest) 
library(tidyverse) 
library(dlnm)
library(splines)
library(tsModel)
library(ggplot2)
library(openair)
library(officer) 
library(knitr)
library(dplyr)
library(purrr)
library(shiny)
library(splines)
library("readxl")
library(descriptr)
library(corrgram) 
library(mice)
library(reshape2) 
library(openxlsx)
library(e1071)
library(rpart)
library(rpart.plot)
library(xlsx2dfs)
library("corrplot")
library(party)
library(vivid)
library(ranger)
library(vegan)
library(patchwork)
library(rfPermute)
library(zoo)  
library(export)
library(reshape2)
library(corrplot)
library(broom.mixed)
library(ggpubr)
library(jtools)
library("devtools")
library(conflicted)
library(readxl)
library("network")
library(factoextra)
library(cluster)
library("sna")
library("intergraph")
library("network")
library("vivo")
library("sna")
library("intergraph")
set.seed(1701)
#########################
#With 2021 data
##################################################################
setwd("C:/Users/congc/OneDrive - California Institute of Technology/OTHERS/enegry project/01_Team_Folder/06_Final_Data/2021/largecata/2011")
data <- read.csv("cleaned_2011_data_largecata.csv", sep = ",")
# 将 CURRENTENERGYEFFICIENCY 转换为数值型数据
data$CURRENTENERGYEFFICIENCY <- as.numeric(data$CURRENTENERGYEFFICIENCY)
# 执行随机森林 for ENVIRONMENT_IMPACT_CURRENT
dat <- data[,-c(1:5,14)]
# 检查响应变量的数据类型
class(dat$ENVIRONMENTIMPACTCURRENT)
# 如果数据类型不是数值型的，尝试将其转换为数值型
dat$ENVIRONMENTIMPACTCURRENT <- as.numeric(as.character(dat$ENVIRONMENTIMPACTCURRENT))

df2011 <- dat[,-8]
# 假设你的数据框为dat
df2011 <- as.data.frame(lapply(df2011, as.numeric))

####################GLM MODEL2  ###
library(MASS)
library(mgcv)

# 设置随机数种子以确保结果可重复
set.seed(123)

# 随机抽样索引，将数据集分成80%训练集和20%测试集
train_index <- sample(1:nrow(df2011), 0.8 * nrow(df2011))
test_index <- setdiff(1:nrow(df2011), train_index)

# 根据索引划分数据集
train_set <- df2011[train_index, ]
test_data <- df2011[test_index, ]
 
GLM11envir <- glm(ENVIRONMENTIMPACTCURRENT ~ ., data = train_set, family = gaussian())

test_data <- na.omit(test_data)
predicted_GLM11envir <- predict(GLM11envir, newdata = test_data, type = "response")
# 计算预测值与实际观测值之间的差异
residuals <- predicted_GLM11envir - test_data$ENVIRONMENTIMPACTCURRENT
# 计算均方误差（MSE）
mseENVIR <- mean(residuals^2);mseENVIR
# 计算均方根误差（RMSE）
rmseENVIR <- sqrt(mseENVIR);rmseENVIR

predicted_values <- predict(GLM11envir, newdata = test_data, type = "response")
library(ggplot2)

png('ENVIRONMENTIMPACTCURRENT 2011.png',
    height = 20,
    width = 35,
    units = 'cm',
    res = 300)
plot.new()
# 创建数据框，包含实际值和预测值
plot_data <- data.frame(Observations = 1:length(test_data$ENVIRONMENTIMPACTCURRENT),
                        Actual = test_data$ENVIRONMENTIMPACTCURRENT,
                        Predicted = predicted_values)

# 绘制实际和预测曲线
ggplot(plot_data, aes(x = Observations)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(x = "Observations", y = "Environment impact current values", color = "Lines",
       title = "Actual vs. Predicted Values") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, length(test_data$ENVIRONMENTIMPACTCURRENT), by = 100))
dev.off()



dat2 <- data[,-c(1:5,13,15)]
dat2 <- as.data.frame(lapply(dat2, as.numeric))
library(MASS)
library(mgcv)
# 读取数据集

# 设置随机种子以确保结果可重复
set.seed(42)

# 随机抽样索引，将数据集分成80%训练集和20%测试集
train_index <- sample(1:nrow(dat2), 0.8 * nrow(dat2))
test_index <- setdiff(1:nrow(dat2), train_index)

# 根据索引划分数据集
train_set <- dat2[train_index, ]
test_set <- dat2[test_index, ]



GLM21energ<- glm(CURRENTENERGYEFFICIENCY ~ ., train_set,family = gaussian())

# Check column names of train_set
names(train_set)
train_set <- na.omit(train_set)
# Check column names of test_data
names(test_set)
test_set <- na.omit(test_set)
predicted_values <- predict(GLM21energ, newdata = test_set, type = "response")
library(ggplot2)

png('CURRENTENERGYEFFICIENCY 2011.png',
    height = 20,
    width = 35,
    units = 'cm',
    res = 300)
plot.new()
# 创建数据框，包含实际值和预测值
plot_data <- data.frame(Observations = 1:length(test_data$CURRENTENERGYEFFICIENCY),
                        Actual = test_data$CURRENTENERGYEFFICIENCY,
                        Predicted = predicted_values)
library(stats)
# 绘制实际和预测曲线
ggplot(plot_data, aes(x = Observations)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(x = "Observations", y = "Energy efficiency values", color = "Lines",
       title = "Actual vs. Predicted Values") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, length(test_data$CURRENTENERGYEFFICIENCY), by = 100))
dev.off()
#### Done
  

GLM21energ<- glm(CURRENTENERGYEFFICIENCY ~ ., train_set,family = gaussian())
test_set <- na.omit(test_set)
predicted_values <- predict(GLM21energ, newdata = test_set, type = "response")
residuals <- predicted_values - test_set$CURRENTENERGYEFFICIENCY
# 计算均方误差（MSE）
mse <- mean(residuals^2);mse
# 计算均方根误差（RMSE）
rmse <- sqrt(mse);rmse



