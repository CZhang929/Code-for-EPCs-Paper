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
data <- read.csv("cleaned_2021_data_largecata.csv", sep = ",")
 
# 将 CURRENTENERGYEFFICIENCY 转换为数值型数据
data$CURRENTENERGYEFFICIENCY <- as.numeric(data$CURRENTENERGYEFFICIENCY)
  
# 执行随机森林 for ENVIRONMENT_IMPACT_CURRENT
dat <- data[,-c(1:5,14)]
# 检查响应变量的数据类型
class(dat$ENVIRONMENTIMPACTCURRENT)
# 如果数据类型不是数值型的，尝试将其转换为数值型
dat$ENVIRONMENTIMPACTCURRENT <- as.numeric(as.character(dat$ENVIRONMENTIMPACTCURRENT))

df2021 <- dat
# 假设你的数据框为dat

# 设置随机数种子以确保结果可重复
set.seed(123)

# 创建一个随机序列来分割数据
indices <- sample(1:nrow(dat), size = nrow(dat), replace = FALSE)

# 计算用于训练集和测试集的观测数
train_size <- round(0.8 * nrow(dat))
test_size <- nrow(dat) - train_size

# 使用随机序列分割数据
train_data <- dat[indices[1:train_size], ]
test_data <- dat[indices[(train_size + 1):nrow(dat)], ]

#########################
rf <- randomForest(dat$ENVIRONMENTIMPACTCURRENT ~ ., 
                   data = dat, mtry = 3, importance = TRUE, proximity = FALSE)
model <- rf
importance(model)
png('ENVIRONMENTIMPACTCURRENT.png',
    height = 20,
    width = 35,
    units = 'cm',
    res = 300)
plot.new()
varImpPlot(model, main = "Random forest variable importance plot for Environment impact current", cex.main = 0.3, cex.axis = 0.7)
dev.off()
##########################
# 执行随机森林 for Current energy efficiency
dat2 <- data[,-c(1:5,15)]
rf2 <- randomForest(CURRENTENERGYEFFICIENCY ~ ., 
                    data = dat2, mtry = 3, importance = TRUE, proximity = FALSE)
model2 <- rf2
importance(model2)
png('CURRENTENERGYEFFICIENCY.png',
    height =20,
    width = 35,
    units = 'cm',
    res = 300)
plot.new()
varImpPlot(model2, main = "Random forest variable importance plot for Current energy efficiency",
           cex.main = 0.3, cex.axis = 0.7)
dev.off()
######################## 
library(dplyr)
####Plot  heatmap FOR ENVIRONMENTIMPACTCURRENT.png#####
viviRf  <- vivi(fit = model, 
                data = dat, 
                response = "ENVIRONMENTIMPACTCURRENT",
                gridSize = 50,
                importanceType = "agnostic",
                nmax = 500,
                reorder = TRUE,
                predictFun = NULL,
                numPerm = 4,
                showVimpError = FALSE)


png('Heatmap for ENVIRONMENTIMPACTCURRENT.png',
    height = 35,
    width = 35,
    units = 'cm',
    res = 300)
plot.new()
heatmap_obj <- viviHeatmap(mat = viviRf)
# Decrease the label size within the heatmap object
heatmap_obj$heatmaps[[1]]$labels_cex <- 0.5  # Adjust this value as needed
# Plot the adjusted heatmap without x-axis labels
plot(heatmap_obj, xlab = "")
dev.off()
# 调整网络图标签大小
png('RF for ENVIRONMENTIMPACTCURRENT.png',
    height =35,
    width = 35,
    units = 'cm',
    res = 300)
plot.new()
par(cex.lab = 0.8)  # 设置标签大小为默认大小的 80%
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)
viviNetwork(mat = viviRf, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))
dev.off()
library(network)


####Plot  heatmap FOR CURRENTENERGYEFFICIENCY#####


viviRf2  <- vivi(fit = model2, 
                 data = dat2, 
                 response = "CURRENTENERGYEFFICIENCY",
                 gridSize = 50,
                 importanceType = "agnostic",
                 nmax = 500,
                 reorder = TRUE,
                 predictFun = NULL,
                 numPerm = 4,
                 showVimpError = FALSE)
png('Heatmap for CURRENTENERGYEFFICIENCY.PNG',
    height = 35,
    width = 35,
    units = 'cm',
    res = 300)
plot.new()
heatmap_obj2 <- viviHeatmap(mat = viviRf2)
# Decrease the label size within the heatmap object
heatmap_obj2$heatmaps[[1]]$labels_cex <- 0.5  # Adjust this value as needed
# Plot the adjusted heatmap without x-axis labels
plot(heatmap_obj2, xlab = "")
dev.off()
# 调整网络图标签大小
png('RF for CURRENTENERGYEFFICIENCY.PNG',
    height = 35,
    width = 35,
    units = 'cm',
    res = 300)
plot.new()
par(cex.lab = 0.8)  # 设置标签大小为默认大小的 80%
viviNetwork(mat = viviRf2, intThreshold = 0.12, removeNode = FALSE)
viviNetwork(mat = viviRf2, intThreshold = 0.12, removeNode = TRUE)
viviNetwork(mat = viviRf2, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))

library(network)



# # nydata <- cbind(data[,c(1,2,15:21)],comData)
 
rf2 <- randomForest(ENVIRONMENTIMPACTCURRENT ~ ., 
                    data = data, mtry = 3, importance = TRUE, proximity = FALSE)
set.seed(1701)
viviRf  <- vivi(fit = rf2, 
                data = data, 
                response = "ENVIRONMENTIMPACTCURRENT",
                gridSize = 50,
                importanceType = "agnostic",
                nmax = 500,
                reorder = TRUE,
                predictFun = NULL,
                numPerm = 4,
                showVimpError = FALSE)
viviHeatmap(mat = viviRf)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)
viviNetwork(mat = viviRf, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))









##################### Plot EPC 2021 data ############3
d2021 <- read.csv("final_EPC_2021_London_only.csv")
EPCY1 <- randomForest(CURRENT_ENERGY_EFFICIENCY~.,data=d2021,mtyr=3,importance=T,proximity=F)

data <- d2021
set.seed(1701)
viviRf  <- vivi(fit = EPCY1, 
                data = d2021, 
                response = "CURRENT_ENERGY_EFFICIENCY",
                gridSize = 50,
                importanceType = "agnostic",
                nmax = 500,
                reorder = TRUE,
                predictFun = NULL,
                numPerm = 4,
                showVimpError = FALSE)
viviHeatmap(mat = viviRf)

viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)
viviNetwork(mat = viviRf, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))


EPCY2 <- randomForest(ENVIRONMENT_IMPACT_CURRENT~.,data=d2021,mtyr=3,importance=T,proximity=F)
set.seed(1701)
viviRf  <- vivi(fit = EPCY2, 
                data = d2021, 
                response = "ENVIRONMENT_IMPACT_CURRENT",
                gridSize = 50,
                importanceType = "agnostic",
                nmax = 500,
                reorder = TRUE,
                predictFun = NULL,
                numPerm = 4,
                showVimpError = FALSE)
viviHeatmap(mat = viviRf)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)
viviNetwork(mat = viviRf, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))
##################### Plot EPC 2011 data ############3
d2011 <- read.csv("final_EPC_2011_London_only.csv")
EPCY1_2011 <- randomForest(CURRENT_ENERGY_EFFICIENCY~.,data=d2021,mtyr=3,importance=T,proximity=F)
set.seed(1701)
viviRf  <- vivi(fit = EPCY1_2011, 
                data = d2021, 
                response = "CURRENT_ENERGY_EFFICIENCY",
                gridSize = 50,
                importanceType = "agnostic",
                nmax = 500,
                reorder = TRUE,
                predictFun = NULL,
                numPerm = 4,
                showVimpError = FALSE)
viviHeatmap(mat = viviRf)

viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)
viviNetwork(mat = viviRf, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))


EPCY2_2011 <- randomForest(ENVIRONMENT_IMPACT_CURRENT~.,data=d2021,mtyr=3,importance=T,proximity=F)
set.seed(1701)
viviRf  <- vivi(fit = EPCY2_2011, 
                data = d2021, 
                response = "ENVIRONMENT_IMPACT_CURRENT",
                gridSize = 50,
                importanceType = "agnostic",
                nmax = 500,
                reorder = TRUE,
                predictFun = NULL,
                numPerm = 4,
                showVimpError = FALSE)
viviHeatmap(mat = viviRf)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)
viviNetwork(mat = viviRf, 
            layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))

###Done


