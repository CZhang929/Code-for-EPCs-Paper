library("readxl")
library(ggplot2)
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
library(readxl)
library(party)
library("randomForest")
library(vivid) # for visualisations 
library(randomForest) # for model fit
library(ranger)       # for model fit
library(vegan)
library(rfUtilities)
library(patchwork)
library(rfPermute)
library(export)
library(tidyverse)
library(reshape2)
library(corrplot)
library(broom.mixed)
library(ggpubr)
library(jtools)
library(dplyr)
library(arrow)
 setwd("C:/Users/congc/OneDrive - California Institute of Technology/OTHERS/enegry project/01_Team_Folder/06_Final_Data/2021")
# Provide the correct file path to the Parquet file
parquet_file <- "EPC_filtered_before2021.parquet"
# Read the Parquet file
data <- read_parquet(parquet_file)

# 使用str()函数查看数据框的结构
str(data)

# 选择所有字符型变量的列索引
character_vars <- which(sapply(data, is.character))

# 将所有字符型变量转换为因子型变量
data[, character_vars] <- lapply(data[, character_vars], as.factor)

# 查看数据摘要
summary(data)
# 计算每个变量的缺失值比例
missing_percent <- colMeans(is.na(data)) * 100
missing_percent
# 计算整个数据集的缺失值比例
total_missing_percent <- mean(is.na(data)) * 100
total_missing_percent #12.27%

# write.csv(data, file = "factor_2021_data.csv")


 
 dat <- na.fill(data, "mean")
 dat <- as.data.frame(dat)
 # Convert character variables to numeric
 dat$CURRENT_ENERGY_EFFICIENCY <- as.numeric(as.character(dat$CURRENT_ENERGY_EFFICIENCY))
 dat$ENVIRONMENT_IMPACT_CURRENT <- as.numeric(as.character(dat$ENVIRONMENT_IMPACT_CURRENT))
 
 # Define response variables
 Y1 <- dat$CURRENT_ENERGY_EFFICIENCY
 Y2 <- dat$ENVIRONMENT_IMPACT_CURRENT 
  
 # Convert categorical variables to factors
 dat$MAINHEAT_ENERGY_EFF <- as.factor(dat$MAINHEAT_ENERGY_EFF)
 dat$MAIN_FUEL <- as.factor(dat$MAIN_FUEL)
 dat$CONSTRUCTION_AGE_BAND <- as.factor(dat$CONSTRUCTION_AGE_BAND)
 dat$TENURE <- as.factor(dat$TENURE)
 dat$HOT_WATER_ENERGY_EFF <- as.factor(dat$HOT_WATER_ENERGY_EFF)
 dat$WINDOWS_ENERGY_EFF <- as.factor(dat$WINDOWS_ENERGY_EFF)
 dat$WALLS_ENERGY_EFF <- as.factor(dat$WALLS_ENERGY_EFF)
 dat$ROOF_ENERGY_EFF <- as.factor(dat$ROOF_ENERGY_EFF)
 dat$LIGHTING_ENERGY_EFF <- as.factor(dat$LIGHTING_ENERGY_EFF)
 dat$MAINHEAT_ENV_EFF <- as.factor(dat$MAINHEAT_ENV_EFF)
 dat$HOT_WATER_ENV_EFF <- as.factor(dat$HOT_WATER_ENV_EFF)
 dat$WINDOWS_ENV_EFF <- as.factor(dat$WINDOWS_ENV_EFF)
 dat$WALLS_ENV_EFF <- as.factor(dat$WALLS_ENV_EFF)
 dat$ROOF_ENV_EFF <- as.factor(dat$ROOF_ENV_EFF)
 dat$BUILT_FORM <- as.factor(dat$BUILT_FORM)
 dat$PROPERTY_TYPE <- as.factor(dat$PROPERTY_TYPE)
 dat$LIGHTING_ENV_EFF <- as.factor(dat$LIGHTING_ENV_EFF)
 
 # Define predictors for each model
 predictors1 <- c("MAINHEAT_ENERGY_EFF", "MAIN_FUEL", "CONSTRUCTION_AGE_BAND", 
                  "TENURE", "PROPERTY_TYPE", "BUILT_FORM",
                  "HOT_WATER_ENERGY_EFF", "WINDOWS_ENERGY_EFF", "WALLS_ENERGY_EFF", 
                  "ROOF_ENERGY_EFF", "LIGHTING_ENERGY_EFF")
 
 predictors2 <- c("MAINHEAT_ENERGY_EFF", "MAIN_FUEL", "CONSTRUCTION_AGE_BAND",
                  "PROPERTY_TYPE", "BUILT_FORM",
                  "TENURE", "HOT_WATER_ENV_EFF", "WINDOWS_ENV_EFF", "WALLS_ENV_EFF", 
                  "ROOF_ENV_EFF", "LIGHTING_ENV_EFF")

 # Fit linear models
 fit1 <- lm(Y1 ~ ., data = dat[, predictors1])
 fit2 <- lm(Y2 ~ ., data = dat[, predictors2])
 
 
 
 
 # Install and load the glmnet package if not already installed
 # install.packages("glmnet")
 library(glmnet)

 # Fit ridge regression model
 ridge_fit1 <- glmnet(as.matrix(dat[, predictors1]), Y1, alpha = 0, lambda = 0.1)
 ridge_fit2 <- glmnet(as.matrix(dat[, predictors2]), Y2, alpha = 0, lambda = 0.1)
 
  
 # Print coefficients for the ridge regression model
 coef(ridge_fit1)
 coef(ridge_fit2)
 
 
 library(glmnet)
 library(ggplot2)
 
 # Fit ridge regression model
 ridge_fit1 <- glmnet(as.matrix(dat[, predictors1]), Y1, alpha = 0, lambda = 0.2)
 ridge_fit2 <- glmnet(as.matrix(dat[, predictors2]), Y2, alpha = 0, lambda = 0.2)
 
 # Extract coefficients
 coef1 <- coef(ridge_fit1)
 coef2 <- coef(ridge_fit2)
 
 # Create data frames for coefficients
 coef_df1 <- data.frame(variable = rownames(coef1)[-1], coefficient = coef1[-1])
 coef_df2 <- data.frame(variable = rownames(coef2)[-1], coefficient = coef2[-1])
 
 # # Create plots
 # ggplot(coef_df1, aes(x = variable, y = coefficient)) +
 #   geom_bar(stat = "identity", fill = "skyblue") +
 #   ggtitle("Ridge Regression Coefficients for Model 1")
 # 
 # ggplot(coef_df2, aes(x = variable, y = coefficient)) +
 #   geom_line() +
 #   ggtitle("Ridge Regression Coefficients for Model 2")
 # 
 # 
 # 
 # png('ridge model 1.png',
 #     height = 45,
 #     width = 45,
 #     units = 'cm',
 #     res = 300)
 # plot.new()
 # custom_colors <- c("#93cc82","#e8c559")
 # title("Plot for Current energy efficiency model and environmental impact current model")
 # plot_summs(ridge_fit1, ridge_fit2, robust = list(FALSE, "HC1", "HC3"),
 #            model.names = c("Current energy efficiency model", "Environment impact current model"))
 # dev.off()
 # 
 # 
 # 

 png('MODEL 1 Linear.png',
     height = 45,
     width = 45,
     units = 'cm',
     res = 300)
 plot.new()
 custom_colors <- c("#93cc82","#e8c559")
 title("Plot for Current energy efficiency model and environmental impact current model")
 plot_summs(fit1, fit2, robust = list(FALSE, "HC1", "HC3"),
            model.names = c("Current energy efficiency model", "Environment impact current model"))
 dev.off()
 
 ###### # Fit non linear models
 fit1 <- lm(Y1 ~ poly(MAINHEAT_ENERGY_EFF, 2) + poly(MAIN_FUEL, 2) +
              poly(CONSTRUCTION_AGE_BAND, 2) + poly(BUILT_FORM, 2) +
              poly(PROPERTY_TYPE, 2) + poly(TENURE, 2) +
              HOT_WATER_ENERGY_EFF + WINDOWS_ENERGY_EFF + WALLS_ENERGY_EFF + 
              ROOF_ENERGY_EFF + LIGHTING_ENERGY_EFF, 
            data = dat[, predictors1])
 
 fit2 <- lm(Y2 ~   poly(MAINHEAT_ENERGY_EFF, 2) + poly(MAIN_FUEL, 2) +
              poly(CONSTRUCTION_AGE_BAND, 2) + poly(BUILT_FORM, 2) +
              poly(PROPERTY_TYPE, 2) + poly(TENURE, 2) +
              HOT_WATER_ENV_EFF + WINDOWS_ENV_EFF + WALLS_ENV_EFF + 
              ROOF_ENV_EFF + LIGHTING_ENV_EFF, 
            data = dat[, predictors2])
 
 summary(fit1)
 summary(fit2)
 
 # 使用逐步回归进行特征选择
 # step_model1 <- step(fit1)
 # step_model2 <- step(fit2)
 
 png('MODEL 1 PLOT.png',
     height = 45,
     width = 45,
     units = 'cm',
     res = 300)
 plot.new()
 custom_colors <- c("#93cc82","#e8c559")
 title("Plot for Current energy efficiency model and environmental impact current model")
 plot_summs(fit1, fit2, robust = list(FALSE, "HC1", "HC3"),
            model.names = c("Current energy efficiency model", "Environment impact current model"))
 dev.off()
 ######
 ######
