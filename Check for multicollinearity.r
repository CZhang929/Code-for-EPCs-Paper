# 导入所需的库
library(MASS)  # 用于进行主成分分析
library(dplyr) # 用于数据处理

# 保存列名
col_names1 <- colnames(dat[, predictors1])
col_names2 <- colnames(dat[, predictors2])

# 将因子变量转换为虚拟变量
dat_dummies1 <- model.matrix(~., data = dat[, predictors1])
dat_dummies2 <- model.matrix(~., data = dat[, predictors2])

# 检查数据中是否存在常量列或零方差列，并删除
dat_dummies1 <- dat_dummies1[, apply(dat_dummies1, 2, var) != 0] # 删除零方差列
dat_dummies2 <- dat_dummies2[, apply(dat_dummies2, 2, var) != 0] # 删除零方差列

# 进行主成分分析
pca1 <- prcomp(dat_dummies1, scale. = TRUE) # 对第一个模型的自变量进行PCA
pca2 <- prcomp(dat_dummies2, scale. = TRUE) # 对第二个模型的自变量进行PCA

# 提取主成分得分
X_pca1 <- pca1$x
X_pca2 <- pca2$x

# 确保主成分得分和因变量具有相同数量的行
n_rows <- min(nrow(dat), nrow(X_pca1))

# 将主成分得分转换为数据框并恢复列名
X_pca1_df <- as.data.frame(X_pca1[1:n_rows,])
colnames(X_pca1_df) <- paste0("PC_", 1:ncol(X_pca1_df))

X_pca2_df <- as.data.frame(X_pca2[1:n_rows,])
colnames(X_pca2_df) <- paste0("PC_", 1:ncol(X_pca2_df))

# 确保因变量 Y1 和主成分得分具有相同数量的行
n_rows_pca1 <- min(nrow(dat), nrow(X_pca1))
n_rows_Y1 <- min(nrow(dat), length(dat$Y1))
n_rows_fit1 <- min(n_rows_pca1, n_rows_Y1)

# 确保因变量 Y2 和主成分得分具有相同数量的行
n_rows_pca1 <- min(nrow(dat), nrow(X_pca1))
n_rows_Y2 <- min(nrow(dat), length(dat$Y2))
n_rows_fit2 <- min(n_rows_pca1, n_rows_Y2)

# 拟合线性模型
fit2 <- lm(Y2 ~ ., data = data.frame(Y2 = dat$Y2[1:n_rows_fit2], X_pca1[1:n_rows_fit2,]))
 
 
 
# 查看模型摘要
summary(fit1)
summary(fit2)

