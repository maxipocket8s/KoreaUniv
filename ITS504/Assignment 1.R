library(moments)
library(corrplot)
library(rsq)

house_info <- read.csv("Assignment1.csv")

perf_eval <- function(tgt_y, pred_y) {
  MAE <- mean(abs(tgt_y - pred_y))
  MAPE <- 100*mean(abs((tgt_y - pred_y)/tgt_y))
  RMSE <- sqrt(mean((tgt_y - pred_y)**2))
  return(c(MAE, MAPE, RMSE))
}

# 데이터 컬럼 정리
house_info <- house_info[-c(1, 2, 17)]
nHouse <- nrow(house_info)
nVar <- ncol(house_info)

# 잔차 평균 및 표준편차, 왜도, 첨도 매트릭스 초기화
house_mat <- matrix(0, nrow=4, ncol=nVar)
rownames(house_mat) <- c('Mean', 'Std_deviation', 'Skewness', 'Kurtosis')
colnames(house_mat) <- colnames(house_info)
house_mat <- house_mat[, -1]

# 데이터 분리
house_trn_idx <- sample(1:nHouse, round(0.7*nHouse))
house_trn_data <- house_info[house_trn_idx, ]
house_val_data <- house_info[-house_trn_idx, ]

# boxplot 초기
for (i in 1:(nVar-1)) {
  boxplot(house_trn_data[i+1])
}

# 잔차 평균 및 표준편차, 왜도, 첨도 매트릭스
for (i in 1:(nVar-1)) {
  house_mat[1, i] <- mean(resid(lm(price ~ house_trn_data[[i]], data=house_trn_data)))
  house_mat[2, i] <- sqrt(var(resid(lm(price ~ house_trn_data[[i]], data=house_trn_data))))
  house_mat[3, i] <- skewness(resid(lm(price ~ house_trn_data[[i]], data=house_trn_data)))
  house_mat[4, i] <- kurtosis(resid(lm(price ~ house_trn_data[[i]], data=house_trn_data)))
}

# 이상치 제거
for (i in 1:(nVar-1)) {
  up <- which(house_trn_data[i+1] > (summary(house_trn_data[[i+1]])[5] + 1.5*IQR(house_trn_data[[i+1]])))
  down <- which(house_trn_data[i+1] < (summary(house_trn_data[[i+1]])[2] - 1.5*IQR(house_trn_data[[i+1]])))
  house_trn_data[up, i+1] <- NA
  house_trn_data[down, i+1] <- NA
}

# boxplot 이상치 제거 후
for (i in 1:(nVar-1)) {
  boxplot(house_trn_data[i+1])
}

# 상관관계 계산
for (i in 1:(nVar-2)) {
  for (j in (i+1):(nVar-1)) {
    plot(house_trn_data[, i+1], house_trn_data[, j+1], xlab=colnames(house_trn_data[i+1]), ylab=colnames(house_trn_data[j+1]))
  }
}

corrplot(cor(house_trn_data, use='complete.obs'), method='circle', type='upper', addCoef.col='black', tl.col='darkgreen', tl.srt=45, number.cex=0.7)

# MLR 모델 학습
mlr_house <- lm(price ~ ., data=house_trn_data)
rsq(mlr_house, adj=TRUE)
summary(mlr_house)
plot(mlr_house)

# MAE, MAPE, RMSE 계산
mlr_house_haty <- predict(mlr_house, newdata=house_val_data)
perf_eval(house_val_data$price, mlr_house_haty)

# 7개 변수로 MLR 모델 학습
house_trn_data <- house_trn_data[-c(2, 5, 7, 8, 11, 12, 14, 16, 17, 18)]
mlr_house <- lm(price ~ ., data=house_trn_data)
rsq(mlr_house, adj=TRUE)
summary(mlr_house)
plot(mlr_house)

# 7개 변수로 MAE, MAPE, RMSE 계산
mlr_house_haty <- predict(mlr_house, newdata=house_val_data)
perf_eval(house_val_data$price, mlr_house_haty)