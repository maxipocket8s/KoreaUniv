library(moments)
library(corrplot)
library(dplyr)

admission_data <- read.csv('Assignment2.csv')

# 결측치 확인
for (i in 1:9) {
  cat('column', i, "결측치:", sum(is.na(admission_data[i])), '\n')
}

# boxplot 도시 및 Mean, Standard deviation, Skewness, Kurtosis
par(mfrow=c(3, 3))
for (i in 1:8) {
  boxplot(admission_data[i+1], main=colnames(admission_data[i+1]))
}
dev.off()

admission_mat <- matrix(0, nrow=4, ncol=8)
rownames(admission_mat) <- c('Mean', 'Std', 'Skewness', 'Kurtosis')
colnames(admission_mat) <- colnames(admission_data[c(2:9)])
for (i in 1:8) {
  admission_mat[1, i] <- mean(admission_data[[i+1]])
  admission_mat[2, i] <- sqrt(var(admission_data[[i+1]]))
  admission_mat[3, i] <- skewness(admission_data[[i+1]])
  admission_mat[4, i] <- kurtosis(admission_data[[i+1]])
}
admission_mat

# 이상치 제거
for (i in 1:8) {
  admission_data[which(admission_data[, i+1] > admission_mat[1, i] + 2*admission_mat[2, i]
                       | admission_data[, i+1] < admission_mat[1, i] - 2*admission_mat[2, i]), i] <- NA
}

# 산점도 및 상관관계
par(mfrow=c(3, 4))
for (i in 1:7) {
  for (j in (i+1):8) {
    plot(admission_data[, i+1], admission_data[, j+1], xlab=colnames(admission_data[i+1]), ylab=colnames(admission_data[j+1]))
  }
}
dev.off()

corrplot(cor(admission_data[, -1], use='complete.obs'), method='circle', type='upper', addCoef.col='black', tl.col='darkgreen', tl.srt=45, number.cex=0.7)

# binary target variable 전환 및 데이터 분리
admission_data[, 9] <- ifelse(admission_data[, 9] > 0.8, 1, 0)

trn_idx <- sample(1:nrow(admission_data), round(0.7*nrow(admission_data)))
admission_data <- admission_data[, -1]
trn_data <- admission_data[trn_idx, ]
val_data <- admission_data[-trn_idx, ]

# Logistic Regression 학습
trn_data[, 1:7] <- scale(trn_data[, 1:7], center=T, scale=T)
full_lr <- glm(Chance.of.Admit ~ ., family=binomial, trn_data)
summary(full_lr)

# val_data 예측
perf_eval <- function(cm) {
  TPR <- cm[2, 2]/sum(cm[2, ])
  TNR <- cm[1, 1]/sum(cm[1, ])
  FPR <- cm[1, 2]/sum(cm[1, ])
  FNR <- cm[2, 1]/sum(cm[2, ])
  ACC <- (cm[1, 1]+cm[2, 2])/sum(cm)
  BCR <- sqrt(TPR*TNR)
  PRE <- cm[2, 2]/sum(cm[, 2])
  F1 <- 2*TPR*PRE/(TPR+PRE)
  return(c(TPR, TNR, FPR, FNR, ACC, BCR, PRE, F1))
}

val_data[, 1:7] <- scale(val_data[, 1:7], center=T, scale=T)
lr_response <- predict(full_lr, type='response', newdata=val_data)
lr_predict <- rep(0, nrow(val_data))
lr_predict[which(lr_response >= 0.5)] <- 1
cm_full <- table(val_data[, 8], lr_predict)
cm_full
perf_eval(cm_full)

# AUROC
TPR_list <- c()
FPR_list <- c()
for (i in c(seq(0, 1, 0.1))) {
  lr_predict <- rep(0, nrow(val_data))
  lr_predict[which(lr_response >= i)] <- 1
  cm <- matrix(0, 2, 2)
  cm[1, 1] <- length(which(val_data[, 8] == 0 & lr_predict == 0))
  cm[1, 2] <- length(which(val_data[, 8] == 0 & lr_predict == 1))
  cm[2, 1] <- length(which(val_data[, 8] == 1 & lr_predict == 0))
  cm[2, 2] <- length(which(val_data[, 8] == 1 & lr_predict == 1))
  perf_eval(cm)
  TPR_list <- c(TPR_list, perf_eval(cm)[1])
  FPR_list <- c(FPR_list, perf_eval(cm)[3])
}
plot(FPR_list, TPR_list, type='l', xlim=c(0, 1), ylim=c(0, 1), xlab='FPR', ylab='TPR', main='AUROC')
