library(nnet)

building_data <- read.csv('Assignment5.csv')
building_input <- building_data[, 2:39]
building_target <- as.factor(building_data[, 40])

str(building_input)
barplot(table(building_input$land_surface_condition), xlab='condition', main='land_surface_condition')
barplot(table(building_input$foundation_type), xlab='type', main='foundation_type')
barplot(table(building_input$roof_type), xlab='type', main='roof_type')
barplot(table(building_input$ground_floor_type), xlab='type', main='ground_floor_type')
for (i in c(12:14, 26)) {
  barplot(table(building_input[i]), main=colnames(building_input[i]))
}

building_input[, -c(8:14, 26)] <- scale(building_input[, -c(8:14, 26)], center=T, scale=T)
input_data <- building_input
for (i in c(8:14, 26)) {
  one_hot_input <- class.ind(building_input[, i])
  idx_1 <- which(colnames(input_data)==colnames(building_input[i]))
  input_data <- cbind(input_data[, 1:(idx_1-1)], one_hot_input, building_input[, (i+1):38])
  idx_2 <- which(colnames(input_data)==colnames(building_input[i+1]))
  for (j in idx_1:(idx_2-1)) {
    input_data <- data.frame(input_data)
    colnames(input_data)[j] <- paste0(colnames(building_input[i]), '_', (j+1-idx_1))
  }
}

target_data <- class.ind(building_target)


# dummy_w <- rep(0, nrow(building_input))
# dummy_v <- rep(0, nrow(building_input))
# dummy_a <- rep(0, nrow(building_input))
# w_idx <- which(building_input$legal_ownership_status=='w')
# v_idx <- which(building_input$legal_ownership_status=='v')
# a_idx <- which(building_input$legal_ownership_status=='a')
# dummy_w[w_idx] <- 1
# dummy_v[v_idx] <- 1
# dummy_a[a_idx] <- 1
# ownership <- cbind(dummy_w, dummy_v, dummy_a)
# colnames(ownership) <- c('W', 'V', 'A')
# ownership

perf_matrix <- matrix(0, 2, 2)
colnames(perf_matrix) <- c('ACC', 'BCR')
rownames(perf_matrix) <- c('MLR', 'ANN')

ml_input_data <- data.frame(input_data, damage_grade = building_target)

trn_data <- ml_input_data[1:200000, ]
tst_data <- ml_input_data[-c(1:200000), ]

perf_eval <- function(cm) {
  ACC <- sum(diag(cm))/sum(cm)
  BCR <- 1
  for (i in 1:dim(cm)[1]) {
    BCR <- BCR * (cm[i, i]/sum(cm[i, ]))
  }
  BCR <- BCR^(1/dim(cm)[1])
  return (c(ACC, BCR))
}


ml_logit <- multinom(damage_grade ~ ., data=trn_data)
t(summary(ml_logit)$coefficients)
ml_logit_prey <- predict(ml_logit, newdata=tst_data)
cfmatrix <- table(tst_data$damage_grade, ml_logit_prey)
perf_matrix[1, ] <- perf_eval(cfmatrix)
perf_matrix

ann_trn_input <- input_data[1:200000, ]
ann_tst_input <- input_data[-c(1:200000), ]
ann_trn_target <- target_data[1:200000, ]
ann_tst_target <- target_data[-c(1:200000), ]

start <- proc.time()
nH <- seq(23, 35, 5)
iter <- seq(50, 400, 160)
val_idx <- sample(c(1:4), dim(trn_data)[1], replace=T, prob=rep(0.25, 4))
val_perf_matrix <- matrix(0, length(nH)*length(iter), 4)
colnames(val_perf_matrix) <- c('nH', 'iter', 'ACC', 'BCR')
for (i in 1:length(nH)) {
  eval_fold <- c()
  for (j in 1:length(iter)) {
    cat('Training ANN: the number of hidden nodes and max-iteration:', nH[i], '/', iter[j], '\n')
    for (k in c(1:4)) {
      tmp_trn_input <- ann_trn_input[which(val_idx != k), ]
      tmp_trn_target <- ann_trn_target[which(val_idx != k), ]
      tmp_nnet <- nnet(tmp_trn_input, tmp_trn_target, size=nH[i], decay=5e-4, maxit=iter[j], MaxNWts=5000)
      tmp_val_input <- ann_trn_input[which(val_idx == k), ]
      tmp_val_target <- ann_trn_target[which(val_idx == k), ]
      eval_fold <- rbind(eval_fold, cbind(max.col(tmp_val_target), max.col(predict(tmp_nnet, tmp_val_input))))
    }
    cfmatrix <- table(eval_fold[, 1], eval_fold[, 2])
    val_perf_matrix[3*i-3+j, 1] <- nH[i]
    val_perf_matrix[3*i-3+j, 2] <- iter[j]
    val_perf_matrix[3*i-3+j, 3:4] <- t(perf_eval(cfmatrix))
  }
}
end <- proc.time()
end-start

ordered_val_perf <- val_perf_matrix[order(val_perf_matrix[, 4], decreasing = TRUE),]

ann <- nnet(ann_trn_input, ann_trn_target, size=ordered_val_perf[1, 1], decay=5e-4, maxit=ordered_val_perf[1, 2], MaxNWts=5000)
ann_prey <- predict(ann, ann_tst_input)
tst_cm <- table(max.col(ann_tst_target), max.col(ann_prey))
perf_matrix[2, ] <- perf_eval(tst_cm)
perf_matrix