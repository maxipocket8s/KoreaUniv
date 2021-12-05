library(moments)
library(tree)
library(party)
library(ROCR)

health_data <- read.csv('Assignment4.csv')
health_data$target <- as.factor(health_data$target)
trn_idx <- sample(1:nrow(health_data), 200)
health_trn <- data.frame(health_data[trn_idx, ])
health_tst <- data.frame(health_data[-trn_idx, ])

perf_eval <- function(cm){
  TPR <- cm[2, 2]/sum(cm[2, ])
  TNR <- cm[1, 1]/sum(cm[1, ])
  Precision <- cm[2, 2]/sum(cm[, 2])
  Accuracy <- (cm[1, 1]+cm[2, 2])/sum(cm)
  BCR <- sqrt(TPR*TNR)
  F1 <- 2*TPR*Precision/(TPR+Precision)
  return (c(TPR, TNR, Precision, Accuracy, BCR, F1))
}

perf_mat <- matrix(0, 2, 6)
rownames(perf_mat) <- c('Post-pruning', 'Pre-pruning')
colnames(perf_mat) <- c('TPR', 'TNR', "Precision", 'Accuracy', 'BCR', 'F1')

CART_post <- tree(target ~ ., health_trn)
summary(CART_post)
plot(CART_post)
text(CART_post, pretty=1)

CART_post_cv <- cv.tree(CART_post, FUN = prune.misclass)
plot(CART_post_cv$size, CART_post_cv$dev, type='b')
CART_post_cv

CART_post_prune <- prune.misclass(CART_post, best=6)
plot(CART_post_prune)
text(CART_post_prune, pretty=1)

CART_post_pred <- predict(CART_post_prune, health_tst, type='class')
CART_post_cm <- table(health_tst$target, CART_post_pred)
perf_mat[1, ] <- perf_eval(CART_post_cm)
perf_mat

idx_trn <- sample(trn_idx, 100)
trn_health <- health_data[idx_trn, ]
val_health <- health_trn[-idx_trn, ]
tst_health <- health_data[-trn_idx, ]

min_criterion = c(0.9, 0.95, 0.99)
min_split= c(5, 10, 15, 20)
max_depth = c(0, 5, 10)

CART_pre_search_mat <- matrix(0, length(min_criterion)*length(min_split)*length(max_depth), 11)
colnames(CART_pre_search_mat) <- c('min_criterion', 'min_split', 'max_depth', 'TPR', "TNR", 'Precision', 'Accurary',
                                   'BCR', 'F1', 'AUROC', 'N_leaves')

iter_cnt <- 1
for (i in 1:length(min_criterion)) {
  for (j in 1:length(min_split)) {
    for (k in 1:length(max_depth)) {
      cat("Min criterion:", min_criterion[i], 'Min_split:', min_split[j], 'Max_depth:', max_depth[k], '\n')
      tmp_control <- ctree_control(mincriterion = min_criterion[i], minsplit = min_split[j], maxdepth = max_depth[k])
      tmp_tree <- ctree(target ~ ., data=trn_health, controls = tmp_control)
      tmp_tree_val_pred <- predict(tmp_tree, newdata=val_health)
      tmp_tree_val_cm <- table(val_health$target, tmp_tree_val_pred)
      tmp_tree_val_response <- treeresponse(tmp_tree, newdata=val_health)
      tmp_tree_val_rocr <- prediction(unlist(tmp_tree_val_response, use.name=F)[seq(2, nrow(val_health)*2, 2)], val_health$target)
      
      CART_pre_search_mat[iter_cnt, 1] <- min_criterion[i]
      CART_pre_search_mat[iter_cnt, 2] <- min_split[j]
      CART_pre_search_mat[iter_cnt, 3] <- max_depth[k]
      CART_pre_search_mat[iter_cnt, 4:9] <- perf_eval(tmp_tree_val_cm)
      CART_pre_search_mat[iter_cnt, 10] <- unlist(performance(tmp_tree_val_rocr, 'auc')@y.values)
      CART_pre_search_mat[iter_cnt, 11] <- length(nodes(tmp_tree, unique(where(tmp_tree))))
      
      iter_cnt <- iter_cnt+1
    }
  }
}
CART_pre_search_mat <- CART_pre_search_mat[order(CART_pre_search_mat[, 10], decreasing = T), ]
CART_pre_search_mat

tree_control <- ctree_control(mincriterion = 0.9, minsplit = 5, maxdepth = 0)
CART_pre <- ctree(target ~ ., data=health_trn, controls = tree_control)
CART_pre_pred <- predict(CART_pre, newdata=health_tst)
CART_pre_cm <- table(health_tst$target, CART_pre_pred)
perf_mat[2, ] <- perf_eval(CART_pre_cm)
perf_mat

CART_pre_response <- treeresponse(CART_pre, newdata=health_tst)
CART_pre_rocr <- prediction(unlist(CART_pre_response, use.names=F)[seq(2, nrow(health_tst)*2, 2)], health_tst$target)
plot(performance(CART_pre_rocr, 'tpr', 'fpr'), col=5, lwd=3, main='AUROC')

plot(CART_pre)
plot(CART_pre, type='simple')
