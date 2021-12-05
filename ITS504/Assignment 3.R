library(moments)
library(rsq)
library(GA)
library(glmnet)

weather_data <- read.csv('Assignment3.csv')

# 결측치 확인
for (i in 1:10) {
  cat('column', i, "결측치:", sum(is.na(weather_data[i])), '\n')
}

# 데이터 분할
trn_idx <- sample(nrow(weather_data), 250)
trn_weather <- weather_data[trn_idx, ]
tst_weather <- weather_data[-trn_idx, ]

# MLR 모델 학습
mlr_weahter <- lm(Mean_temperature ~ ., data=trn_weather)
rsq(mlr_weahter, adj=TRUE)
summary(mlr_weahter)

# RMSE, MAE, MAPE 산출
perf_eval <- function(tgt_y, pred_y) {
  RMSE <- sqrt(mean((tgt_y - pred_y)**2))
  MAE <- mean(abs(tgt_y - pred_y))
  MAPE <- mean(abs((tgt_y - pred_y)/tgt_y))
  return(c(RMSE, MAE, MAPE))
}

pred_y <- predict(mlr_weahter, newdata=tst_weather)
perf_eval(tst_weather$Mean_temperature, pred_y)

trn_weather[1]

# ES
start <- proc.time()
list <- c()
cnt <- 1
for (i in 1:9) {
  colname1 <- colnames(trn_weather[i])
  formula1 <- paste('Mean_temperature ~ ', colname1, collapse='')
  mlr_weahter_ES <- lm(as.formula(formula1), data=trn_weather)
  cat(cnt, '-', i, '-th Adj.R^2:', rsq(mlr_weahter_ES, adj=TRUE), '\n')
  cnt <- cnt+1
  list <- c(list, rsq(mlr_weahter_ES, adj=T))
  if (i == 9) next
  for (j in (i+1):9) {
    colname2 <- colnames(trn_weather[j])
    formula2 <- paste(formula1, colname2, sep='+')
    mlr_weahter_ES <- lm(as.formula(formula2), data=trn_weather)
    cat(cnt, '-', i, j, '-th Adj.R^2:', rsq(mlr_weahter_ES, adj=TRUE), '\n')
    cnt <- cnt+1
    list <- c(list, rsq(mlr_weahter_ES, adj=T))
    if (j == 9) next
    for (k in (j+1):9) {
      colname3 <- colnames(trn_weather[k])
      formula3 <- paste(formula2, colname3, sep='+')
      mlr_weahter_ES <- lm(as.formula(formula3), data=trn_weather)
      cat(cnt, '-', i, j, k, '-th Adj.R^2:', rsq(mlr_weahter_ES, adj=T), '\n')
      cnt <- cnt+1
      list <- c(list, rsq(mlr_weahter_ES, adj=T))
      if (k == 9) next
      for (l in (k+1):9) {
        colname4 <- colnames(trn_weather[l])
        formula4 <- paste(formula3, colname4, sep='+')
        mlr_weahter_ES <- lm(as.formula(formula4), data=trn_weather)
        cat(cnt, '-', i, j, k, l, '-th Adj.R^2:', rsq(mlr_weahter_ES, adj=T), '\n')
        cnt <- cnt+1
        list <- c(list, rsq(mlr_weahter_ES, adj=T))
        if (l == 9) next
        for (m in (l+1):9) {
          colname5 <- colnames(trn_weather[m])
          formula5 <- paste(formula4, colname5, sep='+')
          mlr_weahter_ES <- lm(as.formula(formula5), data=trn_weather)
          cat(cnt, '-', i, j, k, l, m, '-th Adj.R^2:', rsq(mlr_weahter_ES, adj=T), '\n')
          cnt <- cnt+1
          list <- c(list, rsq(mlr_weahter_ES, adj=T))
          if (m == 9) next
          for (n in (m+1):9) {
            colname6 <- colnames(trn_weather[n])
            formula6 <- paste(formula5, colname6, sep='+')
            mlr_weahter_ES <- lm(as.formula(formula6), data=trn_weather)
            cat(cnt, '-', i, j, k, l, m, n, '-th Adj.R^2:', rsq(mlr_weahter_ES, adj=T), '\n')
            cnt <- cnt+1
            list <- c(list, rsq(mlr_weahter_ES, adj=T))
            if (n == 9) next
            for (a in (n+1):9) {
              colname7 <- colnames(trn_weather[a])
              formula7 <- paste(formula6, colname7, sep='+')
              mlr_weahter_ES <- lm(as.formula(formula7), data=trn_weather)
              cat(cnt, '-', i, j, k, l, m, n, a, '-th Adj.R^2:', rsq(mlr_weahter_ES, adj=T), '\n')
              cnt <- cnt+1
              list <- c(list, rsq(mlr_weahter_ES, adj=T))
              if (a == 9) next
              for (b in (a+1):9) {
                colname8 <- colnames(trn_weather[b])
                formula8 <- paste(formula7, colname8, sep='+')
                mlr_weahter_ES <- lm(as.formula(formula8), data=trn_weather)
                cat(cnt, '-', i, j, k, l, m, n, a, b, '-th Adj.R^2:', rsq(mlr_weahter_ES, adj=T), '\n')
                cnt <- cnt+1
                list <- c(list, rsq(mlr_weahter_ES, adj=T))
              }
            }
          }
        }
      }
    }
  }
}
end <- proc.time()
end-start

mlr_ES <- lm(Mean_temperature ~ Max_termperature+Min_temperature+Dewpoint+Sea_level_pressure+Standard_pressure+Wind_speed, data=trn_weather)
summary(mlr_ES)
pred_y_ES <- predict(mlr_ES, newdata=tst_weather)
perf_eval(tst_weather$Mean_temperature, pred_y_ES)

# Forward Selection
start <- proc.time()
tmp_x <- paste(colnames(trn_weather[-10]), collapse=' + ')
tmp_xy <- paste('Mean_temperature ~ ', tmp_x, collapse='')
forward_model <- step(lm(Mean_temperature ~ 1, data=trn_weather), scope=list(upper=as.formula(tmp_xy), lower=Mean_temperature ~ 1), direction='forward', trace=1)
summary(forward_model)
rsq(forward_model, adj=T)
pred_y_FS <- predict(forward_model, newdata=tst_weather)
perf_eval(tst_weather$Mean_temperature, pred_y_FS)
end <- proc.time()
end-start

# Backward Elimination
start <- proc.time()
tmp_x <- paste(colnames(trn_weather[-10]), collapse=' + ')
tmp_xy <- paste('Mean_temperature ~ ', tmp_x, collapse='')
backward_model <- step(mlr_weahter, scope=list(upper=as.formula(tmp_xy), lower=Mean_temperature ~ 1), direction='backward', trace=1)
summary(backward_model)
rsq(backward_model, adj=T)
pred_y_BE <- predict(backward_model, newdata=tst_weather)
perf_eval(tst_weather$Mean_temperature, pred_y_BE)
end <- proc.time()
end-start

# Stepwise Selection
start <- proc.time()
tmp_x <- paste(colnames(trn_weather[-10]), collapse=' + ')
tmp_xy <- paste('Mean_temperature ~ ', tmp_x, collapse='')
stepwise_model <- step(lm(Mean_temperature ~ 1, data=trn_weather), scope=list(upper=as.formula(tmp_xy), lower=Mean_temperature ~ 1), direction='both', trace=1)
summary(stepwise_model)
rsq(stepwise_model, adj=T)
pred_y_SS <- predict(stepwise_model, newdata=tst_weather)
perf_eval(tst_weather$Mean_temperature, pred_y_SS)
end <- proc.time()
end-start

# GA
start <- proc.time()
fit_Adj <- function(string) {
  sel_var_idx <- which(string==1)
  sel_x <- x[, sel_var_idx]
  xy <- data.frame(sel_x, y)
  GA_mlr <- lm(y ~ ., data=xy)
  return (rsq(GA_mlr))
}
x <- as.matrix(trn_weather[, -10])
y <- trn_weather[, 10]
GA_Adj <- ga(type='binary', fitness=fit_Adj, nBits=ncol(x), names=colnames(x),
             popSize=50, pcrossover=0.3, pmutation=0.03, maxiter=50, elitism=2)
best_var_idx <- which(GA_Adj@solution==1)
GA_trn_weather <- trn_weather[, c(best_var_idx, 10)]
GA_tst_weather <- tst_weather[, c(best_var_idx, 10)]
GA_model <- lm(Mean_temperature ~ ., data=GA_trn_weather)
summary(GA_model)
pred_y_GA <- predict(GA_model, newdata=GA_tst_weather)
perf_eval(GA_tst_weather$Mean_temperature, pred_y_GA)
end <- proc.time()
end-start