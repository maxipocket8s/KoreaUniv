library(arules)
library(arulesViz)
library(RColorBrewer)
library(wordcloud)
install.packages('aprior')
mooc_data <- read.csv('Assignment7.csv')

basket <- mooc_data[, c(2:3, 10:11)]
basket[, 3] <- gsub(' ', '', basket[, 3])
RawTransactions <- data.frame(paste(basket[, 1], basket[, 2], basket[, 3], basket[, 4], sep='_'))
Mooc_transaction <- paste(mooc_data[, 6], RawTransactions[, 1], sep=' ')
write.csv(Mooc_transaction, file = 'Assignment7_Mooc_User_Course.csv')

transaction_data <- read.transactions('Assignment7_Mooc_User_Course.csv', format='single', cols=c(2, 3))
summary(transaction_data)
inspect(transaction_data)

itemName <- itemLabels(transaction_data)
itemCount <- itemFrequency(transaction_data)*nrow(transaction_data)
col <- brewer.pal(7, 'Dark2')
wordcloud(words=itemName, freq=itemCount, min.freq=150, scale = c(2, 0.5), col=col, random.order=F)
itemFrequencyPlot(transaction_data, support=0.01, cex.name=0.7)

support_list <- c(0.01, 0.15, 0.1)
confidence_list <- c(0.15, 0.35, 0.5)
perf_mat <- matrix(0, nrow=length(support_list), ncol=length(confidence_list))
rownames(perf_mat) <- c(paste('support =', support_list))
colnames(perf_mat) <- c(paste('confidence =', confidence_list))
for (i in 1:length(support_list)) {
  for (j in 1:length(confidence_list)) {
    rules <- apriori(transaction_data, parameter=list(c(support=support_list[i], confidence=confidence_list[j])))
    perf_mat[i, j] <- rules
  }
}
apriori(transaction_data, parameter=list(c(support=0.001, confidence=0.05)))
rules
i
perf_mat
