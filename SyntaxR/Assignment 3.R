library(dplyr)

data <- read.csv('Assignment 3.csv', header=T)
data

# Q1
data %>% select(Name, Sex, Age, Survived, Cabin) %>% head(5)


# Q2
data %>% filter(!is.na(Cabin)) %>% select(Cabin) %>% unique %>% head(5)
unique(select(filter(data, !is.na(Cabin)), Cabin)) %>% head(5)
# filter(data, !is.na(Cabin))


# Q3
data %>% group_by(Survived) %>% summarize(Fare_mean = mean(Fare, na.rm=TRUE))
summarize(group_by(data, Survived), Fare_mean = mean(Fare, na.rm=TRUE))


# Q4
data %>% filter(Survived == 1 & Age <= 20) %>% group_by(Pclass) %>% summarize(Class_count = n(), Fare_mean = mean(Fare, na.rm=T)) %>% arrange(desc(Pclass))
arrange(summarize(group_by(filter(data, Survived == 1, Age <= 20), Pclass), Class_count = n(), Fare_mean = mean(Fare, na.rm=T)), desc(Pclass))


# Q5
dataQ5 <- data.frame(data)
dataQ5 <- dataQ5[grep('th', dataQ5$Name), ]
summarize(select(dataQ5, PassengerId, Name), totalPassenger = n())
dataQ5 %>% select(PassengerId, Name) %>% head(5)


# Q6
data %>% group_by(Pclass) %>% summarize(Age_mean = mean(Age, na.rm=T)) %>% arrange(desc(Age_mean))
data %>% group_by(Sex) %>% summarize(Age_mean = mean(Age, na.rm=T)) %>% arrange(desc(Age_mean))
arrange(summarize(group_by(data, Pclass), Age_mean = mean(Age, na.rm=TRUE)), desc(Age_mean))
arrange(summarize(group_by(data, Sex), Age_mean = mean(Age, na.rm=TRUE)), desc(Age_mean))


# Q7
Surv_Pclass <- arrange(summarize(group_by(filter(data, Age<=15, Survived==1), Pclass), Surv_n = n()), Pclass)
Total_Pclass <- arrange(summarize(group_by(data, Pclass), Total_n = n()), Pclass)
preTable <- cbind(Surv_Pclass, Total_Pclass[, 2]) %>% mutate(Surv_percentage = round((Surv_n / Total_n)*100, 2))
FnlTable <- cbind(preTable[1], preTable[4])


# Q8
df_data <- data.frame(data %>% filter(!is.na(Name), !is.na(Age), !is.na(Sex), !is.na(Survived)) %>% select(Name, Age, Sex, Survived))
for (i in 1:length(df_data[, 4])) {  ifelse (df_data[i, 4] == 1, df_data[i, 4] <- 'Yes', df_data[i, 4] <- 'No')  }
preSummary <- list(1:length(df_data[, 4]))
for (i in 1:length(df_data[, 4])) {
  preSummary[[1]][i] <- paste0('[Name: ', df_data[i, 1], ' | Age: ', df_data[i, 2], ' | Sex: ', df_data[i, 3], ' | Survived: ', df_data[i, 4], ']')
}
Summary <- unlist(preSummary)
Summary %>% head(5)


# Q9
preData <- mutate(filter(data, !is.na(Age), !is.na(Sex)), Passenger_char = case_when(
  Sex == 'male' & Age < 30 ~ 'male_under_30',
  Sex == 'male' & Age >= 30 ~ 'male_over_30',
  Sex == 'female' & Age < 30 ~ 'female_under_30',
  TRUE ~ 'female_over_30'
))
fnlData <- select(preData, Age, Sex, Passenger_char)
fnlData %>% head(5)


# Q10
nDeath <- summarize(group_by(select(filter(preData, Survived==0), Pclass, Passenger_char, Survived), Passenger_char, Pclass), nDeath = n())
nTotal <- summarize(group_by(select(preData, Pclass, Passenger_char, Survived), Passenger_char, Pclass), nTotal = n())
Result <- arrange(select(mutate(cbind(nDeath, nTotal[, 3]), Death_percentage = round(nDeath/nTotal*100, 2)), Pclass, Passenger_char, Death_percentage), Death_percentage)
Result


# Q11
red_wine <- read.csv("Assignment 3_wine_red.csv", sep=';', head=T) %>% mutate(Type = 'R') %>% data.frame()
white_wine <- read.csv("Assignment 3_wine_white.csv", sep=';', head=T) %>% mutate(Type = "W") %>% data.frame()
wine <- rbind(red_wine, white_wine)
wine


# Q12
print(select(wine, contains('dioxide')))
print(select(wine, matches('dioxide')))


# Q13
preResult <- filter(wine, density > mean(wine$density))
preResult[which.max(preResult$volatile.acidity), ]


# Q14
wineQ14 <- mutate(wine, dioxide_ratio = free.sulfur.dioxide / total.sulfur.dioxide)


# Q15
wineQ15 <- wine %>% filter(quality == 5)
wineQ15_max <- wineQ15[which.max(wineQ15$fixed.acidity), ]
wineQ15_min <- wineQ15[which.min(wineQ15$fixed.acidity), ]
arrange(select(rbind(wineQ15_max, wineQ15_min), fixed.acidity, volatile.acidity), desc(volatile.acidity))


# Q16
mutate(wine, Q = case_when(
  quality < 6 ~ 'Low Quality',
  6 <= quality & quality <= 7 ~ 'Medium Quality',
  TRUE ~ 'High Quality'
)) %>% group_by(Q) %>% summarize(avg_residual.sugar = mean(residual.sugar))


# Q17
wine <- mutate(wine, qlabel = case_when(
  quality < 6 ~ 'L',
  6 <= quality & quality <= 7 ~ 'M',
  TRUE ~ 'H'
))


# Q18
wine %>% group_by(qlabel) %>% summarize(avg_pH = mean(pH), avg_citric.acid = mean(citric.acid))


# Q19
wineQ19pre <- wine %>% group_by(Type, qlabel) %>% summarize(total_qlabel = n())
wineQ19fnl <- wineQ19pre %>% group_by(Type) %>% mutate(ratio_qlabel = total_qlabel / sum(total_qlabel)) %>% select(Type, qlabel, ratio_qlabel)
print(wineQ19fnl)


# Q20
wineQ20 <- wineQ14 %>% mutate(acid_mean = (volatile.acidity + citric.acid)/2) %>% filter(dioxide_ratio > acid_mean)
wineQ20 %>% head(5)