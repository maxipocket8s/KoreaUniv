---
title: "정신건강지표 분석 보고서"
author: '박주원'
date: '2021-10-25'
output:
  html_notebook: default
  html_document:
    df_print: paged
  pdf_document: default
---

한 국가의 정신건강 문제는 사회경제적 차원의 문제이며, 인권의 문제이다. 국가는 국민들의 정신건강을 반드시 지켜내어야 하며, 궁극적으로 정신건강의 가치를 높여 사회적 치유를 도모하여야 한다. 이에 현재(데이터 기준 2019년) 대한민국의 정신건강지표에 대해 분석해보려 한다.
  
다음 보고서에 사용된 데이터의 경우, 모두 [**통계청**](https://kostat.go.kr/portal/korea/index.action)에서 추출한 데이터임을 밝힌다.
  
***
>### **1인당 정신건강예산**

```{r, fig.width = 10, fig.height = 5}

library(dplyr)
library(ggplot2)

file <- data.frame(read.csv('Assignment 5_budget_for_cities.csv'))
names(file) <- c('Cities', 'Budget', 'BudgetX1')
file <- file[-1, ]
file <- file[-1, ]
file <- arrange(file, Cities)
file$BudgetX1 <- as.numeric(file$BudgetX1)

ggplot(file, aes(x=Cities, y=BudgetX1)) + geom_bar(stat='identity', fill='grey') + geom_text(aes(label=BudgetX1), vjust=0.7, size=3) + labs(x='', y='', title='1인당 정신건강예산', subtitle='17개 지자체', caption='출처: 통계청') + scale_y_continuous(labels=function(x){paste0(x/1000, '천원')}) + coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 1))

```
  
지차체별 1인당 정신건강예산 금액 수준 상위 3곳은 **전라북도**, **강원도**, **광주광역시**로, 하위 3곳은 **세종특별자치시**, **경상남도**, **인천광역시**로 집계되었다. 또한 국민의 최다 인구를 포함하는 **서울특별시** 및 **경기도**는 각각 10위, 9위로 나타났다. 지자체별 정신건강예산은 최대 5,956원 차이가 나며, 이는 세종특별자치시의 경우 1인당 정신건강예산이 *전라북도의 약 1/3 수준*에 불과함을 의미한다.
  
***
>### **인구 1000명당 정신건강질환**

```{r, fig.width = 10, fig.height = 7}

data1 <- data.frame(read.csv('Assignment 5_diagnosis_for_cities.csv'))
data1 <- data1[, -3]
data1 <- rbind(data1[26:42, ], data1[47:63, ], data1[89:105, ], data1[131:147, ], data1[173:189, ], data1[194:210, ])
data1 <- data1[, -2]
names(data1) <- c('Diagnosis', 'Cities', 'pt2018', 'pt2019')
data1$pt2019 <- as.numeric(data1$pt2019)

data2 <- read.csv('Assignment 5_total_population_for_cities.csv')
data2 <- cbind(data2[, 1], data2[, 2])
data2 <- data.frame(data2)
data2 <- data2[-1, ]
names(data2) <- c('Cities', 'population')
data2$population <- as.numeric(data2$population)

data3 <- merge(data1, data2, all.x=TRUE)
data3 <- merge(data3, summarize(group_by(data3, Cities), total19pt = sum(pt2019)))
data3 <- mutate(data3, per19population = pt2019 / population * 1000, total19per = total19pt / population * 1000) %>% arrange(desc(Cities))

textlabel <- data3 %>% filter(Diagnosis == '주요 우울 장애') %>% arrange(desc(Cities))
ggplot(data3, aes(x=Cities, y=per19population, fill=Diagnosis)) + geom_bar(stat='identity') + geom_text(textlabel, mapping=aes(label=round(total19per, 2)), size=3, nudge_y=50) + labs(x='', y='', title='인구 1000명당 정신건강질환', subtitle='17개 지자체', caption='출처: 통계청') + scale_y_continuous(labels=function(x){paste0(x, '명/1000명')}) + coord_flip() + theme_bw() + theme(legend.position = 'bottom') + scale_fill_brewer(palette='RdGy', direction=1) + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 1))

```
  
위 그래프에서 확인할 수 있듯이 인구 1000명당 정신건강질환 환자 규모 상위 3곳은 **부산광역시**, **대전광역시**, **대구광역시**이며, 하위 3곳은 **세종특별자치시**, **경기도**, **전라남도**이다.
  
특히나 세종특별자치시는 앞서 살펴보았던 지차체별 1인당 정신건강예산 그래프에서도 하위 3군데 중 한 곳으로 꼽혔으며, 위 두 가지 데이터로 미루어 볼 때 해당 지자체 주민들의 정신건강이 비교적 건전하여 예산 규모가 크지 않았음을 추측할 수 있다. 반면 서울특별시의 경우, 지자체별 1인당 정신건강예산 금액 수준은 상위 10위에 머물렀으나 인구 1000명당 정신건강질환 환자 규모는 상위 4위로 책정되었다. 즉, 예산 금액을 상향할 필요성이 보인다.
  
***
>### **진단비율**

```{r, fig.width = 12, fig.height = 12}

data4 <- data3 %>% mutate(percentage = pt2019 / total19pt * 100)
ggplot(data4, aes(x='', y=percentage, fill=Diagnosis)) + geom_bar(width=1, stat='identity') + coord_polar("y", start=0) + scale_fill_brewer(palette='RdGy') + facet_wrap(~Cities, ncol=4) + labs(x = '', y = '', title = '진단비율', subtitle = '17개 지자체', caption = '출처: 통계청') + theme_bw() + theme(plot.title = element_text(size = 15, hjust = 0.5), plot.subtitle = element_text(hjust = 1))

```

  
각 지자체별 진단비율을 추가로 알아보자. 다수의 지자체에서 주요우울장애의 비율이 가장 크며, 신체형 장애 및 중증정신질환이 그 뒤를 잇는다. 이 자료를 통해 국민들의 우울감을 가늠할 수 있으며, 적절한 중재가 적시에 개입될 수 있도록 국가 차원의 정책적 지원이 필요함을 알 수 있다.
  
*** 
>### **인구 10만명당 인력**

```{r, fig.width = 10, fig.height = 10}

df <- data.frame(read.csv('Assignment 5_resources_for_cities.csv'))
names(df) <- c('Cities', 'resource1', 'resource2', 'X18', 'X19')
df <- df %>% filter(resource1 == '인구 10만당 인력 (명)')
df <- df[-1, ]
df <- df[-1, ]

fnldf <- data.frame(cbind(df$Cities, df$resource2, df$X19))
names(fnldf) <- c('Cities', 'resource2', 'X19')
fnldf$X19 <- as.numeric(fnldf$X19)
fnldf$Cities <- factor(fnldf$Cities)
fnldf$Color[fnldf$resource2=='상근인력'] <- 'firebrick'
fnldf$Color[fnldf$resource2=='전문인력'] <- 'navy'
fnldf <- arrange(fnldf, desc(Cities))

dotchart(fnldf$X19, color=fnldf$Color, groups=fnldf$Cities, gcolor='black', gdata=c(mean(fnldf$X19[fnldf$Cities==fnldf$Cities[1]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[3]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[5]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[7]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[9]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[11]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[13]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[15]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[17]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[19]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[21]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[23]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[25]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[27]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[29]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[31]]), mean(fnldf$X19[fnldf$Cities==fnldf$Cities[33]])), gpch=13, main='인구 10만명당 인력')
```
  
지자체별 인구 10만명당 상근인력 및 전문인력을 살펴보도록 하자. 상근인력은 빨간색 동그라미 표식으로, 전문인력은 파란색 동그라미 표식으로 나타내었고, 두 표식 사이에 있는 검은색 동그라미 곱표는 상근인력과 전문인력의 평균값을 의미한다.
  
상근인력 상위 3곳은 **충청남도**, **전라남도**, **광주광역시**이며, 하위 3곳은 **세종특별자치시**, **울산광역시**, **경기도**이다. 이어서 전문인력 상위 3곳은 **광주광역시**, **전라북도**, **부산광역시**이며, 하위 3곳은 **세종특별자치시**, **울산광역시**, **인천광역시**이다. 종합적으로 두 인력의 평균값 상위 3곳은 **광주광역시**, **전라남도**, **충청남도**이며, 하위 3곳은 **세종특별자치시**, **울산광역시**, **경기도**이다.
  
광주광역시는 상근인력, 전문인력 모두 충분한 수준으로 보이며, 다시 한 번 상기하자면 지자체별 1인당 정신건강예산에서도 상위권을 차지했었다. 해당 지차제가 타 지자체에 비해 주민들의 정신건강에 상당 부분 할애함을 알 수 있다. 한편 전라남도의 경우, 정신건강과 관련하여 *환자 대비 인력의 비율*이 높은 것으로 사료된다.
  
*** 
>### **사례관리자 1인당 정신질환자**

```{r, fig.width = 10, fig.height = 7}

case_f <- data.frame(read.csv('Assignment 5_num_of_case_for_cities.csv'))
case_f <- case_f[-1, ]
names(case_f) <- c('index', 'Cities', 'X2019_1', 'X2019_2', 'X2019_3')
case_f$X2019_3 <- as.numeric(case_f$X2019_3)
case_f <- case_f %>% filter(index=='시도별') %>% select(Cities, X2019_3) %>% arrange(desc(Cities))

ggplot(case_f, aes(x=X2019_3, y=Cities)) + geom_point() + theme_bw() + scale_x_continuous(breaks=seq(from=0, to=60, by=10)) + scale_y_discrete(limits=case_f$Cities) + geom_text(aes(label=X2019_3), vjust=0.7, hjust=-0.3, size=3) + labs(x='', y='', title='사례관리자 1인당 정신질환자', subtitle='17개 지자체', caption='출처: 통계청') + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 1))

```
각 지자체마다 사례관리자 1인당 담당하는 정신질환자는 20명을 넘는다. 제주특별자치도는 최대 57명에 이르기까지 한다.
  
모든 데이터를 감안하면, 전라남도의 경우 예산은 평균 수준이지만 정신질환자가 많지 않으며 상근 및 전문인력을 충분히 두어 사례관리자 1인당 정신질환자가 타 지자체에 비해 크게 적음을 발견할 수 있다. 그러나 전라남도와 비슷한 예산 수준인 서울특별시의 경우 정신질환자도 많을뿐더러 인력도 평균에 살짝 못 미치는 규모이며, 위 차트에서도 볼 수 있듯이 사례관리자 1인당 정신질환자도 근소한 차이로 상위 4위에 위치할 만큼 정신건강 관리에 부족함이 많음을 느낄 수 있다.


***
정신건강은 현대에 이르러 더욱 큰 관심을 받게 되었다. 정신이 건강한 사람은 긍정적인 자아정체감을 가지며, 현실을 왜곡하지 않은 채 삶을 영위할 수 있다. 뿐만 아니라 타인을 존중하며, 자신과 타인의 한계에 대해 이해와 수용의 자세를 지니게 된다. 이는 국가적 차원으로 확대되어 사회적 문제 역시 원활하게 해결할 수 있는 자양분이 된다.

물론 여기에서 [그림(plot)]을 제외하면 모두 박주원의 사견일 뿐..ㅎ
