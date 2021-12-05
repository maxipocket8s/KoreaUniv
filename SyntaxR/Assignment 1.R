# Q1
for (i in 1:9) {
  for (j in 1:9) {
    cat (i, '*', j, '=', i * j, '\n')
  }
}

Gugudan <- function(x) {
  for (i in 1:9) {
    cat (x, '*', i, '=', x * i, '\n')
  }
}

Gugudan(1)
Gugudan(2)
Gugudan(3)
Gugudan(4)
Gugudan(5)
Gugudan(6)
Gugudan(7)
Gugudan(8)
Gugudan(9)


# Q2
n = 0
for (i in 1:100) {
  n <- n+i
  if (n >= 250) {
    cat ("합은", n, "이고, 마지막으로 더해진 수는", i, "이다.")
    break
  }
}


# Q3
top <- c('후드티', '체크셔츠', '줄무늬 셔츠', '니트', '파자마 상의')
bottom <- c('청바지', '슬랙스', '트레이닝 바지', '면바지', '파자마 하의')
i <- 1
j <- 1
while (i <= 5) {
  while (j <= 5) {
    if (i == 5 | j == 5) {
      cat(top[i], "와", bottom[j], "를 고르셨습니다. 다시 골라주세요!\n")
    } else {
      cat("가능한 조합: ", top[i], '+', bottom[j], "\n")
    }
    # print(j)
    j <- j+1
  }
  # print(i)
  j <- 1
  i <- i+1
}


# Q4
n = 0
for (i in 1:100) {
  if (i %% 6 == 0) {
    n <- n+1
  }
}
cat("1부터 100까지 6의 배수는", n, "개이다.")


# Q5
x <- c(1:5)
mat <- matrix(0, nrow=5, ncol=5)
for (i in x) {
  for (j in x) {
    mat[i, j] <- ifelse (i >= j, i-j, j-i)
  }
}
mat


# Q6
for (i in 1:5) {
  print(rep('*', i))
}


# Q7
v <- c(1, 2, 3, 4)
for (i in 1:4) {
  print(v[i]^3)
}


# Q8
vec <- c(0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1)
n0 <- 0
n1 <- 0
for (i in 1:length(vec)) {
  ifelse (vec[i] == 0, n0 <- n0+1, n1 <- n1+1)
}
cat("0의 개수는", n0, ", 1의 개수는", n1, ".")


# Q9
kpast <- 0
know <- 1
repeat {
  print(know)
  tmp <- know
  know <- tmp + kpast
  kpast <- tmp
  if (know > 377) break
}


# Q10
sentence = '데이터사이언스'
wrong_sentence = '테이어사이언스'
sentence_letter = strsplit(sentence, '')
wrong_sentence_letter = strsplit(wrong_sentence, '')
vec_sentence <- as.vector(sentence_letter[[1]])
vec_wrong_sentence <- as.vector(wrong_sentence_letter[[1]])
i <- 1
repeat {
  if (vec_sentence[i] != vec_wrong_sentence[i]) {
    cat(i, "번째 글자가 틀렸습니다.", sentence, "로 다시 입력하세요.\n")
  }
  i <- i+1
  if (i > nchar(sentence)) break
}


# Q11
A <- c(100, -0.009, 3, -30, -0.10)
B <- c(-20, 2, -0.9, 0.085, 5)
C <- 0
for (i in 1:length(A)) {
  for (j in 1:length(B)) {
    C <- c(C, A[i]+B[j], A[i]-B[j], B[j]-A[i], A[i]*B[j], A[i]/B[j], B[j]/A[i])
  }
}
print(C)
print(max(C))


# Q12
i <- 0
f <- 1
while (i <= 10) {
  if (i==0 | i==1) {
    f <- f
  } else {
    for (j in 1:i) {
      f<-f*j
    }
  }
  i <- i+1
  print (f)
  f <- 1
}


# Q13
signal = c("초록", '초록', '노랑', '빨강', '노랑', '초록')
for (sig in signal) {
  if (sig=='초록') {
    print(paste0(sig, '불입니다. 이동'))
  } else if (sig=='노랑') {
    print(paste0(sig, '불입니다. 천천히'))
  } else {
    print(paste0(sig, '불입니다. 정지'))
  }
}


# Q14
menus <- c('떡', '어묵', '소스', '떡볶이')
calories <- c(541, 213, 120, NA)
menu_cal <- data.frame(menus, calories)

# Q14-1
cal_chk <- c('떡', '어묵', '소스')
total_Cal <- 0
for (menu in cal_chk) {
  for (i in 1:nrow(menu_cal)) {
    if (menu_cal[i, 1] == menu) {
      cal = menu_cal[i, 2]
      total_Cal = total_Cal + cal
      print(paste0(menu, '의 칼로리는 ', cal))
    }
  }
}
print(paste0('떡/어묵/소스 칼로리의 합은 ', total_Cal))

# Q14-2
for (i in 1:nrow(menu_cal)) {
  for (j in 1:ncol(menu_cal)) {
    if (is.na(menu_cal[i, j])) {
      menu_cal[i, j] = total_Cal
    }
  }
}
print(menu_cal)


# Q15
student <- c('Annie', 'Theo', 'Steve', 'Hannah')
grade1 <- c(85, 65, 85, 100)
grade2 <- c(90, 75, 90, 90)
grade3 <- c(75, 55, 80, 85)
grade4 <- c(95, 75, 100, 90)
math_grade <- data.frame(name=student, exam1=grade1, exam2=grade2, exam3=grade3, exam4=grade4)
math_grade

# Q15-1
total_grade <- 0
for (i in 1:nrow(math_grade)) {
  for (j in 2:ncol(math_grade)) {
    total_grade <- (total_grade + math_grade[i, j])
  }
  avg_grade <- (total_grade / (ncol(math_grade)-1))
  print(paste0('이번 학기 ', math_grade[i, 1], '의 평균 점수는 ', avg_grade, '점입니다.'))
  total_grade <- 0
}

# Q15-2
total_grade <- 0
for (i in 2:ncol(math_grade)) {
  for (j in 1:nrow(math_grade)) {
    total_grade <- (total_grade + math_grade[j, i])
  }
  if ((total_grade/4)<80) {
    print(paste0(i-1, '번째 시험은 어려웠습니다.'))
  }
  total_grade <- 0
}

# Q15-3
max_grade <- 0
for (i in 1:nrow(math_grade)) {
  for (j in 2:ncol(math_grade)) {
    if (math_grade[i, j] > max_grade) {
      max_grade <- math_grade[i, j]
    }
  }
  if (max_grade > 90) {
    print(paste0('이번 학기 ', math_grade[i, 1], '의 최고 점수는 ', max_grade, '입니다.'))
  }
  max_grade <- 0
}


# Q16
install.packages('MASS')
library(MASS)
df = Cars93
head(df)

# Q16-1
library(dplyr)
arrange(df, Manufacturer)
sel_df <- select(df, Manufacturer, Price) # sel_df <- cbind(df$Manufacturer, df$Price)
n <- 1
total_price <- 0
while (n <= nrow(df)) {
  for (i in n:nrow(df)) {
    total_price <- (total_price + sel_df[i, 2])
    if (i == nrow(df)) break
    if (sel_df[n, 1] != sel_df[i+1, 1]) {
      print(paste0(df[n, 1], '사 자동차의 평균 가격은 ', total_price/(i-n+1), '원이다.'))
      n <- i+1
      total_price <- 0
    }
  }
}

# Q16-2
tb_df <- as_tibble(df)
i <- 1
cnt_na <- 0
cnt_del <- 0
while (i <= nrow(tb_df)) {
  for (j in 2:ncol(tb_df)) {
    if (is.na(tb_df[i, j])) {
      cnt_na <- (cnt_na + 1)
      cat('(', i, ',', j, ')')
    }
  }
  if (cnt_na >= 2) {
    tb_df <- tb_df[-i, ]
    cnt_del <- (cnt_del + 1)
  }
  cnt_na <- 0
  i <- (i + 1)
}
print(paste0('na 값을 2개 이상 가지는 row는 ', cnt_del, '개이다.'))