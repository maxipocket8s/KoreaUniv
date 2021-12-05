# Q1
ESearch <- function(x) {
  i <- 1
  A <- sample(1:50, 20)
  while (i <= 20) {
    if (x == A[i]) {
      print("TRUE")
      break
    } else {
      i <- i+1
      if (i > 20) {
        cat("FALSE")
      }
    }
  }
}

ESearch(6)


# Q2
PayCoin <- function(x) {
  a <- x %/% 500
  b <- (x-500*a) %/% 100
  c <- (x-500*a-100*b) %/% 50
  d <- (x-500*a-100*b-50*c) %/% 10
  cat(paste0('500원: ' , a, '개\n', '100원: ', b, '개\n', '50원: ', c, '개\n', '10원: ', d, '개\n'))
}

PayCoin(2370)


# Q3
PridMovie <- function(x=sample(0:10, 5)) {
  score <- mean(x)
  if ((x[5]-score) < 0) {
    prid_score <- 0
  } else if (0 < (x[5]-score) & (x[5]-score) < 10) {
    prid_score <- (x[5]-score)
  } else {
    prid_score <- 10
  }
  return (prid_score)
}

PridMovie()


# Q4
Production <- function(x=sample(1:30, 10)) {
  y <- x*sample(1:5, 1)
  return (y)
}
DefectCheck <- function(x=Production()) {
  cat(paste0('생산 사이즈: ', x, '\n'))
  cnt_defect <- 0
  for (i in 1:length(x)) {
    if (x[i] < 10) {
      cnt_defect <- cnt_defect + 1
    } else if (x[i] > 30) {
      cnt_defect <- cnt_defect + 1
    }
  }
  return (cnt_defect)
}

DefectCheck()


# Q5
Calculator <- function(x, y, z) {
  if (z == '+') {
    sprintf('%s + %s = %s', x, y, x+y)
  } else if (z == '-') {
    sprintf('%s - %s = %s', x, y, x-y)
  } else if (z == '*') {
    sprintf('%s * %s = %s', x, y, x*y)
  } else if (z == '/') {
    print(paste(x, '/', y, '=', '몫)', x%/%y, '나머지)', x%%y))
  }
}

Calculator(12, 5, '+')
Calculator(12, 5, '-')
Calculator(12, 5, '*')
Calculator(12, 5, '/')


# Q6
Fibonacci <- function(n) {
  if (n == 1) {
    return (1)
  }
  return (Fibonacci(n-1) + n)
}

Fibonacci(10)


# Q7
WTriangle <- function(n) {
  i <- 1
  while (i <= n) {
    print(rep("*", i))
    i <- i+1
  }
}
FTriangle <- function(n) {
  for (i in 1:n) {
    print(rep("*", i))
  }
}

WTriangle(10)
FTriangle(10)


# Q8
AI_rps <- function(n) {
  rps <- c('Rock'=0, 'Paper'=1, 'Scissor'=2)
  cnt_w <- 0
  cnt_d <- 0
  cnt_l <- 0
  while (i <= n) {
    YT <- sample(1:3, 1)
    AI <- sample(1:3, 1)
    if ((rps[YT]-rps[AI]) == -2 | (rps[YT]-rps[AI]) == 1) {
      cnt_w <- cnt_w + 1
    } else if (rps[YT]-rps[AI] == 0) {
      cnt_d <- cnt_d + 1
    } else {
      cnt_l <- cnt_l + 1
    }
    i <- i + 1
  }
  print(paste0('영탁은 AI에 대항하여 ', cnt_w, '승, ', cnt_d, '무, ', cnt_l, '패를 기록했습니다.'))
}

AI_rps(10)


# Q9
BubbleSort <- function(x=100){
  start_time <- Sys.time()
  Bubble <- sample(1:1000, x)
  while (x > 2) {
    for (i in 1:(x-1)) {
      if (Bubble[i] < Bubble[i+1]) {
        i <- i+1
      } else if (Bubble[i] > Bubble[i+1]) {
        temp <- Bubble[i+1]
        Bubble[i+1] <- Bubble[i]
        Bubble[i] <- temp
        i <- i+1
      }
    }
    x <- x-1
  }
  print(Bubble)
  end_time <- Sys.time()
  print(end_time - start_time)
}

BubbleSort()


# Q10
quickSort <- function(Bubble) {
  mid <- sample(Bubble, 1)
  left <- c()
  right <- c()
  lapply(Bubble[Bubble != mid], function(d) {
    if (d < mid) {
      left <<- c(left, d)
    }
    else {
      right <<- c(right, d)
    }
  })
  if (length(left) > 1) {
    left <- quickSort(left)
  }
  if (length(right) > 1) {
    right <- quickSort(right)
  }
  return (c(left, mid, right))
  }

Bubble <- sample(1:1000, 100)
quickSort(Bubble)