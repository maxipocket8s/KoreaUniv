library(dplyr)
library(stringr)
library(httr)
library(rvest)

Title <- NULL
Writer <- NULL
averageRating <- NULL
ratingAmount <- NULL
reviewAmount <- NULL
Summary <- NULL

url <- "https://www.goodreads.com/search?page=1&qid=mnu1gwvQtL&query=harry+potter&tab=books&utf8=%E2%9C%93"
# url_main <- "https://www.goodreads.com/search?page=1"
# url_sub <- "&qid=mnu1gwvQtL&query=harry+potter&tab=books&utf8=%E2%9C%93"

bookCount <- read_html(url) %>% html_nodes('div.leftContainer > h3') %>% html_text
bookCount <- strsplit(bookCount, ' ')[[1]][5] %>% as.integer()
pageCount <- if (bookCount == 1) {
  1
} else if ((bookCount-1) %/% 20 < 100) {
  (bookCount-1) %/% 20
} else {
  100
}

for (i in 1:pageCount) {
  tmp_page_url <- modify_url(url, query = list(page=i))
  tmp_page_url <- gsub('%2B', '+', tmp_page_url)
  # tmp_page_url <- modify_url(url_main, query = list(page=i))
  # tmp_page_url <- paste0(tmp_page_url, url_sub)
  
  book_url <- read_html(tmp_page_url) %>% html_nodes('a[href^="/book/show/"]') %>% html_attr('href')
  book_url <- paste0("https://goodreads.com", book_url)

    for (j in 1:20) {
    tmp_book_url <- book_url[2*j-1]
    
    # Title
    tmp_book_title <- read_html(tmp_book_url) %>% html_nodes('div.leftContainer') %>% 
      html_nodes('div.last.col.stacked') %>% html_nodes('div.last.col > h1') %>% html_text
    tmp_book_title <- gsub('\\s+', ' ', tmp_book_title) %>% str_trim
    cat(i, j, tmp_book_title, '\n')
    Title <- c(Title, tmp_book_title)
    
    # Writer
    tmp_book_writer <- read_html(tmp_book_url) %>% html_nodes('div.authorName__container') %>%
      html_nodes('a.authorName > span') %>% html_text %>% str_trim %>%
      strsplit(',')
    tmp_book_writer <- toString(tmp_book_writer)
    cat(i, j, tmp_book_writer, '\n')
    Writer <- c(Writer, tmp_book_writer)
    
    tryCatch({
      # averageRating
      tmp_book_averageRating <- read_html(tmp_book_url) %>% html_nodes('div.last.col') %>%
        html_nodes('div.uitext.stacked > span') %>% html_text %>% str_trim
      tmp_book_averageRating <- tmp_book_averageRating[2] %>% as.numeric()
      averageRating <- c(averageRating, tmp_book_averageRating)
    }, error = function(e){cat("An error occurs, skip the",j, "-th book of", i, '-th page\n')})
    
    # ratingAmount % reviewAmount
    tmp_book_Amount <- read_html(tmp_book_url) %>% html_nodes('div.uitext.stacked') %>%
      html_nodes('a.gr-hyperlink') %>% html_text %>% str_trim
    tmp_book_Amount <- gsub(' ratings', '', tmp_book_Amount)
    tmp_book_Amount <- gsub(' reviews', '', tmp_book_Amount)
    tmp_book_Amount <- gsub('\n', '', tmp_book_Amount) %>% str_trim
    
    tmp_book_ratingAmount <- tmp_book_Amount[1]
    tmp_book_ratingAmount <- gsub(',', '', tmp_book_ratingAmount) %>% as.integer()
    cat(i, j, tmp_book_ratingAmount, '\n')
    ratingAmount <- c(ratingAmount, tmp_book_ratingAmount)
    
    tmp_book_reviewAmount <- tmp_book_Amount[2]
    tmp_book_reviewAmount <- gsub(',', '', tmp_book_reviewAmount) %>% as.integer()
    cat(i, j, tmp_book_reviewAmount, '\n')
    reviewAmount <- c(reviewAmount, tmp_book_reviewAmount)
    
    # Summary
    tmp_book_summary <- read_html(tmp_book_url) %>% html_nodes('div.last.col') %>%
      html_nodes('div.readable.stacked > span') %>% html_text %>% str_trim
    tmp_book_summary <- tmp_book_summary[2]
    tmp_book_summary <- gsub('\\s+', ' ', tmp_book_summary)
    cat(i, j, tmp_book_summary, '\n')
    Summary <- c(Summary, tmp_book_summary)
    
    cat(j, '-th book of', i,'-th page done\n')
    Sys.sleep(1)
    }
  
  cat(i, '-th page done\n')
  Sys.sleep(1)
}

goodreadsInfo <- data.frame(cbind(Title, Writer, averageRating, ratingAmount, reviewAmount, Summary))
save(goodreadsInfo, file='Assignment 4_Book1.RData')
write.csv(goodreadsInfo, file='Assignment 4_Book1.csv')


searchBook <- function(x) {
  
  Title <- NULL
  Writer <- NULL
  averageRating <- NULL
  ratingAmount <- NULL
  reviewAmount <- NULL
  Summary <- NULL
  
  url <- modify_url("https://www.goodreads.com/search?page=1&q=harry+potter", query=list(q=x))
  url <- gsub('%20', '+', url)
  
  bookCount <- read_html(url) %>% html_nodes('div.leftContainer > h3') %>% html_text
  bookCount <- strsplit(bookCount, ' ')[[1]][5] %>% as.integer()
  pageCount <- if (bookCount == 1) {
    1
  } else if ((bookCount-1) %/% 20 < 100) {
    (bookCount-1) %/% 20
  } else {
    100
  }
  
  for (i in 1:2) {
    tmp_page_url <- modify_url(url, query = list(page=i))
    tmp_page_url <- gsub('%2B', '+', tmp_page_url)
    
    book_url <- read_html(tmp_page_url) %>% html_nodes('a[href^="/book/show/"]') %>% html_attr('href')
    book_url <- paste0("https://goodreads.com", book_url)
    
    for (j in 1:10) {
      tmp_book_url <- book_url[2*j-1]
      
      # Title
      tmp_book_title <- read_html(tmp_book_url) %>% html_nodes('div.leftContainer') %>% 
        html_nodes('div.last.col.stacked') %>% html_nodes('div.last.col > h1') %>% html_text
      tmp_book_title <- gsub('\\s+', ' ', tmp_book_title) %>% str_trim
      Title <- c(Title, ifelse(is.na(tmp_book_title), 'E', tmp_book_title))
      
      # Writer
      tmp_book_writer <- read_html(tmp_book_url) %>% html_nodes('div.authorName__container') %>%
        html_nodes('a.authorName > span') %>% html_text %>% str_trim %>%
        strsplit(',')
      tmp_book_writer <- toString(tmp_book_writer)
      Writer <- c(Writer, ifelse(is.na(tmp_book_writer), 'E', tmp_book_writer))
      
      # averageRating
      tmp_book_averageRating <- read_html(tmp_book_url) %>% html_nodes('div.last.col') %>%
        html_nodes('div.uitext.stacked > span') %>% html_text %>% str_trim
      tmp_book_averageRating <- tmp_book_averageRating[2] %>% as.numeric()
      averageRating <- c(averageRating, ifelse(is.na(tmp_book_averageRating), 'E', tmp_book_averageRating))
      
      # ratingAmount % reviewAmount
      tmp_book_Amount <- read_html(tmp_book_url) %>% html_nodes('div.uitext.stacked') %>%
        html_nodes('a.gr-hyperlink') %>% html_text %>% str_trim
      tmp_book_Amount <- gsub(' ratings', '', tmp_book_Amount)
      tmp_book_Amount <- gsub(' reviews', '', tmp_book_Amount)
      tmp_book_Amount <- gsub('\n', '', tmp_book_Amount) %>% str_trim
      
      tmp_book_ratingAmount <- tmp_book_Amount[1]
      tmp_book_ratingAmount <- gsub(',', '', tmp_book_ratingAmount) %>% as.integer()
      ratingAmount <- c(ratingAmount, ifelse(is.na(tmp_book_ratingAmount), 'E', tmp_book_ratingAmount))
      
      tmp_book_reviewAmount <- tmp_book_Amount[2]
      tmp_book_reviewAmount <- gsub(',', '', tmp_book_reviewAmount) %>% as.integer()
      reviewAmount <- c(reviewAmount, ifelse(is.na(tmp_book_reviewAmount), 'E', tmp_book_reviewAmount))
      
      # Summary
      tmp_book_summary <- read_html(tmp_book_url) %>% html_nodes('div.last.col') %>%
        html_nodes('div.readable.stacked > span') %>% html_text %>% str_trim
      tmp_book_summary <- tmp_book_summary[2]
      tmp_book_summary <- gsub('\\s+', ' ', tmp_book_summary)
      Summary <- c(Summary, ifelse(is.na(tmp_book_summary), 'E', tmp_book_summary))
      
      cat(j, '-th book of', i,'-th page done\n')
      Sys.sleep(1)
    }

    cat(i, '-th page done\n')
  }
  Keyword <- rep(x, 20)
  Book <- data.frame(cbind(Keyword, Title, Writer, averageRating, ratingAmount, reviewAmount, Summary))
  write.csv(Book, file='Assignment 4_Book.csv')
}

searchBook('Harry Potter')

book1 <- read.csv('Assignment 4_Book1.csv')
book2 <- read.csv('Assignment 4_Book2.csv')
book3 <- read.csv("Assignment 4_Book3.csv")
Book <- data.frame(rbind(book1, book2, book3))

write.csv(Book, file='Assignment 4_Book.csv')
