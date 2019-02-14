library(recommenderlab)
library(dplyr)

#loading preprocessed data
books_clean <- read.csv("data/books_clean.csv", header = TRUE, stringsAsFactors=FALSE)
ratings_sample <- read.csv("data/ratings_sample.csv", header = TRUE)

book_recommendation <- function(input1,input2,input3, input4, rating1, rating2, rating3, rating4) {
  
  #find row numbers of selected books
  row_num1 <- which(books_clean$unique_title == input1) 
  row_num2 <- which(books_clean$unique_title == input2)
  row_num3 <- which(books_clean$unique_title == input3)
  row_num4 <- which(books_clean$unique_title == input4)
  
  userSelect <- matrix(NA,length(unique(ratings_sample$book_id))) #creating empty rating matrix for current user

  userSelect[row_num1] <- rating1 #assigning given rating to book1
  userSelect[row_num2] <- rating2 #assigning given rating to book2
  userSelect[row_num3] <- rating3 #assigning given rating to book3
  userSelect[row_num4] <- rating4 #assigning given rating to book4
  
  
  ratingmat <- sparseMatrix(ratings_sample$book_id, ratings_sample$user_id, x=ratings_sample$rating) # book x user matrix
  
  ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
  ratingmat <- ratingmat[rowSums(ratingmat) != 0, ] # remove books with no ratings (arab boooks)
  
  #assigning dimnames
  dimnames(ratingmat) <- list(book_id = as.character(sort(unique(ratings_sample$book_id))),
                              user_id = as.character(sort(unique(ratings_sample$user_id))))

  # add user's ratings as first column to rating matrix
  rmat <- cbind(userSelect, ratingmat)
  rm(ratingmat)
  #replace NAs with zeros
  rmat[is.na(rmat) == T] <- 0
  
  # get the indices of which cells in the matrix should be predicted
  # predict all books the current user has not yet rated
  items_to_predict <- which(rmat[, 1] == 0)
  prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
  
  # run the ubcf-alogrithm
  res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
  
  # sort, organize, and return the results
  user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
  user_predicted_ids <- as.numeric(names(user_results))
  rm(res)
  
  books_clean_rows <- vector(length = 20, mode = "integer")
  for (i in 1:20) {
    books_clean_rows[i] <- which(books_clean$book_id == user_predicted_ids[i])
  }
  

  recom_results <- data.table(Rank = 1:20, 
                              BookID = as.character(user_predicted_ids), 
                              Author = books_clean$authors[books_clean_rows], 
                              Title = books_clean$title[books_clean_rows],
                              #Predicted_rating =  user_results,
                              Average_rating = books_clean$average_rating[books_clean_rows],
                              See_more = createLink(books_clean$goodreads_book_id[books_clean_rows]))
  
  return(recom_results)
  
}
createLink <- function(val) {
  sprintf('<a href="https://www.goodreads.com/book/show/%s" target="_blank" class="btn btn-primary btn-outline">Info</a>',val)
}
