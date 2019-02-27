library(dplyr)
library(rAmCharts)
library(ggplot2)
library(DT)
library(reshape2)

# loading data
books <- read.csv("data/books.csv", header = TRUE, stringsAsFactors=FALSE)
ratings <- read.csv("data/ratings.csv", header = TRUE)

glimpse(books)
glimpse(ratings)

# rating distribution
ratings_smry <- ratings %>% group_by(rating) %>%
  summarise(count=n()) %>% mutate(rating = as.character(rating))

amBarplot(x = "rating", y = "count", data = ratings_smry, xlab = "Rating", ylab = "Frequency", depth = 15, show_values = T)


#distribution of number of ratings made by each user
user_ratings_smry <- ratings %>% 
  group_by(user_id) %>%
  summarise(count=n())

get_descriptives(user_ratings_smry$count, "user_ratings_count")

amHist(x = user_ratings_smry$count, col = "#CECECE", creditsPosition = "top-right", control_hist = list(breaks = 10),
       ylab = "Number of users", xlab = "User ratings frequency")


#distribution of number of ratings made for each book
book_ratings_smry <- ratings %>% 
  group_by(book_id) %>%
  summarise(count=n())

get_descriptives(book_ratings_smry$count, "book_ratings_count")

length(which(book_ratings_smry$count > 2000))

#number of ratings per book
book_ratings_smry_filtered <- book_ratings_smry %>%
  filter(count < 2000) 

amHist(x = book_ratings_smry_filtered$count, col = "#CECECE", creditsPosition = "top-right",xlim = c(0,20),
       ylab = "Number of books", xlab = "Book ratings frequency")


# comparation of average ratings, the one from books df and other based on available ratings
average_rating <- books %>% select(average_rating) %>%
                  mutate(type = "global")

average_rating_sample <- ratings %>% 
  group_by(book_id) %>%
  summarise(average_rating=mean(rating)) %>%
  mutate(type = "sample") %>% select(-book_id)

average_rating_comp <- bind_rows(average_rating, average_rating_sample)

ggplot(average_rating_comp, aes(average_rating, fill = type)) + geom_density(alpha = 0.2)

# examining parameters of descriptive statistics for average rating, both on sample and globally
desc_1 <- get_descriptives(average_rating$average_rating, "global_mean")
desc_2 <- get_descriptives(average_rating_sample$average_rating, "sample_mean")

datatable(bind_rows(desc_1, desc_2),rownames = c("global_mean","sample_mean"))

##### UBCF algorithm

ratingmat <- dcast(ratings, user_id~book_id, value.var = "rating", na.rm=FALSE)
ratingmat <- ratingmat[,-1]
ratingmat[1:5,1:5]

rownames(ratingmat) <- as.character(sort.int(unique(ratings$user_id)))

# selecting random user and getting indexes of books he rated
current_user <- "2929"
rated_items <- which(!is.na((as.data.frame(ratingmat[current_user, ]))))

# let's take a deeper look at his ratings, they are mostly positive, but there is 9 books he dislikes
user_2929_ratings <- ratings %>% filter(user_id == 2929)
table(user_2929_ratings$rating)

# now we can isolate other users from our rating matrix, here we chose
# only those users that have 30 book ratings in common with user 2929
# picking lower number will result in more users to calculate similarities for and vice versa
# second condition is to make sure that user 2929 is excluded from similar ones
user_2929 <- ratingmat[rownames(ratingmat) == "2929",]
selected_users <- names(which(apply(!is.na(ratingmat[ ,rated_items]), 1, sum) >= 30 & rownames(ratingmat) != "2929"))
length(selected_users)
head(selected_users, 10)

ratingmat_selected_users <- ratingmat[selected_users, ]
user_mean_ratings <- rowMeans(ratingmat_selected_users,na.rm=T)
ratingmat_selected_users <- ratingmat_selected_users - user_mean_ratings

# similarities between users is calculated, top 5 similar users are shown based on correlation with user 2929
similarities <- cor(t(ratingmat_selected_users), t(user_2929), use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
head(res, 5)

# once again, we will isolate those 5 users
similar_users <- names(res[1:5])

# in order to get predictions, mean is calculated for items that user 2929 hasn't yet rated
similar_users_ratings <- data.frame(item = rep(colnames(ratingmat_selected_users), length(similar_users)),
                                    rating = c(t(as.data.frame(ratingmat_selected_users[similar_users,])))) %>%
                                    filter(!is.na(rating))

predictions <- similar_users_ratings %>% 
  filter(!(item %in% as.character(user_2929_ratings$book_id))) %>% 
  group_by(item) %>% summarize(mean_rating = round(mean(rating),4))

# and finally, these are the books predicted for our test user
predictions %>% 
  arrange(-mean_rating) %>% 
  top_n(10, wt = mean_rating) %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))




###################### Function ##########################
get_descriptives <- function(var, var_name){
  descriptives <- data.frame(min = round(min(var),2),
                             mean = round(mean(var),2),
                             median = round(median(var),2),
                             max = round(max(var),2))
  rownames(descriptives) <- var_name
  #datatable(descriptives)
  descriptives
}

