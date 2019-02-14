library(dplyr)

# loading data
books <- read.csv("data/books.csv", header = TRUE, stringsAsFactors=FALSE)
ratings <- read.csv("data/ratings.csv", header = TRUE)

seed <- 20
set.seed(seed)

# for better efficiency, we will only consider ratings made by 50% of total users
sample_size <- 0.1
users <- unique(ratings$user_id)
sample_users <- sample(users, round(sample_size * length(users)))

# Arab chars show as undefined strings hence those books are excluded
ara_indexes <- which(books$language_code != 'ara')

# filtered df
ratings_sample <- ratings %>% 
  filter(user_id %in% sample_users) %>%
  filter(book_id %in% ara_indexes)

# each book has at least 5 ratings in our sample

#ratings_smry <- ratings_sample %>% 
#  group_by(book_id) %>%
#  summarise(n=n())

# We don't need all data so only relevant columns will be extracted. Additionaly, because of encoding issues with arabic
# characters, books with arabic language_code, 64 in total, will be removed
books_clean <- books %>%
  filter(language_code != 'ara') %>%
  select(book_id, authors, original_publication_year, original_title, title, average_rating, ratings_count, goodreads_book_id)


# It looks like values in title and original_title columns aren't unique. As title is what we
# want to show to user in order to let him pick a book he likes/dislikes something has to be done

#length(unique(books_clean$original_title)) # 9217 out of 9936 is unique
#length(unique(books_clean$title)) # 9900 out of 9936 is unique

# We can further investigate those duplicates by creating a df made of titles and their corresponding frequencies

freq_df <- data.frame(table(books$title))

duplicates <- data.frame()
duplicates <- books_clean %>%
  filter(books_clean$title %in% freq_df$Var1[freq_df$Freq> 1])


# To fix this, new column is made. It's the combination of title and authors columns separated with "-"
# for duplicated titles. For non-duplicate values original titles are used (column 'title')
books_clean <- books_clean %>%
  mutate(unique_title = ifelse(books_clean$book_id %in% duplicates$book_id,
                               paste(title, authors, sep = " - "),
                               books_clean$title))

# Now all titles are unique

#length(unique(books_clean$unique_title))

write.csv(books_clean, 'data/books_clean.csv')
write.csv(ratings_sample, 'data/ratings_sample.csv')
