
# loading packacges
library(tidyverse)

# loading covid19.csv dataset
reviews <- read_csv("book_reviews.csv")

# analyze dataset

# Determine the dimension
dim(reviews) # 2000 rows, 4 columns

# Determine the column names
colnames(reviews) # [1] "book"   "review" "state"  "price"

# the types of each of the columns
for (c in colnames(reviews)) {
  print(typeof(reviews[[c]]))
} #[1] "character"[1] "character"[1] "character"[1] "double"

# unique values present in each of the columns
for (c in colnames(reviews)) {
  print(unique(reviews[[c]]))
}

# columns have data missing
complete_reviews <- reviews %>%
  filter(!(is.na(review)))
dim(complete_reviews)

# convert the reviews into numerical form
complete_reviews <- complete_reviews %>%
  mutate(review_num = case_when(review == "Poor" ~ 1,
                                review == "Fair" ~ 2,
                                review == "Good" ~ 3,
                                review == "Great" ~ 4,
                                review == "Excellent" ~ 5),
         is_high_review = if_else(review_num >= 4, TRUE, FALSE))

# We'll define most profitable book in terms of prices
complete_reviews3 <- complete_reviews %>%
  group_by(book) %>%
  summarise(
    sum_price = mean(price))
arrange(complete_reviews3, -sum_price)

