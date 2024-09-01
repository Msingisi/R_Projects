# Creating An Efficient Data Analysis Workflow

# import libraries
library(tidyverse)

# load dataset
reviews <- read_csv("C:\\Users\\mzing\\Documents\\Projects\\R-Projects\\R-Guided-Project\\book_reviews.csv")

# Getting Familiar With The Data
# How big is the dataset?
dim(reviews)

# What are the column names?
colnames(reviews)

# What are the column types?
for (c in colnames(reviews)) {
  print(typeof(reviews[[c]]))
}

# What are the unique values in each column?
for (c in colnames(reviews)) {
  print("Unique values in the column:")
  print(c)
  print(unique(reviews[[c]]))
  print("")
}

## The `reviews` column represents what the score that the reviewer gave the book
## The `book` column indicates which particular textbook was purchased
## The `state` column represents the state where the book was purchased
##  The `price` column represents the price that the book was purchased for

# Handling Missing Data

complete_reviews = reviews %>% 
  filter(!is.na(review))
dim(complete_reviews)

## There were about 200 reviews that were removed from the dataset
## This is about 10% of the original dataset

# Dealing With Inconsistent Labels

## We'll use the shortened postal codes instead since they're shorter

complete_reviews <- complete_reviews %>% 
  mutate(
    state = case_when(
      state == "California" ~ "CA",
      state == "New York" ~ "NY",
      state == "Texas" ~ "TX",
      state == "Florida" ~ "FL",
      TRUE ~ state # ignore cases where it's already postal code
    )
  )

# Transforming the Review Data

complete_reviews <- complete_reviews %>% 
  mutate(
    review_num = case_when(
      review == "Poor" ~ 1,
      review == "Fair" ~ 2,
      review == "Good" ~ 3,
      review == "Great" ~ 4,
      review == "Excellent" ~ 5
    ),
    is_high_review = if_else(review_num >= 4, TRUE, FALSE)
  )
head(complete_reviews)

# Analyzing The Data

# Most profitable book in terms of how many books there was sold. 
complete_reviews %>% 
  group_by(book) %>% 
  summarize(
    purchased = n()
  ) %>% 
  arrange(-purchased)

## The books are relatively well matched in terms of purchasing, 
## but "Fundamentals of R For Beginners" has a slight edge over everyone else


# Is there any relationship between state and the books purchased there?
state_count <- complete_reviews %>% 
  group_by(state, book) %>% 
  summarize(count = n())

ggplot(state_count, aes(x = state, y = count, fill = book)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

## In CA the most purchased book is R for dummies, followed slightly by Fundamentals of R For Beginners
## IN FL R Made Easy, Secrets Of R For Advanced Students, and Top 10 Mistakes R Beginners Make are relatively matched in terms of purchasing
## In NY Secrets Of R For Advanced Students, Top 10 Mistakes R Beginners Make are the mostly purchased books


# How are reviews for this books looks like
complete_reviews %>% 
  group_by(review) %>% 
  summarize(
    count_reviews = n()
  ) %>% 
  arrange(-count_reviews)

## Most of the books were reviewed as fair, followed by poor and good

# mean review score for each book
book_pularity <- complete_reviews %>% 
  group_by(book) %>% 
  summarise(avg_score = mean(review_num))

book_pularity <- book_pularity %>% 
  arrange(book_pularity)

book_pularity

ggplot(book_pularity, aes(x = book, y = avg_score)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90))

# Fundamentals of R For Beginners, and Top 10 Mistakes R Beginners Make each have a rating slightly above 3

book_profitability <- complete_reviews %>% 
  group_by(book) %>% 
  summarise(total_revenue = sum(price))

ggplot(book_profitability, aes(x = book, y = total_revenue)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))
  