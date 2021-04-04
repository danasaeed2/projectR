if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(levels(movieId))[movieId],
         title = as.character(title),
         genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# 'Validation' set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in 'validation' set are also in 'edx' set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from 'validation' set back into 'edx' set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

set.seed(123, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)


head(edx)
str(edx)
dim(edx)

tp<- edx %>% group_by(genres) %>% 
summarise(n=n())
head(tp)

tp<- edx %>% group_by(rating) %>% summarize(n=n())
head(tp)

boxplot(edx$rating)

train_set <- train_set %>% select(userId, movieId, rating, title)
test_set  <- test_set  %>% select(userId, movieId, rating, title)

set.seed(123, sample.kind = "Rounding")

# Create the probability of each rating
p <- function(x, y) mean(y == x)
rating <- seq(0.5,5,0.5)

# Estimate the probability of each rating with Monte Carlo simulation
B <- 10^3
M <- replicate(B, {
  s <- sample(train_set$rating, 100, replace = TRUE)
  sapply(rating, p, y= s)
})
prob <- sapply(1:nrow(M), function(x) mean(M[x,]))

# Predict random ratings
y_hat_random <- sample(rating, size = nrow(test_set), 
                       replace = TRUE, prob = prob)


mse <- function(yhat,y)
{
  return(  mean((yhat - y)^2))
}
# Create a table with the error results
pred <- tibble(Method = "Project Goal", RMSE = 0.8649, MSE = NA, MAE = NA)
pred <- bind_rows(pred, 
                  tibble(Method = "Monte Carlo Random prediction", 
                         RMSE = caret::RMSE(test_set$rating,y_hat_random),
                         MSE  = mse(test_set$rating, y_hat_random),
                         MAE  = caret::MAE(test_set$rating,y_hat_random)))


pred


# Mean of observed values
mu <- mean(train_set$rating)

# Update the error table  
pred <- bind_rows(pred, 
                  tibble(Method = "Mean model", 
                         RMSE = caret::RMSE(test_set$rating, mu),
                         MSE  = mse(test_set$rating, mu),
                         MAE  = caret::MAE(test_set$rating, mu)))
# Show the RMSE improvement  
pred

```

After getting the predictions on the linear model, the next step is to include the effect of the movie on the linear model so the extension to the linear model can be defined from the code below

```{r movieaffect}
# Movie effects (bi)
bi <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
head(bi)


#predict

y_hat_bi <- mu + test_set %>% 
  left_join(bi, by = "movieId") %>% 
  .$b_i

# Calculate the RMSE  
pred <- bind_rows(pred, 
                  tibble(Method = "Mean + bi", 
                         RMSE = caret::RMSE(test_set$rating, y_hat_bi),
                         MSE  = mse(test_set$rating, y_hat_bi),
                         MAE  = caret::MAE(test_set$rating, y_hat_bi)))

# Show the RMSE improvement  
pred

bu <- train_set %>% 
  left_join(bi, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Prediction
y_hat_bi_bu <- test_set %>% 
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Update the results table
pred <- bind_rows(pred, 
                  tibble(Method = "Mean + bi + bu", 
                         RMSE = caret::RMSE(test_set$rating, y_hat_bi_bu),
                         MSE  = mse(test_set$rating, y_hat_bi_bu),
                         MAE  = caret::MAE(test_set$rating, y_hat_bi_bu)))

# Show the RMSE improvement  
pred


train_set %>% 
  left_join(bi, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:15)

titles <- train_set %>% 
  select(movieId, title) %>% 
  distinct()

bi %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(-b_i) %>% 
  select(title) %>%
  head()

bi %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(b_i) %>% 
  select(title) %>%
  head() 



train_data <- train_set %>% 
  select(userId, movieId, rating) %>% 
  spread(movieId, rating) %>% 
  as.matrix()

if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
set.seed(123, sample.kind = "Rounding") # This is a randomized algorithm

# Convert the train and test sets into recosystem input format
train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
# Create the model object
r <-  recosystem::Reco()
# Select the best tuning parameters
opts <- r$tune(train_data, opts = list( 
  lrate = c(0.1, 0.2), 
  costq_l2 = c(0.01, 0.1),
  nthread  = 4, niter = 1))
# Train the algorithm  
r$train(train_data, opts = c(opts$min, nthread = 4, niter = 1))

y_hat_reco <-  r$predict(test_data, out_memory())
head(y_hat_reco, 10)
pred<- bind_rows(pred, 
                 tibble(Method = "Matrix Factorization", 
                        RMSE = caret::RMSE(test_set$rating, y_hat_reco),
                        MSE  = mse(test_set$rating, y_hat_reco),
                        MAE  = caret::MAE(test_set$rating, y_hat_reco)))
pred


set.seed(1234, sample.kind = "Rounding")
# Convert 'edx' and 'validation' sets to recosystem input format
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
validation_reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))
# Create the model object
r <-  recosystem::Reco()
# Tune the parameters
opts <-  r$tune(edx_reco, opts = list( 
  lrate = c(0.1, 0.2),
  
  costq_l2 = c(0.01, 0.1),
  nthread  = 4, niter = 1))
# Train the model
r$train(edx_reco, opts = c(opts$min, nthread = 4, niter = 1))

# Calculate the prediction
y_hat_final_reco <-  r$predict(validation_reco, out_memory())

# Update the result table
pred <- bind_rows(pred, 
                  tibble(Method = "Final Matrix Factorization ", 
                         RMSE = caret::RMSE(validation$rating, y_hat_final_reco),
                         MSE  = mse(validation$rating, y_hat_final_reco),
                         MAE  = caret::MAE(validation$rating, y_hat_final_reco)))
pred