#environement configuration

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(vroom)) install.packages("vroom", repos = "http://cran.us.r-project.org")
if(!require(feather)) install.packages("feather", repos = "http://cran.us.r-project.org")
if(!require(fst)) install.packages("fst", repos = "http://cran.us.r-project.org")
if(!require(gdata)) install.packages("gdata", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(gridExtra)
library(scales)
library(vroom)
library(feather)
library(fst)
library(gdata)
library(anytime)
library(Metrics)

#we want to the set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#We want to create a specific folder of files storages, if not existed
if (file.exists("Data") == FALSE){
  dir.create("Data")
} 
  
#We want to download the data file, if not already downloaded
if(file.exists('Data/ml-10m.zip') == FALSE) {
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", 
                'Data/ml-10m.zip')
} 


#For computer time optimization, we want to store the data set after processing and cleaning in the computer
#Different methods are available, lets test execution time and file size for each method

#function to calculate time spent and file size with files extensions, storage functions' names 
#and method names as variables
storage_compare <- function(x,a){ 
  time_results <- data.frame()
  for (i in 1:dim(a)[1]){
    time_spent <- system.time(get(a$storage_functions[i])(x,a$storage_files[i])) #execute the function with file extension name
    time_results <- bind_rows(time_results,data_frame(filetype = a$storage_modes[i], #add results to data frame 
                                                      user.self = time_spent[1], sys.self = time_spent[2], elapsed = time_spent[3],
                                                      size = humanReadable(file.info(a$storage_files[i])$size)))
  }
  time_results
}

storage_files <- c("Data/ml-10M100K/ratings.rda","Data/ml-10M100K/ratings.cvs", #file extension name
                   "Data/ml-10M100K/ratings.feather","Data/ml-10M100K/ratings.fst")
storage_modes <- c("RDS", "CVS", "Feather", "FST") #method names
storage_functions <- c("saveRDS", "vroom_write", "write_feather", "write_fst") #functions to be called
storage_variables <- data.frame(storage_modes,storage_files,storage_functions)

ratings <- fread(text = gsub("::", "\t", readLines(unzip('Data/ml-10m.zip', 
                                                         "ml-10M100K/ratings.dat",exdir = "Data"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

storage_compare_result <- storage_compare(ratings,storage_variables)
storage_compare_result

#cleaning of files and variables used in the storage testing function
file.remove("data/ml-10M100K/ratings.dat")
unlink("data/ml-10M100K", recursive = TRUE)
file.remove(storage_files)
rm(storage_files,storage_modes, storage_functions, storage_variables, ratings, storage_compare_result,storage_compare)


# 3- Data Collection

#Lets unzip and download data sets to create a movielens object
if(file.exists("Data/ml-10M100K/movielens.fst") == TRUE){
  movielens <- read_fst("Data/ml-10M100K/movielens.fst") %>% data.table()
} else {
  ratings <- fread(text = gsub("::", "\t", readLines(unzip('Data/ml-10m.zip', "ml-10M100K/ratings.dat",exdir = "Data"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip('Data/ml-10m.zip', "ml-10M100K/movies.dat",exdir = "Data")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  #Save data frame for future use
  write_fst(movielens,"Data/ml-10M100K/movielens.fst") 
  
  #cleaning of remove files and variables
  file.remove("Data/ml-10M100K/ratings.dat","Data/ml-10M100K/movies.dat")
  rm(ratings,movies)
}

# 4- Data Processing

str(movielens)

#Lets extract the movie rating year from timestamp
if (length(which(names(movielens) == "year_rating")) == 0) {
  movielens <- movielens %>% mutate(timestamp = anytime(timestamp))
  movielens <- movielens %>% mutate(year_rating = year(timestamp))
}

#Lets split title into movie title and movie production year
if (length(which(names(movielens) == "year_movie")) == 0) {
  movielens <- movielens %>% mutate(year_movie = str_extract(title,
    "[0-9][0-9][0-9][0-9]\\)$")) %>% mutate(year_movie = sub("\\)$", "", year_movie))
  movielens$year_movie <- as.integer(movielens$year_movie)
}

if (length(which(names(movielens) == "movie_title")) == 0) {
  movielens <- movielens %>% mutate(movie_title = sub(" \\([0-9]+\\)$", "", title))
}

#Save data frame for future use
write_fst(movielens,"Data/ml-10M100K/movielens.fst")

# 5- Data Exploration and Analysis

dim(movielens)

str(movielens)

## 5.1- UserID 

n_distinct(movielens$userId)

sum(is.na(movielens$userId) == TRUE)

#Lets analyse how many reviews are done by unique users
p1 <- movielens %>% group_by(userId) %>% summarise(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(aes(fill=..count..)) +
  scale_x_log10() +
  scale_fill_gradient(low="#C0C0C0", high="#808080") +
  scale_y_continuous(labels = comma) +
  labs(title="Number of users per number of review' levels",y= "Number of users (count)", x = "Number of review's levels") 

#Lets analyse what is the mean rating level by unique users
p2 <- movielens %>% group_by(userId) %>% 
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(binwidth = 0.25, color = "green") +
  scale_y_continuous(labels = comma) +
  labs(title="Mean rating per number of users' levels",y= "Number of users (count)", x = "Mean rating") 

grid.arrange(p1, p2, ncol=2)

## 5.2- Ratings

n_distinct(movielens$rating)

as.factor(movielens$rating) %>% levels()

sum(is.na(movielens$rating) == TRUE)

#Lets analyse the number of reviews by rating level
movielens %>% ggplot(aes(rating)) +
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=0.5) + 
  scale_fill_gradient(low="#C0C0C0", high="#808080") +
  labs(title="Number of reviews per rating levels",y= "Number of review (density)", x = "Rating's level")


## 5.3- MovieID 

n_distinct(movielens$movieId)

sum(is.na(movielens$movieId) == TRUE)

#Lets analyse how many reviews are done by unique movies
p1 <- movielens %>% group_by(movieId) %>% summarise(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(aes(fill=..count..)) +
  scale_x_log10() +
  scale_fill_gradient(low="#C0C0C0", high="#808080") +
  scale_y_continuous(labels = comma) +
  labs(title="Number of movies per number of review' levels",y= "Number of movies (count)", x = "Number of review's levels") 

#Lets analyse what is the mean rating level by unique movies
p2 <- movielens %>% group_by(movieId) %>% 
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(binwidth = 0.25, color = "green") +
  scale_y_continuous(labels = comma) +
  labs(title="Mean rating per number of movies' levels",y= "Number of movies (count)", x = "Mean rating")

grid.arrange(p1, p2, ncol=2)


## 5.4- movie year 

n_distinct(movielens$year_movie)

as.factor(movielens$year_movie) %>% levels()

sum(is.na(movielens$year_movie) == TRUE)

#Lets analyse number of movies per year of production
p1 <- movielens %>% ggplot(aes(year_movie)) + 
  geom_histogram() +
  scale_y_continuous(labels = comma) +
  labs(title="Number of reviews per production year",y= "Number of review (count)", x = "Year movie produced")  

#Lets analyse what is the mean rating level by year of production
p2 <- movielens %>% mutate(age_movie = 2020 - year_movie) %>%
  group_by(age_movie) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot() +
  geom_bar(aes(x = age_movie, y = mean_rating), stat = "identity") +
  labs(title="Mean rating per age of movie",y= "Mean rating level", x = "Age of movie")  

grid.arrange(p1, p2, ncol=2)

## 5.5- rating year 

n_distinct(movielens$year_rating)

as.factor(movielens$year_rating) %>% levels()

sum(is.na(movielens$year_rating) == TRUE)

#Lets analyse number of movies per year of rating
p1 <- movielens %>% ggplot(aes(year_rating)) + 
  geom_histogram() + 
  scale_y_continuous(labels = comma) +
  labs(title="Number of reviews per rating year",y= "Number of review (count)", x = "Year movie rated")  

#Lets analyse what is the mean rating level by year of rating
p2 <- movielens %>% mutate(age_rating = 2020 - year_rating) %>%
  group_by(age_rating) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_bar(aes(x = age_rating, y = mean_rating), stat = "identity") +
  labs(title="Mean rating per rating year",y= "Mean rating level", x = "Rating year")  

grid.arrange(p1, p2, ncol=2)

## 5.6- rating age 

test <- movielens %>% mutate(rating_age = year_rating - year_movie) %>% mutate(rating_age = as.factor(rating_age))
test$rating_age %>% levels()

movielens[which(test$rating_age == "-2"),] %>% select(userId,movieId,rating,title,timestamp)

movielens <- movielens[-which(test$rating_age == "-1" | test$rating_age == "-2"),]

#Lets analyse number of movies per age of rating
p1 <- movielens %>% mutate(rating_age = year_rating - year_movie) %>%
  ggplot(aes(rating_age)) + 
  geom_histogram() +
  scale_y_continuous(labels = comma) +
  labs(title="Number of reviews per rating age",y= "Number of review (count)", x = "Age of rating")  

#Lets analyse what is the mean rating level by age of rating
p2 <- movielens %>% mutate(rating_age = year_rating - year_movie) %>%
  group_by(rating_age) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot() +
  geom_bar(aes(x = rating_age, y = mean_rating), stat = "identity")

grid.arrange(p1, p2, ncol=2)

rm(p1,p2,test) #cleaning of files and variables used
write_fst(movielens,"Data/ml-10M100K/movielens.fst") #Save data frame for future use


# 6- Model preparation
#creation of a validation set that will be 10% of MovieLens data
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

#making sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#adding rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

nrow(movielens) - nrow(edx) - nrow(validation)

#defining the RMSe function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#cleaning of files and variables used
rm(temp,test_index,removed,movielens)

# 7- Model deployment

##Level 1 - Mean:
mu_hat <- mean(edx$rating)
level1_rmse <- RMSE(validation$rating, mu_hat)
level1_rmse

rmse_conclusions <- data_frame(method = "Level1 - Mean:", RMSE = level1_rmse)

##Level 2 - Movie Bias:

#first, estimate the effect of each movie bi
mu <- mean(edx$rating)

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

# then, make the prediction using the new model y= mu + b
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

level2_rmse <- RMSE(predicted_ratings, validation$rating)
level2_rmse

# Add the rmse result of the new model to the rmse data frame
rmse_conclusions <- bind_rows(rmse_conclusions,
                          data_frame(method="Level2 - Movie Bias:",  
                                     RMSE = level2_rmse ))

##Level 3 - User Bias:

#first, estimate the effect of each movie bi
mu <- mean(edx$rating)

user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarise(b_ii = mean(rating - mu))

# then, make the prediction using the new model y= mu + b
predicted_ratings <- mu + validation %>% 
  left_join(user_avgs, by='userId') %>%
  pull(b_ii)

level3_rmse <- RMSE(predicted_ratings, validation$rating)
level3_rmse

# Add the rmse result of the new model to the rmse data frame
rmse_conclusions <- bind_rows(rmse_conclusions,
                              data_frame(method="Level3 - User Bias:",  
                                         RMSE = level3_rmse ))
##Level 4 - Movie & User Bias:

# Let's compute the new term bu in our mode y= mu+bi+bu +eps
mu <- mean(edx$rating)

movie_user_mean  <- edx %>% 
  left_join(movie_avgs,n, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

# Now, compute the predicted ratings using the new model with user and movie effects:
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(movie_user_mean, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

level4_rmse <- RMSE(predicted_ratings, validation$rating)
level4_rmse

# Add the rmse result of the new model to the rmse data frame
rmse_conclusions <- bind_rows(rmse_conclusions,
                              data_frame(method="Level4 - User & Movie Bias:",  
                                         RMSE = level4_rmse ))
rmse_conclusions


##Level 5 - Regularization:

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)

lambdas[which.min(rmses)]
level5_rmse <- min(rmses)

rmse_conclusions <- bind_rows(rmse_conclusions,
                              data_frame(method="Level5 - Regularization:",  
                                         RMSE = level5_rmse ))
rmse_conclusions
