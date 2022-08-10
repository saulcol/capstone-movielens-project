################################################################################################
#
# CAPSTONE PROJECT: MOVIELENS
# Building a Movie Recommendation system using MovieLens dataset
#
# author: "Saúl Santillán Gutiérrez"
# date: "August 10, 2022"
# 
################################################################################################


################################################################################################
#
# Preliminary
# 
################################################################################################
#
# See the Rmarkdown document "report_movielens_proj.Rmd" to get more information about it.



################################################################################################
#
# General Instructions by HarvardX's team
# 
################################################################################################
#
# You will be creating your own recommendation system using all the tools we have shown you throughout
# the courses in this series. We will use the 10M version of the MovieLens dataset from
# (https://grouplens.org/datasets/movielens/10m/) to make the computation a little easier.
#
# The links to download the 10M version of the MovieLens dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
#
# Develop your algorithm using the `edx` set. For a final test of your final algorithm, predict
# movie ratings in the `validation` set (the final hold-out test set) as if they were unknown.
# "RMSE" will be used to evaluate how close your predictions are to the true values in the
# `validation` set (the final hold-out test set).  
#
# IMPORTANT: The `validation` data (the final hold-out test set) should **NOT** be used for training,
# developing, or selecting your algorithm and **it should ONLY be used for evaluating the RMSE of
# your final algorithm**. The final hold-out test set should only be used at the end of your project
# with your final model. **It may not be used to test the RMSE of multiple models during model development**.
# You should split the `edx` data into separate training and test sets to design and test your algorithm.
#
# IMPORTANT: Please be sure not to use the validation set (the final hold-out test set) for training
# or regularization - you should create an additional partition of training and test sets from the
# provided edx dataset to experiment with multiple parameters or use cross-validation.
#
# Remember your goal is to get a RMSE < 0.86490.



################################################################################################
#
# 1. Overview
# 
################################################################################################
#
# See the Rmarkdown document "report_movielens_proj.Rmd" to get more information about it.




################################################################################################
#
# 2. Methods and Analysis
# 
################################################################################################
#
# 
################################################################################################
#
# 2.1 Preparing the Data Science Project and the Datasets
#
################################################################################################
#
#+++++++++++++++++++++++++  Install and Load the packages required +++++++++++++++++++++++++++++
#
# Install the libraries if they do not exist and load them
if(!require(this.path)) install.packages("this.path", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("scales", repos = "http://cran.us.r-project.org")

library(this.path)
library(tidyverse)
library(caret)
library(data.table)
library(ggthemes)
library(scales)
library(lubridate)
library(Hmisc)


# Assign our RMSE goal to RMSE_GOAL variable. Remember our goal is RMSE < 0.86490
RMSE_GOAL <- 0.86490


#+++++++++++++++++++ set the working directory and create a subdirectory ++++++++++++++++++++++
#
# Get the current path
wd <- this.dir()
# Set the working directory
setwd(wd)
# check if the folder “rdas” exists in the current directory, if not creates a "rdas" directory
ifelse(!dir.exists("rdas"), dir.create("rdas"), "Folder rdas exists already")


# +++++++++++++++++++ Get the version of R ++++++++++++++++++++
v <- R.Version() # It is a List


#+++++++ Downloading the MovieLens 10M dataset, create the edx and validation datasets +++++++++
#
# IMPORTANT: We can choose if we run this part of the code only once.
#
# NOTE: If we have trouble with the next code, please skip it and go to the section:
# ++++++++ Run in case of emergency ++++++++
#
# Check if the "ratings.dat" and "movies.dat" files exist in the "ml-10M100K" directory
if(file.exists(paste0(wd, "/ml-10M100K/ratings.dat"))==TRUE & file.exists(paste0(wd, "/ml-10M100K/movies.dat"))==TRUE){
  print("Files ratings.dat and movies.dat exist already")
}else{
  print("Files ratings.dat and movies.dat do NOT exist...downloading and creating the datasets")
  #
  #
  # NOTE: This process could take a couple of minutes. We do not be discouraged.
  #
  #
  #++++++ Downloading the MovieLens 10M dataset, assigning to `ratings` and `movies` variables ++++++
  #
  # MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  #
  # Download the dataset
  dl <- tempfile()
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  # Assign to ratings
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  # Assign to movies
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  
  # +++++++++++++ Make a left_join to `ratings` and `movies` assigning to ++++++++++++++
  # ++++++++ `movielens` variable and set the seed according to the R version ++++++++++
  #
  # Detect the version of R to run the correct code
  if (paste(v$major, v$minor, sep = ".") < "4.0.0"){
    print("version of R is MINOR to 4.0.0, mutate movieId with levels")
    # if using R 3.6 or earlier:
    movies <- as.data.frame(movies) %>%
      mutate(movieId = as.numeric(levels(movieId))[movieId],
             title = as.character(title),
             genres = as.character(genres))
  }else{
    print("version of R is MAJOR or equal to 4.0.0, mutate movieId without levels")
    # if using R 4.0 or later:
    movies <- as.data.frame(movies) %>%
      mutate(movieId = as.numeric(movieId),
             title = as.character(title),
             genres = as.character(genres))
  }
  
  # make a left_join to `ratings` and `movies` assigning to `movielens` variable
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Set the seed according to the R version
  if (paste(v$major, v$minor, sep = ".") < "3.6.0"){
    print("version of R is MINOR to 3.6.0, use set.seed(1)")
    # if using R 3.5 or earlier, use `set.seed(1)`:
    set.seed(1)
  }else{
    print("version of R is MAJOR or equal to 3.6.0, use set.seed(1, sample.kind=Rounding)")
    # if using R 3.6 or later:
    set.seed(1, sample.kind="Rounding")
  }
  
  # +++++++++++++ Create `edx` set, `validation` set (final hold-out test set) +++++++++++++
  # +++++++++++++++++++++ and save them to `/rdas/cp_movielens.rda` ++++++++++++++++++++++++
  #
  # Validation set will be 10% of MovieLens data
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  #++++++++++++++++++++++ Create `validation` set +++++++++++++++++++
  # Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  
  
  #+++++++++++++++++++++ Create `edx` set ++++++++++++++++++++++++++
  edx <- rbind(edx, removed)
  
  # Remove these objects from the Global Environment
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  #+++++++++ save objects "edx", "validation" in the file path: rdas/cp_movielens.rda +++++++++
  save(edx, validation, file = "./rdas/cp_movielens.rda")
}
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
#+++++++ Downloading the MovieLens 10M dataset, create the datasets edx and validation +++++++++
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++ Run in case of emergency +++++++++++++++++++++++++++++++++++++
#
# If we had any trouble with above code, at the moment to downloading or others problems,
# because I saw in the discussion forum some people wrote about it.
# I provided a .rda file; with the objects "edx" and "validation", from my gitlab.
#
# Download the file from gitlab to "rdas" directory
# Discomment the next line, if we had a trouble with the code above. This process is more faster!!
#download.file("https://gitlab.com/saulcol/rdas/-/raw/main/cp_movielens.rda", paste0(wd, "/rdas/cp_movielens.rda"))


# Verify if the "cp_movielens.rda" file exists in the "rdas" directory
if(file.exists(paste0(wd, "/rdas/cp_movielens.rda"))==TRUE){
  print("File cp_movielens.rda exists already")
}else{
  print("File cp_movielens.rda does NOT exist...downloading")
  # Download the file from gitlab to "rdas" directory
  download.file("https://gitlab.com/saulcol/rdas/-/raw/main/cp_movielens.rda", paste0(wd, "/rdas/cp_movielens.rda"))
}


# Remove these objects from the Global Environment if they exist
if(exists("edx")) rm("edx", envir = globalenv())
if(exists("validation")) rm("validation", envir = globalenv())

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.1 Preparing the Data Science Project and the Datasets
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 2.2 Performing Data Exploration and Visualization to "edx" dataset
#
################################################################################################
#
#
# +++++++++++++++++++ Overall Exploration in the Dataset +++++++++++++++++++++++
#
# load objects "edx", "validation" from the file path: rdas/cp_movielens.rda. Take a few seconds
load("./rdas/cp_movielens.rda")


# Knowing its structure and how it is composed the "edx" dataset
glimpse(edx)
#userId    <int>
#movieId   <dbl>
#rating    <dbl>
#timestamp <int>
#title     <chr>
#genres    <chr> 

str(edx)
#userId   : int
#movieId  : num
#rating   : num
#timestamp: int
#title    : chr
#genres   : chr


# How many rows and columns are there in the "edx" dataset?
# Number of rows:
dim(edx)[1]

# Number of columns:
dim(edx)[2]


# The "edx" dataset is composed by 9,000,055 observations (rows) and 6 variables (columns).
# As well, its six columns have these characteristics:
#userId   : integer
#movieId  : numeric (double)
#rating   : numeric (double)
#timestamp: integer
#title    : character
#genres   : character


# View the content of the first six rows of the "edx" dataset
head(edx)

# Get a summary statistics 
summary(edx)

# Other way, to obtain a better summary statistics is with
# the function describe() of the package Hmisc
# NOTE: Take aprox. 1 minute
describe(edx)


# In general, it can be observed the mentioned dataset does not have missing values.
# the "userId" column has a range from 1 to 71567 and has 69878 different users.
# the "movieId" column has a range from 1 to 65133 and has 10677 different users.
# the "rating" column has 10 different ratings that are in a range from 0.5 to 5.0 with increments by 0.5.
# the "timestamp" variable has a range from 789652009 to 1231131736.
# the "title" column has 10676 different titles.
# And the last one column "genres" has 797 different genres and is labeled with one or more genre.

# Recall that the "rating" column is the outcome.


# +++++++++++++++++++ Are there missing values? +++++++++++++++++++++++
#
# Earlier when we used the describe function we saw that the six columns do not have missing values.
# We can corroborate it with these codes.

# Count missing values
sum(is.na(edx))
#[1] 0

any(is.na(edx))
#[1] FALSE

# count total missing values in each column of data frame
sapply(edx, function(x) sum(is.na(x)))
colSums(is.na(edx))


# In effect, this dataset does not have missing values.



# Now we proceed to perform a data analysis on each of the variables of this set.
#
#
#
#+++++++++++++++++++ Data Analysis in the UserId column ++++++++++++++++++++++
#
# How many different users are in the edx dataset?
length(unique(edx$userId))

# Plot a histogram of the Distribution of users rating movies (incorrect)
# Without adjusting the X axis
edx %>% group_by(userId) %>%
  summarise(total=n()) %>%
  ggplot(aes(total)) +
  geom_histogram(color = "black") +
  ggtitle("Distribution of Users",
          subtitle = "Without Adjusting the X axis.") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  theme_bw()


# Plot a histogram of the Distribution of users rating movies (correct)
# Adjusting the X axis with scale_x_log10()
edx %>% group_by(userId) %>%
  summarise(total=n()) %>%
  ggplot(aes(total)) +
  geom_histogram(color = "black") +
  scale_x_log10() + 
  ggtitle("Distribution of Users",
          subtitle = "Adjusting the X axis with scale_x_log10().") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  theme_bw()

# The major part of users rate less movies, while a few users rate more than a thousand movies.
# We notice that the distribution is right skewed.



#+++++++++++++++++++ Data Analysis in the MovieId column ++++++++++++++++++++++
#
# How many different movies are in the edx dataset?
n_distinct(edx$movieId)

# Which movie has the greatest number of ratings?
edx %>%
  group_by(movieId, title) %>%
  summarise(total = n()) %>%
  top_n(20) %>%
  arrange(desc(total))

# Plot a histogram of the Distribution of movies (correct)
# Adjusting the X axis with scale_x_log10()
edx %>% group_by(movieId) %>%
  summarise(total=n()) %>%
  ggplot(aes(total)) +
  geom_histogram(color = "black") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies",
          subtitle = "Adjusting the X axis with scale_x_log10().") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") + 
  theme_bw()



#+++++++++++++++++++ Data Analysis in the Ranting column ++++++++++++++++++++++
#
# As we know, the "rating" column is the outcome and it has 10 different ratings
# that are in a range from 0.5 to 5.0 with increments by 0.5.
#
# How many zeros were given as ratings in the edx dataset?
edx %>%
  filter(rating == 0) %>%
  count()


# How many threes were given as ratings in the edx dataset?
edx %>%
  filter(rating == 3) %>%
  count()

# What are the five most given ratings in order from most to least?
edx %>%
  group_by(rating) %>%
  summarise(total = n()) %>%
  top_n(5) %>%
  arrange(desc(total))


# In general, half star ratings are less common than whole star ratings (e.g., there are fewer
# ratings of 3.5 than there are ratings of 3 or 4, etc.).
#
# Numerically you can get it with this code:
edx %>%
  group_by(rating) %>%
  summarise(total = n()) %>%
  arrange(desc(total))


# Graphically it can be obtained with this code:
edx %>%
  group_by(rating) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line() +
  ggtitle("Rating Distribution") +
  xlab("Type of Rating") +
  ylab("Total") +
  theme_bw()



#+++++++++++++++++++ Data Analysis in the Timestamp column ++++++++++++++++++++++
#
# If we remember the "timestamp" variable has a range from 789652009 to 1231131736.
# And it is measured in seconds since January 1st, 1970.
#
# Let us to use the as_datetime() function of the package lubridate with the minimum value
# to know what is the starting date
as_datetime(min(edx$timestamp))

# Now the same process but with the maximum value to know what is the ending date
as_datetime(max(edx$timestamp))

# Next step, we need to transform the variable into an appropriate date format
# using the as_datetime() function
edx <- mutate(edx, date = as_datetime(timestamp))

# Verify which is the range period of the time of ratings
tibble(`Starting Date` = date(as_datetime(min(edx$timestamp), origin="1970-01-01")),
       `Ending Date` = date(as_datetime(max(edx$timestamp), origin="1970-01-01"))) %>%
  mutate(`Range Period` = duration(max(edx$timestamp)-min(edx$timestamp)))

# Plot the histogram of rating distribution by years
edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "black") +
  ggtitle("Rating Distribution By Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma) +
  theme_bw()

# Dates with more ratings
edx %>% mutate(date = date(as_datetime(timestamp, origin="1970-01-01"))) %>%
  group_by(date, title) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  head(10)



#+++++++++++++++++++ Data Analysis in the Genres column ++++++++++++++++++++++
#
# How many different genres are in the edx dataset?
length(unique(edx$genres))


# View the ten most viewed genres
edx %>% group_by(genres) %>% 
  summarise(total=n()) %>%
  top_n(10) %>%
  arrange(desc(total))


# Various movies are organized or cataloged by more than one genre.
# View the ten genres that have most number of different genres for each movie
tibble(num_genres = str_count(edx$genres, fixed("|")), 
       genres = edx$genres) %>% 
  group_by(num_genres, genres) %>%
  summarise(n = n()) %>%
  top_n(10) %>%
  arrange(desc(num_genres))


# See the top ten most genres by movie
edx %>% group_by(genres, movieId) %>% 
  summarise(total=n()) %>%
  top_n(10) %>%
  arrange(desc(total))

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.2 Performing Data Exploration and Visualization to "edx" Data set
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 2.3 Preparing and Cleaning the Training and Testing Data sets
#
################################################################################################
#
# Before we start building our model we need to divide the edx set in two new sets: One for
# training and the other for testing as instructed by Harvardx's team. Here are the instructions
# again:
#
# IMPORTANT: The `validation` data (the final hold-out test set) should **NOT** be used for training,
# developing, or selecting your algorithm and **it should ONLY be used for evaluating the RMSE of
# your final algorithm**. The final hold-out test set should only be used at the end of your project
# with your final model. **It may not be used to test the RMSE of multiple models during model development**.
# You should split the `edx` data into separate training and test sets to design and test your algorithm.
#
# IMPORTANT: Please be sure not to use the validation set (the final hold-out test set) for training
# or regularization - you should create an additional partition of training and test sets from the
# provided edx data set to experiment with multiple parameters or use cross-validation.
#
# Remember your goal is to get a RMSE < 0.86490.
#
#
#+++++++++++++++++++++ Create the train set and test set from the edx set ++++++++++++++++++++++
#
#
# Set the seed according to the R version
if (paste(v$major, v$minor, sep = ".") < "3.6.0"){
  print("version of R is MINOR to 3.6.0, use set.seed(1)")
  # if using R 3.5 or earlier, use `set.seed(1)`:
  set.seed(1)
}else{
  print("version of R is MAJOR or equal to 3.6.0, use set.seed(1, sample.kind=Rounding)")
  # if using R 3.6 or later:
  set.seed(1, sample.kind="Rounding")
}


# +++++++++++++++++++ Create `train` set, `test` set from the `edx` set ++++++++++++++++++++
# +++++++++++++++++++++ and save them to `/rdas/cp_movielens.rda` ++++++++++++++++++++++++
#
# We must split the 'edx' set in 2 parts: the training and test sets.
# We use the same process employed to create 'edx' and 'validation' sets.
#
# "test" set will be 10% and "train" set 90% of "edx" data
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

#++++++++++++++++++++++ Create `test` set +++++++++++++++++++
# Make sure userId and movieId in `test` set are also in `train` set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from `test` set back into `train` set
removed <- anti_join(temp, test)

#+++++++++++++++++++++ Create `train` set ++++++++++++++++++++++++++
train <- rbind(train, removed)


# Remove these objects from the Global Environment
rm(test_index, temp, removed)


#+++++++++++++++++++++ Cleaning the `train` and `test` set ++++++++++++++++++++++++++
#
# As prior to this talked about, various or many characteristics can be used to predict the rating for a given user.
# But nevertheless, a large number of predictors increment the model complexity and need more computer resources,
# In this project the estimated rating applies to movie and user information only.

train <- train %>% select(userId, movieId, rating, title)
test  <- test  %>% select(userId, movieId, rating, title)


#+++++++++ save objects "train", "test" in the file path: rdas/cp_movielens_train_test.rda +++++++++
save(train, test, file = "./rdas/cp_movielens_train_test.rda")

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.3 Preparing and Cleaning the Training and Testing Data sets
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 2.4 Modeling approach
#
################################################################################################
#
# See the Rmarkdown document "report_movielens_proj.Rmd" to get more information about it.
#
# 2.4.1 Linear Model
#
# 2.4.2 Regularization
#
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2.4 Modeling approach
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. Methods and Analysis
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




################################################################################################
#
# 3. Results
# 
################################################################################################
#
#
################################################################################################
#
# 3.1 Defining the RMSE Function to Evaluate the Model
#
################################################################################################
#
# Define the loss function
#
# Define the Root Mean Squared Error (RMSE) function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.1 Defining the RMSE Function to Evaluate the Model
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 3.2 Linear Model
#
################################################################################################
#
# It is important to remember that before creating the model we need to know that:
#
# The model must be built on the training set, and the test set must be employed to test
# the model. After that, when the model is finished and ready, we will use the 'validation' set
# to compute the final RMSE.
#
# To carry out this model we are going to follow the procedures stipulated and learned in
# this lesson:
#
# https://rafalab.github.io/dsbook/large-datasets.html#recommendation-systems
#
# We are going to build the linear model based on the formula:
#
# y_hat = mu + bi + bu + epsilon u,i

# we can load the objects "train", "test" to simplify the construction of our model if we want,
# just by running this code. In case it is necessary.
#
# load objects "train", "test" from the file path: rdas/cp_movielens_train_test.rda. Take a few seconds
load("./rdas/cp_movielens_train_test.rda")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2.1. Predict the Mean of the Rantings
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# The first prediction is the mean of the ratings (mu) represented for this formula:
#
# y_hat = mu
#
# Mean of observed values
mu <- mean(train$rating)

# Create a table to store the error scores
scores <- tibble(Method = "Project Goal", RMSE = RMSE_GOAL)

# Update the error table
scores <- bind_rows(scores,
                    tibble(Method = "Mean",
                           RMSE = RMSE(test$rating, mu)))

# Display the RMSE improvement
scores %>% knitr::kable()

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2.1. Predict the Mean of the Rantings
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2.2. Adding the Movie Effect (bi)
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# bi is the movie effect (bias) for movie i. The formula is:
#
# y_hat = mu + bi
#
# Movie effects (bi)
bi <- train %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

head(bi)

# Plot the distribution of movie effects
bi %>% ggplot(aes(x = b_i)) +
  geom_histogram(bins=10, col = I("black")) +
  ggtitle("Movie Effects Distribution") +
  xlab("Movie effects") +
  ylab("Counts") +
  scale_y_continuous(labels = comma) +
  theme_bw()

# Predict the rating with mean + bi
y_hat_bi <- mu + test %>%
  left_join(bi, by = "movieId") %>%
  .$b_i

# Compute the RMSE and update the scores table
scores <- bind_rows(scores,
                    tibble(Method = "Mean + bi",
                           RMSE = RMSE(test$rating, y_hat_bi)))

# Display the RMSE improvement
scores %>% knitr::kable()

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2.2. Adding the Movie Effect (bi)
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2.3. Adding the User Effect (bu)
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# bu is the user effect (bias) for user u. The formula is:
#
# y_hat = mu + bi + bu
#
# User effects (bu)
bu <- train %>%
  left_join(bi, by = 'movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

# Plot the distribution of user effects
bu %>% ggplot(aes(x = b_u)) +
  geom_histogram(col = I("black")) +
  ggtitle("User Effect Distribution") +
  xlab("User effects") +
  ylab("Counts") +
  scale_y_continuous(labels = comma) +
  theme_bw()

# Predict the rating with mean + bi + bu
y_hat_bi_bu <- test %>%
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Compute the RMSE and update the scores table
scores <- bind_rows(scores,
                    tibble(Method = "Mean + bi + bu",
                           RMSE = RMSE(test$rating, y_hat_bi_bu)))

# Display the RMSE improvement
scores %>% knitr::kable()

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2.3. Adding the User Effect (bu)
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2.4. Verifying the models
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# First, we are going to check if the model y_hat = mu + bi (Movie Effect) makes good ratings predictions.
# For that, we need to create this new dataset using the "train" set that connects movieId to movie title.
titles_bi <- train %>% 
  select(movieId, title) %>% 
  distinct()

# Now, we are going to display the 10 best movies based on bi with this code. Here we are using the bi set,
# titles_bi set and ranked by bi.
bi %>% 
  inner_join(titles_bi, by = "movieId") %>% 
  arrange(-b_i) %>%
  slice(1:10) %>%
  select(title)

# And here are the 10 worst movies according to bi.
bi %>% 
  inner_join(titles_bi, by = "movieId") %>% 
  arrange(b_i) %>%
  slice(1:10) %>%
  select(title)


# Number of ratings for 10 best movies based on bi using the train set
train %>% 
  left_join(bi, by = "movieId") %>%
  arrange(-b_i) %>% 
  group_by(title) %>% 
  summarise(n = n()) %>%
  slice(1:10)


# Let us inspect how often they are rated.
train %>% count(movieId) %>% 
  left_join(bi, by="movieId") %>%
  arrange(-b_i) %>% 
  slice(1:10) %>% 
  pull(n)

train %>% count(movieId) %>% 
  left_join(bi, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2.4. Verifying the model
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.2 Linear Model
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 3.3 Regularization
#
################################################################################################
#
# The next procedure is Regularization, for that we need to regularize the movie and user effects
# aggregating a penalty term or factor which is known as lambda and which is also a tuning parameter.
# we establish a set of values for lambda and use cross-validation to pick the best value that
# minimizes the RMSE.

# establish a set of values for lambda
lambdas <- seq(0, 10, 0.25)

# use cross-validation for tuning lambda
rmses <- sapply(lambdas, function(lambda){
  
  # Mean
  mu <- mean(train$rating)
  
  # Movie effects (bi)
  b_i <- train %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+lambda))
  
  # User effects (bu)
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    filter(!is.na(b_i)) %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  # Predict mu + bi + bu
  predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    filter(!is.na(b_i), !is.na(b_u)) %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$rating))
})


# Plot the Lambdas vs RMSE
tibble(Lambdas = lambdas, RMSE = rmses) %>%
  ggplot(aes(x = Lambdas, y = RMSE)) +
  geom_point() +
  ggtitle("Regularization", 
          subtitle = "Choose the best value of Lambda that minimizes the RMSE.") +
  theme_bw()


# Here is the best value of lambda
lambda <- lambdas[which.min(rmses)]
lambda


# Now, we use the optimal value of lambda on the linear model
#
# Mean
mu <- mean(train$rating)

# Movie effects (bi)
b_i <- train %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n()+lambda))

# User effects (bu)
b_u <- train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Predict mu + bi + bu
y_hat_reg <- test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Compute the RMSE and update the scores table
scores <- bind_rows(scores, 
                    tibble(Method = "Regularized bi + bu", 
                           RMSE = RMSE(test$rating, y_hat_reg)))

# Display the RMSE improvement
scores %>% knitr::kable()

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.3 Regularization
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# 3.4 Ending Results in the Validation Set
#
################################################################################################
#
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.4.1. Linear Model With Regularization
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Previously we were able to verify that our Linear model with Regularization reached the
# goal of RMSE. Now we will proceed to perform the final validation on the validation set.
# Here is the code.
#
# Mean
mu_end_edx <- mean(edx$rating)

# Movie effects (bi)
bi_end_edx <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu_end_edx)/(n()+lambda))

# User effects (bu)
bu_end_edx <- edx %>% 
  left_join(bi_end_edx, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu_end_edx)/(n()+lambda))

# Predict mu + bi + bu
y_hat_end_edx <- validation %>% 
  left_join(bi_end_edx, by = "movieId") %>%
  left_join(bu_end_edx, by = "userId") %>%
  mutate(pred = mu_end_edx + b_i + b_u) %>%
  pull(pred)

# Compute the RMSE and update the scores table
scores <- bind_rows(scores, 
                    tibble(Method = "Ending Regularization on edx and validation", 
                           RMSE = RMSE(validation$rating, y_hat_end_edx)))

# Display the RMSE improvement
scores %>% knitr::kable()


# Check if the Ending Regularization on edx and validation is minor than RMSE GOAL
scores[6,]

scores[6,2] < RMSE_GOAL

# As we could see, we achieved our goal.


# Top 10 best movies
validation %>%
  left_join(bi_end_edx, by = "movieId") %>%
  left_join(bu_end_edx, by = "userId") %>%
  mutate(pred = mu_end_edx + b_i + b_u) %>%
  arrange(-pred) %>%
  group_by(title) %>%
  select(title) %>%
  top_n(10)

# Top 10 worst movies
validation %>%
  left_join(bi_end_edx, by = "movieId") %>%
  left_join(bu_end_edx, by = "userId") %>%
  mutate(pred = mu_end_edx + b_i + b_u) %>%
  arrange(pred) %>%
  group_by(title) %>%
  select(title) %>%
  top_n(10)

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.4.1. Linear Model With Regularization
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3.4 Ending Results in the Validation Set
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
