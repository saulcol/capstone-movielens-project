################################################################################################
#
# CAPSTONE PROJECT: MOVIELENS
# Building a Movie Recommendation system using MovieLens dataset
#
# author: "Saúl Santillán Gutiérrez"
# date: "August 4, 2022"
# 
################################################################################################


################################################################################################
#
# General Description
# 
################################################################################################
#
# 

################################################################################################
#
# General Instructions
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

# IMPORTANT: The `validation` data (the final hold-out test set) should **NOT** be used for training,
# developing, or selecting your algorithm and **it should ONLY be used for evaluating the RMSE of
# your final algorithm**. The final hold-out test set should only be used at the end of your project
# with your final model. **It may not be used to test the RMSE of multiple models during model development**.
# You should split the `edx` data into separate training and test sets to design and test your algorithm.
#
# IMPORTANT: Please be sure not to use the validation set (the final hold-out test set) for training
# or regularization - you should create an additional partition of training and test sets from the
# provided edx dataset to experiment with multiple parameters or use cross-validation.
# Remember your goal is to get a RMSE < 0.86490.



################################################################################################
#
# Preparing the data science project
#
################################################################################################
#
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

library(this.path)
library(tidyverse)
library(caret)
library(data.table)
library(ggthemes)
library(scales)
library(lubridate)


# Assign your RMSE goal to RMSE_GOAL variable. Remember your goal is RMSE < 0.86490
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


#+++++++ Downloading the MovieLens 10M dataset, create the datasets edx and validation +++++++++
#
# IMPORTANT: You can choose if you run this part of the code only once.
#
# NOTE: If you have trouble with the next code, please skip it and go to the section:
# Run in case of emergency
#
# Check if the "ratings.dat" and "movies.dat" files exist in the "ml-10M100K" directory
if(file.exists(paste0(wd, "/ml-10M100K/ratings.dat"))==TRUE & file.exists(paste0(wd, "/ml-10M100K/movies.dat"))==TRUE){
  print("Files ratings.dat and movies.dat exist already")
}else{
  print("Files ratings.dat and movies.dat do NOT exist...downloading and creating the datasets")
  #
  #
  # NOTE: This process could take a couple of minutes. Do not be discouraged.
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
# If you had any trouble with above code, at the moment to downloading or others problems,
# because I saw in the discussion forum some people wrote about it.
# I provided a .rda file; with the objects "edx" and "validation", from my gitlab.
# Download the file from gitlab to "rdas" directory
# Error: cannot open destfile 'D:/test_r/rda/', reason 'Permission denied'
# solution: add the name of the file
# Discomment the next line, if you had a trouble with the code above. This process is more faster!!
#download.file("https://gitlab.com/saulcol/rdas/-/raw/main/cp_movielens.rda", paste0(wd, "/rdas/cp_movielens.rda"))


# Verify if the "cp_movielens.rda" file exists in the "rdas" directory
if(file.exists(paste0(wd, "/rdas/cp_movielens.rda"))==TRUE){
  print("File cp_movielens.rda exists already")
}else{
  print("File cp_movielens.rda does NOT exist...downloading")
  # Download the file from gitlab to "rdas" directory
  # Error: cannot open destfile 'D:/test_r/rda/', reason 'Permission denied'
  # solution: add the name of the file
  download.file("https://gitlab.com/saulcol/rdas/-/raw/main/cp_movielens.rda", paste0(wd, "/rdas/cp_movielens.rda"))
}


# Remove these objects from the Global Environment if they exist
if(exists("edx")) rm("edxx", envir = globalenv())
if(exists("validation")) rm("validation", envir = globalenv())
#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# Preparing the data science project
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################################
#
# Perform Data Analysis and Exploratory Data Analysis (EDA) to "edx" dataset
#
################################################################################################
#
# load objects "edx", "validation" from the file path: rdas/cp_movielens.rda. Take a few seconds
load("./rdas/cp_movielens.rda")


# Knowing its structure and how it is composed
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

# Other way, summary statistics
# pendiente library(Hmisc) describe(df1)


# 


# Are there missing values?
# Count missing values
sum(is.na(edx))
#[1] 0

any(is.na(edx))
#[1] FALSE

# count total missing values in each column of data frame
sapply(edx, function(x) sum(is.na(x)))
colSums(is.na(edx))


# Get the positions of missing values of each column in your data set,
# use the apply() function
#apply(is.na(edx), 2, which)


# As you can see, this dataset does not have missing values.


# How many zeros were given as ratings in the edx dataset?
edx %>%
  filter(rating == 0) %>%
  count()


# How many threes were given as ratings in the edx dataset?
edx %>%
  filter(rating == 3) %>%
  count()


# How many different movies are in the edx dataset?
n_distinct(edx$movieId)


# How many different users are in the edx dataset?
length(unique(edx$userId))


# How many different genres are in the edx dataset?
length(unique(edx$genres))


# Which movie has the greatest number of ratings?
edx %>%
  group_by(movieId, title) %>%
  summarize(count = n()) %>%
  top_n(20) %>%
  arrange(desc(count))


# What are the five most given ratings in order from most to least?
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  top_n(5) %>%
  arrange(desc(count))





# In general, half star ratings are less common than whole star ratings (e.g., there are fewer
# ratings of 3.5 than there are ratings of 3 or 4, etc.).
#
# Graphically it can be obtained with this code:
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


# Numerically you can get it with this code:
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))



#
#++++++++++++++++++++++++++++++++++++++ End of: ++++++++++++++++++++++++++++++++++++++++++++++++
#
# Perform Data Analysis and Exploratory Data Analysis (EDA)
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




