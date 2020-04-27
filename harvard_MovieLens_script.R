#########################################
# STEP 1: Create edx set, validation set
#########################################
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") #extra package
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org") #extra package
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org") #extra package
if(!require(rstudioapi)) install.packages("rstudioapi", repos = "http://cran.us.r-project.org") #extra package

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)




#########################################
# STEP 2: Inspect, Transfrom, Analyse
#########################################
theme_set(theme_pubr()) #common theme for all plots

# --- Inspection
rows_edx <- nrow(edx) # 9000055 rows
cols_edx <- ncol(edx) # 6 columns
numb_movies_edx <- uniqueN(edx$movieId) #10,677 unique movies
numb_users_edx <-uniqueN(edx$userId) # 69,878 unique users
head_edx <- head(setDT(edx)) # inspect top few rows of data
summ_stats_edx <- summary(edx) # descriptive statistics - takes a few seconds to run
edx <- setDT(edx) #using data table to help with transformations 
med_users_movie <- median(edx[,uniqueN(userId),movieId]$V1) #medium number of reviews for a movies
med_movies_user <- median(edx[,uniqueN(movieId),userId]$V1) #median number of movies for a reviewer
ratings_prop <- edx[,.N,rating][order(-N)] %>% 
                    mutate(Proportion=paste0(round(100*N/sum(N),0),"%")) %>% 
                    rename(Rating=rating,Count=N) #Most popular rating was 4

# --- Transformations
# Movie title & year made
edx <- suppressWarnings(edx %>% separate(title, #use regex to separate title into name & year columns
                          c("title_name", "year_made"), 
                          "\\s*\\((?=\\d+\\)$)|\\)$", 
                          remove = F))
edx$year_made = as.numeric(as.character(edx$year_made)) #change years to numerics
# Rating timestamps
edx <- edx[, ts_date_time:=as_datetime(timestamp)] #readable date & time
edx <- edx[, ts_date:=as_date(ts_date_time)] # date of review
edx <- edx[, ts_hour:=format(ymd_hms(ts_date_time), "%H")] # hour of review
edx$ts_hour = as.numeric(as.character(edx$ts_hour)) #change hours to numerics
edx <- edx[, ts_weekday:=wday(ts_date)] #day of week of review (1 = Monday, 2 = Tuesday, etc.)
edx <- edx[, ts_day:=day(ts_date_time)] #day of review
edx <- edx[, ts_month:=month(ts_date_time)] #month of review
edx <- edx[, ts_year:=year(ts_date_time)] #year of review
edx <- edx[, diff_years:=ts_year-year_made] #time between year made and year of review
edx <- edx[diff_years>=0, ] #cannot have reviews before release
# Separation of genres
all_genres <- strsplit(edx$genres, split = "\\|") #separate each genere per row
genres_unique <- unique(unlist(all_genres, use.names = FALSE)) #collect unique generes
genre_identifiers <- t(sapply(all_genres, function(e) genres_unique %in% e)) #make indiator columns for each unique genre
mode(genre_identifiers) <- "integer" #set data type to integer
colnames(genre_identifiers) <- genres_unique #label each genere column
edx <- cbind(edx, genre_identifiers) #join new genre columns to edx dataset
rm(genre_identifiers); rm(all_genres) #remove unwanted data
head_edx_clean <- head(edx)

cols_not_needed <- c("timestamp","title", "title_name", "ts_date_time")
edx <- setDT(edx %>% select(-all_of(cols_not_needed))) #remove uncecessary columns 
edx <- setDT(edx %>% distinct()) #remove duplicates

# --- Analysis of data 
# median rating over time
mean_ratings_OT <- edx[,.(mean(rating), .N),ts_year] %>% 
                    rename(mean_Rating=V1, Volume=N, Year_Rating=ts_year) %>%
                    filter(Year_Rating!="1995") %>% #due to low volumes
                    ggplot(aes(Year_Rating,mean_Rating)) + 
                            geom_line() + 
                            labs(title="Mean Rating per Rating Year (excl 1995)", y="Rating", x="Year of Rating")
mean_ratings_release_OT <- edx[,.(mean(rating), .N),year_made] %>% 
                    rename(mean_Rating=V1, Volume=N, Relese_year=year_made) %>%
                    ggplot(aes(Relese_year,mean_Rating)) + 
                            geom_line() + 
                            labs(title="Mean Rating per Release Year", y="Rating", x="Year of Release")
# volume of users over time
users_volumes_OT <- edx[,uniqueN(userId),year_made] %>% 
                      rename(Volume=V1, Year=year_made) %>%
                      ggplot(aes(x=Year,y=Volume)) + 
                              geom_line() + 
                              labs(title="Number of Ratings per Release Year",  x="Year of Release") 
# volume of movies over time
movie_volumes_OT <- edx[,uniqueN(movieId),year_made] %>% 
                        rename(Volume=V1, Year_Released=year_made) %>%
                        ggplot(aes(x=Year_Released,y=Volume)) + 
                                geom_line() + 
                                labs(title="Number of Movies made per Year", x="Year of Release") 
# volume of movies over time
diff_years_volumes_OT <- edx[,uniqueN(movieId),diff_years][order(diff_years)] %>% 
                                rename(Volume=V1, Year=diff_years) %>%
                                ggplot(aes(x=Year,y=Volume)) + 
                                geom_line() + 
                                labs(title="Number of Movies made by Year Difference", x="Years between release and review")
diff_years_ratings_OT <- edx[,mean(rating),diff_years][order(diff_years)] %>% 
                                rename(Mean_Rating=V1, Year=diff_years) %>%
                                ggplot(aes(x=Year,y=Mean_Rating)) + 
                                geom_line() + 
                                labs(title="Mean Rating by Year Difference", x="Years between release and review")
# funtion which  aggregates movie counts, users, average ratings & time, per genre
genre_counts <- function(genre_input){ 
  genre_string <- genre_input
  movie_volumes <- edx[get(genre_input)==1, uniqueN(movieId), year_made][order(year_made)] %>% 
                        rename(Volume_Movies=V1, Year=year_made) %>% 
                        mutate(Genre=genre_string)
  user_volumes <- edx[get(genre_input)==1, uniqueN(userId), ts_year][order(ts_year)] %>% 
                        rename(Volume_Users=V1, Year=ts_year) %>% 
                        mutate(Genre=genre_string)
  mean_rating <- edx[get(genre_input)==1, round(mean(rating),2), year_made][order(year_made)] %>% 
                        rename(Mean_Rating=V1, Year=year_made) %>% 
                        mutate(Genre=genre_string)
  hours_rating <- edx[get(genre_input)==1, round(mean(rating),2), ts_hour][order(ts_hour)] %>% 
                        rename(Mean_Rating=V1, Hour=ts_hour) %>% 
                        mutate(Genre=genre_string)
  month_rating <- edx[get(genre_input)==1, round(mean(rating),2), ts_month][order(ts_month)] %>% 
                        rename(Mean_Rating=V1, Month=ts_month) %>% 
                        mutate(Genre=genre_string)
  weekday_rating <- edx[get(genre_input)==1, round(mean(rating),2), ts_weekday][order(ts_weekday)] %>% 
                        rename(Mean_Rating=V1, Weekday=ts_weekday) %>% 
                        mutate(Genre=genre_string)
  list(Movies=movie_volumes,
      Users=user_volumes, 
      Ratings=mean_rating,
      Hours=hours_rating,
      Months=month_rating,
      Weekdays=weekday_rating)
}
#loop funtion for all genres and join together
movie_genres = list()
users_genres = list()
ratings_genres = list()
hours_genres = list()
months_genres = list()
weekday_genres = list()
for (i in 1:length(genres_unique)){
  chosen_genre <- genres_unique[i]
  movie_genres[[i]] = genre_counts(chosen_genre)$Movies
  users_genres[[i]] = genre_counts(chosen_genre)$Users
  ratings_genres[[i]] = genre_counts(chosen_genre)$Ratings
  hours_genres[[i]] = genre_counts(chosen_genre)$Hours
  months_genres[[i]] = genre_counts(chosen_genre)$Months
  weekday_genres[[i]] = genre_counts(chosen_genre)$Weekdays
}
movie_genres_all = setDT(do.call(rbind, movie_genres))
users_genres_all = setDT(do.call(rbind, users_genres) %>% filter(Year!="1995"))  #due to low volumes
ratings_genres_all = setDT(do.call(rbind, ratings_genres))
hours_genres_all = setDT(do.call(rbind, hours_genres))
months_genres_all = setDT(do.call(rbind, months_genres))
weekday_genres_all = setDT(do.call(rbind, weekday_genres))
# Genre summary tables of volume, users & median rating
movie_genres_all_table <- setDT(movie_genres_all[,sum(Volume_Movies),Genre][order(-V1)] %>% 
                                    mutate(Movies_Prop=paste0(round(100*V1/sum(V1),0),"%")) %>% 
                                    rename(Movies_Count=V1))
users_genres_all_table <- setDT(users_genres_all[,sum(Volume_Users),Genre][order(-V1)] %>% 
                                    mutate(Users_Prop=paste0(round(100*V1/sum(V1),0),"%")) %>% 
                                    rename(Users_Count=V1))
ratings_genres_all_table <- setDT(ratings_genres_all[,median(Mean_Rating),Genre][order(-V1)] %>% 
                                    mutate(Ratings_Prop=paste0(round(100*V1/sum(V1),0),"%")) %>% 
                                    rename(Ratings_Median=V1))
Genre_all_table <- merge(merge(movie_genres_all_table,
                         users_genres_all_table,
                         by="Genre"),
                         ratings_genres_all_table,
                         by="Genre")[order(-Movies_Count)]
top_10_genres <- unique(Genre_all_table[1:10]$Genre) 
# top 10 genres make up 86% of volume - use these in charts below to produce cleaner charts
# volume of movies per genre over time
movies_genre_OT <- movie_genres_all %>% 
                    filter(Genre %in% top_10_genres) %>%
                    ggplot(aes(x=Year, 
                               y=Volume_Movies, 
                               group=Genre,
                               color=Genre, fill=Genre)) + 
                        geom_bar(position="fill", stat="identity")+
                        scale_y_continuous(labels = percent) +
                        labs(title="Prop of Movies per Top 10 Genre per Year", y=element_blank(), x="Year of Release")
                    
# volume of users per genre over time
users_genres_OT <- users_genres_all %>% 
                    filter(Genre %in% top_10_genres) %>% 
                    ggplot(aes(x=Year, 
                               y=Volume_Users, 
                               group=Genre,
                               color=Genre, fill=Genre)) + 
                        geom_bar(position="fill", stat="identity")+
                        scale_y_continuous(labels = percent) +
                        labs(title="Prop of Users per Top 10 Genre per Year", y=element_blank(), x="Year of Rating")
# mean rating per genre over time
ratings_genres_OT <- ratings_genres_all %>% 
                      filter(Genre %in% top_10_genres) %>% 
                      ggplot(aes(x=Year, 
                                 y=Mean_Rating, 
                                 group=Genre,
                                 color=Genre)) + 
                          geom_line() + 
                          labs(title="Median Rating per Top 10 Genre per Year", y="Median Rating", x="Year of Release")

# Looking into time columns
hours_genres_OT <- setDT(hours_genres_all) %>% 
                    filter(Genre %in% top_10_genres) %>% 
                    ggplot(aes(x=Hour, 
                               y=Mean_Rating, 
                               group=Genre,
                               color=Genre)) +
                      geom_line()+ 
                      labs(title="Mean Rating per Top 10 Genre per Hour", y="Mean Rating", x="Hour of Review")
months_genres_OT <- setDT(months_genres_all) %>% 
                    filter(Genre %in% top_10_genres) %>% 
                    ggplot(aes(x=Month, 
                               y=Mean_Rating, 
                               group=Genre,
                               color=Genre)) +
                    geom_line()+ 
                    labs(title="Mean Rating per Top 10 Genre per Month", y="Mean Rating", x="Month of Review")
weekday_genres_OT <- setDT(weekday_genres_all) %>% 
                    filter(Genre %in% top_10_genres) %>% 
                    ggplot(aes(x=Weekday, 
                               y=Mean_Rating, 
                               group=Genre,
                               color=Genre)) +
                    geom_line()+ 
                    labs(title="Mean Rating per Top 10 Genre per Weekday", y="Mean Rating", x="Weekday of Review")

#remove objects no longer needed
rm(movie_genres_all, 
   users_genres_all, 
   ratings_genres_all, 
   hours_genres_all,
   months_genres_all,
   weekday_genres_all,
   movie_genres,
   users_genres,
   ratings_genres,
   hours_genres,
   months_genres,
   weekday_genres,
   movie_genres_all_table,
   users_genres_all_table,
   ratings_genres_all_table,
   i,
   cols_not_needed,
   genres_unique,
   top_10_genres,
   chosen_genre)


edx2 <- setDT(edx %>% distinct() %>% select(-unique(Genre_all_table[1:10]$Genre)) %>% separate_rows(genres, sep ="\\|")) #remove duplicates
rm(edx)
cols_edx_clean <- ncol(edx2)
rows_edx_clean <- ncol(edx2)
str(edx2) #check that all remaining columns are numeric/integer



#########################################
# STEP 3: Split edx data into Train & Test
# 80/20 split will be used for model building
#########################################
set.seed(1)
edx_test_index <- createDataPartition(y = edx2$rating, times = 1, p = 0.2, list = FALSE)
edx_train <- edx2[-edx_test_index,] #create training set
edx_test <- edx2[edx_test_index,] #create test set
rm(edx2, edx_test_index) #no longer needed 
edx_test <- edx_test %>% 
  semi_join(edx_train, by = "movieId") %>% 
  semi_join(edx_train, by = "userId") #matches movies & users in training & test sets
nrow_edx_train <- nrow(edx_train)
nrow_edx_test <- nrow(edx_test)
colnames(edx_train) <- make.names(colnames(edx_train)) #clean column names for caret package
colnames(edx_test) <- make.names(colnames(edx_test)) #clean column names for caret package


#########################################
# STEP 4: Models
#########################################

#function to calculate RMSE's
RMSE <- function(true, predicted){ 
  sqrt(mean((true - predicted)^2))
}

# Simple average of ratings
y_hat1 <- mean(edx_train$rating) 
first_rmse<- RMSE(y_hat1,edx_test$rating) 

# GLM with only Movie effect 
movie_means <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_movie = mean(rating - y_hat1)) #mean rating of each movie
y_hat2 <- y_hat1 + edx_test %>% 
  left_join(movie_means, by='movieId') %>% 
  pull(b_movie) 
movie_rmse <- RMSE(y_hat2, edx_test$rating) 
rm(y_hat2)

# GLM with Movie & User effects
user_means <- edx_train %>% 
  left_join(movie_means, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_user = mean(rating - (y_hat1 + b_movie))) #mean rating of each user, minus movie & overall means
y_hat3 <- edx_test %>% 
  left_join(movie_means, by='movieId') %>% 
  left_join(user_means, by='userId') %>% 
  mutate(pred = y_hat1 + b_movie + b_user) %>% 
  pull(pred)
movie_user_rmse <- RMSE(edx_test$rating, y_hat3) 
rm(y_hat3)

# GLM with Movie, User & Genre effects
genre_means <- edx_train %>% 
  left_join(movie_means, by='movieId') %>% 
  left_join(user_means, by='userId') %>%
  group_by(genres) %>%
  summarize(b_genre = mean(rating - (y_hat1 + b_movie + b_user))) #mean rating per genre, minus user, movie & overall means
y_hat4 <- edx_test %>% 
  left_join(movie_means, by='movieId') %>% 
  left_join(user_means, by='userId') %>% 
  left_join(genre_means, by='genres') %>% 
  mutate(pred = y_hat1 + b_movie + b_user + b_genre + b_genre) %>% 
  pull(pred)
movie_user_genre_rmse <- RMSE(edx_test$rating, y_hat4) 
rm(y_hat4)

# Regularisation & Penalised LS - to control variability - takes time to run
lambdas <- seq(0, 10, 0.5)
rmses <- sapply(lambdas, function(l){
  mu_hat <- mean(edx_train$rating)
  b_movie <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - mu_hat)/(n()+l))
  b_user <- edx_train %>%
    left_join(b_movie, by="movieId") %>% 
    group_by(userId) %>%
    summarize(b_user = sum(rating - b_movie - mu_hat)/(n()+l))
  b_genre <- edx_train %>%
    left_join(b_movie, by="movieId") %>% 
    left_join(b_user, by="userId") %>%
    group_by(genres) %>%
    summarize(b_genre = sum(rating - b_movie - b_user - mu_hat)/(n()+l))
  y_hat <- edx_test %>%
    left_join(b_movie, by = "movieId") %>% 
    left_join(b_user, by = "userId") %>% 
    left_join(b_genre, by = "genres") %>% 
    mutate(pred = mu_hat + b_movie + b_user + b_genre) %>% 
    pull(pred)
  return(RMSE(y_hat, edx_test$rating)) })
lambda_table <- as.data.frame(cbind(lambdas, rmses))
lambda_plot <- lambda_table %>%
                ggplot(aes(lambdas, rmses)) +
                        geom_line()+geom_point()+
                        labs(title="Test set RMSE vs Lambda")
lambda_min <- lambdas[which.min(rmses)] # pick lambda with lowest RMSE
rm(lambda_table)

# Penalised GLM - Movie effect
movie_reg_means <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_movie_r = sum(rating - y_hat1)/(n()+lambda_min))
user_reg_means <- edx_train %>%
  left_join(movie_reg_means, by="movieId") %>% 
  group_by(userId) %>%
  summarize(b_user_r = sum(rating - b_movie_r - y_hat1)/(n()+lambda_min))
genre_reg_means <- edx_train %>%
  left_join(movie_reg_means, by="movieId") %>% 
  left_join(user_reg_means, by = "userId") %>% 
  group_by(genres) %>%
  summarize(b_genre_r = sum(rating - b_movie_r - b_user_r - y_hat1)/(n()+lambda_min))
y_hat5 <- edx_test %>%
  left_join(movie_reg_means, by = "movieId") %>% 
  left_join(user_reg_means, by = "userId") %>% 
  left_join(genre_reg_means, by = "genres") %>% 
  mutate(pred = y_hat1 + b_movie_r + b_user_r + b_genre_r) %>% 
  pull(pred)
movie_user_genre_rmse_reg <- RMSE(edx_test$rating, y_hat5) 
rm(y_hat5, movie_means, user_means, genre_means)

#collect RMSE's
test_rmse_table_labels <- c("Mean", "Mean + Movie", "Mean + Movie + User", "Mean + Movie + User + Genre", "Penalised Mean + Movie + User + Genre")
test_rmse_table_values <- round(c(first_rmse, movie_rmse, movie_user_rmse, movie_user_genre_rmse, movie_user_genre_rmse_reg),5)
test_rmse_table <- as.data.table(cbind(test_rmse_table_labels,test_rmse_table_values))
colnames(test_rmse_table) <- c("Model Type", "Test Data RMSE (to 5 d.p.)")
rm(test_rmse_table_labels, test_rmse_table_values)


#########################################
# STEP 5: Validation Set & RMSE
#########################################
#clean valudation set
validation2 <- validation %>% 
  separate_rows(genres, sep ="\\|") %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId") #matches movies & users in training & validation sets
rm(validation)

#predict ratings from final model
y_hat_final <- validation2 %>%
  left_join(movie_reg_means, by = "movieId") %>% 
  left_join(user_reg_means, by = "userId") %>%  
  left_join(genre_reg_means, by = "genres") %>% 
  mutate(pred = y_hat1 + b_movie_r + b_user_r + b_genre_r) %>% 
  pull(pred)

#final RMSE 
Final_RMSE <- RMSE(validation2$rating, y_hat_final) #0.865678
rm(y_hat_final, movie_reg_means, user_reg_means, genre_reg_means, validation2, edx_test, edx_train)

#saves all objects to be used in markdown file - uses current location of file
save.image(paste0(getSourceEditorContext()$path,"data"))

