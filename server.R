library(recommenderlab)
library(Matrix)
library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)

# load the Genre matrix
load("data/genre_matrix.rda")
load("data/recommender.UBCF.rda")


TOP_N = 10
## Read files from the data folder. 
## There are 3 files we need to read - 
## 1 Movies, 2. Ratings 3. Movie Images URL
read <- function(fileName, separators) {
  data <- readLines(con <- file(fileName))
  close(con)
  records <- sapply(data, strsplit, split=separators)
  dataFrame <- data.frame(t(sapply(records,c)))
  rownames(dataFrame) <- 1: nrow(dataFrame)
  return(as.data.frame(dataFrame,stringsAsFactors = FALSE))
}

# Read movies 
movies = read("data/movies.dat", "::")
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")


#Read ratings
ratings = read("data/ratings.dat", "::")
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$MovieID = as.integer(ratings$MovieID)
ratings$Rating = as.integer(ratings$Rating)

#Create movie Image url 
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))



# System 1 calculation

## Get all unique Genres 
genre_all = colnames(genre_matrix)


#Get all average Movie ratings based on their Movie Id
get_all_movie_rating = function(){
  ratings_per_movie = ratings %>% 
    group_by(MovieID) %>% 
    summarize(
      num_ratings = n(), 
      avg_ratings = round(mean(Rating), dig=4)
    )
  return(ratings_per_movie)
}

# Get all movie data by genre 
# Using a left join between get_all_movie_rating and movies from the genre
# It is a dense_rank list.
get_all_movie_data = function(genre_name){
  matching_movie_from_genre_idx = genre_matrix[,genre_name] == 1
  movie_list_for_genre = movies[matching_movie_from_genre_idx,]
  ratings_per_movie = get_all_movie_rating()
  all_movies_for_genre = movie_list_for_genre %>%
    left_join(ratings_per_movie, by = 'MovieID') %>%
    replace(is.na(.), 0) %>% 
    mutate(avg_ratings_rank = dense_rank(desc(avg_ratings))) %>% 
    mutate(num_ratings_rank = dense_rank(desc(num_ratings))) %>% 
    arrange(desc(avg_ratings), desc(num_ratings))
  return(all_movies_for_genre)
}

# Get All Movies by Genre
get_top_movies_by_genre = function(genre_name){
  all_movies_for_genre = get_all_movie_data(genre_name)
  movie_output = all_movies_for_genre %>%
    mutate(final_rank = as.double(avg_ratings_rank + num_ratings_rank)) %>%
    top_n(TOP_N, -((final_rank))) %>%
    select('MovieID', 'Title', 'final_rank', 'avg_ratings_rank', 'num_ratings_rank', 'Genres') %>%
    arrange(desc (-(final_rank)))
}





# System 2

# On Load funciton call to get User ratings 

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# System 2  - Predict top 10 movies based on UBCF
get_usr_recom = function(user_ratings){
  movieId_list = unique(ratings$MovieID)
  j = paste0('m', movieId_list)
  user_data = matrix(data=NA, 1, length(j))
  colnames(user_data) = j
  top_ten_mov_ids = c()
  if(nrow(user_ratings) == 0){
    top_ten_mov_ids = get_popular_movies(top_ten_mov_ids, c())
    
  }else{
    user_data[1, user_ratings$MovieID] = user_ratings$Rating
    Rmat_user_data = as(user_data, "realRatingMatrix")
    p.UBCF <- predict(recommender.UBCF, Rmat_user_data, type="ratings")
    p.UBCF <- as.numeric(as(p.UBCF, "matrix"))
    top_ten_mov_ids = head(order(as.vector(p.UBCF), decreasing = TRUE, na.last = NA), 10)
    if(length(top_ten_mov_ids)< 10){
      top_ten_mov_ids = get_popular_movies(top_ten_mov_ids, user_ratings$MovieID)
    }
  }
  return(top_ten_mov_ids)
}

# System 2 - get all time popular movies if there are NA entries found.
# Get the most number of rated movies first
# Remove Movie Ids if any of those are already watched by the user.
get_popular_movies = function(top_ten_mov_ids, movieIds){
  all_movies_by_rating = get_all_movie_rating()
  all_movies_by_rating = all_movies_by_rating[order(all_movies_by_rating$num_ratings, decreasing = TRUE, na.last = NA), ]
  all_movies_by_rating =  all_movies_by_rating[ ! all_movies_by_rating$MovieID %in% movieIds, ]
  na_entries = 10 - length(top_ten_mov_ids)
  all_movies_by_rating = head(all_movies_by_rating, na_entries)
  index = na_entries - 9
  for(i in (index : 10)){
    top_ten_mov_ids[i] = all_movies_by_rating$MovieID[i]
  }
  return(top_ten_mov_ids)
}


shinyServer(function(input, output, session) {

  df_genre <- eventReactive(input$btnGenre, {
    withBusyIndicatorServer("btnGenre", {
      value_list = reactiveValuesToList(input)
      selected_genre = value_list$genrePickList
      top_movies = get_top_movies_by_genre(selected_genre)
      user_results = (1:10)/10
      genre_results <- data.table(Rank = 1:10, 
                                  MovieID = top_movies$MovieID, 
                                  Title = top_movies$Title, 
                                  Predicted_rating =  user_results)
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df_ratings <- eventReactive(input$btnRating, {
    withBusyIndicatorServer("btnRating", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      
      user_results = (1:10)/10
      user_predicted_ids = get_usr_recom(user_ratings)
      recom_results <- data.table(Rank = 1:10, 
                                    MovieID = movies$MovieID[user_predicted_ids], 
                                    Title = movies$Title[user_predicted_ids], 
                                    Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  output$results_by_genre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result = df_genre()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        movie_idx = (i - 1) * num_movies + j
        movie_id = recom_result$MovieID[movie_idx]
        movie_title = recom_result$Title[movie_idx]
        rec_movie = movies[movies$MovieID == movie_id,]
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", movie_idx),
            div(style = "text-align:center", 
                a(img(src = rec_movie$image_url, height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movie_title)
            )
            
        )        
      }))) # columns
    }) # rows
  })
  
  
  
  output$ratings_movie <- renderUI({
    num_rows <- 50
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  outputOptions(output, "ratings_movie", suspendWhenHidden = FALSE)  
  
  #Genre picklist
  output$genres_picklist <- renderUI({
    selectInput("genrePickList", "Select a Genre", as.list(genre_all))
  })
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_ratings()
    lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              div(style = "text-align:center", 
                  a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
              ),
              div(style="text-align:center; font-size: 100%", 
                  strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
              )
              
          )         
        }))) # columns
      }) # rows

  }) # renderUI function
  
}) # server function