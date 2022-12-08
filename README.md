# Movie Recommender App

We created a shiny app using R based on Users input. Here is the link - https://ii3qh6-arnab-kar0sarkar.shinyapps.io/movie-recommender-app

## Details

This movie recommender app recommends top 10 movies based on different user inputs. There are two ways we are recommending these movies.

    1. Recommendations by Genre - Based on a selected Genre by the users, the system will show top 10 movies for that genre.
    2. Recommendations by reviews  - A User based collaborative recommendation system. It asks users to rate (1-5) as many movies as possible and then shows top 10 recommendations. 

We used the movielens dataset - https://grouplens.org/datasets/movielens/

### Recommendations by Genre (System I)

To get recommendations for a genre, we take all the movies and their ratings and then based on their popularity and average ratings, it will show the recommendations. 

We considered 2 factors while recommending for this system - 
    1. avg_ratings - An average rating based on the movie Ids and number of ratings. 
    2. num_ratings - The number of reviews provided for a movie. 

Below is an excerpt of the factors 

    ```r
    ratings %>% 
    group_by(MovieID) %>% 
    summarize(
      num_ratings = n(), 
      avg_ratings = round(mean(Rating), dig=4)
    )
    ```

Once we get these two factors we then used `denseRank` method to order the movies and rank them. We then used both the ranks and summed them up to generate the final ranking of the movie. 

We used both popularity and ranking to generate the final recommendations because using only one method would not provide an accurate recommendation.



### Recommendations by Reviews (System II)

User-based CF is a model based recommendation which searches similar users based movies based on what the users have already liked. We used ratings matrix to get the data. 

We used `recommenderlab` r package to generate the recommendations. 

However, in UBCF there are certain use cases when it returns NA or less than 10 recommendations, we have used the most popular movies to show for those uses cases. 

For example, if an user gives 2 movies with 5 ratings, the recommendations often come as NA for all 10 movies. We replaced those NA ones with the most popular movies in the entire dataset.

## Project Team Members

 Name          | NetID    | Email                 |
:----------------|:---------|:----------------------|
 Brittany West      | bnwest2  | bnwest2@illinois.edu  |
 Arnab KarSarkar | arnabk2  | arnabk2@illinois.edu  |
 Shivani Senguttuvan  | shivani2 | shivani2@illinois.edu |