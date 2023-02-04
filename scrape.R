library(tidyverse)
library(rvest)
library(glue)
library(lubridate)

# Movies from  IMDb recent movies page
html <- read_html("https://www.imdb.com/movies-in-theaters/")
# Extract the movie identifier strings
movie_identifiers <- html |> 
 html_elements(".title a") |> 
 html_attr("href") |> 
 word(4, sep = fixed("/"))
movie_titles <- html_data |> 
 html_elements(".title a") |> 
 html_text() 
 
#############

# Movies starting today
date <- today()-1
html_chunks <- 
  glue("https://www.imdb.com/search/title/?title_type=feature&release_date={date},{date}&view=simple",
     date = date) |> 
  read_html() |> html_elements(".lister-item-header a")
movies <- tibble(
  id = html_chunks |> 
    html_attr("href") |> 
    word(3, sep = fixed("/")),
  title = html_chunks |> 
    html_text("href")
)

# Extract the rating distribution data from url
get_rating_distribution <- function(imdb_id)  glue(
 "https://www.imdb.com/title/{id}/ratings",
 id = imdb_id) |> 
 read_html() |> 
 html_elements(css = "td+ td .leftAligned") |> 
 html_text() |> str_remove(",") |> 
 as.numeric()
save(movies,file = "/home/janlo/Documents/projects/movieratingdistributions/movies_released_2023-02-03")


## Read new distirbutions daily
load("/home/janlo/Documents/projects/movieratingdistributions/movies_released_2023-02-03")
distr_list <- map(movies$id, get_rating_distribution, .progress = TRUE)
save(distr_list,file = paste0(
  "/home/janlo/Documents/projects/movieratingdistributions/movies_released_2023-02-03_ratings_", today()))



distr_list



distr <- distr_list |> 
  set_names(movies$id) |> 
  as_tibble() |> 
  mutate(stars = rev(1:10)) |> 
  pivot_longer(starts_with("tt"), names_to = "id") |> 
  arrange(name,stars) |> 
  left_join(movies, by = "name")












#####################



# Read the HTML from the URL
distr_list <- map(movie_identifiers[1:10], get_rating_distribution, .progress = TRUE)
distr <- distr_list |> 
 set_names(movie_identifiers[1:10]) |> 
 as_tibble() |> 
 mutate(stars = rev(1:10)) |> 
 pivot_longer(starts_with("tt")) |> 
 arrange(name,stars) |> 
 left_join(tibble(name = movie_identifiers, title = movie_titles), by = "name")

distr |> ggplot(aes(x=stars, y=value)) + geom_col() + facet_wrap(~title, scales = "free_y")
