library(tidyverse)
library(ggplot2)
library(readr)


# Initial data read and variable work:
poke_data <- read_csv("pokemon.csv") %>% tibble::as_tibble()


poke_data <- poke_data %>%
  mutate(capture_rate = as.integer(capture_rate),
         generation = as.factor(generation),
         is_legendary = ifelse(is_legendary == 1, "Yes", "No"),
         pokedex_website = paste0("https://www.pokemon.com/us/pokedex/", 
                                  pokedex_number),
         type2 = ifelse(is.na(type2), "none", type2),
         experience_growth = as.numeric(experience_growth)) %>% 
  select("name", everything())

# This code forms a vector of numerical variable names.
numeric_checker <- function(n){
  return(is.numeric(poke_data[[n]]))
}
poke_variables <- names(poke_data)
# The label vars are text vars with too many levels to be suitable to visualize
poke_label_vars <- c("abilities", "name", "classfication", "japanese_name", "pokedex_website")
poke_numerical <- unlist(lapply(as.list(poke_variables), FUN = numeric_checker))
poke_numerical_vars <- poke_variables[poke_numerical]
poke_discrete_vars <- setdiff(poke_variables, c(poke_numerical_vars, poke_label_vars))