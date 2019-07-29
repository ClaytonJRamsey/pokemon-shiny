library(tidyverse)
library(ggplot2)
library(readr)
library(plotly)
library(shiny)
library(class)
library(randomForest)

# Initial data read and variable work:
poke_data <- read_csv("pokemon.csv") %>% tibble::as_tibble()
# This has to do with the info tab.
x <- 1
# This is to prevent the user predicting with no rf model
rf_model_generated <- "no"

poke_data <- poke_data %>%
  mutate(capture_rate = as.integer(capture_rate),
         generation = as.factor(generation),
         legendary_factor = ifelse(is_legendary == 1, "Yes", "No"),
         percentage_male = ifelse(is.na(percentage_male), 
                                  "genderless", 
                                  percentage_male),
         pokedex_website = paste0("https://www.pokemon.com/us/pokedex/", 
                                  pokedex_number),
         type2 = ifelse(is.na(type2), "none", type2),
         experience_growth = as.numeric(experience_growth)) %>% 
  select("name", everything()) %>% select(-"pokedex_number")
# Because pokedex number is the same as row number in the file.

# This code forms vector of the names of different variable types.
numeric_checker <- function(n){
  return(is.numeric(poke_data[[n]]))
}

# Standardization functions.
# This is a general one.
standardize_data <- function(x){
  return((x-mean(x))/sd(x))
}
# This one standardizes a named value or vector based on the parameters of the same column
# in the numeric data. Needed for prediction.
standardize_variable <- function(x, n){
  return((x-mean(poke_data_numeric[[n]]))/sd(poke_data_numeric[[n]]))
}

# Defining some vectors of variable names:

# all the names
poke_variables <- names(poke_data)
# text vars with too many levels to be suitable to visualize
poke_label_vars <- c("abilities", "name", "classfication", "japanese_name", "pokedex_website")
# The remaining numerical variables
poke_numerical <- unlist(lapply(as.list(poke_variables), FUN = numeric_checker))
poke_numerical_vars <- poke_variables[poke_numerical]
# Discrete variables suitable for modeling on
poke_discrete_vars <- setdiff(poke_variables, c(poke_numerical_vars, poke_label_vars))

# A data frame of the numeric variables - complete only
poke_data_numeric <- poke_data %>% filter(complete.cases(.)) %>% select(!!poke_numerical_vars)
# standardized numeric variables
poke_data_numeric_standard <- as.data.frame(lapply(poke_data_numeric, standardize_data))
# Names of complete cases
poke_data_names <- poke_data %>% filter(complete.cases(.)) %>% select(name)

# Testing Data for k-means and knn
poke_testing <- poke_data_numeric_standard %>% sample_n(nrow(poke_data_numeric_standard)/5)
# training Data
poke_training <- setdiff(poke_data_numeric_standard, poke_testing)

# A data frame of the discrete variables
poke_data_discrete <- poke_data %>% filter(complete.cases(.)) %>% select(!!poke_discrete_vars) 

# A data frame of all the modeling vars with factor vars for categorical.
# Dropping is_legendary because there is a factor variable version of it.
poke_data_model <- cbind(poke_data_numeric, poke_data_discrete) %>%
  select(-is_legendary) %>% mutate(percentage_male = as.factor(percentage_male),
                                   type1 = as.factor(type1),
                                   type2 = as.factor(type2),
                                   legendary_factor = as.factor(legendary_factor))
# Testing and training Data for the full vars data
poke_testing_full <- poke_data_model %>% sample_n(nrow(poke_data_model)/5)
poke_training_full <- setdiff(poke_data_model, poke_testing_full)

# a function to use knn to predict legendary status
# Use standardized values
knn_legendary <- function(var1, var2, predict_set, k){
  knn_fit <- knn(train = select(poke_training, var1, var2),
                 test = select(predict_set, var1, var2),
                 cl = poke_training$is_legendary,
                 k = k)
  return(knn_fit)
}