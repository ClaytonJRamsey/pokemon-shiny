library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # Initial data read and variable work:
  poke_data <- read_csv("pokemon.csv") %>% tibble::as_tibble()
  
  poke_data <- poke_data %>% 
    mutate(capture_rate = as.integer(capture_rate),
           generation = as.factor(generation),
           pokedex_website = paste0("https://www.pokemon.com/us/pokedex/", 
                                    pokedex_number))
  
  observe({
    # For the sidebar on Tab #1.
      if(input$pokemon_basic == "Number"){
        x <- input$entry_number
      }else{
        if(input$entry_number_random){
          x <- sample(1:nrow(poke_data), 1)
        }
      }
      
      output$pokemon_info <- 
        renderUI(tagList(
          strong(paste0(poke_data$name[x], 
                    " is a ", poke_data$classfication[x], 
                    " originally introduced in Generation ", 
                    poke_data$generation[x], ".")),
          br(),
          tags$a(href = poke_data$pokedex_website[x], 
                 "Click here for more information.")
          )
        )
      })
  })
  

