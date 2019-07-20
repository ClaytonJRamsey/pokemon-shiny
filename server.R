library(shiny)
library(tidyverse)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # Initial data read and variable work:
  poke_data <- read_csv("pokemon.csv") %>% tibble::as_tibble()
  x <- 1
  
  poke_data <- poke_data %>% select("name", everything()) %>%
    mutate(capture_rate = as.integer(capture_rate),
           generation = as.factor(generation),
           pokedex_website = paste0("https://www.pokemon.com/us/pokedex/", 
                                    pokedex_number),
           type2 = ifelse(is.na(type2), "none", type2))

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
      
      # the data table on the second tab:
      attack_values <- input$var_attack
      defense_values <- input$var_defense
      
      filter_table <- poke_data %>% 
        filter(as.character(generation) %in% input$var_generation,
               attack <= attack_values[2] & 
                 attack >= attack_values[1],
               defense <= defense_values[2] & 
                 defense >= defense_values[1]
               ) %>%
        select(c("name", input$var_boxes))
      
      output$dynamic_table <- DT::renderDT(filter_table)
      
      # file download for table
      file_name <- paste0(input$filename, ".csv")
      
      output$csv_download <- downloadHandler(
        filename = file_name,
        content = function(file){
          write.csv(filter_table, file)
          }
      )
    })
################ End of observe function ###################33
    
  # The clear button in tab 2
  observeEvent(input$var_clear,
    updateCheckboxGroupInput(session, "var_boxes",
                             selected = character(0))
     )
  

  })
  

