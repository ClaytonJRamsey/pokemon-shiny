library(shiny)
library(tidyverse)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # Initial data read and variable work:
  poke_data <- read_csv("pokemon.csv") %>% tibble::as_tibble()
  x <- 1
  
  poke_data <- poke_data %>%
    mutate(capture_rate = as.integer(capture_rate),
           generation = as.factor(generation),
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
  poke_numerical <- unlist(lapply(as.list(poke_variables), FUN = numeric_checker))
  poke_numerical_vars <- poke_variables[poke_numerical]
  poke_discrete_vars <- setdiff(poke_variables, poke_numerical_vars)
  

####################### Observation Zone ##################################33
  observe({
    # For the sidebar on Tab #1.
      if(input$pokemon_basic == "Number"){
        x <- input$entry_number
      }else{
        if(input$entry_number_random){
          x <- sample(1:nrow(poke_data), 1)
        }
      }
    
    # dynamic UI for single variable graphs
    one_var_visual <- input$one_var_visual # numerical variables
    one_var_plot_type <- input$one_var_plot_type # Density or Histogram
    fill_by_generation <- input$fill_by_generation
    one_var_gen_facets <- input$one_var_gen_facets
    # Appears only when graph is in density with generation fill but not faceted
    one_var_alpha <- input$one_var_alpha
    one_var_interactive <- input$one_var_interactive
    
    var <- poke_data[[one_var_visual]]
    
    updateSliderInput(session, "hist_bins",
                      min = 5 + floor(min(var, na.rm = TRUE)),
                      max = 10 + floor(max(var, na.rm = TRUE)/4)
                      )
    
    g <- ggplot(poke_data)
    if(one_var_plot_type == "Density"){
      if(fill_by_generation == "No"){
        onevar_g <- g + geom_density(aes(x = var), na.rm = TRUE) + 
          labs(x = one_var_visual)
      }else{ # If yes,
        onevar_g <- g + geom_density(aes(x = var, 
                                         fill = generation,
                                         alpha = I(one_var_alpha)), na.rm = TRUE) + 
          labs(x = one_var_visual)
      }
    }else{ # Histogram plot
      if(fill_by_generation == "No"){
        onevar_g <- g + geom_histogram(aes(x = var), na.rm = TRUE, 
                                       bins = input$hist_bins) + 
          labs(x = one_var_visual)
      }else{
        onevar_g <- g + geom_histogram(aes(x = var, fill = generation), na.rm = TRUE,
                                       bins = input$hist_bins) + 
          labs(x = one_var_visual)
      }
    }
    # Faceting
    if(one_var_gen_facets == "Yes"){
      onevar_g <- onevar_g + facet_grid(generation ~ .)
    }
    
    #Plotly
    if(one_var_interactive == "Yes"){
      onevar_g <- ggplotly(onevar_g)
      
      output$one_var_graph <- 
        renderUI(
          tagList(renderPlotly(onevar_g),
                  paste0("The min is: ", min(var, na.rm = TRUE)), br(),
                  paste0("The mean is: ", mean(var, na.rm = TRUE)), br(),
                  paste0("The median is: ", median(var, na.rm = TRUE)), br(),
                  paste0("The max is: ", max(var, na.rm = TRUE)), br(),
                  paste0("The sd is: ", sd(var, na.rm = TRUE)), br())
        )
    }else{
      output$one_var_graph <-
        renderUI(
          tagList(renderPlot(onevar_g),
                  paste0("The min is: ", min(var, na.rm = TRUE)), br(),
                  paste0("The mean is: ", mean(var, na.rm = TRUE)), br(),
                  paste0("The median is: ", median(var, na.rm = TRUE)), br(),
                  paste0("The max is: ", max(var, na.rm = TRUE)), br(),
                  paste0("The sd is: ", sd(var, na.rm = TRUE)), br())
        )
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
      speed_values <- input$var_speed
      
      filter_table <- poke_data %>% 
        filter(as.character(generation) %in% input$var_generation,
               attack <= attack_values[2] & 
                 attack >= attack_values[1],
               defense <= defense_values[2] & 
                 defense >= defense_values[1],
               speed <= speed_values[2] &
                 speed >= speed_values[1]
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
      
      # single variable visualization
      
    })
################ End of observe function ###################33
    
  # The clear button in tab 2
  observeEvent(input$var_clear,
    updateCheckboxGroupInput(session, "var_boxes",
                             selected = character(0))
     )
  

  })
  

