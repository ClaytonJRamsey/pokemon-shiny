library(shiny)
library(tidyverse)
library(ggplot2)
library(readr)

shinyServer(function(input, output, session) {

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
    
    g <- ggplot(poke_data)
    
    
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
    
    # Two variable Visualizations:
    two_var_plot <- input$two_var_plot # Scatter, Box, or Count
    smooth_type <- input$smooth_type
    id_legendary <- input$id_legendary
    two_var_interactive <- input$two_var_interactive
    
    
    # Two var graph logic
    if(two_var_plot == "Scatter"){
      xvar_name <- input$two_var_x
      yvar_name <- input$two_var_y
      xvar <- poke_data[[xvar_name]]
      yvar <- poke_data[[yvar_name]]
      if(id_legendary == "Yes"){
        twovar_g <- g + geom_point(aes(x = xvar, y = yvar, color = is_legendary), na.rm = TRUE)
      }else{
        twovar_g <- g + geom_point(aes(x = xvar, y = yvar), na.rm = TRUE)
      }
      if(smooth_type == "Loess"){
        twovar_g <- twovar_g + geom_smooth(aes(x = xvar, y = yvar), method = "loess", na.rm = TRUE)
      }
      if(smooth_type == "LM"){
        twovar_g <- twovar_g + geom_smooth(aes(x = xvar, y = yvar), method = "lm", na.rm = TRUE)
      }
    }
    if(two_var_plot == "Count"){
      xvar_name <- input$two_var_x_disc
      yvar_name <- input$two_var_y_disc
      xvar <- poke_data[[xvar_name]]
      yvar <- poke_data[[yvar_name]]
      twovar_g <- g + geom_count(aes(x = xvar, y = yvar), na.rm = TRUE) + 
        theme(axis.text.x = element_text(angle=90))
    }
    if(two_var_plot == "Box"){
      xvar_name <- input$two_var_x_disc
      yvar_name <- input$two_var_y
      xvar <- poke_data[[xvar_name]]
      yvar <- poke_data[[yvar_name]]
      twovar_g <- g + geom_boxplot(aes(x = xvar, y = yvar), na.rm = TRUE) + 
        theme(axis.text.x = element_text(angle=90))
    }
    # labels
    twovar_g <- twovar_g + labs(x = xvar_name, y = yvar_name)
    #Plotly
    if(two_var_interactive == "Yes"){
      twovar_g <- ggplotly(twovar_g)
      output$two_var_graph <- renderUI(tagList(renderPlotly(twovar_g)))
    }else{
      output$two_var_graph <- renderUI(tagList(renderPlot(twovar_g)))
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
      

      # Grouping table from the third tab.
      if(input$to_group_by == 1){
        poke_group_table <- poke_data %>% group_by(poke_data[[input$group_var1]])
      }
      if(input$to_group_by == 2){
        poke_group_table <- poke_data %>% group_by(poke_data[[input$group_var1]], 
                                                   poke_data[[input$group_var2]])
      }
      if(input$to_group_by == 3){
        poke_group_table <- poke_data %>% group_by(poke_data[[input$group_var1]],
                                                   poke_data[[input$group_var2]],
                                                   poke_data[[input$group_var3]])
      }
      summ_var_name <- input$summary_var
      poke_group_table <- poke_group_table %>% 
        summarise(`Mean` = mean(eval(parse(text = summ_var_name)), na.rm = TRUE),
                  `Standard Deviation` = sd(eval(parse(text = summ_var_name)), na.rm = TRUE))
      
      output$grouping_table <- renderTable(poke_group_table)
     
     
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
      
      output$dynamic_table <- DT::renderDT(filter_table, rownames = FALSE)
      
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
  

