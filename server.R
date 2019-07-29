

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
        twovar_g <- g + geom_point(aes(x = xvar, y = yvar, color = legendary_factor), na.rm = TRUE)
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
        poke_group_table <- poke_data %>% group_by(`Variable 1`=poke_data[[input$group_var1]])
      }
      if(input$to_group_by == 2){
        poke_group_table <- poke_data %>% group_by(`Variable 1`=poke_data[[input$group_var1]], 
                                                   `Variable 2`=poke_data[[input$group_var2]])
      }
      if(input$to_group_by == 3){
        poke_group_table <- poke_data %>% group_by(`Variable 1`=poke_data[[input$group_var1]],
                                                   `Variable 2`=poke_data[[input$group_var2]],
                                                   `Variable 3`=poke_data[[input$group_var3]])
      }
      summ_var_name <- input$summary_var
      poke_group_table <- poke_group_table %>% 
        summarise(`Mean` = mean(eval(parse(text = summ_var_name)), na.rm = TRUE),
                  `Standard Deviation` = sd(eval(parse(text = summ_var_name)), na.rm = TRUE))
      
      output$grouping_table <- DT::renderDT(poke_group_table, rownames = FALSE)
     
     
      attack_values <- input$var_attack
      defense_values <- input$var_defense
      speed_values <- input$var_speed
      hp_values <- input$var_hp
      
      filter_table <- poke_data %>% 
        filter(as.character(generation) %in% input$var_generation,
               attack <= attack_values[2] & 
                 attack >= attack_values[1],
               defense <= defense_values[2] & 
                 defense >= defense_values[1],
               speed <= speed_values[2] &
                 speed >= speed_values[1],
               hp <= hp_values[2] &
                 hp >= hp_values[1]
               ) %>%
        select(c("name", input$var_boxes))
      
      output$dynamic_table <- DT::renderDT(filter_table, rownames = FALSE)
      
      # file download for tables
      file_name <- paste0(input$filename, ".csv")
      file_name2 <- paste0(input$filename2, ".csv")
      
      output$csv_download <- downloadHandler(
        filename = file_name,
        content = function(file){
          write_csv(filter_table, file)
          }
      )
      
      output$csv_download2 <- downloadHandler(
        filename = file_name2,
        content = function(file){
          write_csv(poke_group_table, file)
        }
      )
      
      # The predictor will crash if these are the same.
      updateSelectInput(session, "knn_var2", choices = setdiff(poke_numerical_vars, input$knn_var1))
      
    })
################ End of observe function ###################
      
  # The clear button in tab 2
  observeEvent(input$var_clear,
    updateCheckboxGroupInput(session, "var_boxes",
                             selected = character(0))
              )
  
  # the knn check misclassifications button
  observeEvent(input$check_misclass,
                {
                  kval <- 1:10
                  misclass <- numeric(10)
                  # K nearest neighbors fits
                  for(k in kval){
                    knn_fit <- knn_legendary(input$knn_var1, 
                                             input$knn_var2, 
                                             poke_testing,
                                             k)
                    misclass_status <- (knn_fit == as.factor(poke_testing$is_legendary))
                    misclass[k] <- 1 - as.numeric(sum(misclass_status))/length(knn_fit)
                  }
                  
                  output$k_mis_class <- 
                    renderTable({data.frame(k = kval,
                                            `Misclassification rate` = misclass)})
                }
            )
  
  # the knn make prediction button
  observeEvent(input$knn_makepred,
                {
                 # names
                 pred1 <- input$knn_var1
                 pred2 <- input$knn_var2
                 # values
                 pred1val <- input$knn_pred1
                 pred2val <- input$knn_pred2
                 predict_data <- cbind(
                    standardize_variable(data.frame(pred1val), pred1),
                    standardize_variable(data.frame(pred2val), pred2)
                 )
                 names(predict_data) <- c(pred1, pred2)
                 knn_predict <- knn(train = select(poke_training, pred1, pred2),
                                    test = predict_data,
                                    cl = poke_training$is_legendary,
                                    k = input$k_value)
                 output$knn_predtext <- renderText({knn_predict == levels(knn_predict)[2]})
                 
               }
              )
  
  # the random forest generate model button
  observeEvent(input$rf_generate,
               {
                 rf_vars_to_use <- input$rf_vars_to_use # up to 11
                 rf_response <- input$rf_response
                 
                 rf_fit <- randomForest(eval(parse(text = paste0(rf_response, " ~ ."))), 
                                        data = poke_training_full, 
                                        mtry = rf_vars_to_use, 
                                        importance = TRUE)
                 rf_pred <- predict(rf_fit, newdata = poke_testing_full)
                 rf_RMSE <- sqrt(mean((rf_pred-poke_testing_full[[rf_response]])^2))
                 output$rf_RMSE <- 
                   renderText({paste0("RMSE of this model: ", rf_RMSE)})
                 output$rf_mean <- 
                   renderText({paste0("Mean of ", rf_response, ": ",
                                      as.character(mean(poke_data_model[[rf_response]])))})
                 rf_model_generated <- "yes"
               })
  
  observeEvent(input$rf_predict,{
                    # Randomization:
                    random_stats <- 
                      as.data.frame(lapply(as.list(names(poke_training_full)), FUN = stat_sampler))
                    names(random_stats) <- names(poke_training_full)
                    
                    rf_response <- input$rf_response
                    output$random_name <- renderUI({h3(random_pokemon())})
                    
                    # generate a model if the user didn't already.
                    # These aren't generally observed because they take so long to make.
                    if(rf_model_generated == "no"){
                      rf_fit <- randomForest(eval(parse(text = paste0(rf_response, " ~ ."))), 
                                             data = poke_training_full, 
                                             mtry = input$rf_vars_to_use, 
                                             importance = TRUE)
                    }
                    
                    rf_pred <- predict(rf_fit, newdata = random_stats)
                    output$rf_prediction <- renderText({paste0(rf_response,
                                                               ": ",
                                                               rf_pred)})
                    
                    stat_names <- names(random_stats)
                    stat_values <- t(random_stats)
                    random_stats <- data.frame(Stat = stat_names, Value = stat_values)
                    
                    output$rf_random_stats <- renderTable({random_stats})
                  })

  
  })
  

