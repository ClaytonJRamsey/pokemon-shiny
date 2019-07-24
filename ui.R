library(ggplot2)
library(plotly)

source("dataread.r")

shinyUI(fluidPage(
  tabsetPanel(
    # An information page that describes the data and abilities of the app:
    tabPanel("Pokémon Dataset",
             sidebarLayout(
               sidebarPanel(
                 h3("Find out more about Pokémon:"),
                 selectInput(
                   "pokemon_basic",
                   "Select by number or randomly?",
                   c("Number", "Random"),
                   selected = "Number",
                   multiple = FALSE
                 ),
                 conditionalPanel(
                   condition = "input.pokemon_basic == 'Number'",
                   numericInput("entry_number",
                                label = "Enter a number from 1 to 801:",
                                value = 1,
                                min = 1,
                                max = nrow(poke_data)#,
                                #step = 1
                                )
                 ),
                 conditionalPanel(
                   condition = "input.pokemon_basic == 'Random'",
                   actionButton("entry_number_random",
                                "Click for random Pokémon"
                   )
                 ),
                 uiOutput("pokemon_info")
               ),
               mainPanel(
                 h1("The Complete Pokémon Dataset"), br(),
                 tags$a(href = "https://www.kaggle.com/rounakbanik/pokemon",
                        "Available from Kaggle"), br(),
                 p("Pokémon is a popular entertainment francise in which players capture, train, and battle with fantasy monsters. The term Pokémon comes from the Japanese name, 'Pocket Monsters.' The first Pokémon video game was released in 1996. Since then there have been a variety of films, TV shows, games, and books based on these creatures. \n\n This dataset contains 41 variables describing the variety of properties of each Pokémon from the first 7 generations of the video game.")
               )
             )
    ),

    # A page that allows the user to scroll through the data 
    # (or subset of data of interest)
    tabPanel("Explore the data with tables",
             sidebarLayout(
               sidebarPanel(
                 actionButton(
                   inputId = "var_clear",
                   label = "Clear all variables"
                 ),
                 checkboxGroupInput(
                   inputId = "var_boxes",
                   label = "Check variables to show:",
                   choices = poke_variables[2:length(poke_variables)],
                   selected = poke_variables[2:length(poke_variables)]
                 )
               ),
               mainPanel(
                 checkboxGroupInput(inputId = "var_generation",
                             label = "Generation:",
                             choices = as.character(1:7),
                             selected = as.character(1:7),
                             inline = TRUE
                             ),
                 sliderInput(inputId = "var_attack",
                             label = "Attack",
                             min = min(poke_data$attack, na.rm = TRUE),
                             max = max(poke_data$attack, na.rm = TRUE),
                             value = c(min(poke_data$attack, na.rm = TRUE),
                                       max(poke_data$attack, na.rm = TRUE))
                             ),
                 sliderInput(inputId = "var_defense",
                             label = "Defense",
                             min = min(poke_data$defense, na.rm = TRUE),
                             max = max(poke_data$defense, na.rm = TRUE),
                             value = c(min(poke_data$defense, na.rm = TRUE),
                                       max(poke_data$defense, na.rm = TRUE))
                             ),
                 sliderInput(inputId = "var_speed",
                             label = "Speed",
                             min = min(poke_data$speed, na.rm = TRUE),
                             max = max(poke_data$speed, na.rm = TRUE),
                             value = c(min(poke_data$speed, na.rm = TRUE),
                                       max(poke_data$speed, na.rm = TRUE))
                             ),
                 DT::DTOutput("dynamic_table"),
                 textInput("filename", "Enter file name:", value = "pokemonData"),
                 downloadButton("csv_download", 
                                label = "Download this table as CSV")
               )
             )
    ),
    
    # A data exploration page where common numeric and graphical 
    # summaries can be created by the user
    tabPanel("Explore the data numerically & graphically",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "number_vars",
                   label = "Summary Type:",
                   choices = c("1 var. graph","2 var. graph","Tabular"),
                   selected = "1 var. graph"
                 ),
                 conditionalPanel("input.number_vars == '1 var. graph'",
                                  selectInput(
                                    inputId = "one_var_visual",
                                    label = "Which variable to view:",
                                    choices = poke_numerical_vars,
                                    selected = "hp"
                                  ),
                                  selectInput(
                                    inputId = "one_var_plot_type",
                                    label = "Plot Type",
                                    choices = c("Histogram", "Density"),
                                    selected = "Histogram"
                                  ),
                                  conditionalPanel(
                                    condition = "input.one_var_plot_type == 'Histogram'",
                                    sliderInput("hist_bins", "Bins",
                                                min = 10,
                                                max = 30,
                                                value = 30)
                                  ),
                                  selectInput(
                                    inputId = "fill_by_generation",
                                    label = "Color by Generation?",
                                    choices = c("Yes", "No"),
                                    selected = "Yes"
                                  ),
                                  selectInput(
                                    inputId = "one_var_gen_facets",
                                    label = "Separate Graphs by Generation?",
                                    choices = c("Yes", "No"),
                                    selected = "No"
                                  ),
                                  conditionalPanel(
                                    condition = "input.one_var_gen_facets == 'No' && 
                                    input.fill_by_generation == 'Yes' &&
                                    input.one_var_plot_type == 'Density'",
                                    sliderInput("one_var_alpha",
                                                label = "Opacity",
                                                min = 0,
                                                max = 1,
                                                step = 0.1,
                                                value = 1.0)
                                  ),
                                  selectInput("one_var_interactive",
                                              label = "Enable Interactive Plots?",
                                              choices = c("Yes", "No"),
                                              selected = "No"
                                              )
                                ),
                 conditionalPanel("input.number_vars == '2 var. graph'",
                                  p("two var controls"),
                                  selectInput(inputId = "two_var_plot",
                                              label = "Choose Plot Type:",
                                              choices = c("Scatter", "Box", "Count"),
                                              selected = "Scatter"),
                                  
                                  # Conditional for y-variable selector
                                  # Count is discrete on y, the others are continuous
                                  conditionalPanel(condition = "input.two_var_plot != 'Count'",
                                                   selectInput(inputId = "two_var_y",
                                                               label = "Vertical Axis:",
                                                               choices = poke_numerical_vars,
                                                               selected = sample(poke_numerical_vars,
                                                                                 1)
                                                   )
                                  ),
                                  conditionalPanel(condition = "input.two_var_plot == 'Count'",
                                                   selectInput(inputId = "two_var_y_disc",
                                                               label = "Vertical Axis:",
                                                               choices = poke_discrete_vars,
                                                               selected = sample(poke_discrete_vars,
                                                                                 1)
                                                   )
                                  ),
                                  
                                  # Conditional for x-variable selector
                                  # Box and Count use a discrete x, Scatter is continuous on x.
                                  conditionalPanel(condition = "input.two_var_plot == 'Scatter'",
                                                   selectInput(inputId = "two_var_x",
                                                               label = "Horizontal Axis:",
                                                               choices = poke_numerical_vars,
                                                               selected = sample(poke_numerical_vars,
                                                                                 1)
                                                                ),
                                                   # Extra Controls for scatterplot.
                                                   selectInput(inputId = "smooth_type",
                                                               label = "Smooth Type",
                                                               choices = c("None",
                                                                           "Loess",
                                                                           "LM"),
                                                               selected = "None"),
                                                   selectInput(inputId = "id_legendary",
                                                               label = "Identify Legendary?",
                                                               choices = c("No", "Yes"),
                                                               selected = "No")
                                                   ),
                                  conditionalPanel(condition = "input.two_var_plot != 'Scatter'",
                                                   selectInput(inputId = "two_var_x_disc",
                                                               label = "Horizontal Axis:",
                                                               choices = poke_discrete_vars,
                                                               selected = sample(poke_discrete_vars,
                                                                                 1)
                                                                )
                                                    ),
                                  
                                  
                                  selectInput(inputId = "two_var_interactive",
                                              label = "Interactive mode?",
                                              choices = c("No", "Yes"),
                                              selected = "No")
                                ),
                 conditionalPanel("input.number_vars == 'Tabular'",
                                  p("table controls"),
                                  sliderInput("to_group_by",
                                              label = "Number of variables to group by:",
                                              min = 1,
                                              max = 3,
                                              step = 1,
                                              value = 1),
                                  # This one always shows up
                                  selectInput("group_var1",  
                                              "First variable", 
                                              poke_discrete_vars),
                                  conditionalPanel("input.to_group_by >= 2",
                                                   # This one on 2 or 3
                                                   selectInput("group_var2",  
                                                               "Second variable", 
                                                               poke_discrete_vars)),
                                  conditionalPanel("input.to_group_by == 3",
                                                   selectInput("group_var3",  
                                                               "Third variable", 
                                                               poke_discrete_vars)),
                                  selectInput("summary_var",  
                                              "Summary variable:", 
                                              poke_numerical_vars)
                                )
               ),
               mainPanel(
                 conditionalPanel("input.number_vars == '1 var. graph'",
                                  p("One Variable:"),
                                  uiOutput("one_var_graph")
               ),
                 conditionalPanel("input.number_vars == '2 var. graph'",
                                  p("Two Variables:"),
                                  uiOutput("two_var_graph")
               ),
                 conditionalPanel("input.number_vars == 'Tabular'",
                                  p("Summary Table:"),
                                  tableOutput("grouping_table")
               )
             )
          )
    ),
    
    # A page with either clustering (include a dendogram) 
    # or principal components analysis (include a biplot) - 
    # again where the user can specify aspects of the algorithm
    tabPanel("Cluster Analysis",
             p("Clusters")
    ),
    
    # A page for modeling - see below for details
    tabPanel("Modeling",
             p("Analysis")
    )
  )
))
