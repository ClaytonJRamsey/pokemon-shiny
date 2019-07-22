library(ggplot2)
library(plotly)

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
                                paste0("Enter a number from 1 to ",
                                       nrow(poke_data)),
                                value = 1,
                                min = 1,
                                max = nrow(poke_data),
                                step = 1
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
                   choices = names(poke_data)[2:length(names(poke_data))],
                   selected = names(poke_data)[2:length(names(poke_data))]
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
                             min = min(poke_data$attack),
                             max = max(poke_data$attack),
                             value = c(min(poke_data$attack),
                                       max(poke_data$attack))
                             ),
                 sliderInput(inputId = "var_defense",
                             label = "Defense",
                             min = min(poke_data$defense),
                             max = max(poke_data$defense),
                             value = c(min(poke_data$defense),
                                       max(poke_data$defense))
                             ),
                 sliderInput(inputId = "var_speed",
                             label = "Speed",
                             min = min(poke_data$speed),
                             max = max(poke_data$speed),
                             value = c(min(poke_data$speed),
                                       max(poke_data$speed))
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
                   label = "How many variables to visualize:",
                   choices = c("1","2","3"),
                   selected = "1"
                 ),
                 conditionalPanel("input.number_vars == '1'",
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
                                                label = "Alpha Transparency",
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
                 conditionalPanel("input.number_vars == '2'",
                                  p("two var controls")
                                ),
                 conditionalPanel("input.number_vars == '3'",
                                  p("three var controls")
                                )
               ),
               mainPanel(
                 conditionalPanel("input.number_vars == '1'",
                                  p("one var graphs"),
                                  uiOutput("one_var_graph")
               ),
                 conditionalPanel("input.number_vars == '2'",
                                  p("two var graphs")
               ),
                 conditionalPanel("input.number_vars == '3'",
                                  p("three var graphs")
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
