library(ggplot2)

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
             p("Graphs")
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
