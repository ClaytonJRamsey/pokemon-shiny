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
                        "Available from Kaggle"), br()
                 
               )
             )
    ),
    
    # A data exploration page where common numeric and graphical 
    # summaries can be created by the user
    tabPanel("Explore the Data Numerically & Graphically",
             p("Graphs")
    ),
    
    # A page that allows the user to scroll through the data 
    # (or subset of data of interest)
    tabPanel("Explore the data Tables",
             p("Tables")
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
