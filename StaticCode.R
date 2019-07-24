library(tidyverse)
library(shiny)
library(ggplot2)
library(plot3D)
library(plotly)

poke_data <- read_csv("pokemon.csv") %>% tibble::as_tibble()

poke_data <- poke_data %>% select("name", everything()) %>%
  mutate(capture_rate = as.integer(capture_rate),
         generation = as.factor(generation),
         pokedex_website = paste0("https://www.pokemon.com/us/pokedex/", 
                                  pokedex_number),
         type2 = ifelse(is.na(type2), "none", type2))

poke_data %>% DT::datatable()

poke_data %>% select(c("name", "against_bug")) %>% DT::datatable()

x <- sample(1:nrow(poke_data), 1)
print(paste0(poke_data$name[x],
             "is a ",
             poke_data$classfication[x],
             " originally introduced in generation ",
             poke_data$generation[x],
             "."))

poke_group_table <- poke_data %>% group_by(poke_data[["generation"]],
                                           poke_data[["type1"]],
                                           poke_data[["type2"]])

summ_var_name <- poke_numerical_vars[3]
poke_group_table <- poke_group_table %>% 
  summarise(`Mean` = mean(eval(parse(text = summ_var_name)), na.rm = TRUE),
            `Standard Deviation` = sd(eval(parse(text = summ_var_name)), na.rm = TRUE))
poke_group_table

(
poke_group_table <- poke_data %>% 
    group_by(type1, generation) %>%
    summarise(`Ave. Cap. Rate` = mean(capture_rate, na.rm = TRUE),
                `Ave. Attack` = mean(attack, na.rm = TRUE),
                `Ave. Def.` = mean(defense, na.rm = TRUE)) %>% 
    DT::datatable()
)

(
g <- ggplot(poke_data) + geom_boxplot(aes(x = generation, 
                                          y = capture_rate))
)

(
  g2 <- ggplot(poke_data) + geom_point(aes(x = attack, y = capture_rate,
                                           color = generation))
)

# dynamic ui for single variable graphs
one_var_visual <- "hp" # numerical variables
one_var_plot_type <- "Histogram" # Density or Histogram
fill_by_generation <- "Yes"
one_var_gen_facets <- "No"
one_var_alpha <- 0.5 # Appears only when graph is in density with generation fill but not faceted
one_var_interactive <- "No"

var <- poke_data[[one_var_visual]]
g <- ggplot(poke_data)

# Logic for single var graphs
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
    onevar_g <- g + geom_histogram(aes(x = var), na.rm = TRUE) + 
    labs(x = one_var_visual)
  }else{
    onevar_g <- g + geom_histogram(aes(x = var, fill = generation), na.rm = TRUE) + 
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
}
onevar_g

# Dynamic UI for 2-variable graphs
# This one will choose based on the variables
two_var_plot <- "Scatter" # Scatter, Box, or Count
two_var_x <- "attack" # updated dynamically based on plot type
xvar <- poke_data[[two_var_x]]
two_var_y <- "defense"
yvar <- poke_data[[two_var_y]]
smooth_type <- "Loess"
id_legendary <- "Yes"
two_var_interactive <- "No"

# Two var graph logic
if(two_var_plot == "Scatter"){
  if(id_legendary == "Yes"){
    twovar_g <- g + geom_point(aes(x = xvar, y = yvar, color = is_legendary))
  }else{
    twovar_g <- g + geom_point(aes(x = xvar, y = yvar))
  }
  if(smooth_type == "Loess"){
    twovar_g <- twovar_g + geom_smooth(aes(x = xvar, y = yvar), method = "loess")
  }
  if(smooth_type == "LM"){
    twovar_g <- twovar_g + geom_smooth(aes(x = xvar, y = yvar), method = "lm")
  }
}
if(two_var_plot == "Count"){
  twovar_g <- g + geom_count(aes(x = xvar, y = yvar)) + theme(axis.text.x = element_text(angle=90))
}
if(two_var_plot == "Box"){
  twovar_g <- g + geom_boxplot(aes(x = xvar, y = yvar)) + theme(axis.text.x = element_text(angle=90))
}
# labels
twovar_g <- twovar_g + labs(x = two_var_x, y = two_var_y)
#Plotly
if(two_var_interactive == "Yes"){
  twovar_g <- ggplotly(twovar_g)
}
twovar_g

# This one is needed for a selector control
numeric_checker <- function(n){
  return(is.numeric(poke_data[[n]]))
}

poke_variables <- names(poke_data)
poke_numerical <- unlist(lapply(as.list(poke_variables), FUN = numeric_checker))
poke_numerical_vars <- poke_variables[poke_numerical]
