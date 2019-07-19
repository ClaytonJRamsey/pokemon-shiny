library(tidyverse)
library(shiny)

poke_data <- read_csv("pokemon.csv") %>% tibble::as_tibble()

poke_data <- poke_data %>% 
  mutate(capture_rate = as.integer(capture_rate),
                                generation = as.factor(generation),
                                pokedex_website = paste0("https://www.pokemon.com/us/pokedex/", pokedex_number))

x <- sample(1:nrow(poke_data), 1)
print(paste0(poke_data$name[x],
             "is a ",
             poke_data$classfication[x],
             " originally introduced in generation ",
             poke_data$generation[x],
             "."))

(
poke_group_table <- poke_data 
  %>% group_by(type1, generation) 
  %>% summarise(`Ave. Cap. Rate` = mean(capture_rate, na.rm = TRUE),
                `Ave. Attack` = mean(attack, na.rm = TRUE),
                `Ave. Def.` = mean(defense, na.rm = TRUE)) 
  %>% DT::datatable()
)

(
g <- ggplot(poke_data) + geom_boxplot(aes(x = generation, 
                                          y = capture_rate))
)

(
  g2 <- ggplot(poke_data) + geom_point(aes(x = attack, y = capture_rate,
                                           color = generation))
)
