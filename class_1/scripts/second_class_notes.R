library(tidyverse)
library(UsingR)

galton <- UsingR::galton

# extraer la moda, media y mediana para ambas distribuciones. 

# Metodo 1: summary(galton)
# Metodo 2: pipes 
# galton %>% summarise(mean_child = mean(child), mean_parent = mean(parent))
# Metodo 3: 
# Tidyverse -> Purrr -> map
map_df(galton, ~tibble(mean=mean(.), 
                    median=median(.), 
                    mode=(plyr::count(.) %>% arrange(-freq))$x[1]))

# hacer un histograma de ambas variables y comentar
(ggplot(galton) + 
    geom_histogram(mapping = aes(x=child), 
                   fill='orange', alpha=.5, binwidth = 1) +
    geom_histogram(mapping = aes(x=parent), fill='blue', alpha=.2, binwidth = 1)) + 
  theme(panel.background = element_blank())



