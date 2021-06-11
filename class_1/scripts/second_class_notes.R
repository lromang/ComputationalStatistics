library(tidyverse)
library(UsingR)
library(mixR)

#### VIS VARIABLES NUMERICAS

## HISTOGRAMAS

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


# Variables no sim, y multimodales

stamps <- mixR::Stamp


(ggplot(tibble(stamp_width = stamps)) + 
    geom_histogram(aes(x=stamp_width)) + 
    geom_density(aes(x=stamp_width), color='blue')
    
  )


summary(stamps)

# Variables con mucho sesgo

# 1.- Cual es la longitud de peliculas mas frecuente
# 2.- Cual es el nombre y anio de las 10 peliculas mas largas
# 3.- Haz un histograma de la longitud de las peliculas
#     después de eliminar las que se encuentren a una distancia
#     mayor a 1.5*(q1-q2) de la mediana. 
# 4.- haz un boxplot de longitud por decada



movies <- ggplot2movies::movies

(ggplot(data = movies) +
  geom_boxplot(aes(x=length))
  )


