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
#     mayor a 1.5*(q3-q1) de la mediana. 
# 4.- haz un boxplot de longitud por decada
movies <- ggplot2movies::movies


# 1
(plyr::count(movies$length) %>% 
    arrange(-freq) %>% 
    head(1))


# 2
(movies %>% 
    arrange(-length) %>% 
    dplyr::select('title', 'year', 'length') %>% 
    mutate(length = length/60) %>%
    head(10)) 

# 3
len_q <- quantile(movies$length, 
                  probs = c(.25, .5, .75))
iqr <- as.numeric(len_q[3] - len_q[1])

(movies %>%
    filter(length <= len_q[2] + 1.5*iqr ))



# 4
central_movies <- (movies %>%
                     filter(length <= len_q[2] + 1.5*iqr ))

(central_movies %>% 
    dplyr::select('length', 'year') %>% 
    mutate(decade = map_chr(central_movies$year, 
                            ~paste0(str_sub(toString(.), 1, 3), 
                                    '0'))) %>%
    ggplot() + 
    geom_boxplot(aes(x=decade, y=length)) + 
    coord_flip()
)


marginacion <- marginacion <- read_csv('../../data/class_1/marginacion.csv', 
                                       locale = readr::locale(encoding = "iso-8859-1"))


(ggplot(marginacion, aes(x=ENT, y= IM)) +
  geom_jitter(position=position_jitter(0.2), alpha=.2) +
  geom_point(data= (marginacion %>%
                      group_by(ENT) %>% 
                      summarise(mean_im = mean(IM, na.rm = T))),
             aes(x=ENT, y=mean_im), size=3, color='blue') +
  theme(axis.text.x = element_text(face = "bold", angle = 90)))



diabetes <- read_csv('../../data/class_1/diabetes.csv')

(diabetes %>% dplyr::select(DefunDiabetes, P20ymas))

diabetes <- (diabetes %>% 
  mutate(DefunDiabetes = (DefunDiabetes %>% 
                           str_replace(',', '.') %>% 
                           parse_number())))

diab_quants <- (diabetes$DefunDiabetes %>%
                  quantile(., c(.25, .75)))

(diabetes %>% 
    filter(DefunDiabetes <= diab_quants[1]) %>% 
    summarise(mean_pop = mean(P20ymas)))

(diabetes %>%
    filter(DefunDiabetes >= diab_quants[2]) %>%
    summarise(mean_pop = mean(P20ymas))
    )



### Generar numeros aleatorios
m <- 2^31 - 1
a <- 16807 
M <- 10000
rand_nums <- vector(length=M)
rand_nums[1] <-  2
for(i in seq_along(rand_nums)){
  rand_nums[i+1] <- (a*rand_nums[i]) %% m
}
rand_nums <- rand_nums/m

(tibble(r_nums = rand_nums) %>% 
    ggplot(.) + 
    geom_histogram(aes(x=r_nums)))


### Birthdays
n <- 10
p_todos_dist <- prod(map_dbl(1:n, ~365-(.-1)))/(365**n)
p_at_least_2 <- 1-p_todos_dist
p_at_least_2

p_at_least_2 <- function(n)1-prod(map_dbl(1:n, ~365-.))/(365**n)

(map_df(1:100, ~tibble(n=., p=p_at_least_2(.))) %>%
  ggplot(.)+
  geom_point(aes(x=n, y=p)))






