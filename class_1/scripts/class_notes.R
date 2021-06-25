library(tidyverse)
library(purrr)

## Programación Funcional
map(letters, str_to_upper)


## Pipes de DW

(mpg %>% 
    filter(year == 1999) %>% 
    summarise(mean_displ = mean(displ, na.rm = T)))


## Alumnos
(fatalities %>% 
    group_by(YEAR, STATE.NAME) %>% 
    summarise(FATALITIES = sum(FATALITIES))  %>% 
    pivot_wider(names_from = YEAR, values_from = FATALITIES) %>% 
    transmute(yoy = (`2012` - `1975`)/`1975`*100, 
              STATE.NAME = STATE.NAME, 
              neg=yoy<0) %>% 
    ggplot(.) + 
    geom_bar(aes(x = reorder(STATE.NAME, -yoy), 
                 y = yoy, 
                 fill=neg),
             stat='identity', 
             width=.2) + 
    coord_flip() + 
    theme(panel.background = element_blank())
    )


## Let's take some random samples from the numeric columns in diamonds
numeric_cols <- names((diamonds %>% 
                   select(where(~typeof(.) == 'double'))))

d_size <- nrow(diamonds)

d_samps <- (diamonds[numeric_cols][sample(1:d_size, 1000), ] %>%
              apply(., 2, function(t){t <- list(l_b = mean(t)-1.96*sd(t)/sqrt(length(t)), 
                                                u_b = mean(t)+1.96*sd(t)/sqrt(length(t)))}))
## Perform 1000 of size samples of size 100 for each column 

conf_samps <- do.call(rbind.data.frame, map(1:1000,~
map(numeric_cols, 
    ~do.call(between, 
             as.list(c(mean(unlist(diamonds[.][sample(1:d_size, 1000),])), 
                       as.numeric(unlist(d_samps[.]))))))))


apply(conf_samps, 2, sum)



### Let's estimate the variability of the sampling statistic. 

### Tomar una muestra de distintas variables
### ver como se distribuyen las medias. Obtener desviación estándar y media
### Comparar contra desviación estándar y media de las variables.
### Send it to sampling distribution as well (histogram).
n_sampling <- 1000
sample_size <- 100

sampling_dist_data <- do.call(rbind,
        map(1:n_sampling,
            ~map(diamonds[numeric_cols],
                 ~(mean(sample(., sample_size))))))

ggplot(tibble(price = as.numeric(do.call(rbind,
                                         +         map(1:n_sampling,
                                                       +             ~map(diamonds['price'],
                                                                          +                  ~(mean(sample(., sample_size))))))))) + geom_histogram(aes(x=price))

sampling_dist <- apply(do.call(rbind,
                               map(1:n_sampling,
                                   ~map(diamonds[numeric_cols],
                                        ~(mean(sample(., sample_size)))))), 2, 
                       function(t)tibble(mean=mean(as.numeric(t)), 
                                         sd=sd(as.numeric(t))))


pop_dist <- apply(diamonds[numeric_cols], 2,       
                  function(t)tibble(mean=mean(as.numeric(t)), 
                                    sd=sd(as.numeric(t))))



### Plot of confidence intervals of each sample
cl = .95
cv = 1 - (1-cl)/2
n_samples = 100
sample_size = 1000


sample_intervals <- 
  apply(do.call(rbind, 
                map(1:n_samples, ~
                      map(diamonds[numeric_cols], 
                          ~ (tibble(data=sample(., sample_size)) %>% 
                               summarise(mean = as.numeric(mean(data, na.rm = T)),
                                         se = as.numeric(sd(data, na.rm = T)/sqrt(sample_size))) %>%
                               transmute(ub = mean +c *se, 
                                         c = mean,
                                         lb = mean - qnorm(cv)*se)
                             )))), 2, 
        function(t)tibble(do.call(rbind, t)) %>% mutate(smp=row_number()))


pop_mean <- map(diamonds[numeric_cols], ~ mean(.))



var <- 'carat'
(ggplot(data = (sample_intervals[[var]] %>% 
                  mutate(contains = if_else(pop_mean[[var]]>= lb & 
                                              pop_mean[[var]]<=ub, 'yes', 'no')))) + 
    geom_segment(aes(x=smp, y=lb, xend=smp, yend=ub, color=contains), 
                 alpha=.5) + 
    geom_hline(data=tibble(mean = as.numeric(pop_mean[var])), 
               aes(yintercept = mean)) + 
    theme(panel.background = element_blank())
    )









