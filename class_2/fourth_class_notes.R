library(tidyverse)

M <- 1000
samp_size <- 1000 # 1000 see the difference in predicted v observed

test_norm <- map_df(1:M, ~map(seq(.1, .9, by=.1), 
                              ~tibble(mean=mean(rbinom(samp_size, 10,  .)), 
                                      p=toString(.))))

(ggplot(test_norm) + 
    geom_histogram(aes(x=mean, fill=p), alpha=.3, binwidth = .01) + 
    theme(panel.background = element_blank())
)

(test_norm %>% 
    group_by(p) %>% 
    summarise(samp_mean=mean(mean),
              samp_sd=sd(mean)) %>%
    mutate(t_var = 10*parse_number(p)*(1-parse_number(p))/sqrt(samp_size))
)

# 

nrow(map_df(1:M,  ~tibble(samp=sample(diamonds$price, 
                                 samp_size)) %>%
         summarise(n = n())))


M <- 1000
samp_size <- 1000
alpha <- .01

pop_mean <- mean(diamonds$price)

test <- map_df(1:M, ~tibble(samp=sample(diamonds$price, 
                                        samp_size)) %>%
                 summarise(n = n(),
                           s_mean = mean(samp, na.rm = T), 
                           s_sd = sd(samp, na.rm = T), 
                           lb = s_mean - qt(1 - alpha/2, n-1)*s_sd/sqrt(n), 
                           ub = s_mean + qt(1 - alpha/2, n-1)*s_sd/sqrt(n)
                           ) %>%
                 mutate(contains = between(pop_mean, lb, ub) %>% as.numeric)
                 )

sum(test$contains)/samp_size





alpha <-1-0.05/2
samp_size <- 1000
M <- 100

sample_intervals <- map_df(1:M, ~map_df(numeric_names, 
                                        ~tibble(var=., samp=sample(diamonds[[.]], samp_size)) %>%
                                          summarise(lb = mean(samp)-qt(alpha, samp_size)*sd(samp)/sqrt(samp_size), 
                                                    ub =  mean(samp)+qt(alpha, samp_size)*sd(samp)/sqrt(samp_size), 
                                                    mean = mean(samp), 
                                                    var = first(var))))

population_params <- apply(diamonds[numeric_names], 2, mean)


interval_plots <- lapply(numeric_names,function(t)
  (ggplot(sample_intervals %>% 
            filter(var == t) %>% 
            mutate(contains = if_else(population_params[[t]]>=  lb & 
                                        population_params[[t]]<= ub, 'yes', 'no'))
  ) +
    geom_segment(aes(x=1:M, 
                     xend=1:M, 
                     y=lb, 
                     yend=ub, 
                     color=contains)) + 
    geom_hline(aes(yintercept = population_params[[t]])) + 
    ggtitle(t)
  ))

multiplot(plotlist = interval_plots, cols = 2)

### Pruebas de hipotesis
library(magrittr)

n_alumns <- 34
alumns <- tibble(p1 = rnorm(n_alumns, 95, 20), 
                 p2 = rnorm(n_alumns, 90, 13.3))

(alumns %>% 
    pivot_longer(cols=names(alumns)) %>% 
    magrittr::set_colnames(c('group', 'score')) %>%
    ggplot(.) + 
    geom_density(aes(x=score, fill=group), alpha=.5) + 
    theme(panel.background = element_blank())
    )

alumns_stats <- alumns %>% summarise(m1 = mean(p1, na.rm = T), 
                     m2 = mean(p2, na.rm = T), 
                     sd1 = sd(p1, na.rm = T), 
                     sd2 = sd(p2, na.rm = T), 
                     se = sqrt(sd1**2/n_alumns + sd2**2/n_alumns),
                     stat = (m1 - m2)/se
                     ) 

stat_val <- alumns_stats %>% 
  dplyr::select(stat) %>% as.numeric


2*(1-pt(stat_val, n_alumns-1))


alpha <- .05

alumns_stats %>% 
  mutate(lb = (m1-m2)-qt(1-alpha/2, n_alumns - 1)*se, 
         ub = (m1-m2)+qt(1-alpha/2, n_alumns - 1)*se)  %>%
  dplyr::select(lb, ub)



### Bootstrap
M <- 1500
B <- 500

pop_param <- quantile(diamonds$price, .75)

p_sample <- sample(diamonds$price, M)

B75 <- map_df(1:B, ~tibble(p75 = quantile(sample(p_sample, M, replace=T), .75)))

ggplot(B75) + geom_histogram(aes(x=p75), binwidth = 30)


bsd <- (B75 %>% 
          summarise(sd75 = sd(p75, na.rm = T)) %>% 
          dplyr::select(sd75) %>%
          as.numeric
          )

p_samp_param <- quantile(p_sample, .75) %>% as.numeric

c(p_samp_param - qt(1-alpha/2, M-1)*bsd, 
  p_samp_param + qt(1-alpha/2, M-1)*bsd)


#### 
# Replicar experimento de las muestras que contienen el parámetro poblacional
# sobre diamonds$price generar 100 muestras de tamaño 1000
# generar un intervalo de confianza de 95% para cada una de las muestras
# que porcentaje de las muestras contienen el parámetro poblacional. 
####








