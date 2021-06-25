# ---------------------------------------------------
# Generate 1,000 samples of size 100
# from a X_i~Binom(10, p_i) where p_i = i/10,
# for i in 1, 2, ...,9.
# Compute the sample mean for each X_i.
# Plot a histogram of the distributions of means.
# Notice any pattern (play with the binwidth)?
# Compute the mean and standard deviation for each
# distribution of means. 
# Compare the results with the predictions of the 
# CLT
# -> descubrimiento: !! <- evalua expresiones
# -> podemos pasar funciones aribtrarias a cada
#    función de dplyr siempre y cuando haga sentido 
#    el resultado final
# -> no sobreescribir nombre de una columna en mutate!
# ----------------------------------------------------
M <- 1000
samp_size <- 100 # 1000 see the difference in predicted v observed

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

# Repite el ejercicio anterior para una normal variando media y varianza
# mu = -5, -4, -3, ..., 0, 1, ...,.4, 5
# sd = .1, .2, ..., .9
create_sample_dist <- function(params){
  map_df(1:M, ~map2(params[[1]], params[[2]],
                   ~tibble(mean=mean(rnorm(samp_size, .x,  .y)), 
                           mu=toString(.x), 
                           sd=toString(.y))))
}

test_norm <- create_sample_dist(list(mu=seq(-4, 4, by=1), 
                                     sd=seq(.1, .9, by=.1)))

# Change to prop? 
# concentration matters!
(ggplot(test_norm) + 
    geom_histogram(aes(x = mean, fill=mu, y=..count../sum(..count..)), binwidth = .01) + 
    theme(panel.background = element_blank()) + ylim(0, .05)
)
  
# Multiple arguments??
# explore do.call
## Tomemos 1000 muestras de tamaño 100 del dataset mpg
## Checar qqplot vs tamaño de muestra

## Let's take some random samples from the numeric columns in mpg

 
## Generate a qqplot for all your variables against a normal distribution

qqplots <- map(sample_pop_std, 
    ~tibble(var_percentiles = as.numeric(quantile(., 
                                                 probs = seq(.01, .09, by=.01))), 
            norm_percentiles = as.numeric(quantile(rnorm(length(.)), 
                                                   probs=seq(.01, .09, by=.01)))) %>%
      ggplot(.) + 
      geom_point(aes(x=norm_percentiles, y=var_percentiles)) +
      geom_line(aes(x=norm_percentiles, y=norm_percentiles), 
                color='red'))


multiplot(plotlist=qqplots, cols=3)

# Take any variable that you like, generate 1000 samples of 100 observations without replacement, 
# compute the mean, check
# the percentage that falls within the 95% confidence interval. Replicate this plot.

# Take any variable that you like, generate 1000 samples, compute a 95% confidence interval 
# for each sample check how many contain the population parameter. Replicate this plot.
# - Decrease the sample size, what happens with the percentage of intervals that contain
# the values?
# - Change qnorm for a qt
# Note that the distributions that were very skewed tend to contain the lesser number of 
# intervals that contain the actual population value!
# One condition that we are always going to check is for skweeness
# Ofcourse (also independence, this is fundamental from the central limit theorem.)
alpha <-1-0.05/2
samp_size <- 1000
M <- 100

sample_intervals <- map_df(1:M, ~map_df(numeric_names, 
       ~tibble(var=., samp=sample(diamonds[[.]], samp_size)) %>%
         summarise(lb = mean(samp)-qnorm(alpha)*sd(samp)/sqrt(samp_size), 
                   ub =  mean(samp)+qnorm(alpha)*sd(samp)/sqrt(samp_size), 
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


# Generate two populations
# Suppose we have two samples: 
# -> rnorm(100, 95, 32) | 17
# -> rnorm(100, 75, 13.3) | 17
# Generate a 95% confidence interval for the difference in means
# HINT sd = sqrt(sd1^2/n1 + sd2^2/n2)
# Do we have evidence to discard the hipotesis that they come from the same distribution?
# check with a sample of 100,000
# if the sample increases, we get artificially low p-values. 
# Note that in these case, we shall use a t distribution with degrees of freedom 
# min(n1-1, n2-1)
# Check independence within sample groups, random, less than 10 percent of population. 
# Check independence between groups. 
# The larger the skeweness the larger the sample -> more than 30% data
# After getting the interval
# H0, there is no difference, ((95-75)-0)/sqrt(sd1^2/n1 + sd2^2/n2)
# Idealmente sería
# p = 1 - (pt((95-75)/sqrt(32**2/17 + 13.3**2/17), 16)-pt(-(95-75)/sqrt(32**2/17 + 13.3**2/17), 16))
# Como es simétrica
# p = 2*(1 - pt((95-75)/sqrt(32**2/17 + 13.3**2/17), 16))
# if they are paired N(mean(diff), sd(diff)/sqrt(n)) degrees=n-1



###############
# CLASS NOTES #
###############

# Primera prueba
p_p_e <- .87
p_p_ne <- .02
pe <- 1/1000000
p_e_p <- p_p_e*pe/(p_p_e*pe+p_p_ne*(1-pe))
# Segunda prueba
p_e_2p <- p_p_e*p_e_p/(p_p_e*p_e_p+p_p_ne*(1-p_e_p))
# Graficar probabilidad de estar enfermo vs numero de pruebas
n_test <- 10
posterior <- function(pe) p_p_e*pe/(p_p_e*pe+p_p_ne*(1-pe))
probs <- vector(length = n_test)
# Primera prueba
probs[1] <- posterior(pe)
# Iterar a partir de la primera prueba
# dando como input el valor anterior
for(i in seq_along(probs))
  probs[i+1] <- posterior(probs[i])
# 
ggplot(tibble(prob_enfermo=probs,
              n_pruebas=1:(n_test+1))) + 
  geom_line(aes(x=n_pruebas, y=prob_enfermo)) +
  theme(panel.background = element_blank())

#########
nfailures <- 20
p <- .5

# P(X=x)
fx <- function(x)(1-p)**x*p
# P(X<=x)
Fx <- function(x)1-(1-p)**(x+1)


ggplot(tibble(failures = 1:nfailures, 
              prob=map_dbl(1:nfailures, fx))) + 
  geom_point(aes(x=failures, y=prob))


ggplot(tibble(failures= 1:nfailures, 
              prob=map_dbl(1:nfailures, Fx))) + 
  geom_point(aes(x=failures, y=prob))


# pgeom = Fx
# dgeom = fx

ggplot(tibble(failures = 1:nfailures, 
              prob=map_dbl(1:nfailures, ~dgeom(., prob=p)))) + 
  geom_point(aes(x=failures, y=prob))


ggplot(tibble(failures = 1:nfailures, 
              prob=map_dbl(1:nfailures, ~pgeom(., prob=p)))) + 
  geom_point(aes(x=failures, y=prob))



### BINOM

# dbinom
p <- .5
n <- 10
success <- 1:10

# dbinom
# pbinom
# rbinom

(map_df(seq(.1, .9, by=.1), 
       ~tibble(n_succes=rbinom(1000, size=10, p=.), 
               p = toString(.))) %>%
  ggplot(.) + 
    geom_histogram(aes(x=n_succes), position = 'dodge') + 
    facet_wrap(~p) + 
    theme(panel.background = element_blank()))

## 

plot_list <- map(seq(.1, .9, by=.1), 
        ~tibble(n_succes=rbinom(1000, size=10, p=.), 
                p = toString(.)))


for(i in seq_along(plot_list)){
  if(i==1){
    the_plot <- (ggplot(plot_list[[i]]) + 
      geom_density(aes(x=n_succes, fill=p, 
                       alpha=parse_number(p)*(1-parse_number(p)))))
  }else{
    the_plot <- the_plot + 
      geom_density(data=plot_list[[i]], 
                     aes(x=n_succes, fill=p, 
                         alpha=parse_number(p)*(1-parse_number(p))))
  }
}

lambda<- .5

ggplot(map_df(0:10, ~tibble(exp=dexp(., rate =lambda)))) + 
  geom_point(aes(x=0:10, y=exp))

ggplot(map_df(0:10, ~tibble(exp=pexp(., rate =lambda)))) + 
  geom_point(aes(x=0:10, y=exp))


## Norm

ggplot(map_df(seq(-5,5, length.out = 100), ~tibble(norm=pnorm(., mean = 0, sd = 1)))) + 
  geom_line(aes(x=seq(-5,5, length.out=100), y=norm))


ggplot(map_df(seq(-5,5, length.out = 100), ~tibble(norm=dnorm(., mean = 0, sd = 1)))) + 
  geom_line(aes(x=seq(-5,5, length.out=100), y=norm))

# P(X >= -2.5 n X<=2.5)
pnorm(2.5, mean=0, sd=1) - pnorm(-2.5, mean=0, sd=1)


# P(X >= -k n X <= k) = .95 k?
qnorm(0.025, mean=0, sd=1)
qnorm(0.975, mean=0, sd=1)

# Prob acumulada del intervalo = 1-alpha
# alpha =.5
# mean +- qnorm(1-alpha/2)*sd

# Sobre un mismo plot
# - graficar normal 0, 1
# - graficar t freedom degrees = 1, 5, 10, 20, 50, 100
norm_plot <- ggplot(map_df(seq(-5,5, length.out = 100), 
                           ~tibble(norm=dnorm(., mean = 0, sd = 1)))) + 
  geom_line(aes(x=seq(-5,5, length.out=100), y=norm))

for(i in 1:100){
  norm_plot <- norm_plot + geom_line(data=(map_df(seq(-5,5, length.out = 100), 
                                                  ~tibble(t=dt(., df = i)))), 
                                           aes(x=seq(-5,5, length.out = 100), 
                                               y=t), color=toString(i), alpha=1/log(i+1))
}
norm_plot



### Generar 1000 muestras de tamaño 100 de Binom(10, p_i)  p_i = .1, .2, ..., .9
### Calcular las medias para cada una de las muestras
M <- 1000
sample_size <- 100

binom_samples <- map_df(1:M, ~
map_df(seq(.1, .9, by=.1), 
    ~tibble(mean=mean(rbinom(sample_size, 10, .)), 
            p = toString(.))))


  ggplot(.) + geom_histogram(aes(x=mean, fill=p), binwidth = .01)

# Para cada valor de pi 
# 1. Obtener la media y desviación estándar
# 2. Comparar con predicciones del TCL
#
# x_i ~ Binomial(n, p_i) => E(x_i) = n*p_i, SD(x_i) = sqrt(n*p_i*(1-p_i))

results <- binom_samples %>% group_by(p) %>% summarise(s_mean=mean(mean), 
                                            s_sd=sd(mean))  

results %>% mutate(p = parse_number(p),  
                   t_mean = 10*p, 
                   t_sd = sqrt(10*p*(1-p))/sqrt(sample_size))


#### TOMAR TODAS LAS COLUMNAS NUMERICAS DE DIAMONDS
## 1. Obtener 1,000 muestras de tamaño 100 de cada una de las variables numericas
## 2. Obtener la media para cada una de las muestras
## 3. Obtener la media y desviación estandar de la distribución de medias
## 4. Comparar contra media y desviación estandar poblacional!
M <- 1000
sample_size <- 100

# Columnas numericas
numeric_names <- names(diamonds %>% 
                         dplyr::select(where(is.numeric)))


## Muestras
sample_population <- map_df(1:M, 
                            ~map_df(numeric_names, 
                                    ~tibble(samp = mean(sample(diamonds[[.]], 
                                                               sample_size)), 
                                            var = .)))

sample_population %>% ggplot() + geom_histogram(aes(x=samp)) + facet_wrap(~var)

##
sample_pop_std <- sample_population %>% 
  pivot_wider(names_from = var, 
              values_from=samp, 
              values_fn = list) %>%
  apply(., 2, function(t)(t[[1]] - 
                            mean(t[[1]], na.rm = T))/
          sd(t[[1]], na.rm = T)) %>%
  as_tibble() 

(ggplot(data=sample_pop_std%>%
          pivot_longer(cols = all_of(numeric_names))) + 
    geom_histogram(aes(x=value)) + 
    facet_grid(~name))
































