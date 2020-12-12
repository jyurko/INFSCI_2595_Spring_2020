### demonstrate the influence of correlated inputs on the posterior 
### covariance matrix of the beta parameters

library(tidyverse)

### use two additive inputs, `x1` and `x2`

### for simplicity assume that the two inputs are distributed with MVN
### with marginal standard deviations of 1 and means 0

num_inputs <- 2

### start with correlation coefficient of 0

x_mean <- rep(0, num_inputs)

x_covmat_rho_zero <- diag(num_inputs)

x_covmat_rho_zero

### generate 100 random observations of the two inputs

set.seed(7131)

xinputs_rho_zero <- MASS::mvrnorm(n = 100, mu=x_mean, Sigma = x_covmat_rho_zero) %>% 
  as.data.frame() %>% tibble::as_tibble() %>% 
  purrr::set_names(sprintf("x%d", 1:num_inputs))

### create the design matrix

X_rho_zero <- model.matrix( ~ x1 + x2, xinputs_rho_zero)

X_rho_zero %>% head()

### matrix sum of squares
SSmat_rho_zero <- t(X_rho_zero) %*% X_rho_zero

SSmat_rho_zero

### calculate the posterior covariance matrix assuming
### the noise (residual error), sigma, is 1

solve(SSmat_rho_zero)

### look at the posterior correlation between the 2 slopes
### it's very small!

cov2cor(solve(SSmat_rho_zero))

### scatter plot between the two inputs
xinputs_rho_zero %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(alpha = 0.5, size = 4) +
  theme_bw()

### check by calculating the covariance matrix
### need to multiply by the number of rows!
(nrow(xinputs_rho_zero)) * cov(xinputs_rho_zero)

### to be exact we first need to CENTER the inputs!
xinputs_rho_zero %>% 
  mutate(x1_c = x1 - mean(x1),
         x2_c = x2 - mean(x2)) %>% 
  select(x1_c, x2_c) %>% 
  cov() * (nrow(xinputs_rho_zero))

### now generate 100 random samples when the inptu correlation
### is very high, 0.9

x_covmat_rho_high <- matrix(c(1, 0.9,
                              0.9, 1),
                            nrow = 2,
                            byrow = TRUE)

x_covmat_rho_high

set.seed(7131)

xinputs_rho_high <- MASS::mvrnorm(n = 100, mu=x_mean, Sigma = x_covmat_rho_high) %>% 
  as.data.frame() %>% tibble::as_tibble() %>% 
  purrr::set_names(sprintf("x%d", 1:num_inputs))

### scatter plot between the two inputs
xinputs_rho_high %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(alpha = 0.5, size = 4) +
  theme_bw()

### compare the two settings

xinputs_rho_zero %>% 
  mutate(rho = 0) %>% 
  bind_rows(xinputs_rho_high %>% 
              mutate(rho = 0.9)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(alpha = 0.5, size = 4) +
  coord_equal() +
  facet_wrap(~rho, labeller = "label_both") +
  theme_bw()

### create the design matrix

X_rho_high <- model.matrix( ~ x1 + x2, xinputs_rho_high)

X_rho_high %>% head()

### matrix sum of squares
SSmat_rho_high <- t(X_rho_high) %*% X_rho_high

SSmat_rho_high

### calculate the posterior covariance matrix assuming the noise is 1
solve(SSmat_rho_high)

### look at the posterior corrleation
cov2cor(solve(SSmat_rho_high))

### what about if the inputs are "highly negatively" correlated or
### anticorrelated?
x_covmat_rho_neg <- matrix(c(1, -0.9,
                             -0.9, 1),
                           nrow = 2,
                           byrow = TRUE)

x_covmat_rho_neg

set.seed(7131)

xinputs_rho_neg <- MASS::mvrnorm(n = 100, mu=x_mean, Sigma = x_covmat_rho_neg) %>% 
  as.data.frame() %>% tibble::as_tibble() %>% 
  purrr::set_names(sprintf("x%d", 1:num_inputs))

### compare all three settings
xinputs_rho_zero %>% 
  mutate(rho = 0) %>% 
  bind_rows(xinputs_rho_high %>% 
              mutate(rho = 0.9)) %>% 
  bind_rows(xinputs_rho_neg %>% 
              mutate(rho = -0.9)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(alpha = 0.5, size = 4) +
  coord_equal() +
  facet_wrap(~rho, labeller = "label_both") +
  theme_bw()

### create the design matrix

X_rho_neg <- model.matrix( ~ x1 + x2, xinputs_rho_neg)

X_rho_neg %>% head()

### matrix sum of squares
SSmat_rho_neg <- t(X_rho_neg) %*% X_rho_neg

SSmat_rho_neg

### calculate the posterior covariance matrix assuming the noise is 1
solve(SSmat_rho_neg)

### look at the posterior corrleation
cov2cor(solve(SSmat_rho_neg))

### compare the posterior standard deviations

sqrt(diag(solve(SSmat_rho_zero)))

sqrt(diag(solve(SSmat_rho_high)))

sqrt(diag(solve(SSmat_rho_neg)))

### use simulation to study the posterior correlation coefficient
### and the posterior marginal standard deviations as the 
### input correlation increases

sim_dataset <- function(rep_id, rho_value, num_samples, xmean_use, xsd_use)
{
  xcovmat_use <- matrix(c(xsd_use[1], rho_value,
                          rho_value, xsd_use[2]),
                        nrow = 2,
                        byrow = TRUE)
  
  xinputs <- MASS::mvrnorm(n = num_samples, 
                           mu=xmean_use, 
                           Sigma = xcovmat_use) %>% 
    as.data.frame() %>% tibble::as_tibble() %>% 
    purrr::set_names(sprintf("x%d", 1:length(xmean_use)))
  
  ### create the design matrix use all inputs as additive terms
  
  X <- model.matrix( ~ ., xinputs)
  
  ### matrix sum of squares
  SSmat <- t(X) %*% X
  
  ### calculate the posterior covariance matrix assuming
  ### the noise (residual error), sigma, is 1
  
  post_covmat <- solve(SSmat)
  
  post_sds <- sqrt(diag(post_covmat))
  
  ### look at the posterior correlation matrix
  
  post_corrmat <- cov2cor(solve(SSmat))
  
  list(rep_id = rep_id, 
       rho = rho_value,
       post_sd_b0 = post_sds[1],
       post_sd_b1 = post_sds[2], 
       post_sd_b2 = post_sds[3],
       post_slope_corr = post_corrmat[2, 3])
}

### create a wrapper which allows running each randomly generated set
### of input values a desired number of times

run_sim <- function(num_replicates, rho_values, num_samples, xmean_use, xsd_use)
{
  study_grid <- expand.grid(rep_id = 1:num_replicates,
                            rho_value = rho_values,
                            KEEP.OUT.ATTRS = FALSE,
                            stringsAsFactors = FALSE) %>% 
    as.data.frame() %>% tibble::as_tibble()
  
  purrr::map2_dfr(study_grid$rep_id, study_grid$rho_value,
                  sim_dataset,
                  num_samples = 100,
                  xmean_use = xmean_use,
                  xsd_use = xsd_use)
}

### use 50 replications of 100 randomly generated input combinations
### for 51 values of the correlation coefficeint
set.seed(71231)
study_results <- run_sim(50, seq(0, 0.9, length.out = 51),
                         num_samples = 100,
                         xmean_use = x_mean,
                         xsd_use = c(1, 1))

study_results %>% 
  ggplot(mapping = aes(x = rho, y = post_sd_b1)) +
  stat_summary(fun.data = "mean_se") +
  labs(y = "posterior standard deviation beta_1") +
  theme_bw()

study_results %>% 
  ggplot(mapping = aes(x = rho, y = post_slope_corr)) +
  stat_summary(fun.data = "mean_se") +
  labs(y = "posterior correlation between beta_1 and") +
  theme_bw()
