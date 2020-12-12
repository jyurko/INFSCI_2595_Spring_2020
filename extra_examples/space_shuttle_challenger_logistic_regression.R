### demonstrate bayesian logistic regression using real data
### the space shuttle challenger data set from the 
### Bayesian methods for Hackers book:
### https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers

library(tidyverse)

### read in the data

shuttle_url <- 'https://raw.githubusercontent.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/master/Chapter2_MorePyMC/data/challenger_data.csv'

challenger_data <- readr::read_csv(shuttle_url, col_names = TRUE)

challenger_data

challenger_data %>% tail() ### last observation is the accident

### remove the missing and last observation, change the data type
### of the `Damage Incident` variable and rename

clean_df <- challenger_data %>% 
  filter(`Damage Incident` %in% c('0', '1')) %>% 
  select(Date, Temperature, outcome = `Damage Incident`) %>% 
  mutate_at(c("outcome"), as.numeric)

clean_df

### plot the training data
clean_df %>% 
  ggplot(mapping = aes(x = Temperature, y = outcome)) +
  geom_jitter(height = 0.05, size = 4.5, alpha = 0.5) +
  theme_bw()

### standardize the temperature
train_df <- clean_df %>% 
  mutate(x = (Temperature - mean(Temperature))/sd(Temperature))

### define the log-posterior function

logistic_logpost <- function(unknowns, my_info)
{
  # unpack the parameter vector
  beta_0 <- unknowns[1]
  beta_1 <- unknowns[2]
  
  # calculate linear predictor
  eta <- beta_0 + beta_1 * my_info$xobs
  
  # calculate the event probability
  mu <- boot::inv.logit(eta)
  
  # evaluate the log-likelihood
  log_lik <- sum(dbinom(x = my_info$yobs,
                        size = 1,
                        prob = mu,
                        log = TRUE))
  
  # evaluate the log-prior
  log_prior <- sum(dnorm(x = c(beta_0, beta_1),
                         mean = my_info$mu_beta,
                         sd = my_info$tau_beta,
                         log = TRUE))
  
  # sum together
  log_lik + log_prior
}

### define the list of required information using the a regularizing
### prior...prior standard deviation of 3
info_use <- list(
  xobs = train_df$x,
  yobs = train_df$outcome,
  mu_beta = 0,
  tau_beta = 3
)

### define the wrapper function
eval_logpost <- function(b0_val, b1_val, logpost_func, my_info)
{
  logpost_func(c(b0_val, b1_val), my_info)
}

### define the grid of intercept and slope values
beta_grid <- expand.grid(beta_0 = seq(-4, 4, length.out = 251),
                         beta_1 = seq(-4, 4, length.out = 251),
                         KEEP.OUT.ATTRS = FALSE,
                         stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

### calculate the log-posterior over the grid
log_post_2d_grid <- purrr::map2_dbl(beta_grid$beta_0,
                                    beta_grid$beta_1,
                                    eval_logpost,
                                    logpost_func = logistic_logpost,
                                    my_info = info_use)

### visualize the log-posterior surface
beta_grid %>% 
  mutate(log_post = log_post_2d_grid) %>% 
  mutate(log_post_2 = log_post - max(log_post)) %>% 
  ggplot(mapping = aes(x = beta_1, y = beta_0)) +
  geom_raster(mapping = aes(fill = log_post_2)) +
  stat_contour(mapping = aes(z = log_post_2),
               breaks = log(c(0.01/100, 0.01, 0.1, 0.5, 0.9)),
               size = 1.05,
               color = "black") +
  coord_fixed(ratio = 1) +
  scale_fill_viridis_c(guide = FALSE, option = "viridis",
                       limits = log(c(0.01/100, 1.0))) +
  labs(x = expression(beta[1]), y = expression(beta[0])) +
  theme_bw() +
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

### compare to a weakly infomrative prior with prior sd of 25
info_weak <- list(
  xobs = train_df$x,
  yobs = train_df$outcome,
  mu_beta = 0,
  tau_beta = 25
)

### calculate the log posterior surface
log_post_2d_grid_weak <- purrr::map2_dbl(beta_grid$beta_0,
                                         beta_grid$beta_1,
                                         eval_logpost,
                                         logpost_func = logistic_logpost,
                                         my_info = info_weak)

### visualize the log-posterior surface from the weak prior
beta_grid %>% 
  mutate(log_post = log_post_2d_grid_weak) %>% 
  mutate(log_post_2 = log_post - max(log_post)) %>% 
  ggplot(mapping = aes(x = beta_1, y = beta_0)) +
  geom_raster(mapping = aes(fill = log_post_2)) +
  stat_contour(mapping = aes(z = log_post_2),
               breaks = log(c(0.01/100, 0.01, 0.1, 0.5, 0.9)),
               size = 1.05,
               color = "black") +
  coord_fixed(ratio = 1) +
  scale_fill_viridis_c(guide = FALSE, option = "viridis",
                       limits = log(c(0.01/100, 1.0))) +
  labs(x = expression(beta[1]), y = expression(beta[0])) +
  theme_bw() +
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

### perform the laplace approximation

my_laplace <- function(start_guess, logpost_func, ...)
{
  # code adapted from the `LearnBayes`` function `laplace()`
  fit <- optim(start_guess,
               logpost_func,
               gr = NULL,
               ...,
               method = "BFGS",
               hessian = TRUE,
               control = list(fnscale = -1, maxit = 1001))
  
  mode <- fit$par
  post_var_matrix <- -solve(fit$hessian)
  p <- length(mode)
  int <- p/2 * log(2 * pi) + 0.5 * log(det(post_var_matrix)) + logpost_func(mode, ...)
  # package all of the results into a list
  list(mode = mode,
       var_matrix = post_var_matrix,
       log_evidence = int,
       converge = ifelse(fit$convergence == 0,
                         "YES", 
                         "NO"),
       iter_counts = as.numeric(fit$counts[1]))
}

### execute the laplace approximation using the regularizing prior
### does the starting guess matter?????
shuttle_laplace <- my_laplace(rep(0, 2), logistic_logpost, info_use)

shuttle_laplace

### generate posteiror samples

generate_glm_post_samples <- function(mvn_result, num_samples)
{
  # specify the number of unknown beta parameters
  length_beta <- length(mvn_result$mode)
  
  # generate the random samples
  beta_samples <- MASS::mvrnorm(n = num_samples, 
                                mu = mvn_result$mode, 
                                Sigma = mvn_result$var_matrix)
  
  # change the data type and name
  beta_samples %>% 
    as.data.frame() %>% tibble::as_tibble() %>% 
    purrr::set_names(sprintf("beta_%02d", (1:length_beta) - 1))
}

### generate the posterior samples
set.seed(912312)
post_betas <- generate_glm_post_samples(shuttle_laplace, 1e4)

### visualize the posterior density
post_betas %>% 
  ggplot(mapping = aes(x = beta_01, y = beta_00)) +
  geom_density2d_filled() +
  geom_density2d(color = "white") +
  theme_bw()

### visualize the posterior histograms
post_betas %>% tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 55) +
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### probability that the slope is greater than 0 using the samples

mean( post_betas$beta_01 > 0 )

### as a comparison, find the MLE to the coefficients

fit_glm <- glm(outcome ~ x, family = "binomial", data = train_df)

fit_glm %>% summary()
