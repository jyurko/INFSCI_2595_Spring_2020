### compare batch mode gradient descent with SGD

### use the simple toy problem with 1 input and 2 hidden units

### this example does not setup the most computationally efficient SGD aglorithm
### the point is to illustrate what happens and to compare the results with
### using all data points (batch mode)

library(tidyverse)

### set up the simple problem with 2 hidden units and the logistic
### non-linear transformation function

### specify the hidden unit parameters
b1 <- c(0.2, 1.2)
b2 <- c(-0.1, -0.8)

### set the input to be evenly spaced between -3 and +3
x <- seq(-3, 3, length.out = 25)

### create the design matrix
Xmat <- model.matrix( ~ x, data = data.frame(x = x))

### calculate the hidden unit linear predictors 
Bmat <- cbind(b1, b2)

Bmat

Amat <- Xmat %*% Bmat

### next calculate the non-linear transformed values per hidden unit
### assuming the logistic function

Hmat <- boot::inv.logit(Amat)

### now define the true output layer parameters
a0true <- 0.2
atrue <- c(-1.5, 1.25)

### calculate the true nnet output
ftrue <- a0true + Hmat %*% as.matrix(atrue)

### set the noise to a low value
sigma_true <- 0.15

### generate noisy observations and package 
### the input with the noisy response
set.seed(1131)
demo_df <- tibble::tibble(x = x) %>% 
  mutate(f = as.vector(ftrue),
         y = rnorm(n = n(), mean = f, sd = sigma_true))

### plot the training set
demo_df %>% 
  ggplot(mapping = aes(x = x)) +
  geom_line(mapping = aes(y = f),
            color = "grey50",
            size = 1.15) +
  geom_point(mapping = aes(y = y),
             size = 3, color = "red") +
  labs(y = "y") +
  theme_bw()

### setup the SSE function we wish to minimize 
### all parameters will be contained in the theta vector
### need to extract them appropriately

my_neuralnet_sse <- function(theta, my_info)
{
  # extract the hidden unit parameters
  X <- my_info$design_matrix
  length_beta <- ncol(X)
  total_num_betas <- my_info$num_hidden * length_beta
  beta_vec <- theta[1:total_num_betas]
  
  # extract the output layer parameters
  a_all <- theta[(total_num_betas + 1):length(theta)]
  
  # reorganize the beta parameters into a matrix
  Bmat <- matrix(beta_vec, nrow = length_beta, byrow = FALSE)
  
  # reorganize the output layer parameters by extracting
  # the output layer intercept (the bias)
  a0 <- a_all[1] # the first element in a_all is the bias
  av <- a_all[-1] # select all EXCEPT the first element
  
  # calculate the neural network in one line of code
  # your homework assignment breaks this operation 
  # into several lines, so do NOT copy and paste!!!
  # you may use this expression to help debug your code
  f <- as.vector( a0 + my_info$transform_hidden(X %*% Bmat) %*% matrix(av) )
  
  # performance metric - calculate the SSE
  sum((my_info$yobs - f)^2)
}

### assemble the list of required information
two_hidden_sse_info <- list(
  yobs = demo_df$y,
  design_matrix = model.matrix( y ~ x, demo_df),
  num_hidden = 2,
  transform_hidden = boot::inv.logit
)

### test out the function
two_hidden_sse_info$num_params <- two_hidden_sse_info$num_hidden * ncol(two_hidden_sse_info$design_matrix) +
  two_hidden_sse_info$num_hidden + 1

my_neuralnet_sse(rep(0, two_hidden_sse_info$num_params), two_hidden_sse_info)

my_neuralnet_sse(rep(0.25, two_hidden_sse_info$num_params), two_hidden_sse_info)

my_neuralnet_sse(rep(-1, two_hidden_sse_info$num_params), two_hidden_sse_info)

### fit the model using optim(), notice that we do NOT set the
### `fnscale` argument since we want to MINIMIZE the SSE

two_hidden_fit_a <- optim(rep(0, two_hidden_sse_info$num_params),
                          my_neuralnet_sse,
                          gr = NULL,
                          two_hidden_sse_info,
                          method = "BFGS",
                          hessian = TRUE,
                          control = list(maxit = 5001))

two_hidden_fit_a

### Put together functions to perform gradient descent

### calculate the gradient, but do so numerically

estimate_sse_grad <- function(unknowns, my_info)
{
  numDeriv::grad(my_neuralnet_sse, unknowns, method="Richardson", 
                 side=NULL, method.args=list(),
                 my_info)
}

### the gradient at the 0 guess
estimate_sse_grad(rep(0, two_hidden_sse_info$num_params), two_hidden_sse_info)

### update the parameters using the gradient and defined step size
grad_descent_update <- function(unknowns, my_info, step_size)
{
  g <- estimate_sse_grad(unknowns, my_info)
  
  unknowns - step_size * g
}

### define a function which executes the gradient descent optimization from a starting guess
### just specify the number of steps for now for simplicity

run_grad_descent <- function(start_guess, num_steps, step_size, my_info)
{
  # initialize
  res <- vector(mode = "list", length = num_steps + 1)
  res[[1]] <- start_guess
  
  for(k in 2:(num_steps+1)){
    res[[k]] <- grad_descent_update(res[[k - 1]], my_info, step_size)
  }
  
  res
}

### test out running the optimization

run_grad_descent(rep(0, two_hidden_sse_info$num_params), 3, step_size = 0.5e-3, two_hidden_sse_info)

### define a function which "tidys" the results
tidy_grad_descent_results <- function(res, iter_id)
{
  purrr::map2_dfr(res, iter_id,
                  function(ll, id){tibble::tibble(theta = ll) %>% 
                      mutate(iter_id = id) %>% 
                      tibble::rowid_to_column("theta_id")})
}

### define a function to run the gradient descent algorithm and tidy the results

run_tidy_gradient_descent <- function(start_guess, num_steps, step_size, my_info)
{
  my_res <- run_grad_descent(start_guess, num_steps, step_size, my_info)
  
  tidy_grad_descent_results(my_res, seq_along(my_res) - 1)
}

### run the optimization from the starting guess of 0

opt_iterations_0 <- run_tidy_gradient_descent(rep(0, two_hidden_sse_info$num_params), 
                                              2500, 
                                              step_size = 10e-3, 
                                              two_hidden_sse_info)

opt_iterations_0 %>% glimpse()

### plot the iteration results per unknown coefficient, compare to the
### optim() based results from that starting guess of zero

opt_iterations_0 %>% 
  ggplot(mapping = aes(x = iter_id, y = theta)) +
  geom_hline(data = tibble::tibble(optim_est = two_hidden_fit_a$par) %>% 
               tibble::rowid_to_column("theta_id"),
             mapping = aes(yintercept = optim_est),
             color = "red", linetype = "dashed", size = 1.) +
  geom_line(mapping = aes(group = theta_id)) +
  facet_wrap(~theta_id, labeller = "label_both") +
  theme_bw()

### zoom in on each parameter
opt_iterations_0 %>% 
  ggplot(mapping = aes(x = iter_id, y = theta)) +
  geom_hline(data = tibble::tibble(optim_est = two_hidden_fit_a$par) %>% 
               tibble::rowid_to_column("theta_id"),
             mapping = aes(yintercept = optim_est),
             color = "red", linetype = "dashed", size = 1.) +
  geom_line(mapping = aes(group = theta_id)) +
  facet_wrap(~theta_id, labeller = "label_both", scales = "free_y") +
  theme_bw()

### now consider stochastic gradient descent (SGD) by randomly selecting
### a single data point at each iteration

sgd_update <- function(unknowns, my_info, step_info)
{
  # randomly select observations
  n_id <- sample(1:length(my_info$yobs), step_info$batch_size, replace = FALSE)
  # subset the training set correctly
  my_info$yobs <- my_info$yobs[n_id]
  my_info$design_matrix <- my_info$design_matrix[n_id, , drop = FALSE]
  
  g <- estimate_sse_grad(unknowns, my_info)
  
  unknowns - step_info$step_size * g
}

run_sgd <- function(start_guess, num_steps, step_info, my_info)
{
  # initialize
  res <- vector(mode = "list", length = num_steps + 1)
  res[[1]] <- start_guess
  
  for(k in 2:(num_steps+1)){
    res[[k]] <- sgd_update(res[[k - 1]], my_info, step_info)
  }
  
  res
}

### test
set.seed(12345)
run_sgd(rep(0, two_hidden_sse_info$num_params), 3, 
        step_info = list(batch_size = 1, step_size = 10e-3), 
        two_hidden_sse_info)

### define a function which "tidys" the results
tidy_sgd_results <- function(res, iter_id)
{
  purrr::map2_dfr(res, iter_id,
                  function(ll, id){tibble::tibble(theta = ll) %>% 
                      mutate(iter_id = id) %>% 
                      tibble::rowid_to_column("theta_id")})
}

### define a function to run the gradient descent algorithm and tidy the results

run_tidy_sgd <- function(start_guess, num_steps, step_info, my_info)
{
  my_res <- run_sgd(start_guess, num_steps, step_info, my_info)
  
  tidy_sgd_results(my_res, seq_along(my_res) - 1)
}

### run the optimization from the starting guess of 0

set.seed(12345)
sgd_iterations_0 <- run_tidy_sgd(rep(0, two_hidden_sse_info$num_params), 
                                 2500, 
                                 step_info = list(batch_size = 1, step_size = 0.1), 
                                 two_hidden_sse_info)

sgd_iterations_0 %>% glimpse()

### compare the gradient descent "BATCH" mode results to the SGD results
opt_iterations_0 %>% tibble::rowid_to_column() %>% 
  mutate(type = 'BATCH') %>% 
  bind_rows(sgd_iterations_0 %>% tibble::rowid_to_column() %>% 
              mutate(type = "SGD")) %>% 
  ggplot(mapping = aes(x = iter_id, y = theta)) +
  geom_hline(data = tibble::tibble(optim_est = two_hidden_fit_a$par) %>% 
               tibble::rowid_to_column("theta_id"),
             mapping = aes(yintercept = optim_est),
             color = "red", linetype = "dashed", size = 1.) +
  geom_line(mapping = aes(group = interaction(theta_id, type),
                          color = type)) +
  facet_wrap(~theta_id, labeller = "label_both", scales = "free_y") +
  ggthemes::scale_color_colorblind("") +
  theme_bw()

### try mini-batch, use 5 data points 
set.seed(12345)
sgd_batch_5 <- run_tidy_sgd(rep(0, two_hidden_sse_info$num_params), 
                            2500, 
                            step_info = list(batch_size = 5, step_size = 0.1), 
                            two_hidden_sse_info)

### try mini-batch use 12 data points, but needed to modify the step size...
set.seed(12345)
sgd_batch_12 <- run_tidy_sgd(rep(0, two_hidden_sse_info$num_params), 
                             2500, 
                             step_info = list(batch_size = 12, step_size = 0.01), 
                             two_hidden_sse_info)


### compare all optimizations results
opt_iterations_0 %>% tibble::rowid_to_column() %>% 
  mutate(type = 'BATCH') %>% 
  bind_rows(sgd_iterations_0 %>% tibble::rowid_to_column() %>% 
              mutate(type = "SGD-01")) %>% 
  bind_rows(sgd_batch_5 %>% tibble::rowid_to_column() %>% 
              mutate(type = "SGD-05")) %>% 
  bind_rows(sgd_batch_12 %>% tibble::rowid_to_column() %>% 
              mutate(type = "SGD-12")) %>% 
  ggplot(mapping = aes(x = iter_id, y = theta)) +
  geom_hline(data = tibble::tibble(optim_est = two_hidden_fit_a$par) %>% 
               tibble::rowid_to_column("theta_id"),
             mapping = aes(yintercept = optim_est),
             color = "red", linetype = "dashed", size = 1.) +
  geom_line(mapping = aes(group = interaction(theta_id, type),
                          color = type,
                          alpha = type)) +
  facet_wrap(~theta_id, labeller = "label_both", scales = "free_y") +
  ggthemes::scale_color_colorblind("") +
  scale_alpha_manual("", 
                     values = c("BATCH" = 1.,
                                "SGD-01" = 0.5,
                                "SGD-05" = 0.5,
                                'SGD-12' = 0.85)) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1.0, size=1.1)))
