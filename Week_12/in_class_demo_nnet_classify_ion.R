library(dplyr)
library(ggplot2)

### use ionosphere data set that we used in week 05

data("Ionosphere", package = "mlbench")

### remove first 2 variables turn the class into a factor
my_ion <- Ionosphere %>% 
  select(-V1, -V2) %>% 
  mutate(Class = factor(Class, levels = c("good", "bad")))

### use the neuralnet package to fit the model

library(neuralnet)

### try a simple single layer with a few hidden units
set.seed(41231)
mod_1layer_small <- neuralnet(Class ~ .,
                              data = my_ion,
                              hidden = 3,
                              err.fct = 'ce',
                              act.fct = 'logistic',
                              linear.output = FALSE,
                              likelihood = TRUE)

plot(mod_1layer_small, rep = "best", show.weights = TRUE)

mod_1layer_small$result.matrix %>% as.data.frame()

### let's build a single layer with more hidden units
set.seed(41231)
mod_1layer <- neuralnet(Class ~ .,
                        data = my_ion,
                        hidden = 15,
                        err.fct = 'ce',
                        act.fct = 'logistic',
                        linear.output = FALSE,
                        likelihood = TRUE)

plot(mod_1layer, rep = "best", show.weights = FALSE, intercept = FALSE)

### use 2 hidden layers with a few hidden units in each layer
set.seed(41231)
mod_2layers_small <- neuralnet(Class ~ .,
                               data = my_ion,
                               hidden = c(3, 3),
                               err.fct = 'ce',
                               act.fct = 'logistic',
                               linear.output = FALSE,
                               likelihood = TRUE)

plot(mod_2layers_small, rep = "best", show.weights = TRUE, intercept = TRUE)

### use 2 hidden layers with more hidden units
set.seed(41231)
mod_2layers <- neuralnet(Class ~ .,
                         data = my_ion,
                         hidden = c(15, 7),
                         err.fct = 'ce',
                         act.fct = 'logistic',
                         linear.output = FALSE,
                         likelihood = TRUE)

plot(mod_2layers, rep = "best", show.weights = FALSE, intercept = FALSE)

### use 3 hidden layers with a small number of hidden units
set.seed(41231)
mod_3layers_small <- neuralnet(Class ~ .,
                               data = my_ion,
                               hidden = c(3, 3, 2),
                               err.fct = 'ce',
                               act.fct = 'logistic',
                               linear.output = FALSE,
                               likelihood = TRUE)

plot(mod_3layers_small, rep = "best", show.weights = TRUE, intercept = TRUE)

### use 3 hidden layers but with more hidden units in each layer
set.seed(41231)
mod_3layers <- neuralnet(Class ~ .,
                         data = my_ion,
                         hidden = c(15, 7, 5),
                         err.fct = 'ce',
                         act.fct = 'logistic',
                         linear.output = FALSE,
                         likelihood = TRUE)

plot(mod_3layers, rep = "best", show.weights = FALSE, intercept = FALSE)

### use information criterion metrics - AIC/BIC - which are non-bayesian
### approximations to the log-Evidence
mod_1layer_small$result.matrix %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tbl_df() %>% 
  slice(1:5) %>% 
  tidyr::spread(rowname, V1) %>% 
  mutate(layer1 = 3, layer2 = 0, layer3 = 0) %>% 
  bind_rows(mod_1layer$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 15, layer2 = 0, layer3 = 0)) %>% 
  bind_rows(mod_2layers_small$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 3, layer2 = 3, layer3 = 0)) %>% 
  bind_rows(mod_2layers$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 15, layer2 = 7, layer3 = 0)) %>% 
  bind_rows(mod_3layers_small$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 3, layer2 = 3, layer3 = 2)) %>% 
  bind_rows(mod_3layers$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 15, layer2 = 7, layer3 = 5)) %>% 
  arrange(aic, bic)

### use k-fold cross-validation to assess performance 

### use caret to perform the cross-validation and manage the book keeping

### we will tune the number of hidden units and the WEIGHT DECAY

library(caret)

### specify the resampling scheme
ctrl_cv05 <- trainControl(method = "cv", number = 5)

### create a grid search to tune the number of hidden units
### and the regularization
nnet_grid <- expand.grid(size = c(3, 4, 10, 15),
                         decay = exp(seq(-6, 3, length.out = 31)))

set.seed(31311)
fit_nnet <- train(Class ~ .,
                  data = my_ion,
                  method = "nnet",
                  metric = "Accuracy",
                  tuneGrid = nnet_grid,
                  preProc = c("center", "scale"),
                  trControl = ctrl_cv05,
                  trace=FALSE)

fit_nnet

plot(fit_nnet, xTrans = log)

### to access the specific tuning parameters that performed
### the best:
fit_nnet$bestTune

### access the model object directly
fit_nnet$finalModel

### visualize the nnet network
library(NeuralNetTools)

plotnet(fit_nnet$finalModel)
