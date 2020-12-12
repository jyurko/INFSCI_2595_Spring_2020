### Example using interactions of multiple inputs

### this example compares elastic net models (using GLMNET) on the concrete data set
### the elastic net models are first fit on the entire data set and compared using
### AIC/BIC to get an idea about the behavior of these two metrics

### linear additive terms are compared to models with interactions. 
### multiple models are tried, including a 5 way interaction model
### I would NOT recommend fitting a 5 way interaction model...however
### it's a useful exercise to try out to get an idea about how the 
### elastic net is capable to TURNING OFF predictors via the LASSO
### penalty term

### use the concrete regression problem example

library(tidyverse)

### go get the data, if you have not downloaded the data already
### please run the following 3 lines of code. 
uci_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls"

download.file(uci_url, destfile = "concrete_data.xls", mode = "wb")

concrete <- readxl::read_xls("concrete_data.xls", col_names = TRUE)

concrete %>% names()

### change the names so they are easier to work with
my_concrete <- concrete %>% 
  purrr::set_names(c("cement", "slag", "flyash", "water", "superplast", "coarseagg",
                     "fineagg", "age", "strength"))


my_concrete %>% glimpse()

### plot the response vs the inputs
my_concrete %>% 
  tibble::rowid_to_column("obs_id") %>% 
  tidyr::gather(key = "key", value = "value", -obs_id, -strength) %>% 
  ggplot(mapping = aes(x = value, y = strength)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw()

### we can fit a linear ADDITIVE model without typing all the input names
### using the `.` shortcut which stands for "EVERTHING ELSE" in the dataset

### check the model matrix

model.matrix(strength ~ ., data = my_concrete) %>% head()

### we can easily apply the standardization using caret

library(caret)

### let's first fit the model on the training set directly, so
### NO resampling, force `caret` to NOT resample

ctrl_none <- trainControl(method = "none")

my_metric <- "RMSE"

### fit a linear model without resampling all additive terms

fit_lm_01 <- train(strength ~ ., data = my_concrete,
                   method = "lm",
                   metric = my_metric,
                   preProcess = c("center", "scale"),
                   trControl = ctrl_none)

fit_lm_01

broom::glance(fit_lm_01$finalModel)

coefplot::coefplot(fit_lm_01$finalModel) + theme_bw()

### remove the intercept

coefplot::coefplot(fit_lm_01$finalModel, 
                   coefficients = names(coef(fit_lm_01$finalModel))[-1]) +
  theme_bw()

### what if I only wanted to consider a few inputs? but was interested
### in their interaction?

model.matrix(strength ~ cement * age, my_concrete) %>% head()

fit_lm_02 <- train(strength ~ cement * age, 
                   data = my_concrete,
                   method = "lm",
                   metric = my_metric,
                   preProcess = c("center", "scale"),
                   trControl = ctrl_none)

fit_lm_02

coefplot::coefplot(fit_lm_02$finalModel, 
                   coefficients = names(coef(fit_lm_02$finalModel))[-1]) +
  theme_bw()

### if I want ALL pair wise interactions and MAIN effects I don't have to
### type this by hand either...I can use a short cut

model.matrix(strength ~ (.)^2, my_concrete) %>% head()

model.matrix(strength ~ (.)^2, my_concrete) %>% colnames()

### fit the model with all pair wise interactions

fit_lm_03 <- train(strength ~ (.)^2, 
                   data = my_concrete,
                   method = "lm",
                   metric = my_metric,
                   preProcess = c("center", "scale"),
                   trControl = ctrl_none)

fit_lm_03

coefplot::coefplot(fit_lm_03$finalModel, 
                   coefficients = names(coef(fit_lm_03$finalModel))[-1]) +
  theme_bw()

### what if we wanted triplet interactions?

model.matrix(strength ~ cement * age * flyash, my_concrete) %>% head()

fit_lm_04 <- train(strength ~ cement * age * flyash, 
                   data = my_concrete,
                   method = "lm",
                   metric = my_metric,
                   preProcess = c("center", "scale"),
                   trControl = ctrl_none)

fit_lm_04

coefplot::coefplot(fit_lm_04$finalModel, 
                   coefficients = names(coef(fit_lm_04$finalModel))[-1]) +
  theme_bw()

### all TRIPLET interactions?

model.matrix(strength ~ (.)^3, my_concrete) %>% colnames()

fit_lm_05 <- train(strength ~ (.)^3, 
                   data = my_concrete,
                   method = "lm",
                   metric = my_metric,
                   preProcess = c("center", "scale"),
                   trControl = ctrl_none)

fit_lm_05

coefplot::coefplot(fit_lm_05$finalModel, 
                   coefficients = names(coef(fit_lm_05$finalModel))[-1]) +
  theme_bw()

### what about 4 way interactions?

model.matrix(strength ~ (.)^4, my_concrete) %>% dim()

fit_lm_06 <- train(strength ~ (.)^4, 
                   data = my_concrete,
                   method = "lm",
                   metric = my_metric,
                   preProcess = c("center", "scale"),
                   trControl = ctrl_none)

fit_lm_06

### 5 way interactions???

model.matrix(strength ~ (.)^5, my_concrete) %>% dim()

model.matrix(strength ~ (.)^5, my_concrete) %>% colnames()

fit_lm_07 <- train(strength ~ (.)^5, 
                   data = my_concrete,
                   method = "lm",
                   metric = my_metric,
                   preProcess = c("center", "scale"),
                   trControl = ctrl_none)

fit_lm_07

### can use other features though! try a 4 degree of freedom spline applied to `age`
### which interacts with 3 other inputs

model.matrix(strength ~ splines::ns(age, 4)*(cement + water + superplast), my_concrete) %>% colnames()

fit_lm_08 <- train(strength ~ splines::ns(age, 4)*(cement + water + superplast), 
                   data = my_concrete,
                   method = "lm",
                   metric = my_metric,
                   preProcess = c("center", "scale"),
                   trControl = ctrl_none)

fit_lm_08

### which model is better?

extract_metrics <- function(mod_object, mod_name)
{
  broom::glance(mod_object) %>% 
    mutate(model_name = mod_name)
}

all_fit_metrics <- purrr::map2_dfr(list(fit_lm_01$finalModel, 
                                        fit_lm_02$finalModel,
                                        fit_lm_03$finalModel,
                                        fit_lm_04$finalModel,
                                        fit_lm_05$finalModel,
                                        fit_lm_06$finalModel,
                                        fit_lm_07$finalModel,
                                        fit_lm_08$finalModel),
                                   c("all additive", 
                                     "2 inputs with interaction",
                                     "all pair wise interaction",
                                     "3 inputs with interactions",
                                     "all triplets",
                                     "all 4-way",
                                     "all 5-way",
                                     "age spline with interactions"),
                                   extract_metrics)

all_fit_metrics

### plot AIC, BIC, and r-squared
all_fit_metrics %>% 
  select(model_name, r.squared, AIC, BIC) %>% 
  tidyr::gather(key = "key", value = "value", -model_name) %>% 
  ggplot(mapping = aes(x = model_name, y = value)) +
  geom_line(size = 1.25, mapping = aes(group = key)) +
  geom_point(size = 7) +
  coord_flip() +
  facet_wrap(~key, scales = "free_x") +
  theme_bw()

### now use GLMNET to handle up to the 5 way interaction
### tune the penalty factor and the mixing fraction using resampling

### elastic net will try and "turn off" predictors via the lasso
### penalty. the pure lasso model corresponds to a mixing fraction
### of 1.mixing fractions around 0.5 are a blend between lasso 
### and ridge

my_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

set.seed(71312)
fit_glmnet_5 <- train(strength ~ (.)^5,
                      data = my_concrete,
                      method = "glmnet",
                      metric = my_metric,
                      preProcess = c("center", "scale"),
                      trControl = my_ctrl)

fit_glmnet_5

ggplot(fit_glmnet_5) + theme_bw()

### create a custom tuning grid

enet_grid <- expand.grid(alpha = seq(0.1, 0.9, by = 0.1),
                         lambda = exp(seq(-6, 0.5, length.out = 25)))

set.seed(71312)
fit_glmnet_5_b <- train(strength ~ (.)^5,
                        data = my_concrete,
                        method = "glmnet",
                        tuneGrid = enet_grid,
                        metric = my_metric,
                        preProcess = c("center", "scale"),
                        trControl = my_ctrl)

plot(fit_glmnet_5_b, xTrans=log)

### look at the coefficients wrt the log of the regularization factor lambda

plot(fit_glmnet_5_b$finalModel, xvar='lambda', label=TRUE)

### what's the optimal tuning paramteters?
fit_glmnet_5_b$bestTune

### the log lambda value
log(fit_glmnet_5_b$bestTune$lambda)

### printing the coefficients will look a little weird...
coef(fit_glmnet_5_b$finalModel)

### so instead specify the optimal value of lambda identified by the
### resampling

coef(fit_glmnet_5_b$finalModel, s = fit_glmnet_5_b$bestTune$lambda)

### how many non-zero features?
coef(fit_glmnet_5_b$finalModel, s = fit_glmnet_5_b$bestTune$lambda) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("coef_name") %>% 
  tibble::as_tibble() %>% 
  purrr::set_names(c("coef_name", "coef_value")) %>% 
  filter(coef_value != 0) %>% 
  nrow()

### use cross validation on all pair wise interactions to have as a comparison
### since it has fewer overall features than the 5 way interaction model
### will help give context to the influence of the remaining features above the pair-wise
### interactions terms

set.seed(71312)
fit_glmnet_2 <- train(strength ~ (.)^2,
                      data = my_concrete,
                      method = "glmnet",
                      tuneGrid = enet_grid,
                      metric = my_metric,
                      preProcess = c("center", "scale"),
                      trControl = my_ctrl)

fit_glmnet_2

plot(fit_glmnet_2, xTrans=log)

### try the custom model with the interactions with the spline features
set.seed(71312)
fit_glmnet_custom <- train(strength ~ splines::ns(age, 4)*(cement + water + superplast),
                           data = my_concrete,
                           method = "glmnet",
                           tuneGrid = enet_grid,
                           metric = my_metric,
                           preProcess = c("center", "scale"),
                           trControl = my_ctrl)

plot(fit_glmnet_custom, xTrans=log)

### and include the linear additive model as comparison

set.seed(71312)
fit_lm_additive <- train(strength ~ .,
                         data = my_concrete,
                         method = "lm",
                         metric = my_metric,
                         preProcess = c("center", "scale"),
                         trControl = my_ctrl)

### compare all the resampling results

my_results <- resamples(list(LM = fit_lm_additive,
                             GLMNET_5way = fit_glmnet_5_b,
                             GLMNET_2way = fit_glmnet_2,
                             GLMNET_custom = fit_glmnet_custom))

### default plot method
dotplot(my_results, metric = "RMSE")

### or with ggplot
ggplot(my_results, metric = "RMSE") + theme_bw()

ggplot(my_results, metric = "Rsquared") + theme_bw()

ggplot(my_results, metric = "MAE") + theme_bw()


