
#Q3.
install.packages("tidymodels")
library(tidymodels)

install.packages("glmnet")
library(glmnet)


#Q4.
library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)


housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

names(housing)

#Q5.
set.seed(123456)





#Q6.
install.packages("rsample")
library(rsample)

housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)



#Q7.

library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)

housing_recipe <- recipe(medv ~ ., data = housing) %>% 
  # convert outcome variable to logs 
  step_log(all_outcomes()) %>% 
  # convert 0/1 chas to a factor 
  step_bin2factor(chas) %>% 
  # create interaction term between crime and nox 
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>% 
  # create square terms of some continuous variables 
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b, lstat,dis,nox, degree=6)
  


# Run the recipe 
housing_prep <- housing_recipe %>% prep(housing_train, retain  = TRUE)
housing_train_prepped <- housing_prep %>% juice 
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)




# create x and y training and test data 
housing_train_x <- housing_train_prepped %>% select(-medv) 
housing_test_x <- housing_test_prepped %>% select(-medv) 
housing_train_y <- housing_train_prepped %>% select( medv) 
housing_test_y <- housing_test_prepped %>% select( medv)

 
dim(housing_train)
# the dimension of housing_train: observations=404 & variables=14

dim(housing_train_prepped)
# the dimension of housing_train_preppaed: observations=404 & variables=75

# How many more X variables do you have than in the original housing data?
# : more 61 variables than origianl housing data




#Q8.
# Following the example from the lecture notes, 
# estimate a LASSO model to predict log median house value,
# where the penalty parameter λ is tuned by 6-fold cross validation. 
# What is the optimal value of λ? What is the in-sample RMSE? 
# What is the out-of-sample RMSE (i.e. the RMSE in the test data)?


lasso_spec <- linear_reg(penalty=0.5,mixture=1) %>%      
  set_engine("glmnet") %>%   
  set_mode("regression") 

lasso_fit <- lasso_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

# predict RMSE in sample
lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict RMSE out of sample
lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict R2 in sample
lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# predict R2 out of sample
lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# in-sample RMSE was 0.413
# out-of-sample RMSE is 0.390
# in-sample R^2 was 0
# out-of-sample R^2 is -0.000467


tune_spec <- linear_reg(
  penalty = tune(), 
  mixture = 1       
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")


lambda_grid <- grid_regular(penalty(), levels = 50)


rec_folds <- vfold_cv(housing_train_prepped, v = 6)


rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) #%>%



rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")


final_lasso <- finalize_workflow(rec_wf, best_rmse)


last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print
# rsme: 0.170
# rsq: 0.809

top_rmse %>% print(n = 1)
# penalty: 0.00139
# mean: 0.0632
# n : 6
# std_err: 0.00503












#Q9.
# Repeat the previous question, but now estimate a ridge regression model 
# where again the penalty parameter λ is tuned by 6-fold CV. 
# What is the optimal value of λ now? 
# What is the out-of-sample RMSE (i.e. the RMSE in the test data)?


ridge_spec <- linear_reg(penalty=0.5,mixture=0) %>%      
  set_engine("glmnet") %>%   
  set_mode("regression") 

ridge_fit <- ridge_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

# predict RMSE in sample
ridge_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict RMSE out of sample
ridge_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# predict R2 in sample
ridge_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# predict R2 out of sample
ridge_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# in-sample RMSE was 0.197
# out-of-sample RMSE is 0.194
# in-sample R^2 was 0.773
# out-of-sample R^2 is 0.752



tune_spec <- linear_reg(
  penalty = tune(), 
  mixture = 0       
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")


lambda_grid <- grid_regular(penalty(), levels = 50)


rec_folds <- vfold_cv(housing_train_prepped, v = 6)


rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) #%>%



rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")


final_lasso <- finalize_workflow(rec_wf, best_rmse)


last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print
# rsme: 0.173
# rsq: 0.802

top_rmse %>% print(n = 1)
# penalty: 0.0373
# mean: 0.0716
# n : 6
# std_err: 0.00585















