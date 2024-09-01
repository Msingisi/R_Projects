# Import libraries
library(tidyverse)
library(tidymodels)

# Importing the dataset
data = read_csv('C:\\Users\\mzing\\Documents\\Projects\\R-Projects\\Credit\\dataset_31_credit-g.csv')
View(data)

# This dataset contains details about past credit applicants, 
# including why they are applying, checking and saving status, home ownership, etc. 
# The last column contains the outcome variable, namely whether this person is actually a good or bad credit risk.

data = mutate_if(data, is.character, as.factor)
skimr::skim(data)

# Home owners have a higher chance of being a good credit risk than people who rent or who live without paying (e.g. with family), 
# but the difference is lower than you might expect (74% vs 61%).

table(data$housing, data$class) |>
  prop.table(margin = 1)

# So, asking for a longer or higher loan is negatively correlated with being a good credit risk,
# while being older is a positive correlate.

data |> 
  mutate(class=ifelse(class=='good', 1, 0)) |>
  select_if(is.numeric) |>
  cor(use = "complete.obs") |>
  corrplot::corrplot(diag=F, type = "lower")

# So, we can see that being a foreign worker or having a deficit on your checking account
# make you relatively more likely to be a bad credit risk.

data |> 
  select_if(is.factor) |>
  pivot_longer(-class) |>
  ggplot(aes(y=value, fill=class)) + geom_bar(position='fill') + facet_wrap("name", scales="free", ncol=2)

# Training and test data
# We use the initial_split function from rsample, which by defaults gives a 75%/25% split

set.seed(123)
split = initial_split(data, strata = class)
train <- training(split)
test <- testing(split)

library(naivebayes)
library(discrim)
nb_spec = naive_Bayes(engine="naivebayes")

nb_spec |> translate()
# Fitting the model
# Next, we use this model to fit the training data:
  
nb_model = fit(nb_spec, class ~ ., data=train)
# Validating the model
# So, how well can this model predict the credit risk? Let’s get the predicted classes for the test set:
  
predictions = predict(nb_model, new_data=test) |>
  bind_cols(select(test, class)) |>
  rename(predicted=.pred_class, actual=class)
head(predictions)

print(paste("Accuracy:", mean(predictions$predicted == predictions$actual)))
table(predictions$predicted, predictions$actual)

# So, on the training set it gets 68% accuracy,
# which is not very good for a two-way classification. We can see that the most common error is mistakenly assigning a good credit score.
  
metrics = metric_set(accuracy, precision, recall, f_meas)
metrics(predictions, truth = actual, estimate = predicted)
# We can see that the model is pretty bad at predicting bad credit risks,
# with a precision of 46% but a recall of about 35%.

# SVM: Tuning models, recipes, and workflows

svm_spec = svm_poly(degree=2, engine="kernlab", mode="classification")
svm_model = fit(svm_spec, class ~ ., data=train)
predictions = predict(svm_model, new_data=test) |>
  bind_cols(select(test, class)) |>
  rename(predicted=.pred_class, actual=class)
metrics(predictions, truth = actual, estimate = predicted)

# This already improves the F measure quite a bit, although the result is still far from great.

# Using preprocessing recipes
  
svm_recipe = recipe(class ~ ., data=train) |>
step_scale(all_numeric_predictors()) |>
step_center(all_numeric_predictors()) |>
step_dummy(all_nominal_predictors()) |>
step_zv(all_predictors())
svm_recipe

prepped = prep(svm_recipe, data=train)
bake(prepped, new_data=train)
# The numerical values are centered and the factors are turned into dummies

# Creating workflows

svm_workflow = workflow() |>
  add_recipe(svm_recipe) |>
  add_model(svm_spec)

# Using workflows

svm_model <- fit(svm_workflow, data = train)
predict(svm_model, new_data=test) |>
  bind_cols(select(test, class)) |>
  rename(predicted=.pred_class, actual=class) |>
  metrics(truth = actual, estimate = predicted)

# Our model performance actually decreased a tiny bit

result = list()
for (cost in 10^(-1:2)) {
  for (degree in 1:3) {
    result[[paste(cost, degree, sep="_")]] = svm_poly(cost=cost, degree=degree, engine="kernlab", mode="classification") |>
      fit(class ~ ., data=train) |>
      predict(new_data=test) |>
      bind_cols(select(test, class)) |>
      metrics(truth = class, estimate = .pred_class) |>
      add_column(cost=cost, degree=degree)
  }
}
bind_rows(result) |> 
  pivot_wider(names_from=.metric, values_from = .estimate) |>
  arrange(-f_meas)

#Grid search with tune
  
svm_workflow = workflow() |>
  add_recipe(svm_recipe) |>
  add_model(svm_poly(mode="classification", cost=tune(), degree=tune()))

grid = svm_workflow %>%
  parameters() %>%
  grid_regular(levels=c(cost=7, degree=3))
grid

# To tune this, we use cross validation:
  
set.seed(456)
folds <- vfold_cv(train, v = 5)

doParallel::registerDoParallel()
grid_results = svm_workflow %>% 
  tune_grid(resamples = folds, grid = grid, metrics = metrics)

collect_metrics(grid_results)
show_best(grid_results, metric = "f_meas")

grid_results %>%
  collect_metrics() %>%
  filter(.metric=="f_meas") |>
  mutate(degree=as.factor(degree)) |>
  ggplot(aes(x=cost, y=mean, color = degree,)) +
  geom_ribbon(aes(
    ymin = mean - std_err,
    ymax = mean + std_err,
    fill=degree
  ),lwd=0, alpha = 0.1) +
  geom_line() +
  scale_x_log10() 

# Using the best model to predict
# To select the best model specification from the grid result

final_svm = grid_results |>
  select_best() |>
  finalize_workflow(x=svm_workflow)
# And use it to fit on the full training set and test it on the test set:
  
svm_model <- fit(final_svm, data = train)
predict(svm_model, new_data=test) |>
  bind_cols(select(test, class)) |>
  rename(predicted=.pred_class, actual=class) |>
  metrics(truth = actual, estimate = predicted)