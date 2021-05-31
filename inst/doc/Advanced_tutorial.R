## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  cache      = FALSE,
  dpi = 72,
  fig.width  = 10,
  fig.height = 7,
  fig.align  = 'center'
  )


## -----------------------------------------------------------------------------
library(fairmodels)

data("adult")
head(adult)
# for this vignette data will be truncated



## -----------------------------------------------------------------------------
library(gbm)
library(DALEX)

adult$salary   <- as.numeric(adult$salary) -1 # 0 if bad and 1 if good risk
protected     <- adult$sex
adult <- adult[colnames(adult) != "sex"] # sex not specified

# making model
set.seed(1)
gbm_model <-gbm(salary ~. , data = adult, distribution = "bernoulli")

# making explainer
gbm_explainer <- explain(gbm_model,
                         data = adult[,-1],
                         y = adult$salary,
                         colorize = FALSE)

model_performance(gbm_explainer)

## -----------------------------------------------------------------------------
fobject <- fairness_check(gbm_explainer, 
                          protected  = protected, 
                          privileged = "Male", 
                          colorize = FALSE)

## -----------------------------------------------------------------------------
print(fobject, colorize = FALSE)

## -----------------------------------------------------------------------------
plot(fobject)

## -----------------------------------------------------------------------------
data_fixed <- disparate_impact_remover(data = adult, protected = protected, 
                            features_to_transform = c("age", "hours_per_week",
                                                      "capital_loss",
                                                      "capital_gain"))

set.seed(1)
gbm_model     <- gbm(salary ~. , data = data_fixed, distribution = "bernoulli")
gbm_explainer_dir <- explain(gbm_model,
                             data = data_fixed[,-1],
                             y = adult$salary,
                             label = "gbm_dir",
                             verbose = FALSE)

## -----------------------------------------------------------------------------
fobject <- fairness_check(gbm_explainer, gbm_explainer_dir,
                          protected = protected, 
                          privileged = "Male",
                          verbose = FALSE)
plot(fobject)

## -----------------------------------------------------------------------------
weights <- reweight(protected = protected, y = adult$salary)

set.seed(1)
gbm_model     <- gbm(salary ~. ,
                     data = adult,
                     weights = weights,
                     distribution = "bernoulli")

gbm_explainer_w <- explain(gbm_model,
                           data = adult[,-1],
                           y = adult$salary,
                           label = "gbm_weighted",
                           verbose = FALSE)

fobject <- fairness_check(fobject, gbm_explainer_w, verbose = FALSE)

plot(fobject)

## -----------------------------------------------------------------------------
# to obtain probs we will use simple linear regression
probs <- glm(salary ~., data = adult, family = binomial())$fitted.values

uniform_indexes      <- resample(protected = protected,
                                 y = adult$salary)
preferential_indexes <- resample(protected = protected,
                                 y = adult$salary,
                                 type = "preferential",
                                 probs = probs)

set.seed(1)
gbm_model     <- gbm(salary ~. ,
                     data = adult[uniform_indexes,],
                     distribution = "bernoulli")

gbm_explainer_u <- explain(gbm_model,
                           data = adult[,-1],
                           y = adult$salary,
                           label = "gbm_uniform",
                           verbose = FALSE)

set.seed(1)
gbm_model     <- gbm(salary ~. ,
                     data = adult[preferential_indexes,],
                     distribution = "bernoulli")

gbm_explainer_p <- explain(gbm_model,
                           data = adult[,-1],
                           y = adult$salary,
                           label = "gbm_preferential",
                           verbose = FALSE)

fobject <- fairness_check(fobject, gbm_explainer_u, gbm_explainer_p, 
                          verbose = FALSE)
plot(fobject)

## -----------------------------------------------------------------------------
# we will need normal explainer 
set.seed(1)
gbm_model <-gbm(salary ~. , data = adult, distribution = "bernoulli")
gbm_explainer <- explain(gbm_model,
                         data = adult[,-1],
                         y = adult$salary,
                         verbose = FALSE)

gbm_explainer_r <- roc_pivot(gbm_explainer,
                             protected = protected,
                             privileged = "Male")


fobject <- fairness_check(fobject, gbm_explainer_r, 
                          label = "gbm_roc",  # label as vector for explainers
                          verbose = FALSE) 

plot(fobject)

## -----------------------------------------------------------------------------
print(fobject, colorize = FALSE)

## -----------------------------------------------------------------------------
set.seed(1)
gbm_model <-gbm(salary ~. , data = adult, distribution = "bernoulli")
gbm_explainer <- explain(gbm_model,
                         data = adult[,-1],
                         y = adult$salary,
                         verbose = FALSE)

# test fairness object
fobject_test <- fairness_check(gbm_explainer, 
                          protected = protected, 
                          privileged = "Male",
                          verbose = FALSE) 

plot(ceteris_paribus_cutoff(fobject_test, subgroup = "Female"))


## -----------------------------------------------------------------------------
plot(ceteris_paribus_cutoff(fobject_test,
                            subgroup = "Female",
                            fairness_metrics = c("ACC","TPR","STP")))

## -----------------------------------------------------------------------------
fc <- fairness_check(gbm_explainer, fobject,
                     label = "gbm_cutoff",
                     cutoff = list(Female = 0.25),
                     verbose = FALSE)

plot(fc)


## -----------------------------------------------------------------------------
print(fc , colorize = FALSE)

## -----------------------------------------------------------------------------
paf <- performance_and_fairness(fc, fairness_metric = "STP",
                                 performance_metric = "accuracy")
plot(paf)

