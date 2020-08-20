## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  cache      = FALSE,
  fig.align  = 'center',
  dpi = 72,
  fig.width  = 10,
  fig.height = 7
)


## -----------------------------------------------------------------------------
library(fairmodels)
data("compas")

head(compas)

## -----------------------------------------------------------------------------
compas$Two_yr_Recidivism <- as.factor(ifelse(compas$Two_yr_Recidivism == '1', '0', '1'))

## -----------------------------------------------------------------------------
library(DALEX)
library(ranger)

# train
rf_compas <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)

# numeric target values
y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1

# explainer
rf_explainer <- explain(rf_compas, data = compas[,-1], y = y_numeric, colorize = FALSE)

## -----------------------------------------------------------------------------

fobject <- fairness_check(rf_explainer,                         # explainer
                          protected = compas$Ethnicity,         # protected variable as factor
                          privileged = "Caucasian",             # level in protected variable, potentially more privileged
                          cutoff = 0.5,                         # cutoff - optional, default = 0.5
                          colorize = FALSE)                         

## -----------------------------------------------------------------------------
print(fobject, colorize = FALSE)

## -----------------------------------------------------------------------------
plot(fobject)

## -----------------------------------------------------------------------------
plot_density(fobject)

## ---- results= "hide"---------------------------------------------------------
library(gbm)

rf_compas_1 <- ranger(Two_yr_Recidivism ~Number_of_Priors+Age_Below_TwentyFive,
                      data = compas,
                      probability = TRUE)

lr_compas_1 <- glm(Two_yr_Recidivism~.,
                   data=compas,
                   family=binomial(link="logit"))

rf_compas_2 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE) 
rf_compas_3 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor,
                      data = compas,
                      probability = TRUE)

rf_compas_4 <- ranger(Two_yr_Recidivism ~.,
                      data = compas,
                      probability = TRUE)
df <- compas
df$Two_yr_Recidivism <- as.numeric(compas$Two_yr_Recidivism)-1
gbm_compas_1<- gbm(Two_yr_Recidivism~., data = df) 

explainer_1 <- explain(rf_compas_1,  data = compas[,-1], y = y_numeric)
explainer_2 <- explain(lr_compas_1,  data = compas[,-1], y = y_numeric)
explainer_3 <- explain(rf_compas_2,  data = compas[,-1], y = y_numeric, label = "ranger_2")
explainer_4 <- explain(rf_compas_3,  data = compas[,-1], y = y_numeric, label = "ranger_3")
explainer_5 <- explain(gbm_compas_1, data = compas[,-1], y = y_numeric)
explainer_6 <- explain(rf_compas_4,  data = compas[,-1], y = y_numeric, label = "ranger_4")


## -----------------------------------------------------------------------------
fobject <- fairness_check(explainer_1, explainer_2,
                            explainer_3, explainer_4,
                            explainer_5, explainer_6,
                            protected = compas$Ethnicity,
                            privileged = "Caucasian",
                            verbose = FALSE) 

## -----------------------------------------------------------------------------
fobject$parity_loss_metric_data

## -----------------------------------------------------------------------------
# for the first model
fobject$groups_data$ranger$TPR

## -----------------------------------------------------------------------------
# for first model
fobject$cutoff$ranger

## -----------------------------------------------------------------------------
sm <- stack_metrics(fobject)
plot(sm)

## -----------------------------------------------------------------------------
cm <- choose_metric(fobject)
plot(cm)

## -----------------------------------------------------------------------------
fair_pca <- fairness_pca(fobject)
print(fair_pca)

## -----------------------------------------------------------------------------
plot(fair_pca)

## -----------------------------------------------------------------------------
fheatmap <- fairness_heatmap(fobject)
plot(fheatmap, text_size = 3)

## -----------------------------------------------------------------------------
fap <- performance_and_fairness(fobject, fairness_metric = "FPR")
plot(fap)

## -----------------------------------------------------------------------------
fobject2 <- fairness_check(explainer_1,explainer_2, 
                                   protected = compas$Ethnicity,
                                   privileged = "Caucasian", 
                                    verbose = FALSE)


gm <- group_metric(fobject2, fairness_metric = "FPR")
plot(gm)

## -----------------------------------------------------------------------------
fradar <- fairness_radar(fobject2)
plot(fradar)

## -----------------------------------------------------------------------------
ac <- all_cutoffs(fobject2)

plot(ac)

## -----------------------------------------------------------------------------
cpc <- ceteris_paribus_cutoff(fobject2, subgroup = "African_American")

plot(cpc)

