% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.metric_scores}
\alias{print.metric_scores}
\title{Print metric scores data}
\usage{
\method{print}{metric_scores}(x, ...)
}
\arguments{
\item{x}{\code{metric_scores} object}

\item{...}{other print parameters}
}
\description{
Print metric scores data
}
\examples{

data("german")

y_numeric <- as.numeric(german$Risk) - 1

lm_model <- glm(Risk ~ .,
  data = german,
  family = binomial(link = "logit")
)

rf_model <- ranger::ranger(Risk ~ .,
  data = german,
  probability = TRUE,
  num.trees = 200,
  num.threads = 1
)

explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)
explainer_rf <- DALEX::explain(rf_model, data = german[, -1], y = y_numeric)

fobject <- fairness_check(explainer_lm, explainer_rf,
  protected = german$Sex,
  privileged = "male"
)

ms <- metric_scores(fobject, fairness_metrics = c("TPR", "STP", "ACC"))
ms
}
