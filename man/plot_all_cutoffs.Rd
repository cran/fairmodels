% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_all_cutoffs.R
\name{plot.all_cutoffs}
\alias{plot.all_cutoffs}
\title{Plot all cutoffs}
\usage{
\method{plot}{all_cutoffs}(x, ..., label = NULL)
}
\arguments{
\item{x}{\code{all_cutoffs} object}

\item{...}{other plot parameters}

\item{label}{character, label of model to plot. Default NULL. If default prints all models.}
}
\value{
\code{ggplot2} object
}
\description{
All cutoffs plot allows to check how parity loss of chosen metrics is affected by the change of cutoff. Values of cutoff
are the same for all subgroups (levels of protected variable) no matter what cutoff values were in \code{fairness_object}.
}
\examples{

data("german")

y_numeric <- as.numeric(german$Risk) - 1

lm_model <- glm(Risk ~ .,
  data = german,
  family = binomial(link = "logit")
)

explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)

fobject <- fairness_check(explainer_lm,
  protected = german$Sex,
  privileged = "male"
)

ac <- all_cutoffs(fobject)
plot(ac)
\donttest{
rf_model <- ranger::ranger(Risk ~ .,
  data = german,
  probability = TRUE,
  num.trees = 100,
  seed = 1
)


explainer_rf <- DALEX::explain(rf_model,
  data = german[, -1],
  y = y_numeric
)

fobject <- fairness_check(explainer_rf, fobject)

ac <- all_cutoffs(fobject)
plot(ac)
}

}
