% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_density.R
\name{plot_density}
\alias{plot_density}
\title{Plot fairness object}
\usage{
plot_density(x, ...)
}
\arguments{
\item{x}{object of class \code{fairness_object}}

\item{...}{other plot parameters}
}
\value{
\code{ggplot2} object
}
\description{
Plot distribution for models output probabilities. See how being in particular subgroup affects models decision.
}
\examples{

data("compas")

glm_compas <- glm(Two_yr_Recidivism ~ ., data = compas, family = binomial(link = "logit"))

y_numeric <- as.numeric(compas$Two_yr_Recidivism) - 1

explainer_glm <- DALEX::explain(glm_compas, data = compas, y = y_numeric)

fobject <- fairness_check(explainer_glm,
  protected = compas$Ethnicity,
  privileged = "Caucasian"
)

plot_density(fobject)
}
