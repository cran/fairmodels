% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.fairness_regression_object}
\alias{print.fairness_regression_object}
\title{Print Fairness Regression Object}
\usage{
\method{print}{fairness_regression_object}(x, ..., colorize = TRUE)
}
\arguments{
\item{x}{\code{fairness_regression_object} object}

\item{...}{other parameters}

\item{colorize}{logical, whether information about metrics should be in color or not}
}
\description{
Print Fairness Regression Object
}
\examples{

set.seed(123)
data <- data.frame(
  x = c(rnorm(500, 500, 100), rnorm(500, 400, 200)),
  pop = c(rep("A", 500), rep("B", 500))
)

data$y <- rnorm(length(data$x), 1.5 * data$x, 100)

# create model
model <- lm(y ~ ., data = data)

# create explainer
exp <- DALEX::explain(model, data = data, y = data$y)

# create fobject
fobject <- fairness_check_regression(exp, protected = data$pop, privileged = "A")

# results

fobject
\donttest{

model_ranger <- ranger::ranger(y ~ ., data = data, seed = 123)
exp2 <- DALEX::explain(model_ranger, data = data, y = data$y)

fobject <- fairness_check_regression(exp2, fobject)

# results
fobject
}

}
