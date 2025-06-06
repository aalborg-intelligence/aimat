% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn_general.R
\name{predict.nn}
\alias{predict.nn}
\title{Predict method for neural network}
\usage{
\method{predict}{nn}(object, newdata, type = "response")
}
\arguments{
\item{object}{Fitted neural network model}

\item{newdata}{Data frame with new data}

\item{type}{One of "response", "class"}
}
\value{
Predicted values
}
\description{
Predict method for neural network
}
\examples{
nn <- nn_fun(Species ~ ., iris, n_hidden = c(3,5), eta = 0.01, iter = 1000, lossfun = "cross-entropy", activation = "Sigmoid", type = "klassifikation", scale = TRUE)
predict(nn, iris, type = "class")
}
