% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RNN.R
\name{Predict.RNN}
\alias{Predict.RNN}
\title{Evaluate Recurrent RNN}
\usage{
Predict.RNN(model, model.par, state, action = NULL)
}
\arguments{
\item{model}{A trained RNN e.g. given by \code{Setup.RNN}.}

\item{state}{A game.state after being encoded by the game.object.}
}
\description{
Evaluate a model based on a game.state
}
