% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NeuralNetwork.R
\name{Predict.Neural.Network}
\alias{Predict.Neural.Network}
\title{Evaluate Neural Network}
\usage{
Predict.Neural.Network(model, model.par, state, action = NULL)
}
\arguments{
\item{model}{A trained Neural Network e.g. given by \code{Setup.Neural.Network}.}

\item{state}{A game.state after being encoded by the game.object.}

\item{action}{Only the relevant action is displayed}
}
\description{
Evaluate a model based on a game.state
}
