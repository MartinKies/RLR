% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RNN.R
\name{Setup.RNN}
\alias{Setup.RNN}
\title{Setup a RNN}
\usage{
Setup.RNN(model.par, game.par)
}
\arguments{
\item{model.par}{Parameters of RNN e.g. given by \code{Get.Def.Par.RNN}}

\item{game.par}{Parameters of Game. Used are \itemize{
\item input.nodes - Number of Input Nodes
\item output.nodes - Number of Actions
}}
}
\description{
Setup the RNN in keras to work with it. Returns a keras stile RNN
}
