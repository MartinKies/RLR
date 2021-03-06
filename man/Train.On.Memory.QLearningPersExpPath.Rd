% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QlearningPersExpPath.R
\name{Train.On.Memory.QLearningPersExpPath}
\alias{Train.On.Memory.QLearningPersExpPath}
\title{Trains model based on memory}
\usage{
Train.On.Memory.QLearningPersExpPath(
  model,
  model.surp = NULL,
  model.fam = NULL,
  model.par,
  model.par.surp = NULL,
  model.par.fam = NULL,
  algo.var,
  algo.par,
  game.object,
  type = "init",
  mem.replay = NULL,
  ...
)
}
\arguments{
\item{model.par}{Model parameters for main model}

\item{model.par.surp}{Model parameters for Surprise model. If \code{NULL}, then no suprise model is trained.}

\item{model.par.fam}{Model parameters for Familiarity model. If \code{NULL}, then no familiarity model is trained.}

\item{type}{May be init for memory initialization and replay for replay.}
}
\description{
Trains model based on memory
}
