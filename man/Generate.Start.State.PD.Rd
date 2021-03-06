% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrisonersDilemmaStratTourn.R
\name{Generate.Start.State.PD}
\alias{Generate.Start.State.PD}
\title{Generates Start State for Prisoners Dilemma Game}
\usage{
Generate.Start.State.PD(game.object)
}
\description{
A state consisting of a human-readable start state. Here all information which are actually state dependend should be saved. All information which does not change from game to game of the same type of game should be saved in game.object.\cr \cr
Note that states have to be structurally identical to each other, even if not all information is needed at the beginning.\cr \cr
Public function, which might be called by an algorithm.
}
