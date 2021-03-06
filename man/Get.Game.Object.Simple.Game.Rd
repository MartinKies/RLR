% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimpleGame.R
\name{Get.Game.Object.Simple.Game}
\alias{Get.Game.Object.Simple.Game}
\title{Get Game Object which fully defines simple game.}
\usage{
Get.Game.Object.Simple.Game(encoding.state = NULL, encoding.action = NULL)
}
\arguments{
\item{encoding.state}{Which method should be used to encode the game? Currently supported:
\itemize{
\item main - [A,B,rounds]
}}

\item{encoding.action}{Which method should be used to encode the action? Currently supported:
\itemize{
\item main - [A,B,A and B]
}}
}
\description{
A simple Game, which delivers A, B or "A and B" - The Goal is to correctly identify which setting is displayed and choosing the solutions 1, 2 or 3. This is repeated 3 times with each round giving 1 Point if correct. If all are scored correct, 5 extra points are awarded.
}
