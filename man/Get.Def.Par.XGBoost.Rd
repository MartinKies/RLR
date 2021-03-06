% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XGBoost.R
\name{Get.Def.Par.XGBoost}
\alias{Get.Def.Par.XGBoost}
\title{Default Parameters for XGBoost}
\usage{
Get.Def.Par.XGBoost(setting = "ThesisOpt")
}
\arguments{
\item{setting}{String which defines the setting of the default parameters. \itemize{
  \item "ThesisOpt" - Default setting. The same parameters are used as for the final results of the thesis of Martin Kies.
  \item "Legacy.v.0.1.6" - Uses the default settings of version 0.1.6 of this package
}}
}
\description{
Returns the 'model.par' necessary for \code{\link{Setup.QLearningPersExpPath}}, \code{\link{Initialise.QLearningPersExpPath}} and \code{\link{Train.QLearningPersExpPath}}
}
