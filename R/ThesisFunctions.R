#' Payoff of best answer against the strategy
#'
#' @export
get.benchmark <- function(){
  benchmark <- rep(NA,10)
  names(benchmark) <- c("strat.a","strat.b","strat.c","strat.d","strat.e","strat.f","strat.g","strat.h","strat.i")
  benchmark["strat.a"] <- 1.321
  benchmark["strat.b"] <- 0.747
  benchmark["strat.c"] <- 1.156
  benchmark["strat.d"] <- 1.317
  benchmark["strat.e"] <- 0.743
  benchmark["strat.f"] <- 1.503
  benchmark["strat.g"] <- 1.328
  benchmark["strat.h"] <- 1.083
  benchmark["strat.i"] <- 1.321
  return(benchmark)
}

#' Conversion factor
#'
#' @export
get.conversion <- function(delta,T){
  return((1/(1-delta)-delta^(T)/(1-delta)))
}

#' Payoff of strategy against itself
#'
#' @export
get.against.itself.benchmark <- function(){
  against.itself <- rep(NA,10)
  names(against.itself) <- c("strat.a","strat.b","strat.c","strat.d","strat.e","strat.f","strat.g","strat.h","strat.i")
  against.itself["strat.a"] <- 0.920
  against.itself["strat.b"] <- 0.495
  against.itself["strat.c"] <- 0.979
  against.itself["strat.d"] <- 0.98
  against.itself["strat.e"] <- 0.672
  against.itself["strat.f"] <- 0.833
  against.itself["strat.g"] <- 0.971
  against.itself["strat.h"] <- 0.962
  against.itself["strat.i"] <- 0.986
  return(against.itself)
}

#' Get vector of names of counter strategy
#'
#' @export
get.antistrat <- function(){
  counter.strat <- rep(NA,9)
  names(counter.strat) <- c("strat.a","strat.b","strat.d","strat.e","strat.f","strat.g","strat.h","strat.i","strat.c")
  counter.strat["strat.i"] <- "counter.strat.i"
  counter.strat["strat.f"] <- "counter.strat.f"
  counter.strat["strat.h"] <- "counter.strat.h"
  counter.strat["strat.a"] <- "counter.strat.a"
  counter.strat["strat.b"] <- "always.coop"
  counter.strat["strat.d"] <- "counter.strat.d"
  counter.strat["strat.e"] <- "counter.strat.e"
  counter.strat["strat.g"] <- "counter.strat.g"
  counter.strat["strat.c"] <- "counter.strat.c"
  return(counter.strat)
}
