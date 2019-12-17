#Play the Prisoners Dilemma of StratTourn

#' Defines the game environment of the IPD
#' 
#' Returns a list with the parameters.
#' 
#' @param setting Here, one defines the setting which should be used as a default. The following settings are currently implemented: \itemize{
#'  \item "BattleOfStrategiesThesis.Baseline" - The setting as defined in the thesis of Martin Kies. 15% Prob. of mistaking a cooperation as a defection. \eqn{delta} 0.95. T.max 60 for numerical purposes as default.  
#'  \item "BattleOfStrategies2019" - The setting as defined the seminar "Fortgeschrittene Analyse von Kooperation mit Spieltheorie und Simulation aka Battle of Strategies" of winter term 2019/2020. 25% Prob. of mistaking an observation in either direction. \eqn{delta} 0.985. T.max 60 for numerical purposes as default.  
#' }
#' @param strats Names of strategies which are to be optimized against. If several strategies are given it is random against which strategy the game is played with each episode. Playing against itself is indicated by "self". Note that this function sets the strategy to NA if "self" is wished and the respective encoding strategy has to be manually filled in.
#' @export
Get.Game.Param.PD <- function(setting="BattleOfStrategiesThesis.Baseline", strats="tit.for.tat"){
  other.strategies <- lapply(strats,FUN=function(x){
    if(x=="self"||!exists(x)){
      return(NA)
    } else {
      return(get(x))
    }
  })
  names(other.strategies) <- strats
  other.strategies <- list(other.strategies=other.strategies)
  
  if(is.null(setting)||setting=="BattleOfStrategiesThesis.Baseline"){
    game.pars <- Get.Game.Param.PD.Legacy.BattleOfStrategies2013.Baseline()
  } else if (setting=="BattleOfStrategies2019"){
    game.pars <- Get.Game.Param.PD.Legacy.BattleOfStrategies2019()
  } else {
    stop("Unknown game setting specified.")
  }
  
  game.par <- c(other.strategies,game.pars)
  
  return(game.par)
}

#' Defines model parameters for 'Prisoners Dilemma'
#'
#' Public Function which might be called by algorithm functions.
#' Output is a list of the following structure:
#' \itemize{
#' \item input.nodes - Length of array as presented by state.2.array
#' \item output.nodes - Number of possible actions
#' \item input.nodes.time - When working with the RNN functions, the state is saved as a matrix instead of an array. input.nodes.time is the number of inputnodes for each single data point in time.
#' }
#' @param game.object A game object as defined by Get.Game.Object.Simple.Game
#' @export
Get.Par.PD <- function(game.object){
  restore.point("Get.Par.PD")
  blank <- State.2.Array.PD(Generate.Start.State.PD(game.object),game.object=game.object)
  input.nodes <- length(blank)
  output.nodes <- Action.Encoding.Info.PD(game.object$encoding.action)$output.nodes
  if(!is.null(dim(blank)) && dim(blank)>1){
    input.nodes.time <- ncol(blank)
  } else {
    input.nodes.time <- NULL
  }
  game.param <- nlist(input.nodes, output.nodes, input.nodes.time)
  return(game.param)
}

#' Generates Start State for Prisoners Dilemma Game
#'
#' A state consisting of a human-readable start state. Here all information which are actually state dependend should be saved. All information which does not change from game to game of the same type of game should be saved in game.object.\cr \cr
#' Note that states have to be structurally identical to each other, even if not all information is needed at the beginning.\cr \cr
#' Public function, which might be called by an algorithm.
#' @export
Generate.Start.State.PD <- function(game.object){
  #restore.point("Generate.Start.State.PD")
  t <- 1 # first round
  me.last.see <- "Default"
  other.last.see <- "Default"
  game.finished <- FALSE

  #Define our strategy
  strat.no <- sample(1:length(game.object$game.pars$other.strategies),1)
  other.strategy <- game.object$game.pars$other.strategies[[strat.no]]

  if(!is.null(names(game.object$game.pars$other.strategies[strat.no])) && names(game.object$game.pars$other.strategies[strat.no]) == "self"){
    self.play = TRUE
  } else {
    self.play = FALSE
  }

  # Draw number of periods from a negative binominal distribution if not defined
  if (is.null(game.object$game.pars$T)) {
    ret = sample.T(delta=game.object$game.pars$delta, sample.delta=game.object$game.pars$delta)
    T = ret$T
    if (!is.null(game.object$game.pars$T.max))
      T = pmin(T, game.object$game.pars$T.max)
  } else {
    T = game.object$game.pars$T
  }

  history.see <- data.frame(me=rep(NA,T), other=rep(NA,T))
  history.real <- data.frame(me=rep(NA,T), other=rep(NA,T))

  #Determine starting variables of other strategy
  par.other.full <- formals(other.strategy)
  par.other <- par.other.full[!(names(par.other.full) %in% c("obs", "i", "t", "..."))]

  U = rep(0,2)

  res <- nlist(round=t, me.last.see, other.last.see, game.finished, history.see, history.real, other.strategy, par.other, T=T, self.play)
  return(res)
}

#' Internal Function to make working with different encodings easier
#'
#' @export
Encoding.Manager.PD <- function(encoding.state,info=NULL){
  restore.point("Encoding.Manager.PD")
  if(is.null(info)){
    stop("Please specify info for encoding manager")
  }

  #Here the Master of Encoding Infos is kept
  info.list <- list(
    list(name="main",state.fun=Encoding.main.PD,is.full=FALSE),
    list(name="full.zero",state.fun=Encoding.full.zero.PD,is.full=TRUE),
    list(name="full.compact",state.fun=Encoding.full.compact.PD,is.full=TRUE),
    list(name="TenTen",state.fun=Encoding.TenTen.PD,is.full=FALSE),
    list(name="maximum.full.Ten",state.fun=Encoding.maximum.full.Ten.PD,is.full=TRUE),
    list(name="last.round",state.fun=Encoding.last.round.PD,is.full=FALSE),
    list(name="Slim.TenTen",state.fun=Encoding.Slim.TenTen.PD,is.full=TRUE),
    list(name="last.three.rounds",state.fun=Encoding.last.three.rounds.PD,is.full=FALSE),
    list(name="static.end.Ten",state.fun=Encoding.static.end.Ten.PD,is.full=FALSE),
    list(name="TimeSeries.minimal",state.fun=Encoding.TimeSeries.minimal.PD,is.full=TRUE),
    list(name="TimeSeries.flexible",state.fun=Encoding.TimeSeries.flexible.PD,is.full=FALSE),
    list(name="XGBoost.Main",state.fun=Encoding.XGBoost.Main.PD,is.full=TRUE),
    list(name="Main.real",state.fun=Encoding.Main.real.PD,is.full=TRUE),
    list(name="Main.real.short20",state.fun=Encoding.XGBoost.Main.real.short20.PD,is.full=TRUE),
    list(name="last.X.rounds",state.fun=Encoding.last.X.rounds.PD,is.full=FALSE),
    list(name="Harper",state.fun=Encoding.Harper.PD,is.full=FALSE)
  )

  name.arr <- sapply(info.list,FUN=function(x){x$name})
  pos <- which(name.arr %in% encoding.state)

  if(info=="is.full"){
    res <- info.list[[pos]]$is.full
  } else if (info=="state.array.fun"){
    res <- info.list[[pos]]$state.fun
  } else {
    stop("info for Encoding Manager not implemented")
  }
  return(res)
}

#' @export
Get.Full.Encoding.Status.PD <- function(encoding.state){
  #restore.point("Get.Full.Encoding.Status.PD")
    if(!is.null(encoding.state)){
      res <- Encoding.Manager.PD(encoding.state,info="is.full")
    } else {
      res <- FALSE
    }
    return(res)
}

#' @export
Get.Feature.List.Encoding.Status.PD <- function(game.object, game.state=NULL){
  #restore.point("Get.Feature.List.Encoding.Status.PD")
  encoding.state <- game.object$encoding.state

  if(is.null(encoding.state)){
    return(NULL)
  }
  if(!is.null(game.state)){
    #Everything is ok
  } else {
    game.state <- Generate.Start.State.PD(game.object)
  }
  res <- Encoding.Manager.PD(encoding.state = encoding.state,info="state.array.fun")(game.state, game.object)$feature.types
  return(res)
}

#' State to Array for Prisoners Dilemma
#'
#' Transforms Game State to readable array. Here a lot of optimization might take place, as different algorithms might like different encodings.
#' @param game.state Game State in human readable form.
#' @param game.object as specified by Get.Game.Object
#' Public Function which might be called by algorithms.
#' @export
State.2.Array.PD <- function(game.state,game.object){
  restore.point("State.2.Array.PD")
  if(is.null(game.object$encoding.state)){
    encoding <- "Main.real"
  } else {
    encoding <- game.object$encoding.state
  }

  if(is.character(encoding)){ #simple case
    res <- Encoding.Manager.PD(encoding.state = encoding,info="state.array.fun")(game.state, game.object)$observed
  } else {
    stop("Flexible encodings not yet implemented.")
  }
  return(res)
}

#' @export
Encoding.main.PD <- function(game.state, game.object){
  #[1] Bit - See C (other)
  #[2] Bit - See D (other)
  #[3] Bit - See C (me)
  #[4] Bit - See D (me)
  #[5] Bit - See C (other) [one round before]
  #[6] Bit - See D (other) [one round before]
  #[7] Bit - See C (me) [one round before]
  #[8] Bit - See D (other) [one round before]
  #[9] Int - Round/100
  #[10] Bit - is first round
  #[11] Int - number of D of other per round
  #[12] Int - number of D of me per round

  arr <- vector("numeric",length=12)

  if(game.state$other.last.see == "C"){
    arr[1] <- TRUE
  }
  if(game.state$other.last.see == "D"){
    arr[2] <- TRUE
  }
  if(game.state$me.last.see == "C"){
    arr[3] <- TRUE
  }
  if(game.state$me.last.see == "D"){
    arr[4] <- TRUE
  }
  if(game.state$round>2 && game.state$history.see[game.state$round-2,2] == "C"){
    arr[5] <- TRUE
  }
  if(game.state$round>2 && game.state$history.see[game.state$round-2,2] == "D"){
    arr[6] <- TRUE
  }
  if(game.state$round>2 && game.state$history.see[game.state$round-2,1] == "C"){
    arr[7] <- TRUE
  }
  if(game.state$round>2 && game.state$history.see[game.state$round-2,1] == "D"){
    arr[8] <- TRUE
  }
  arr[9] <- game.state$round/100
  if(game.state$round == 1){
    arr[10] <- TRUE
  }
  if(game.state$round > 1){
    arr[11] <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
  }
  if(game.state$round > 1){
    arr[12] <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
  }
  res <- list(observed=arr,feature.types=c(rep("discrete",12)))
  return(res)
}

#' @export
Encoding.full.zero.PD <- function(game.state, game.object){
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  arr <- c(me.C,me.D,other.C,other.D)
  res <- list(observed=arr,feature.types=c(rep("discrete",length(arr))))
  return(res)
}

#' @export
Encoding.full.compact.PD <- function(game.state, game.object){
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me <- me.C-me.D
  other <- other.C-other.D
  arr <- c(me,other)
  feature.types <- c(rep("discrete",me),rep("discrete",other))
  res <- list(observed=arr,feature.types=feature.types)
}

#' @export
Encoding.TenTen.PD <- function(game.state,game.object){
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  me <- me.C - me.D
  other <- other.C - other.D

  me.start <- me[1:10]
  other.start <- other[1:10]

  round <- game.state$round/game.object$game.pars$T.max
  if(game.state$round == 1){
    first.round <- TRUE
  } else {
    first.round <- FALSE
  }
  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
  } else {
    av.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
  } else {
    av.me.def <- 0
  }

  me.fin <- me[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.fin)<=10){
    me.fin <- c(me.fin,rep(0,10-length(me.fin)))
  }

  other.fin <- other[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.fin)<=10){
    other.fin <- c(other.fin,rep(0,10-length(other.fin)))
  }

  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  arr <- c(me.start, other.start, other.fin, me.fin, round, av.other.def, av.me.def, prev.val.as.seen)
  feature.types <- c(rep("discrete",length(me.start)),rep("discrete",length(other.start)),rep("discrete",length(other.fin)),rep("discrete",length(me.fin)),"discrete","contin","contin","contin")
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.maximum.full.Ten.PD <- function(game.state,game.object){
  #restore.point("maxmimum.full.Ten.encoding")
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  if(game.object$game.pars$T.max>=game.state$round){
    round.cum <- c(rep(1,game.state$round),rep(0,game.object$game.pars$T.max-game.state$round))
    round.single <- rep(0,game.object$game.pars$T.max)
    round.single[game.state$round] <- 1
  } else {
    round.cum <- rep(1,game.object$game.pars$T.max)
    round.single <- rep(0,game.object$game.pars$T.max)
  }



  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
  } else {
    av.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
  } else {
    av.me.def <- 0
  }

  me.C.fin <- me.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.C.fin)<=10){
    me.C.fin <- c(me.C.fin,rep(0,10-length(me.C.fin)))
  }

  me.D.fin <- me.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.D.fin)<=10){
    me.D.fin <- c(me.D.fin,rep(0,10-length(me.D.fin)))
  }

  other.C.fin <- other.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.C.fin)<=10){
    other.C.fin <- c(other.C.fin,rep(0,10-length(other.C.fin)))
  }

  other.D.fin <- other.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.D.fin)<=10){
    other.D.fin <- c(other.D.fin,rep(0,10-length(other.D.fin)))
  }

  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C, me.D, other.C, other.D, me.C.fin, me.D.fin, other.C.fin, other.D.fin, round.cum, round.single, av.other.def, av.me.def, prev.val.as.seen,prev.val.as.seen.abs)
  feature.types <- c(rep("discrete",length(me.C)),rep("discrete",length(me.D)),rep("discrete",length(other.C)),rep("discrete",length(other.D)),rep("discrete",length(me.C.fin)),rep("discrete",length(me.D.fin)),rep("discrete",length(other.C.fin)),rep("discrete",length(other.D.fin)),rep("discrete",length(round.cum)),rep("discrete",length(round.single)),"contin","contin","contin","contin")
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.last.round.PD <- function(game.state,game.object){
  #restore.point("Encoding.last.round.PD")
  arr <- vector("numeric",length=4)

  if(game.state$other.last.see == "C"){
    arr[1] <- TRUE
  }
  if(game.state$other.last.see == "D"){
    arr[2] <- TRUE
  }
  if(game.state$me.last.see == "C"){
    arr[3] <- TRUE
  }
  if(game.state$me.last.see == "D"){
    arr[4] <- TRUE
  }
  feature.types <- rep("discrete",4)
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.Slim.TenTen.PD <- function(game.state,game.object){
  #restore.point("Slim.Ten.encoding")
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds.bin <- to.binary(game.state$round,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
  } else {
    av.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
  } else {
    av.me.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    diff <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    diff <- 0
  }
  if(diff < 0){
    diff <- abs(diff)
    flip <- 1
  } else {
    flip <- 0
  }
  diff.bin <- c(flip,to.binary(diff,max.dig=ceiling(log2(game.object$game.pars$T.max+1))))

  me.C.fin <- me.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.C.fin)<=10){
    me.C.fin <- c(me.C.fin,rep(0,10-length(me.C.fin)))
  }

  me.D.fin <- me.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.D.fin)<=10){
    me.D.fin <- c(me.D.fin,rep(0,10-length(me.D.fin)))
  }

  other.C.fin <- other.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.C.fin)<=10){
    other.C.fin <- c(other.C.fin,rep(0,10-length(other.C.fin)))
  }

  other.D.fin <- other.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.D.fin)<=10){
    other.D.fin <- c(other.D.fin,rep(0,10-length(other.D.fin)))
  }

  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, other.C-other.D, me.C.fin-me.D.fin, other.C.fin-other.D.fin, rounds.bin, av.other.def, av.me.def, prev.val.as.seen.abs, diff.bin)
  feature.types <- c(rep("discrete",length(me.C)),rep("discrete",length(other.C)),rep("discrete",length(me.C.fin)),rep("discrete",length(other.C.fin)),rep("discrete",length(rounds.bin)),"contin","contin","contin",rep("discrete",length(diff.bin)))
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.last.three.rounds.PD <- function(game.state,game.object){
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  me.C.fin <- me.C[(game.state$round-1):max((game.state$round-3),1)]
  if(length(me.C.fin)<=3){
    me.C.fin <- c(me.C.fin,rep(0,3-length(me.C.fin)))
  }

  me.D.fin <- me.D[(game.state$round-1):max((game.state$round-3),1)]
  if(length(me.D.fin)<=3){
    me.D.fin <- c(me.D.fin,rep(0,3-length(me.D.fin)))
  }

  other.C.fin <- other.C[(game.state$round-1):max((game.state$round-3),1)]
  if(length(other.C.fin)<=3){
    other.C.fin <- c(other.C.fin,rep(0,3-length(other.C.fin)))
  }

  other.D.fin <- other.D[(game.state$round-1):max((game.state$round-3),1)]
  if(length(other.D.fin)<=3){
    other.D.fin <- c(other.D.fin,rep(0,3-length(other.D.fin)))
  }

  arr <- c(me.C.fin,me.D.fin, other.C.fin, other.D.fin)
  feature.types <- c(rep("discrete",length(me.C.fin)),rep("discrete",length(me.D.fin)),rep("discrete",length(other.C.fin)),rep("discrete",length(other.D.fin)))
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.static.end.Ten.PD <- function(game.state,game.object){
  #restore.point("static.end.Ten")

  fin.count <- 10
  start.count <- 20

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  me.C.start <- me.C[1:start.count]
  me.D.start <- me.D[1:start.count]
  other.C.start <- other.C[1:start.count]
  other.D.start <- other.D[1:start.count]

  end.round <- game.state$round>round(game.object$game.pars$T.max/2)
  round <- game.state$round
  if(end.round) round <- 0
  rounds.bin <- to.binary(round,max.dig=ceiling(log2(game.object$game.pars$T.max)))

  rounds.info <- c(end.round, rounds.bin)

  me.C.fin <- me.C[(game.state$round-1):max((game.state$round-fin.count),1)]
  if(length(me.C.fin)<=fin.count){
    me.C.fin <- c(me.C.fin,rep(0,fin.count-length(me.C.fin)))
  }

  me.D.fin <- me.D[(game.state$round-1):max((game.state$round-fin.count),1)]
  if(length(me.D.fin)<=fin.count){
    me.D.fin <- c(me.D.fin,rep(0,fin.count-length(me.D.fin)))
  }

  other.C.fin <- other.C[(game.state$round-1):max((game.state$round-fin.count),1)]
  if(length(other.C.fin)<=fin.count){
    other.C.fin <- c(other.C.fin,rep(0,fin.count-length(other.C.fin)))
  }

  other.D.fin <- other.D[(game.state$round-1):max((game.state$round-fin.count),1)]
  if(length(other.D.fin)<=fin.count){
    other.D.fin <- c(other.D.fin,rep(0,fin.count-length(other.D.fin)))
  }

  arr <- c(me.C.start-me.D.start, other.C.start-other.D.start, me.C.fin-me.D.fin, other.C.fin-other.D.fin, rounds.info)
  feature.types <- c(rep("discrete",length(me.C.start)),rep("discrete",length(other.C.start)),rep("discrete",length(me.C.fin)),rep("discrete",length(other.C.fin)),rep("discrete",length(rounds.info)))
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.TimeSeries.minimal.PD <- function(game.state,game.object){
  restore.point("TimeSeries.minimal")
  no.action <- 2

    hist <- game.state$history.see
  if(any(!is.na(hist[,])&hist[,]=="C")) hist[!is.na(hist[,])&hist[,]=="C"] <- 1
  if(any(!is.na(hist[,])&hist[,]=="D")) hist[!is.na(hist[,])&hist[,]=="D"] <- -1
  if(game.state$round<=nrow(hist)){
    hist[game.state$round,] <- 0
  }
  hist.real <- game.state$history.real
  add.actions <- t(sapply(1:length(hist.real[,1]),FUN=function(y){
    in.res <- rep(0,no.action)
    if(is.na(hist.real[y,1])){
      in.res <- rep(NA,no.action)
    } else if (hist.real[y,1]=="C"){
      in.res[1] <- 1
    } else {
      in.res[2] <- 1
    }
    return(in.res)}
  ))


  arr <- data.matrix(cbind(hist,add.actions))
  feature.types <- NULL
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.TimeSeries.flexible.PD <- function(game.state,game.object){
  restore.point("TimeSeries.flexible")
  par <- game.object$encoding.params
  arr <- matrix(rep(NA,nrow(game.state$history.see)))

  if(is.null(par)) par <- list()

  if(is.null(par$last.rounds)){ #How many lagged rounds to give
    par$last.rounds <- 3
  }

  if(is.null(par$rounds.bin)){ #Should the round be part of the output? [in binary]
    par$rounds.bin <- TRUE
  }

  if(is.null(par$av.def)){ #Should the average defection rate be part of the encoding
    par$av.def <- TRUE
  }

  if(is.null(par$diff.bin)){ #Should the difference of defection be part of the encoding?
    par$diff.bin <- TRUE
  }

  if(is.null(par$prev.val.as.seen)){ #Should the value which can be calculated be part of the encoding?
    par$prev.val.as.seen <- TRUE
  }

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  if(par$rounds.bin){
    rounds.bin <- t(sapply(1:min(game.state$round,nrow(game.state$history.see)), FUN=function(x) {
      to.binary(x,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))
    }))
    blnk.tmp <- matrix(rep(NA,ncol(rounds.bin)*nrow(arr)),nrow=nrow(arr))
    blnk.tmp[1:nrow(rounds.bin),] <- rounds.bin
    arr <- cbind(arr,blnk.tmp)
  }

  if(par$av.def){
    av.def <- t(sapply(1:min(game.state$round,nrow(game.state$history.see)),FUN=function(x){
      if(x > 1){
        av.other.def <- sum(game.state$history.see[1:(x-1),2]=="D")/x
      } else {
        av.other.def <- 0
      }
      if(x > 1){
        av.me.def <- sum(game.state$history.see[1:(x-1),1]=="D")/x
      } else {
        av.me.def <- 0
      }
      return(c(av.other.def, av.me.def))
    }))
    blnk.tmp <- matrix(rep(NA,ncol(av.def)*nrow(arr)),nrow=nrow(arr))
    blnk.tmp[1:nrow(av.def),] <- av.def
    arr <- cbind(arr,blnk.tmp)
  }

  if(par$diff.bin){
    diff.bin <- t(sapply(1:min(game.state$round,nrow(game.state$history.see)),FUN=function(x){
      if(x > 1){
        diff <- sum(game.state$history.see[1:(x-1),1]=="D") - sum(game.state$history.see[1:(x-1),2]=="D")
      } else {
        diff <- 0
      }
      if(diff < 0){
        diff <- abs(diff)
        flip <- 1
      } else {
        flip <- 0
      }
      return(c(flip,to.binary(diff,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))))
    }))
    blnk.tmp <- matrix(rep(NA,ncol(diff.bin)*nrow(arr)),nrow=nrow(arr))
    blnk.tmp[1:nrow(diff.bin),] <- diff.bin
    arr <- cbind(arr,blnk.tmp)
  }

  last.rounds <- t(sapply(1:min(game.state$round,nrow(game.state$history.see)), FUN=function(x){
    if(x==1){
      fin <- rep(0,par$last.rounds*2)
      return(fin)
    }
    me.C.fin <- me.C[(x-1):max((x-par$last.rounds),1)]
    if(length(me.C.fin)<=par$last.rounds){
      me.C.fin <- c(me.C.fin,rep(0,par$last.rounds-length(me.C.fin)))
    }

    me.D.fin <- me.D[(x-1):max((x-par$last.rounds),1)]
    if(length(me.D.fin)<=par$last.rounds){
      me.D.fin <- c(me.D.fin,rep(0,par$last.rounds-length(me.D.fin)))
    }

    other.C.fin <- other.C[(x-1):max((x-par$last.rounds),1)]
    if(length(other.C.fin)<=par$last.rounds){
      other.C.fin <- c(other.C.fin,rep(0,par$last.rounds-length(other.C.fin)))
    }

    other.D.fin <- other.D[(x-1):max((x-par$last.rounds),1)]
    if(length(other.D.fin)<=par$last.rounds){
      other.D.fin <- c(other.D.fin,rep(0,par$last.rounds-length(other.D.fin)))
    }
    return(c(me.C.fin-me.D.fin, other.C.fin-other.D.fin))
  }))
  blnk.tmp <- matrix(rep(NA,ncol(last.rounds)*nrow(arr)),nrow=nrow(arr))
  blnk.tmp[1:nrow(last.rounds),] <- last.rounds
  arr <- cbind(arr,blnk.tmp)

  if(par$prev.val.as.seen){
    prev.val.as.seen <- sapply(1:min(game.state$round,nrow(game.state$history.see)), FUN=function(x){
      if(x>=2){
        prev.val.as.seen <- sum(sapply((1:(x-1)),FUN=function(y){
          if(game.state$history.see[y,1]=="C" && game.state$history.see[y,2]=="C"){
            round.reward <- game.object$game.pars$uCC
          } else if (game.state$history.see[y,1]=="C"&&game.state$history.see[y,2]=="D"){
            round.reward <- game.object$game.pars$uCD
          } else if (game.state$history.see[y,1]=="D"&&game.state$history.see[y,2]=="C"){
            round.reward <- game.object$game.pars$uDC
          } else if (game.state$history.see[y,1]=="D"&&game.state$history.see[y,2]=="D"){
            round.reward <- game.object$game.pars$uDD
          } else {
            stop("something bad happened when calculating payoff")
          }
          return(round.reward)
        }))
      } else {
        prev.val.as.seen <- 0
      }
      prev.val.as.seen <- prev.val.as.seen/game.object$game.pars$T.max
    })
    blnk.tmp <- matrix(rep(NA,nrow(arr)),nrow=nrow(arr))
    blnk.tmp[1:length(prev.val.as.seen),] <- prev.val.as.seen
    arr <- cbind(arr,blnk.tmp)
  }

  if(nrow(arr)==1){
    arr <- t(as.matrix(arr[,-1]))
  } else {
    arr <- arr[,-1]
  }
  arr.big <- matrix(rep(NA,ncol(arr)*(nrow(game.state$history.see))),ncol=ncol(arr))
  arr.big[1:nrow(arr),] <- arr

  hist.real <- game.state$history.real
  add.actions <- t(sapply(1:length(hist.real[,1]),FUN=function(y){
    in.res <- rep(0,2)
    if(is.na(hist.real[y,1])){
      in.res <- rep(NA,2)
    } else if (hist.real[y,1]=="C"){
      in.res[1] <- 1
    } else {
      in.res[2] <- 1
    }
    return(in.res)}
  ))
  arr <- data.matrix(cbind(arr.big,add.actions))

  feature.types <- NULL
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.XGBoost.Main.PD <- function(game.state,game.object){
  #restore.point("XGBoost.Main")
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    av.other.def <- 0
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
  } else {
    av.me.def <- 0
    sum.me.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    quot <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    if(!is.finite(quot)) quot <- 1
  } else {
    diff <- 0
    quot <- 1
  }
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }


  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, other.C-other.D, me.C.fin-me.D.fin, other.C.fin-other.D.fin, rounds, av.other.def,sum.other.def, av.me.def,sum.me.def, prev.val.as.seen.abs, diff, quot)
  feature.types <- c(rep("discrete",length(me.C)),rep("discrete",length(other.C)),rep("discrete",length(me.C.fin)),rep("discrete",length(other.C.fin)),"discrete","contin","discrete","contin","discrete","contin","contin","contin")
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.Main.real.PD <- function(game.state,game.object){
  #restore.point("Main.real")
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    av.other.def <- 0
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    av.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")/game.state$round
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
    sum.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")
  } else {
    av.me.def <- 0
    av.me.real.def <- 0
    sum.me.def <- 0
    sum.me.real.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    quot <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    if(!is.finite(quot)) quot <- 1
  } else {
    diff <- 0
    quot <- 1
  }
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.real.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    me.real.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.C.fin <- c(me.real.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.D.fin <- c(me.real.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }


  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, me.real.C-me.real.D, other.C-other.D, me.C.fin-me.D.fin, me.real.C.fin-me.real.D.fin, other.C.fin-other.D.fin, rounds, av.other.def,sum.other.def, av.me.def, av.me.real.def, sum.me.def, sum.me.real.def, prev.val.as.seen.abs, diff, quot)

  feature.types <- c(rep("discrete",length(me.C)),rep("discrete",length(me.real.C)),rep("discrete",length(other.C)),rep("discrete",length(me.C.fin)),rep("discrete",length(me.real.C.fin)),rep("discrete",length(other.C.fin)),"discrete","contin","discrete","contin","contin","discrete","discrete","contin","contin","contin")
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' Encoding based on Reinforcement Learning Produces Dominant Strategies for the Iterated Prisoner's Dilemma from Harper, Knight, Jones, et. al.
#'
#' @export
Encoding.Harper.PD <- function(game.state,game.object){
  #restore.point("Harper")
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
  } else {
    sum.me.def <- 0
  }

  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }

  arr <- c((me.C-me.D)[1:2], (other.C-other.D)[1:2], (me.C.fin-me.D.fin)[1:2], (other.C.fin-other.D.fin)[1:2], rounds, sum.other.def, sum.me.def)

  feature.types <- c(rep("discrete",2),rep("discrete",2),rep("discrete",2),rep("discrete",2),"discrete","discrete","discrete")
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' Flexible Encoding Function which expects the following encoding.params:
#' real (TRUE or FALSE) [Should a history of own taken actions be memorized?]
#' rounds: How many rounds should be looked into the past?
#' @export
Encoding.last.X.rounds.PD <- function(game.state,game.object){
  restore.point("Encoding.last.X.rounds.PD")
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  #Calculate Sliding View
  most.recent <- (game.state$round-1)
  last.viewable <- max((game.state$round-1)-game.object$encoding.params$rounds+1,1)
  max.view.window <- min(game.object$encoding.params$rounds,game.object$game.pars$T.max)
  missing.rounds <- max(max.view.window-(most.recent-last.viewable+1),0)

  if(game.state$round==1){
    me.C.fin <- rep(0,max.view.window)
    me.D.fin <- rep(0,max.view.window)
    other.C.fin <- rep(0,max.view.window)
    other.D.fin <- rep(0,max.view.window)
    if(game.object$encoding.params$real){
      me.real.C.fin <- rep(0,max.view.window)
      me.real.D.fin <- rep(0,max.view.window)
    }
  } else {
    me.C.fin <- c(me.C[most.recent:last.viewable],rep(0,missing.rounds))
    me.D.fin <- c(me.D[most.recent:last.viewable],rep(0,missing.rounds))
    other.C.fin <- c(other.C[most.recent:last.viewable],rep(0,missing.rounds))
    other.D.fin <- c(other.D[most.recent:last.viewable],rep(0,missing.rounds))
    if(game.object$encoding.params$real){
      me.real.C.fin <- c(me.real.C[most.recent:last.viewable],rep(0,missing.rounds))
      me.real.D.fin <- c(me.real.D[most.recent:last.viewable],rep(0,missing.rounds))
    }
  }

  if(game.object$encoding.params$real){
    arr <- c(me.C.fin-me.D.fin, me.real.C.fin-me.real.D.fin, other.C.fin-other.D.fin)
    feature.types <- c(rep("discrete",length(me.C.fin)),rep("discrete",length(me.real.C.fin)),rep("discrete",length(other.C.fin)))
  } else {
    arr <- c(me.C.fin-me.D.fin, other.C.fin-other.D.fin)
    feature.types <- c(rep("discrete",length(me.C.fin)),rep("discrete",length(other.C.fin)))
  }

  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' @export
Encoding.XGBoost.Main.real.short20.PD <- function(game.state,game.object){
  #restore.point("XGBoost.Main.real.short20")
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    av.other.def <- 0
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    av.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")/game.state$round
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
    sum.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")
  } else {
    av.me.def <- 0
    av.me.real.def <- 0
    sum.me.def <- 0
    sum.me.real.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    quot <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    if(!is.finite(quot)) quot <- 1
  } else {
    diff <- 0
    quot <- 1
  }
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.real.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    me.real.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.C.fin <- c(me.real.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.D.fin <- c(me.real.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }


  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c((me.C-me.D)[1:20], (me.real.C-me.real.D)[1:20], (other.C-other.D)[1:20], (me.C.fin-me.D.fin)[1:20], (me.real.C.fin-me.real.D.fin)[1:20], (other.C.fin-other.D.fin)[1:20], rounds, av.other.def,sum.other.def, av.me.def, av.me.real.def, sum.me.def, sum.me.real.def, prev.val.as.seen.abs, diff, quot)

  feature.types <- c(rep("discrete",20),rep("discrete",20),rep("discrete",20),rep("discrete",20),rep("discrete",20),rep("discrete",20),"discrete","contin","discrete","contin","contin","discrete","discrete","contin","contin","contin")
  res <- list(observed=arr,feature.types=feature.types)
  return(res)
}

#' Array to Action for Prisoners Dilemma
#'
#' Transforms the output of an machine learning algorithm to a readable game state
#' @param output.choice Number of chosen action.
#' @param game.object as specified by Get.Game.Object
#' @export
Choice.2.Action.PD <- function(output.choice, game.object){
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }

  if(encoding == "main"){
    if(output.choice==1){
      chosen <- "C"
    } else {
      chosen <- "D"
    }
    res <- list(chosen=chosen)
    return(res)
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Action to Array for Prisoners Dilemma
#'
#' Transforms the output of a game strategy to the number of choice of the machine learning algorithm
#' @param output Chosen action.
#' @param game.object as specified by Get.Game.Object
#' @export
Action.2.Choice.PD <- function(output, game.object){
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }

  if(encoding == "main"){
    if(output=="C"){
      return(1)
    } else {
      return(2)
    }
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Get Info of Action Encoding
#'
#' Returns Info of Action Encodings
#' @export
Action.Encoding.Info.PD <- function(game.object){
  if(is.null(game.object$encoding.action)){
    encoding <- "main"
  } else {
    encoding <- game.object$encoding.action
  }

  if(encoding=="main"){
    res <- list(output.nodes=2)
    return(res)
  } else {
    stop("Wrong encoding specified.")
  }
}

#' Get next State of Prisoners Dilemma Game
#'
#' Outputs List with three elements:
#' \itemize{
#' \item next.state - next game state (in Complete Form)
#' \item reward - What did we get from transitioning to the next state?
#' \item game.finished - Boolean; is the game over?
#' }
#' Public Function which might be called by algorithms.
#' @param game.state A complete game state, non encoded
#' @param action The choice of action.
#' @param game.object as specified by Get.Game.Object
#' @export
State.Transition.PD <- function(game.state, action, game.object){
  #restore.point("State.Transition.PD")
  action.me <- Choice.2.Action.PD(action,game.object)
  reward <- 0
  game.finished <- FALSE

  if(game.state$round==1){
    obs.other <- list(a=c("C","C"))
  } else {
    obs.other <- list(a=game.state$history.see[game.state$round-1,])
  }

  obs.other$a <- as.vector(obs.other$a, mode="character")

  args = c(list(obs = as.vector(obs.other),i=2,t=game.state$round),game.state$par.other)
  strat.res <- do.call(game.state$other.strategy,args)
  action.other <- strat.res$a
  par.other <- strat.res[-c(1)]

  a <- c(action.me, action.other)

  rand = runif(1)
  if(a[1]=="D"){
    err.D.1 = FALSE
    err.C.1 = rand<game.object$game.pars$err.C.prob
  } else {
    err.D.1 = rand<game.object$game.pars$err.D.prob
    err.C.1 = FALSE
  }

  rand = runif(1)
  if(a[2]=="D"){
    err.D.2 = FALSE
    err.C.2 = rand<game.object$game.pars$err.C.prob
  } else {
    err.D.2 = rand<game.object$game.pars$err.D.prob
    err.C.2 = FALSE
  }

  mine.seen <- a[[1]]
  other.seen <- a[[2]]

  if (err.D.1) mine.seen = "D"
  if (err.C.1) mine.seen = "C"
  if (err.D.2) other.seen = "D"
  if (err.C.2) other.seen = "C"


  #Update state
  game.state$round <- game.state$round+1
  game.state$me.last.see <- mine.seen
  game.state$other.last.see <- other.seen
  game.state$history.see[game.state$round-1,] <- c(mine.seen, other.seen)
  game.state$history.real[game.state$round-1,] <- c(a[1], a[2])
  game.state$par.other <- par.other

  #Intermediate returns for faster convergence
  if(game.object$game.pars$intermed>0){
    rel.round <- game.state$round-1
    if(game.state$history.see[rel.round,1]=="C" && game.state$history.see[rel.round,2]=="C"){
        reward <- game.object$game.pars$uCC * game.object$game.pars$intermed
      } else if (game.state$history.see[rel.round,1]=="C" && game.state$history.see[rel.round,2]=="D"){
        reward <- game.object$game.pars$uCD * game.object$game.pars$intermed
      } else if (game.state$history.see[rel.round,1]=="D" && game.state$history.see[rel.round,2]=="C"){
        reward <- game.object$game.pars$uDC * game.object$game.pars$intermed
      } else if (game.state$history.see[rel.round,1]=="D" && game.state$history.see[rel.round,2]=="D"){
        reward <- game.object$game.pars$uDD * game.object$game.pars$intermed
      } else {
        stop("something bad happened when calculating payoff")
      }
  }

  if(game.object$game.pars$direct.rewards){
    if(!game.state$self.play){
      if(a[1]=="C" && a[2]=="C"){
        reward <- game.object$game.pars$uCC
      } else if (a[1]=="C" && a[2]=="D"){
        reward <- game.object$game.pars$uCD
      } else if (a[1]=="D" && a[2]=="C"){
        reward <- game.object$game.pars$uDC
      } else if (a[1]=="D" && a[2]=="D"){
        reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
    } else {
      if(a[1]=="C" && a[2]=="C"){
        reward <- game.object$game.pars$uCC
      } else if (a[1]=="C" && a[2]=="D"){
        reward <- (game.object$game.pars$uCD +game.object$game.pars$uDC)/2
      } else if (a[1]=="D" && a[2]=="C"){
        reward <- (game.object$game.pars$uDC +game.object$game.pars$uDC)/2
      } else if (a[1]=="D" && a[2]=="D"){
        reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
    }
  }

  #Last round
  if(game.state$round>game.state$T){
    if(!game.object$game.pars$direct.rewards){
      if(!game.state$self.play){
        reward <- sum(sapply((1:game.state$T),FUN=function(x){
          if(game.state$history.real[x,1]=="C" && game.state$history.real[x,2]=="C"){
            round.reward <- game.object$game.pars$uCC
          } else if (game.state$history.real[x,1]=="C"&&game.state$history.real[x,2]=="D"){
            round.reward <- game.object$game.pars$uCD
          } else if (game.state$history.real[x,1]=="D"&&game.state$history.real[x,2]=="C"){
            round.reward <- game.object$game.pars$uDC
          } else if (game.state$history.real[x,1]=="D"&&game.state$history.real[x,2]=="D"){
            round.reward <- game.object$game.pars$uDD
          } else {
            stop("something bad happened when calculating payoff")
          }
          return(round.reward)
        }))/game.state$T
      } else {
        reward <- sum(sapply((1:game.state$T),FUN=function(x){
          if(game.state$history.real[x,1]=="C" && game.state$history.real[x,2]=="C"){
            round.reward <- game.object$game.pars$uCC
          } else if (game.state$history.real[x,1]=="C"&&game.state$history.real[x,2]=="D"){
            round.reward <- (game.object$game.pars$uCD + game.object$game.pars$uDC)/2
          } else if (game.state$history.real[x,1]=="D"&&game.state$history.real[x,2]=="C"){
            round.reward <- (game.object$game.pars$uCD + game.object$game.pars$uDC)/2
          } else if (game.state$history.real[x,1]=="D"&&game.state$history.real[x,2]=="D"){
            round.reward <- game.object$game.pars$uDD
          } else {
            stop("something bad happened when calculating payoff")
          }
          return(round.reward)
        }))/game.state$T
      }
    }
    game.finished <- TRUE
    game.state$game.finished <- TRUE
  }

  return(nlist(next.state=game.state,reward,game.finished))
}

#' Generate Memory where strategies play against themselves
#'
#' Each strategy within the game.object plays against itself one time
#'
#' Outputs List of lists with the following elements:
#' \itemize{
#' \item state - Already encoded game state.
#' \item action - Which of the actions has been taken? This is a single action if algo.par implies classical state-action pairs and the complete action row up to this point if a time.series approach is used (currently only use.rnn)
#' \item next.state - resulting next, encoded, state
#' \item reward - What did we get from transitioning to the next state?
#' \item done - Boolean; is the game over?
#' }
#' Public Function which might be called by algorithms.
#' @param game.object as specified by Get.Game.Object
#' @param algo.par as e.g. given by Get.Def.Par.QLearning
#' @export
Memory.Self.Play.PD <- function(game.object, algo.par){
  #generate Memory
  strats <- game.object$game.pars$other.strategies

  mem <- list()
  for(strat.i in 1:length(game.object$game.pars$other.strategies)){
    #ignore my own initialisation
    #restore.point("before.name")
    if(names(game.object$game.pars$other.strategies)[strat.i] == "self"){
      next
    }
    restore.point("inside.Memory.Self.Play.PD")
    strat <- game.object$game.pars$other.strategies[strat.i]
    strat.go <- game.object
    strat.go$game.pars$other.strategies <- strat
    state <- Generate.Start.State.PD(strat.go)
    obs <- list()
    obs$a <- c("C","C")
    strat.pars <- NULL
    for(counter in 1:state$T){
      args <- c(list(obs=obs, i=1, t=state$round), strat.pars)
      strat.ret <- do.call(strat.go$game.pars$other.strategies[[1]],args)
      # if(!is.null(algo.par$use.rnn) && algo.par$use.rnn){ #Time Series, i.e. Recurrent Neural Network
      #   my.actions <- unlist(state$history.real["me"])
      #   names(my.actions) <- NULL
      #   my.actions[counter] <- strat.ret$a
      #   strat.action.mem <- sapply(my.actions, FUN=function(x){
      #     if(!is.na(x)){
      #       return(Action.2.Choice.PD(output=x,game.object))
      #     } else {
      #       return(NA)
      #     }
      #   },USE.NAMES=FALSE)
      # }
      strat.action <- strat.ret$a
      strat.action <- Action.2.Choice.PD(output=strat.action, game.object)
      strat.pars <- strat.ret[-c("a" %in% names(strat.ret))]

      next.state.full <- State.Transition.PD(game.state = state, action = strat.action, game.object=game.object)

      next.state <- next.state.full$next.state
      reward <- next.state.full$reward
      done <- next.state.full$game.finished
      if(state$round==1){
        start <- TRUE
      } else {
        start <- FALSE
      }

      # #If RNN we want to save history
      # if(!is.null(algo.par$use.rnn) && algo.par$use.rnn){
      #   strat.action <- strat.action.mem
      # }

      if(algo.par$mem.selection=="all" || (algo.par$mem.selection=="end.state" && done)){
        if(algo.par$mem.type=="game.encoded"){
          mem[[length(mem)+1]] <- list(state=t(State.2.Array.PD(game.state=state, game.object=game.object)), action=strat.action, next.state=t(State.2.Array.PD(game.state=next.state, game.object=game.object)), reward=reward, done=done, start=start)
        } else if (algo.par$mem.type=="game.state"){
          mem[[length(mem)+1]] <- list(state=state, action=strat.action, next.state=next.state, reward=reward, done=done, start=start)
        } else if (algo.par$mem.type=="game.encoded.rounds"){
        mem[[length(mem)+1]] <- list(state=t(State.2.Array.PD(game.state=state, game.object=game.object)), action=strat.action, next.state=t(State.2.Array.PD(game.state=next.state, game.object=game.object)), reward=reward, done=done, start=start, round=state$round)
        }
      }

      state <- next.state
      obs <- list(a=c(next.state$me.last.see, next.state$other.last.see))

    }
  }
  return(mem)
}

#' Generate Memory where strategies play against a random strategy
#'
#' Each strategy within the game.object plays against a random strategy of the given defection probability
#'
#' Outputs List of lists with the following elements:
#' \itemize{
#' \item state - Already encoded game state, if algo.par$mem.type=="game.encoded"
#' \item action - Which of the actions has been taken?
#' \item next.state - resulting next, encoded, state
#' \item reward - What did we get from transitioning to the next state?
#' \item done - Boolean; is the game over?
#' }
#' Public Function which might be called by algorithms.
#' @param game.object as specified by Get.Game.Object
#' @param algo.par as e.g. given by Get.Def.Par.QLearning
#' @export
Memory.Random.Play.PD <- function(game.object, algo.par){
  #Generate Random Strategy
  strat <- function(obs,i,t,...) {
      a = sample( c("C","D"), size=1,  prob=c(1-algo.par$def.prob,algo.par$def.prob))
      return(list(a=a))
  }

  mem <- list()
  #ignore my own initialisation
  #restore.point("inside.Memory.Rand.Play.PD")
  strat.go <- game.object
  state <- Generate.Start.State.PD(strat.go)
  obs = c("C","C")
  for(counter in 1:state$T){
    #restore.point("inside.Memory.Rand.Play.PD.for.loop")
    strat.action = sample( c("C","D"), size=1,  prob=c(1-algo.par$def.prob,algo.par$def.prob))
    # if(!is.null(algo.par$use.rnn) && algo.par$use.rnn){ #Time Series, i.e. Recurrent Neural Network
    #   my.actions <- unlist(state$history.real["me"])
    #   names(my.actions) <- NULL
    #   my.actions[counter] <- strat.action
    #   strat.action.mem <- sapply(my.actions, FUN=function(x){
    #     if(!is.na(x)){
    #       return(Action.2.Choice.PD(output=x,game.object))
    #     } else {
    #       return(NA)
    #     }
    #   },USE.NAMES=FALSE)
    # }
    strat.action <- Action.2.Choice.PD(output=strat.action, game.object)

    next.state.full <- State.Transition.PD(game.state = state, action = strat.action, game.object=game.object)

    next.state <- next.state.full$next.state
    reward <- next.state.full$reward
    done <- next.state.full$game.finished

    if(state$round==1){
      start <- TRUE
    } else {
      start <- FALSE
    }

    # #If RNN we want to save history
    # if(!is.null(algo.par$use.rnn) && algo.par$use.rnn){
    #   strat.action <- strat.action.mem
    # }

    if(algo.par$mem.selection=="all" || (algo.par$mem.selection=="end.state" && done)){
      if(algo.par$mem.type=="game.encoded"){
        mem[[length(mem)+1]] <- list(state=t(State.2.Array.PD(game.state=state, game.object=game.object)), action=strat.action, next.state=t(State.2.Array.PD(game.state=next.state, game.object=game.object)), reward=reward, done=done, start=start)
       } else if (algo.par$mem.type=="game.state"){
         mem[[length(mem)+1]] <- list(state=state, action=strat.action, next.state=next.state, reward=reward, done=done, start=start)
       } else if (algo.par$mem.type=="game.encoded.rounds"){
         mem[[length(mem)+1]] <- list(state=t(State.2.Array.PD(game.state=state, game.object=game.object)), action=strat.action, next.state=t(State.2.Array.PD(game.state=next.state, game.object=game.object)), reward=reward, done=done, start=start, round=state$round)
       }
     }

    state <- next.state
    obs <- list(a=c(next.state$me.last.see, next.state$other.last.see))

  }

  return(mem)
}

#' Update Score based on expected Value of reward
#'
#' Returns updated score
#'
#' @export
Discounted.Reward.PD <- function(game.object, game.state, old.score, reward){
  #restore.point("Discounted.Reward.PD")
  discount <- game.object$game.pars$delta
  round <- game.state$round
  new.score <- old.score + reward*discount^(round-1)
  return(new.score)
}

#' Evaluate the current strategy using StratTourn
#'
#' Returns updated score
#'
#' @export
External.Eval.PD <- function(game.object, eval.no){
  restore.point("External.Eval.PD")

  if(length(game.object$game.pars$other.strategies)>1){
    stop("External.Eval.PD only implemented for single opponent strategies!")
  }

  strat = nlist(game.object$eval.strategy, game.object$game.pars$other.strategies[[1]])
  matchings <- matrix(c(1,2),ncol=2)
  game = make.pd.game(uCC=game.object$game.pars$uCC,CD = game.object$game.pars$uCD, uDC = game.object$game.pars$uDC, uDD = game.object$game.pars$uDD, err.D.prob = game.object$game.pars$err.D.prob, err.C.prob = game.object$game.pars$err.C.prob, delta = game.object$game.pars$delta)
  tourn = init.tournament(game=game, strat=strat)
  tourn = run.tournament(tourn=tourn, R = eval.no, matchings=matchings, T.max=game.object$game.pars$T.max) #HIER Anzahl der Spiele einstellen (mehr -> besser aber langsamer)
  r <- get.matches.vs.grid(tourn=tourn)$u1

  return(r)
}

#' Transforms List of Gamestates to std encoding form
#'
#' Returns list with states, next.states, action, reward, done
#'
#' @export
Encode.Game.States.PD <- function(game.object, game.states, expand=TRUE){
  if(!expand){
    states <- t(sapply(game.states, FUN=function(x){
      return(t(game.object$state.2.array(game.state=x$state, game.object=game.object)))
    }))

    next.states <- t(sapply(game.states, FUN=function(x){
      return(t(game.object$state.2.array(game.state=x$next.state, game.object=game.object)))
    }))

    reward <- sapply(game.states, FUN=function(x){
      return(x$reward)
    })

    action <- sapply(game.states, FUN=function(x){
      return(x$action)
    })

    done <- sapply(game.states, FUN=function(x){
      return(x$done)
    })
  } else {
    #x_train
    states <- lapply(game.states, FUN=function(x){
      expand.x <- t(sapply(1:nrow(x$next.state$history.see),FUN=function(y){
        fake.state <- x
        fake.state$state$round <- y
        if(y!=1){
          fake.state$state$history.see[(y):nrow(fake.state$state$history.see),] <- NA
          fake.state$state$me.last.see$chosen <- fake.state$next.state$history.see[y-1,1]
          fake.state$state$other.last.see$chosen <- fake.state$next.state$history.see[y-1,2]
        } else {
          fake.state$state$history.see[(y):nrow(fake.state$state$history.see),] <- NA
          fake.state$state$me.last.see$chosen <- "Default"
          fake.state$state$other.last.see$chosen <- "Default"
        }
        return(t(game.object$state.2.array(game.state=fake.state$state, game.object=game.object)))
      }))
      return(expand.x)
    })
    states <- do.call(rbind, states)

    #x_next
    next.states <- lapply(game.states, FUN=function(x){
      expand.x <- t(sapply(1:nrow(x$next.state$history.see),FUN=function(y){
        fake.state <- x
        if(x$next.state$round<=y+1){
          #do nothing
        } else {
          fake.state$next.state$round <- y+1
          fake.state$next.state$history.see[(y+1):nrow(fake.state$state$history.see),] <- NA
          fake.state$next.state$me.last.see$chosen <- fake.state$next.state$history.see[y,1]
          fake.state$next.state$other.last.see[[1]] <- fake.state$next.state$history.see[y,2]
        }
        return(t(game.object$state.2.array(game.state=fake.state$next.state, game.object=game.object)))
      }))
      return(expand.x)
    })
    next.states <- do.call(rbind, next.states)

    #reward
    reward <- lapply(game.states, FUN=function(x){
      expand.reward <- t(sapply(1:nrow(x$next.state$history.see),FUN=function(y){
        if(y==x$state$T){
          return(x$reward)
        } else {
          return(0)
        }
        return(y$reward)
      }))
      return(expand.reward)
    })
    reward <- do.call(c, reward)

    #action
    action <- lapply(game.states, FUN=function(x){
      expand.a <- t(sapply(1:nrow(x$next.state$history.see),FUN=function(y){
        return(Action.2.Choice.PD(x$next.state$history.real[y,1], game.object))
      }))
      return(expand.a)
    })
    action <- do.call(c, action)

    done <- lapply(game.states, FUN=function(x){
      expand.d <- t(sapply(1:nrow(x$next.state$history.see),FUN=function(y){
        if(y==x$state$T){
          return(TRUE)
        } else {
          return(FALSE)
        }
      }))
      return(expand.d)
    })
    done <- do.call(c, done)
  }
  return(nlist(states,next.states, reward, action, done))
}



#' Get Game Object which fully defines Prisoners Dilemma.
#' @param encoding.state Which feature selection should be used to encode the game? Default case is the main encoding of the thesis of Martin Kies, Main.real
#' @param encoding.action Which method should be used to encode the action? Currently supported:
#' \itemize{
#' \item main - [C,D]
#' }
#' @param encoding.params Insofar the \code{encoding.state} is parametrizable, e.g. "Model.strat.RNN.TimeSeries.flexible" or "Encoding.last.X.rounds.PD", here the encoding specific parameters are designed.
#' @param eval.strategy The name of the strategy used if one wants to evaluate the strategy with the package StratTourn or if self-play is used (i.e. if set to play a tournament with the strategy itself being a viable participant). By default "Model.strat.Main.real.Exp.Path" is used, which is designed to work with the encoding "Main.real".
#' @param basic.name.eval.model Global name under which the name of the model is to be saved.
#' @param basic.name.eval.model.par Global name under which the name of the parameter list of the model is to be saved.
#' @param game.setting Default settings of the game. By default the setting "BattleOfStrategiesThesis.Baseline" is used, which is the relevant one for the thesis of Martin Kies. For more information regarding the implemented settings see \code{link{Get.Game.Param.PD}}.
#' @param strats The strategies against which one wants to play. If several strategy names are given, each episode a random strategy is chosen. The code name "self" implements self play.
#' @export
Get.Game.Object.PD <- function(encoding.state="Main.real", encoding.action=NULL, encoding.params=NULL, eval.strategy=Model.strat.Main.real.Exp.Path, basic.name.eval.model="model.strat.global", basic.name.eval.model.par="model.par.strat.global", game.setting="BattleOfStrategiesThesis.Baseline", strats="tit.for.tat"){
  name <- "Prisoners Dilemma"
  supports <- c("memory.self.play", "memory.random.play","discounted.reward")

  game.par <- Get.Par.PD
  state.transition <- State.Transition.PD
  start.state <- Generate.Start.State.PD
  state.2.array <- State.2.Array.PD
  memory.self.play <- Memory.Self.Play.PD
  memory.random.play <- Memory.Random.Play.PD
  encode.game.states <- Encode.Game.States.PD
  full.encoding <- Get.Full.Encoding.Status.PD(encoding.state)
  discounted.reward <- Discounted.Reward.PD
  feature.types <- Get.Feature.List.Encoding.Status.PD
  external.eval <- External.Eval.PD

  game.pars <- Get.Game.Param.PD(setting=game.setting, strats = strats)
  for(i in 1:length(game.pars$other.strategies)){
    if(names(game.pars$other.strategies)[i]=="self"){
      game.pars$other.strategies[[i]] <- eval.strategy
    }
  }
  
  game.object <- nlist(name, supports, basic.name.eval.model,basic.name.eval.model.par, game.pars, game.par, state.transition, start.state, state.2.array, encoding.state, encoding.action, memory.self.play, memory.random.play, encode.game.states, full.encoding, encoding.params,discounted.reward,feature.types, external.eval, eval.strategy)
  return(game.object)
}

#' The actual strategy after model has been trained
#'
#' Does not work for itself - the "strat.model" variable has to be specified beforehand.
#' @export
NN.strat.main = function(obs,i,t,history.see=NULL,...) {
  #restore.point("NN.strat")
  arr <- rep(0,12)
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(NA,200), other=rep(NA,200))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }

  if(obs$a[j] == "C" && t!=1){
    arr[1] <- TRUE
  }
  if(obs$a[j] == "D" && t!=1){
    arr[2] <- TRUE
  }
  if(obs$a[i] == "C" && t!=1){
    arr[3] <- TRUE
  }
  if(obs$a[i] == "D" && t!=1){
    arr[4] <- TRUE
  }
  if(t>2 && history.see[t-2,2] == "C"){
    arr[5] <- TRUE
  }
  if(t>2 && history.see[t-2,2] == "D"){
    arr[6] <- TRUE
  }
  if(t>2 && history.see[t-2,1] == "C"){
    arr[7] <- TRUE
  }
  if(t>2 && history.see[t-2,1] == "D"){
    arr[8] <- TRUE
  }
  arr[9] <- t/100
  if(t == 1){
    arr[10] <- TRUE
  }
  if(t > 1){
    arr[11] <- sum(history.see[1:(t-1),2]=="D")/t
  }
  if(t > 1){
    arr[12] <- sum(history.see[1:(t-1),1]=="D")/t
  }

  act.values <- predict(model,t(arr))
  choice <- which.max(act.values)

  if(choice==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(list(a=a, history.see=history.see))
}

#' A strategy to be used after model has been trained
#'
#' @export
NN.strat.full.zero = function(obs,i,t,history.see=NULL,...) {
  #restore.point("NN.strat.zero")
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,200), other=rep(0,200))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  me.C <- rep(0,200)
  me.C[history.see[,1]=="C"] <- 1
  me.D <- rep(0,200)
  me.D[history.see[,1]=="D"] <- 1
  other.C <- rep(0,200)
  other.C[history.see[,2]=="C"] <- 1
  other.D <- rep(0,200)
  other.D[history.see[,2]=="D"] <- 1

  arr <- c(me.C,me.D,other.C,other.D)

  act.values <- predict(model,t(arr))
  choice <- which.max(act.values)

  if(choice==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(list(a=a, history.see=history.see))
}

#' A strategy to be used after model has been trained
#'
#' @export
NN.strat.Slim.TenTen.QLearning = function(obs,i,t,history.see=NULL,...) {
  debug.store("NN.strat.Slim.TenTen", i, t)  # Store each call for each player
  debug.restore("NN.strat.Slim.TenTen", i = 1, t = 6)  # Restore call for player i in period t
  #restore.point("NN.strat.Slim.TenTen.QLearning")
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$round <- t

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds.bin <- to.binary(game.state$round,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
  } else {
    av.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
  } else {
    av.me.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    diff <- 0
  }
  diff.bin <- to.binary(diff,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))

  me.C.fin <- me.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.C.fin)<=10){
    me.C.fin <- c(me.C.fin,rep(0,10-length(me.C.fin)))
  }

  me.D.fin <- me.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.D.fin)<=10){
    me.D.fin <- c(me.D.fin,rep(0,10-length(me.D.fin)))
  }

  other.C.fin <- other.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.C.fin)<=10){
    other.C.fin <- c(other.C.fin,rep(0,10-length(other.C.fin)))
  }

  other.D.fin <- other.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.D.fin)<=10){
    other.D.fin <- c(other.D.fin,rep(0,10-length(other.D.fin)))
  }

  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, other.C-other.D, me.C.fin-me.D.fin, other.C.fin-other.D.fin, rounds.bin, av.other.def, av.me.def, prev.val.as.seen.abs, diff.bin)

  arr <- t(as.matrix(arr))

  action.full <- Act.QLearning(state=arr, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=TRUE)
  action <- action.full[["action"]]

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see))
}

#' A strategy to be used after model has been trained
#'
#' @export
NN.strat.Slim.TenTen = function(obs,i,t,history.see=NULL,net.pointer=1,...) {
  debug.store("NN.strat.Slim.TenTen", i, t)  # Store each call for each player
  debug.restore("NN.strat.Slim.TenTen", i = 1, t = 6)  # Restore call for player i in period t
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$round <- t

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds.bin <- to.binary(game.state$round,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
  } else {
    av.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
  } else {
    av.me.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    diff <- 0
  }
  diff.bin <- to.binary(diff,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))

  me.C.fin <- me.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.C.fin)<=10){
    me.C.fin <- c(me.C.fin,rep(0,10-length(me.C.fin)))
  }

  me.D.fin <- me.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.D.fin)<=10){
    me.D.fin <- c(me.D.fin,rep(0,10-length(me.D.fin)))
  }

  other.C.fin <- other.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.C.fin)<=10){
    other.C.fin <- c(other.C.fin,rep(0,10-length(other.C.fin)))
  }

  other.D.fin <- other.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.D.fin)<=10){
    other.D.fin <- c(other.D.fin,rep(0,10-length(other.D.fin)))
  }

  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, other.C-other.D, me.C.fin-me.D.fin, other.C.fin-other.D.fin, rounds.bin, av.other.def, av.me.def, prev.val.as.seen.abs, diff.bin)

  if(t>1){
    if(is.null(net.pointer) || length(algo.var$memory.net[[net.pointer]]$successors[[action]])==0){
      net.pointer <- NULL
    } else {
      net.pointer <- algo.var$memory.net[[net.pointer]]$successors[[action]][which(sapply(algo.var$memory.net[[net.pointer]]$successors[[action]], FUN=function(x){
        identical(arr,c(algo.var$memory.net[[x]]$state))
      }))]
      if(length(net.pointer)==0){
        net.pointer <- NULL
      }
    }
  }

  action <- Act.QPredictions(state=arr, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=TRUE, net.pointer=net.pointer)

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see, net.pointer))
}

#' A strategy to be used after model has been trained
#'
#' @export
NN.strat.static.end.Ten = function(obs,i,t,history.see=NULL,...) {
  debug.store("NN.strat.static.end.Ten", i, t)  # Store each call for each player
  debug.restore("NN.strat.static.end.Ten", i = 1, t = 6)  # Restore call for player i in period t
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$round <- t

  fin.count <- 10
  start.count <- 20

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  me.C.start <- me.C[1:start.count]
  me.D.start <- me.D[1:start.count]
  other.C.start <- other.C[1:start.count]
  other.D.start <- other.D[1:start.count]

  end.round <- game.state$round>round(game.object$game.pars$T.max/2)
  round <- game.state$round
  if(end.round) round <- 0
  rounds.bin <- to.binary(round,max.dig=ceiling(log2(game.object$game.pars$T.max)))

  rounds.info <- c(end.round, rounds.bin)

  me.C.fin <- me.C[(game.state$round-1):max((game.state$round-fin.count),1)]
  if(length(me.C.fin)<=fin.count){
    me.C.fin <- c(me.C.fin,rep(0,fin.count-length(me.C.fin)))
  }

  me.D.fin <- me.D[(game.state$round-1):max((game.state$round-fin.count),1)]
  if(length(me.D.fin)<=fin.count){
    me.D.fin <- c(me.D.fin,rep(0,fin.count-length(me.D.fin)))
  }

  other.C.fin <- other.C[(game.state$round-1):max((game.state$round-fin.count),1)]
  if(length(other.C.fin)<=fin.count){
    other.C.fin <- c(other.C.fin,rep(0,fin.count-length(other.C.fin)))
  }

  other.D.fin <- other.D[(game.state$round-1):max((game.state$round-fin.count),1)]
  if(length(other.D.fin)<=fin.count){
    other.D.fin <- c(other.D.fin,rep(0,fin.count-length(other.D.fin)))
  }

  arr <- c(me.C.start-me.D.start, other.C.start-other.D.start, me.C.fin-me.D.fin, other.C.fin-other.D.fin, rounds.info)

  arr <- t(as.matrix(arr))

  action.full <- Act.QLearning(state=arr, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=TRUE)
  action <- action.full[["action"]]

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see))
}

#' A strategy to be used after model has been trained
#'
#' @export
Model.strat.maximum.full.Ten = function(obs,i,t,history.see=NULL,...) {
  debug.store("Model.strat.maximum.full.Ten", i, t)  # Store each call for each player
  debug.restore("Model.strat.maximum.full.Ten", i = 1, t = 6)  # Restore call for player i in period t
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$round <- t

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  if(game.object$game.pars$T.max>=game.state$round){
    round.cum <- c(rep(1,game.state$round),rep(0,game.object$game.pars$T.max-game.state$round))
    round.single <- rep(0,game.object$game.pars$T.max)
    round.single[game.state$round] <- 1
  } else {
    round.cum <- rep(1,game.object$game.pars$T.max)
    round.single <- rep(0,game.object$game.pars$T.max)
  }



  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
  } else {
    av.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
  } else {
    av.me.def <- 0
  }

  me.C.fin <- me.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.C.fin)<=10){
    me.C.fin <- c(me.C.fin,rep(0,10-length(me.C.fin)))
  }

  me.D.fin <- me.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(me.D.fin)<=10){
    me.D.fin <- c(me.D.fin,rep(0,10-length(me.D.fin)))
  }

  other.C.fin <- other.C[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.C.fin)<=10){
    other.C.fin <- c(other.C.fin,rep(0,10-length(other.C.fin)))
  }

  other.D.fin <- other.D[(game.state$round-1):max((game.state$round-10),1)]
  if(length(other.D.fin)<=10){
    other.D.fin <- c(other.D.fin,rep(0,10-length(other.D.fin)))
  }

  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C, me.D, other.C, other.D, me.C.fin, me.D.fin, other.C.fin, other.D.fin, round.cum, round.single, av.other.def, av.me.def, prev.val.as.seen,prev.val.as.seen.abs)

  arr <- t(as.matrix(arr))

  action.full <- Act.QLearning(state=arr, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=TRUE)
  action <- action.full[["action"]]

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see))
}

Model.strat.XGBoost.Main = function(obs,i,t,history.see=NULL,...) {
  debug.store("Model.strat.XGBoost.Main", i, t)  # Store each call for each player
  debug.restore("Model.strat.XGBoost.Main", i = 1, t = 6)  # Restore call for player i in period t
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$round <- t

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    av.other.def <- 0
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
  } else {
    av.me.def <- 0
    sum.me.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    quot <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    if(!is.finite(quot)) quot <- 1
  } else {
    diff <- 0
    quot <- 1
  }
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }


  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, other.C-other.D, me.C.fin-me.D.fin, other.C.fin-other.D.fin, rounds, av.other.def,sum.other.def, av.me.def,sum.me.def, prev.val.as.seen.abs, diff, quot)

  arr <- t(as.matrix(arr))

  action.full <- Act.QLearning(state=arr, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=TRUE)
  action <- action.full[["action"]]

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see))
}

Model.strat.Main.real = function(obs,i,t,history.see=NULL,history.real=NULL,me.real=NULL,...) {
  debug.store("Model.strat.Main.real", i, t)  # Store each call for each player
  debug.restore("Model.strat.Main.real", i = 1, t = 2)  # Restore call for player i in period t
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(is.null(history.real)){
    history.real <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(NA,game.object$game.pars$T.max))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  if(t>1){
    history.real[t-1,1] <- me.real
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$history.real <- history.real
  game.state$round <- t

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    av.other.def <- 0
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    av.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")/game.state$round
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
    sum.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")
  } else {
    av.me.def <- 0
    av.me.real.def <- 0
    sum.me.def <- 0
    sum.me.real.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    quot <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    if(!is.finite(quot)) quot <- 1
  } else {
    diff <- 0
    quot <- 1
  }
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.real.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    me.real.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.C.fin <- c(me.real.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.D.fin <- c(me.real.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }


  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, me.real.C-me.real.D, other.C-other.D, me.C.fin-me.D.fin, me.real.C.fin-me.real.D.fin, other.C.fin-other.D.fin, rounds, av.other.def,sum.other.def, av.me.def, av.me.real.def, sum.me.def, sum.me.real.def, prev.val.as.seen.abs, diff, quot)

  arr <- t(as.matrix(arr))

  action.full <- Act.QLearning(state=arr, model=model, model.par=model.par, algo.var=algo.var, game.object=game.object, eval.only=TRUE)
  action <- action.full[["action"]]

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see,history.real,me.real=a))
}

#' @export
Model.strat.Main.real.Exp.Path = function(obs,i,t,history.see=NULL,history.real=NULL,me.real=NULL,...) {
  debug.store("Model.strat.Main.real.Exp.Path", i, t)  # Store each call for each player
  debug.restore("Model.strat.Main.real.Exp.Path", i = 1, t = 1)  # Restore call for player i in period t
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(is.null(history.real)){
    history.real <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(NA,game.object$game.pars$T.max))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  if(t>1){
    history.real[t-1,1] <- me.real
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$history.real <- history.real
  game.state$round <- t

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    av.other.def <- 0
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    av.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")/game.state$round
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
    sum.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")
  } else {
    av.me.def <- 0
    av.me.real.def <- 0
    sum.me.def <- 0
    sum.me.real.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    quot <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    if(!is.finite(quot)) quot <- 1
  } else {
    diff <- 0
    quot <- 1
  }
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.real.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    me.real.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.C.fin <- c(me.real.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.D.fin <- c(me.real.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }


  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, me.real.C-me.real.D, other.C-other.D, me.C.fin-me.D.fin, me.real.C.fin-me.real.D.fin, other.C.fin-other.D.fin, rounds, av.other.def,sum.other.def, av.me.def, av.me.real.def, sum.me.def, sum.me.real.def, prev.val.as.seen.abs, diff, quot)

  arr <- t(as.matrix(arr))
  action <- which.is.max(eval(as.name(game.object$basic.name.eval.model.par))$predict(eval(as.name(game.object$basic.name.eval.model)), eval(as.name(game.object$basic.name.eval.model.par)), arr))
  if(!is.null(eval(as.name(game.object$basic.name.eval.model.par))$name)&&eval(as.name(game.object$basic.name.eval.model.par))$name=="Neural.Network.Basic"){
    k_clear_session()
  }
  
  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see,history.real,me.real=a))
}

#' @export
Model.strat.NN.Main.real.Exp.Path = function(obs,i,t,history.see=NULL,history.real=NULL,me.real=NULL,...) {
  debug.store("Model.strat.NN.Main.real.Exp.Path", i, t)  # Store each call for each player
  debug.restore("Model.strat.NN.Main.real.Exp.Path", i = 1, t = 2)  # Restore call for player i in period t
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(is.null(history.real)){
    history.real <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(NA,game.object$game.pars$T.max))
  }
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  if(t>1){
    history.real[t-1,1] <- me.real
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$history.real <- history.real
  game.state$round <- t

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    av.other.def <- 0
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    av.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")/game.state$round
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
    sum.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")
  } else {
    av.me.def <- 0
    av.me.real.def <- 0
    sum.me.def <- 0
    sum.me.real.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    quot <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    if(!is.finite(quot)) quot <- 1
  } else {
    diff <- 0
    quot <- 1
  }
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.real.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    me.real.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.C.fin <- c(me.real.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.D.fin <- c(me.real.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }


  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c(me.C-me.D, me.real.C-me.real.D, other.C-other.D, me.C.fin-me.D.fin, me.real.C.fin-me.real.D.fin, other.C.fin-other.D.fin, rounds, av.other.def,sum.other.def, av.me.def, av.me.real.def, sum.me.def, sum.me.real.def, prev.val.as.seen.abs, diff, quot)

  arr <- t(as.matrix(arr))

  action <- which.is.max(model.par$predict(model.strat,model.par.strat,arr))
  if(is.na(action)) action <- sample(2,1)

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see,history.real,me.real=a))
}

#' @export
Model.strat.XGBoost.Main.real.Exp.Path.short20 = function(obs,i,t,history.see=NULL,history.real=NULL,me.real=NULL,...) {
  debug.store("Model.strat.NN.Main.real.Exp.Path.short20", i, t)  # Store each call for each player
  debug.restore("Model.strat.NN.Main.real.Exp.Path.short20", i = 1, t = 2)  # Restore call for player i in period t
  j = 3-i

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(is.null(history.real)){
    history.real <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(NA,game.object$game.pars$T.max))
  }


  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  if(t>1){
    history.real[t-1,1] <- me.real
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$history.real <- history.real
  game.state$round <- t

  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))

  rounds <- game.state$round

  if(game.state$round > 1){
    av.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")/game.state$round
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    av.other.def <- 0
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    av.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/game.state$round
    av.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")/game.state$round
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
    sum.me.real.def <- sum(game.state$history.real[1:(game.state$round-1),1]=="D")
  } else {
    av.me.def <- 0
    av.me.real.def <- 0
    sum.me.def <- 0
    sum.me.real.def <- 0
  }

  if(game.state$round > 1){
    diff <- sum(game.state$history.see[1:(game.state$round-1),1]=="D") - sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    quot <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")/sum(game.state$history.see[1:(game.state$round-1),2]=="D")
    if(!is.finite(quot)) quot <- 1
  } else {
    diff <- 0
    quot <- 1
  }
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.real.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    me.real.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.C.fin <- c(me.real.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.real.D.fin <- c(me.real.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }

  if(game.state$round>=2){
    prev.val.as.seen <- sum(sapply((1:(game.state$round-1)),FUN=function(x){
      if(game.state$history.see[x,1]=="C" && game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uCC
      } else if (game.state$history.see[x,1]=="C"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uCD
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="C"){
        round.reward <- game.object$game.pars$uDC
      } else if (game.state$history.see[x,1]=="D"&&game.state$history.see[x,2]=="D"){
        round.reward <- game.object$game.pars$uDD
      } else {
        stop("something bad happened when calculating payoff")
      }
      return(round.reward)
    }))/(game.state$round-1)
  } else {
    prev.val.as.seen <- 1
  }

  prev.val.as.seen.abs <- prev.val.as.seen*(game.state$round-1)/game.object$game.pars$T.max

  arr <- c((me.C-me.D)[1:20], (me.real.C-me.real.D)[1:20], (other.C-other.D)[1:20], (me.C.fin-me.D.fin)[1:20], (me.real.C.fin-me.real.D.fin)[1:20], (other.C.fin-other.D.fin)[1:20], rounds, av.other.def,sum.other.def, av.me.def, av.me.real.def, sum.me.def, sum.me.real.def, prev.val.as.seen.abs, diff, quot)

  arr <- t(as.matrix(arr))

  action <- which.is.max(model.par$predict(model.strat,model.par.strat,arr))
  if(is.na(action)) action <- sample(2,1)

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see,history.real,me.real=a))
}

#' @export
Model.strat.NN.Harper = function(obs,i,t,history.see=NULL,history.real=NULL,me.real=NULL,...) {
  #debug.store("Model.strat.NN.Harper", i, t)  # Store each call for each player
  #debug.restore("Model.strat.NN.Harper", i = 1, t = 1)  # Restore call for player i in period t
  j = 3-i
  
  if(is.null(history.see)){
    history.see <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(0,game.object$game.pars$T.max))
  }
  if(is.null(history.real)){
    history.real <- data.frame(me=rep(0,game.object$game.pars$T.max), other=rep(NA,game.object$game.pars$T.max))
  }
  
  
  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  if(t>1){
    history.real[t-1,1] <- me.real
  }
  game.state <- list()
  game.state$history.see <- history.see
  game.state$history.real <- history.real
  game.state$round <- t
  
  me.C <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.D <- c((!is.na(game.state$history.see[,1])&game.state$history.see[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  me.real.C <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.real)))
  me.real.D <- c((!is.na(game.state$history.real[,1])&game.state$history.real[,1]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.C <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="C"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  other.D <- c((!is.na(game.state$history.see[,2])&game.state$history.see[,2]=="D"),rep(0,game.object$game.pars$T.max-nrow(game.state$history.see)))
  
  rounds <- game.state$round
  
  if(game.state$round > 1){
    sum.other.def <- sum(game.state$history.see[1:(game.state$round-1),2]=="D")
  } else {
    sum.other.def <- 0
  }
  if(game.state$round > 1){
    sum.me.def <- sum(game.state$history.see[1:(game.state$round-1),1]=="D")
  } else {
    sum.me.def <- 0
  }
  
  if(game.state$round==1){
    me.C.fin <- rep(0,game.object$game.pars$T.max)
    me.D.fin <- rep(0,game.object$game.pars$T.max)
    other.C.fin <- rep(0,game.object$game.pars$T.max)
    other.D.fin <- rep(0,game.object$game.pars$T.max)
  } else {
    me.C.fin <- c(me.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    me.D.fin <- c(me.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.C.fin <- c(other.C[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
    other.D.fin <- c(other.D[(game.state$round-1):1],rep(0,game.object$game.pars$T.max-game.state$round+1))
  }
  
  arr <- c((me.C-me.D)[1:2], (other.C-other.D)[1:2], (me.C.fin-me.D.fin)[1:2], (other.C.fin-other.D.fin)[1:2], rounds, sum.other.def, sum.me.def)
  
  arr <- t(as.matrix(arr))
  
  action <- which.is.max(eval(as.name(game.object$basic.name.eval.model.par))$predict(eval(as.name(game.object$basic.name.eval.model)), eval(as.name(game.object$basic.name.eval.model.par)), arr))
  
  if(!is.null(eval(as.name(game.object$basic.name.eval.model.par))$name)&&eval(as.name(game.object$basic.name.eval.model.par))$name=="Neural.Network.Basic"){
    k_clear_session()
  }
  
  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }
  
  return(nlist(a, history.see,history.real,me.real=a))
}

#' @export
Model.strat.RNN.TimeSeries.minimal = function(obs,i,t,history.see=NULL,history.real=NULL,me.real=NULL,...) {
  debug.store("Model.strat.RNN.TimeSeries.minimal", i, t)  # Store each call for each player
  debug.restore("Model.strat.RNN.TimeSeries.minimal", i = 1, t = 1)  # Restore call for player i in period t
  restore.point("Model.strat.RNN.TimeSeries.minimal")
  j = 3-i

  T.max <- 60
  mask.value <- -99
  no.action <- 2

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(mask.value,T.max), other=rep(mask.value,T.max))
  }
  if(is.null(history.real)){
    history.real <- data.frame(me=rep(NA,T.max), other=rep(NA,T.max))
  }

  if(t>nrow(history.see)){ #we have to append our data set
    blank.append <- data.frame(me=rep(mask.value,T.max), other=rep(mask.value,T.max))
    history.see <- rbind(history.see,blank.append)
    blank.append.na <- data.frame(me=rep(NA,T.max), other=rep(NA,T.max))
    history.real <- rbind(history.real,blank.append.na)
  }

  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  if(t>1){
    history.real[t-1,1] <- me.real
  }

  hist <- history.see
  if(any(!is.na(hist[,])&hist[,]=="C")) hist[!is.na(hist[,])&hist[,]=="C"] <- 1
  if(any(!is.na(hist[,])&hist[,]=="D")) hist[!is.na(hist[,])&hist[,]=="D"] <- -1
  if(t<=nrow(hist)){
    hist[t,] <- 0
  }
  hist.real <- history.real
  add.actions <- t(sapply(1:length(hist.real[,1]),FUN=function(y){
    in.res <- rep(0,no.action)
    if(is.na(hist.real[y,1])){
      in.res <- rep(NA,no.action)
    } else if (hist.real[y,1]=="C"){
      in.res[1] <- 1
    } else {
      in.res[2] <- 1
    }
    return(in.res)}
  ))


  arr <- t(data.matrix(cbind(hist,add.actions)))
  arr[is.na(arr)] <- mask.value

  action <- which.is.max(eval(as.name(game.object$basic.name.eval.model.par))$predict(eval(as.name(game.object$basic.name.eval.model)), eval(as.name(game.object$basic.name.eval.model.par)), arr))

  if(is.na(action)) action <- sample(2,1)

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see,history.real,me.real=a))
}

#' @export
Model.strat.RNN.TimeSeries.flexible = function(obs,i,t,history.see=NULL,history.real=NULL,me.real=NULL,...) {
  debug.store("Model.strat.RNN.TimeSeries.flexible", i, t)  # Store each call for each player
  debug.restore("Model.strat.RNN.TimeSeries.flexible", i = 1, t = 61)  # Restore call for player i in period t
  restore.point("Model.strat.RNN.TimeSeries.flexible")
  j = 3-i

  T.max <- game.object$game.pars$T.max
  par <- game.object$encoding.params
  mask.value <- -99
  no.action <- 2

  if(is.null(history.see)){
    history.see <- data.frame(me=rep(mask.value,T.max), other=rep(mask.value,T.max))
  }
  if(is.null(history.real)){
    history.real <- data.frame(me=rep(NA,T.max), other=rep(NA,T.max))
  }

  if(t>nrow(history.see)){ #we have to append our data set
    blank.append <- data.frame(me=rep(mask.value,T.max), other=rep(mask.value,T.max))
    history.see <- rbind(history.see,blank.append)
    blank.append.na <- data.frame(me=rep(NA,T.max), other=rep(NA,T.max))
    history.real <- rbind(history.real,blank.append.na)
  }

  if(t>1){
    history.see[t-1,1] <- obs$a[i]
    history.see[t-1,2] <- obs$a[j]
  }
  if(t>1){
    history.real[t-1,1] <- me.real
  }

  hist <- history.see
  if(any(!is.na(hist[,])&hist[,]=="C")) hist[!is.na(hist[,])&hist[,]=="C"] <- 1
  if(any(!is.na(hist[,])&hist[,]=="D")) hist[!is.na(hist[,])&hist[,]=="D"] <- -1
  if(t<=nrow(hist)){
    hist[t,] <- 0
  }
  hist.real <- history.real

  arr <- matrix(rep(NA,nrow(hist)))

  if(is.null(par)) par <- list()

  if(is.null(par$last.rounds)){ #How many lagged rounds to give
    par$last.rounds <- 3
  }

  if(is.null(par$rounds.bin)){ #Should the round be part of the output? [in binary]
    par$rounds.bin <- TRUE
  }

  if(is.null(par$av.def)){ #Should the average defection rate be part of the encoding
    par$av.def <- TRUE
  }

  if(is.null(par$diff.bin)){ #Should the difference of defection be part of the encoding?
    par$diff.bin <- TRUE
  }

  if(is.null(par$prev.val.as.seen)){ #Should the value which can be calculated be part of the encoding?
    par$prev.val.as.seen <- TRUE
  }

  me.C <- c((!is.na(hist[,1])&hist[,1]==1),rep(0,game.object$game.pars$T.max-nrow(hist)))
  me.D <- c((!is.na(hist[,1])&hist[,1]==-1),rep(0,game.object$game.pars$T.max-nrow(hist)))
  other.C <- c((!is.na(hist[,2])&hist[,2]==1),rep(0,game.object$game.pars$T.max-nrow(hist)))
  other.D <- c((!is.na(hist[,2])&hist[,2]==-1),rep(0,game.object$game.pars$T.max-nrow(hist)))

  if(par$rounds.bin){
    rounds.bin <- t(sapply(1:min(t,nrow(hist)), FUN=function(x) {
      to.binary(x,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))
    }))
    blnk.tmp <- matrix(rep(NA,ncol(rounds.bin)*nrow(arr)),nrow=nrow(arr))
    blnk.tmp[1:nrow(rounds.bin),] <- rounds.bin
    arr <- cbind(arr,blnk.tmp)
  }

  if(par$av.def){
    av.def <- t(sapply(1:min(t,nrow(hist)),FUN=function(x){
      if(x > 1){
        av.other.def <- sum(hist[1:(x-1),2]==-1)/x
      } else {
        av.other.def <- 0
      }
      if(x > 1){
        av.me.def <- sum(hist[1:(x-1),1]==-1)/x
      } else {
        av.me.def <- 0
      }
      return(c(av.other.def, av.me.def))
    }))
    blnk.tmp <- matrix(rep(NA,ncol(av.def)*nrow(arr)),nrow=nrow(arr))
    blnk.tmp[1:nrow(av.def),] <- av.def
    arr <- cbind(arr,blnk.tmp)
  }

  if(par$diff.bin){
    diff.bin <- t(sapply(1:min(t,nrow(hist)),FUN=function(x){
      if(x > 1){
        diff <- sum(hist[1:(x-1),1]==-1) - sum(hist[1:(x-1),2]==-1)
      } else {
        diff <- 0
      }
      if(diff < 0){
        diff <- abs(diff)
        flip <- 1
      } else {
        flip <- 0
      }
      return(c(flip,to.binary(diff,max.dig=ceiling(log2(game.object$game.pars$T.max+1)))))
    }))
    blnk.tmp <- matrix(rep(NA,ncol(diff.bin)*nrow(arr)),nrow=nrow(arr))
    blnk.tmp[1:nrow(diff.bin),] <- diff.bin
    arr <- cbind(arr,blnk.tmp)
  }

  last.rounds <- t(sapply(1:min(t,nrow(hist)), FUN=function(x){
    if(x==1){
      fin <- rep(0,par$last.rounds*2)
      return(fin)
    }
    me.C.fin <- me.C[(x-1):max((x-par$last.rounds),1)]
    if(length(me.C.fin)<=par$last.rounds){
      me.C.fin <- c(me.C.fin,rep(0,par$last.rounds-length(me.C.fin)))
    }

    me.D.fin <- me.D[(x-1):max((x-par$last.rounds),1)]
    if(length(me.D.fin)<=par$last.rounds){
      me.D.fin <- c(me.D.fin,rep(0,par$last.rounds-length(me.D.fin)))
    }

    other.C.fin <- other.C[(x-1):max((x-par$last.rounds),1)]
    if(length(other.C.fin)<=par$last.rounds){
      other.C.fin <- c(other.C.fin,rep(0,par$last.rounds-length(other.C.fin)))
    }

    other.D.fin <- other.D[(x-1):max((x-par$last.rounds),1)]
    if(length(other.D.fin)<=par$last.rounds){
      other.D.fin <- c(other.D.fin,rep(0,par$last.rounds-length(other.D.fin)))
    }
    return(c(me.C.fin-me.D.fin, other.C.fin-other.D.fin))
  }))
  blnk.tmp <- matrix(rep(NA,ncol(last.rounds)*nrow(arr)),nrow=nrow(arr))
  blnk.tmp[1:nrow(last.rounds),] <- last.rounds
  arr <- cbind(arr,blnk.tmp)

  if(par$prev.val.as.seen){
    prev.val.as.seen <- sapply(1:min(t,nrow(hist)), FUN=function(x){
      if(x>=2){
        prev.val.as.seen <- sum(sapply((1:(x-1)),FUN=function(y){
          if(hist[y,1]==1 && hist[y,2]==1){
            round.reward <- game.object$game.pars$uCC
          } else if (hist[y,1]==1&&hist[y,2]==-1){
            round.reward <- game.object$game.pars$uCD
          } else if (hist[y,1]==-1&&hist[y,2]==1){
            round.reward <- game.object$game.pars$uDC
          } else if (hist[y,1]==-1&&hist[y,2]==-1){
            round.reward <- game.object$game.pars$uDD
          } else {
            stop("something bad happened when calculating payoff")
          }
          return(round.reward)
        }))
      } else {
        prev.val.as.seen <- 0
      }
      prev.val.as.seen <- prev.val.as.seen/game.object$game.pars$T.max
    })
    blnk.tmp <- matrix(rep(NA,nrow(arr)),nrow=nrow(arr))
    blnk.tmp[1:length(prev.val.as.seen),] <- prev.val.as.seen
    arr <- cbind(arr,blnk.tmp)
  }

  if(nrow(arr)==1){
    arr <- t(as.matrix(arr[,-1]))
  } else {
    arr <- arr[,-1]
  }
  arr.big <- matrix(rep(NA,ncol(arr)*(nrow(hist))),ncol=ncol(arr))
  arr.big[1:nrow(arr),] <- arr

  hist.real <- hist.real
  add.actions <- t(sapply(1:length(hist.real[,1]),FUN=function(y){
    in.res <- rep(0,2)
    if(is.na(hist.real[y,1])){
      in.res <- rep(NA,2)
    } else if (hist.real[y,1]==1){
      in.res[1] <- 1
    } else {
      in.res[2] <- 1
    }
    return(in.res)}
  ))

  arr <- t(data.matrix(cbind(arr.big,add.actions)))
  arr[is.na(arr)] <- mask.value

  action <- which.is.max(eval(as.name(game.object$basic.name.eval.model.par))$predict(eval(as.name(game.object$basic.name.eval.model)), eval(as.name(game.object$basic.name.eval.model.par)), arr))

  if(is.na(action)) action <- sample(2,1)

  if(action==1){
    a <- "C"
  } else {
    a <- "D"
  }

  return(nlist(a, history.see,history.real,me.real=a))
}
