library(ReinforcementLearningwithR)
require(compiler)

strat <- c("strat.a") #"strat.a" to "strat.i" with get.antistrat or any other strategy (vector) without "self" implies self play.
antistrat <- "none" #or "none" if none of the strategies of above or several strategies are given.
file.name <- paste0("opt.run.",paste0(strat,collapse="."),".",Sys.Date(),".NN") #File, where the results are saved

#Parameters of game. Currently supported: "BattleOfStrategiesThesis.Baseline" and "BattleOfStrategies2019"
game.setting <- "BattleOfStrategiesThesis.Baseline"

block.no <- 150 #Number of Blocks to Play. More should be always better, but time increases somewhat linear (given the memory is sufficiently full), while we have strong diminishing return in the final performance depending on the complexity of the strategy.
eval.no <- 1000 #Number of played matches to evaluate final performance of model with the model StratTourn
T.max <- 60 #Number of periods of game. Note: If one wants to change this, it is recommended, that algo.par$batch.size is changed as well.

#If Memory initilization through self.play is wished it may be set here. 0 means no initialization.
memory.initialization <- 0

#Set the most important parameters of a Neural Network here.
## nodes.layer.1 measures the number of nodes in the first hidden layer. More means more complex strategies may be tackled, but risks overfitting and might need more training data and more epochs.
## nodes.layer.2 measures the number of nodes in the second hidden layer. More means more complex strategies may be tackled, but risks overfitting and might need more training data and more epochs.
## batch.size.train is the NN internal size of how big a neural network batch should be (see https://stats.stackexchange.com/questions/153531/what-is-batch-size-in-neural-network). More means more stable, but slower/more epochs needed.
##epochs is the number of how often the complete training data should be propagated through the network. More means more overfitting but better accuracy in describing the training data. Very relevant for speed.
##give.up.precision controls a method used in the thesis of Martin Kies: After training epochs times it is checked whether the new LOSS is better than the one in the block before. If not the training is repeated up to give.up.precision times. The actual number of epochs such may vary between epochs and epochs*give.up.precision. As this is the showcase for a benchmark case the default uses no give.up.precision
func.approx.params <- list(nodes.layer.1=126, nodes.layer.2=64, batch.size.train=32, epochs=50, give.up.precision=0) 

#This defines the function which allows an easy access to the package
generate.best.strat <- function(strat, antistrat="none", game.setting, func.approx.params, memory.initialization, block.no, eval.no, T.max, file.name){
  restore.point("generate.best.strat")
  
  game.object <- Get.Game.Object.PD(encoding.state="Harper",game.setting=game.setting,strats=strat, eval.strategy = "Model.strat.NN.Harper")
  assign("game.object",game.object,envir=.GlobalEnv) #necessary for the tournament
  
  #Define the non-changing parameters of the algorithm like which features and parameters to be used.
  algo.par <- Get.Def.Par.QLearningPersExpPath(setting="QLearning.Basic")
  algo.par$gamma <- game.object$game.pars$delta #recommended as the algorithm otherwise optimises for a different setting as the game itself
  
  #Define the function approximator and its parameters
  model.par <- Get.Def.Par.Neural.Network(setting="ThesisBasic")
  model.par$hidden.nodes[1] <- func.approx.params$nodes.layer.1
  model.par$hidden.nodes[2] <- func.approx.params$nodes.layer.2
  model.par$batch.size.train <- func.approx.params$batch.size.train
  model.par$epochs <- func.approx.params$epochs
  model.par$give.up.precision <- func.approx.params$give.up.precision
  if(func.approx.params$give.up.precision==0){
    model.par$enforce.increasing.precision <- FALSE
  } else {
    model.par$enforce.increasing.precision <- TRUE
  }
  
  
  #Setup the model and other variational aspects
  evaluator <- Setup.QLearningPersExpPath(game.object, algo.par=algo.par, model.par=model.par)
  if(memory.initialization==0){
    memory.init <- "none"
    start.w.training <- FALSE
  } else {
    memory.init <- "self.play"
    start.w.training <- TRUE
  }
  algo.var <- Initialise.QLearningPersExpPath(game.object, algo.par, memory.init=memory.init, memory.param=list(no=memory.initialization), model.par=model.par)
  
  #Execute the algorithm
  res <- Train.QLearningPersExpPath(evaluator=evaluator, model.par=model.par, algo.par=algo.par, algo.var=algo.var, game.object = game.object, blocks=block.no, eval.only=FALSE, start.w.training = start.w.training,out.file=paste0(file.name,".tmp"))
  
  #Save Memory & model
  evaluator <- res$evaluator
  algo.var <- res$algo.var
  idio.name <- paste0("opt.run.NN.full.",paste0(strat,collapse="."))
  file.name <- paste0(idio.name, format(Sys.time(), "%d-%b-%Y %H.%M"),"before.StratTourn", sep=" ")
  save(evaluator, algo.var, algo.par, game.object, model.par, file=file.name)
  
  # Do the StratTourn evaluation
  game = make.pd.game(uCC=game.object$game.pars$uCC, uCD=game.object$game.pars$uCD, uDC=game.object$game.pars$uDC, uDD=game.object$game.pars$uDD, err.D.prob=game.object$game.pars$err.D.prob, err.C.prob=game.object$game.pars$err.C.prob, delta=game.object$game.pars$delta)
  
  #Prepare list of strategies for StratTourn
  if(antistrat!="none"){
    strat.tourn = nlist(Model.strat.NN.Harper,get(strat), get(antistrat))
    names(strat.tourn)[2] <- strat
    names(strat.tourn)[3] <- antistrat
  } else {
    strat.tourn = c(Model.strat.NN.Harper,lapply(strat,FUN=function(x){
      if(x!="self"){
        return(get(x))
      } else {
        return(NULL)
      }
    }))
    names(strat.tourn) <- seq_along(strat.tourn)
    strat.tourn[sapply(strat.tourn, is.null)] <- NULL
    names(strat.tourn)[1] <- "Model.strat.NN.Harper"
    names(strat.tourn)[2:length(names(strat.tourn))] <- strat[strat!="self"]
  }
  
  #Initialize tournament
  tourn = init.tournament(game=game, strat=strat.tourn)
  tourn = run.tournament(tourn=tourn, R = eval.no, T.max=T.max)
  
  #Calculate a single relevant statistic. If against a single strategy this is the return against this strategy. If against a tournament this is the tournament performance.
  if(length(strat)==1){
    r.limit <- get.matches.vs.matrix(tourn$dt)["Model.strat.NN.Harper",strat]
  } else {
    srfm <- strat.rank.from.matches(tourn$dt)
    r.limit <- srfm[srfm$strat=="Model.strat.NN.Harper",mean]
  }
  
  file.name <- paste0(idio.name, format(Sys.time(), "%d-%b-%Y %H.%M"), sep=" ")
  
  #Save Memory & model
  save(evaluator, algo.var, algo.par, game.object, model.par, r.limit, tourn, file=file.name)
  
  #Show Tournament
  show.tournament(tourn)
}

disable.restore.points(TRUE)
enableJIT(3)
generate.best.strat(strat=strat, antistrat=antistrat, game.setting=game.setting, func.approx.params=func.approx.params, memory.initialization=memory.initialization, block.no=block.no, eval.no=eval.no, T.max=T.max, file.name=file.name)
