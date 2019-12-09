#Model Persistent QPathing with Exploration Path

#' Default Parameters for (improved) Q-Learning
#' 
#' Returns the 'algo.par' necessary for \code{\link{Setup.QLearningPersExpPath}}, \code{\link{Initialise.QLearningPersExpPath}} and \code{\link{Train.QLearningPersExpPath}}
#' 
#' @param setting String which defines the setting of the default parameters. \itemize{
#'   \item "ThesisOpt.XGB" - Default setting. The same parameters are used as for the final results of the thesis of Martin Kies. Uses the Gradient Boosting Variant.
#'   \item "ThesisOpt.RNN" - The same parameters are used as for the final results of the thesis of Martin Kies. Uses the Recurrent Neural Network (LSTM) Variant.
#'   \item "Legacy.v.0.1.6" - Uses the default settings of version 0.1.6 of this package
#' }
#'
#' @export
Get.Def.Par.QLearningPersExpPath <- function(setting="ThesisOpt.XGB"){
  
  if(is.null(setting)||setting=="ThesisOpt.XGB"){
    algo.params <- Get.Def.Par.QLearningPersExpPath.Legacy.ThesisOpt.XGB()
  } else if (setting=="ThesisOpt.RNN"){
    algo.params <- Get.Def.Par.QLearningPersExpPath.Legacy.ThesisOpt.RNN()
  } else if (setting=="Legacy.v.0.1.6"){
    algo.params <- Get.Def.Par.QLearningPersExpPath.Legacy.v.0.1.6()
  } else {
    stop("Can't generate default parameters algo.par for QLearningPersExpPath due to misspecified setting.")
  }
  
  return(algo.params)
}

#' Controlled Copying of Models
#'
#' Returns a new, updated evaluator
#'
#' @param evaluator Evaluation consisting of the two models
#' @param new.model Model with which we want to update. If \code{NULL}, no update is done.
#' @param new.model.surp If not \code{NULL}, the surprise model is updated accordingly.
#' @param new.model.fam If not \code{NULL}, the familiarity model is updated accordingly.
#' @param which May be `both`, `cur` or `best` and updates respectively.
#' @export
Update.Evaluator.QLearningPersExpPath <- function(evaluator=evaluator, new.model=NULL, new.model.surp=NULL, new.model.fam=NULL, which.update="both", model.par){
  #restore.point("Update.Evaluator.QLearningPersExpPath")

  #Update main model?
  if(!is.null(new.model)){
    switch(which.update,
           both={
             update.best <- TRUE
             update.cur <- TRUE
           },
           cur={
             update.best <- FALSE
             update.cur <- TRUE
           },
           best={
             update.best <- TRUE
             update.cur <- FALSE
           },
           {#default
             stop("which.update does not specify which model should be updated. 'both', 'cur' and 'best' are supported.")
           }
    )

    if(model.par$name=="Neural.Network.Basic"){
      if(update.best) evaluator$model.best <- new.model
      if(update.cur) evaluator$model.cur <- new.model
    } else {
      if(update.best) evaluator$model.best <- new.model
      if(update.cur) evaluator$model.cur <- new.model
    }
  }

  #Update Surprise?
  if(!is.null(new.model.surp)){
    evaluator$model.surp <- new.model.surp
  }

  #Update Familiarity?
  if(!is.null(new.model.fam)){
    evaluator$model.fam <- new.model.fam
  }

  return(evaluator)
}

# Calculate familiarity Score
#' @export
Update.Familiarity.Model.QLearningPersExpPath <- function(game.object, model.fam, model.par.fam, x_train, action){
  restore.point("Update.Familiarity.Model.QLearningPersExpPath")

  n.act <- game.object$game.par(game.object)$output.nodes

  action.space.composite <- t(sapply(action,FUN=function(x){return((1:n.act)[-x])}))
  action.space.composite.long <- matrix(action.space.composite,ncol=1)
  action.all <- matrix(c(action,action.space.composite.long),ncol=1)
  y_train.all <- matrix(c(rep(1,length(action)),rep(0,length(action.all)-length(action))),ncol=1)

  if(model.par.fam$name=="RNN.Basic"){
    states.l.list <- lapply(1:n.act,FUN=function(x){x_train})
    x_train.all <- abind(states.l.list, along=1)
  } else {
    x_train.all <- do.call(rbind, replicate(n.act, x_train, simplify=FALSE))
  }

  model.fam <- model.par.fam$train(model.fam, model.par.fam, x_train.all, y_train.all, action.all)

  return(model.fam)
}

# Calculate degree of Surprise
#' @export
Update.Surprise.Model.QLearningPersExpPath <- function(game.object, model.surp, model.par.surp, x_train, action, surprise.score){
  #restore.point("Update.Surprise.Model.QLearningPersExpPath")
  model.surp <- model.par.surp$train(model.surp, model.par.surp, x_train, surprise.score, action)$model
  return(model.surp)
}

#Memory.Function
#' Trains model based on memory
#'
#' @param model.par Model parameters for main model
#' @param model.par.surp Model parameters for Surprise model. If \code{NULL}, then no suprise model is trained.
#' @param model.par.fam Model parameters for Familiarity model. If \code{NULL}, then no familiarity model is trained.
#' @param type May be init for memory initialization and replay for replay.
#' @export
Train.On.Memory.QLearningPersExpPath <- function(model, model.surp=NULL, model.fam=NULL, model.par, model.par.surp=NULL, model.par.fam=NULL, algo.var, algo.par, game.object, type="init", mem.replay=NULL, ...){
  Weigh.History <- function(y.pred,Q.path.train, minibatch.no=NULL, algo.par, mem.length, type, MC.factor=NULL){
    #restore.point("Weigh.History")

    if(type=="init"){
      weights <- rep(1, times=mem.length)
    } else {
      if(algo.par$hybrid.switch){
        weights <- MC.factor
      } else {
        weights <- minibatch.no/mem.length #weights according to memory oldness
      }

    }

    Q.path.train[is.na(Q.path.train)] <- y.pred[is.na(Q.path.train)]

    res <- (1-weights)*y.pred + weights*Q.path.train

    return(res)
  }

  restore.point("Train.On.Memory.QLearningPersExpPath")

  #Generate Training sample (Batch) out of complete memory
  if(type=="replay"){
    if(length(algo.var$memory)<algo.par$batch.size){
      batch.size <- length(algo.var$memory)
    } else {
      batch.size <- algo.par$batch.size
    }

    if(algo.par$force.last>batch.size){
      force.last <- batch.size
    } else {
      force.last <- algo.par$force.last
    }
    if(force.last==batch.size){
      minibatch <- algo.var$memory[(length(algo.var$memory)-force.last+1):length(algo.var$memory)]
      minibatch.no <- (length(algo.var$memory)-force.last+1):length(algo.var$memory)
    } else {
      if(force.last==0){
        minibatch.no <- c(sample(1:length(algo.var$memory), batch.size))
      } else {
        minibatch.no <- c((length(algo.var$memory)-force.last+1):length(algo.var$memory),sample(1:length(algo.var$memory), batch.size-force.last))
      }
      minibatch <- algo.var$memory[minibatch.no]
    }
  } else if (type=="init"){
    minibatch <- algo.var$memory
    minibatch.no <- NULL
    batch.size <- length(minibatch)
  } else {
    stop("type in Train.On.Memory.QLearningPersExpPath not supported")
  }

  if(!algo.par$use.rnn){ #Default case (NN, XGBoost, ... etc)
    x_train <- t(sapply(minibatch, FUN=function(x){
      return(x$state)
    }))

    x_next_state <- t(sapply(minibatch, FUN=function(x){
      return(x$next.state)
    }))

    action <- sapply(minibatch, FUN=function(x){
      return(x$action)
    })
  } else {
    if(model.par$single.dimensional){
      input.nodes <- game.object$game.par(game.object)$input.nodes.time
      output.nodes <- game.object$game.par(game.object)$output.nodes
    } else {
      stop("With RNN only single.dimensional is supported") #Is not a problem here but later on
      input.nodes <- game.object$game.par(game.object)$input.nodes.time
    }
    dim.sizes <- c(batch.size,dim(minibatch[[1]]$state)[2],input.nodes)
    x_train <- array(NA, dim=dim.sizes)
    x.state.extract <- lapply(minibatch, FUN=function(x){
      return(t(x$state))
    })

    action <- sapply(minibatch, FUN=function(x){
       return(x$action)
    })

    for (j in 1:length(x.state.extract)) {
      x_train[j,,] <- x.state.extract[[j]]
    }

    x_next_state <- array(NA, dim=dim.sizes)
    x.next.state.extract <- lapply(minibatch, FUN=function(x){
      return(t(x$next.state))
    })
    for (j in 1:length(x.next.state.extract)) {
      x_next_state[j,,] <- x.next.state.extract[[j]]
    }

    # for (j in 1:length(x.next.state.extract)) {
    #   cur.round <- sum(action.hist[[j]][,1]!=model.par$mask.value)+1
    #   action.long <- rep(0,output.nodes)
    #   action.long[action[j]] <- 1
    #   x_next_state[j,,1:input.nodes.state] <- x.next.state.extract[[j]]
    #   x_next_state[j,,(input.nodes.state+1):input.nodes] <- action.hist[[j]]
    #   if(cur.round <= dim(x_next_state)[2]){
    #     x_next_state[j,cur.round,(input.nodes.state+1):input.nodes] <- action.long
    #   } else {
    #     #do nothing, but should never be reached.
    #   }
    # }
  }

  reward <- sapply(minibatch, FUN=function(x){
    return(x$reward)
  })

  done <- sapply(minibatch, FUN=function(x){
    return(x$done)
  })

  if(algo.par$hybrid.Q || algo.par$MC){
    Q.path <- sapply(minibatch, FUN=function(x){
      return(x$Q.path)
    })
  }

  if(algo.par$curio.beta>0){
    R.phi <- sapply(minibatch,FUN=function(x){
      R.phi.orig <- x$R.phi
      R.phi <- R.phi.orig * algo.par$curio.decay.memory^(algo.var$experience.count-x$experience.count)
      return(R.phi)
    })
  }

  if(!is.null(model.fam)){
    fam.score <- model.par.fam$predict(model.fam,model.par.fam, x_train, action)
    model.fam <- Update.Familiarity.Model.QLearningPersExpPath(game.object, model.fam, model.par.fam, x_train, action)$model #Train new model
  }

  if(type=="replay"){
    replay.no <- algo.par$replay.intensive
  } else {
    if(is.null(mem.replay)){
      replay.no <- 1
    } else {
      replay.no <- mem.replay
    }
  }
  if(algo.par$only.experienced){
    Q.model.old <- model.par$predict(model,model.par, x_train, action)
  } else {
    Q.model.old <- model.par$predict(model,model.par, x_train)
  }


  for(i in 1:replay.no){
    if(replay.no>1) print(paste0(i,". round of replay(",replay.no,")"))

    Q.model.next <- apply(model.par$predict(model,model.par, x_next_state), 1, FUN=max)
    #Go through all given that action was taken
    if(algo.par$only.experienced){
      y.pred <- (1-algo.par$a)*Q.model.old + algo.par$a * (reward + algo.par$gamma*Q.model.next) #normal update "Q learning
    } else {
      y.pred <- t(sapply(1:length(action),FUN=function(x){
        #restore.point("y_train.intern")
        res <- Q.model.old[x,]
        res[action[x]] <- (1-algo.par$a)*Q.model.old[x,action[x]] + algo.par$a * (reward[x] + algo.par$gamma*Q.model.next[x]) #normal update "Q learning but for both actions
        return(res)
      }))
    }
    if(algo.par$hybrid.Q || algo.par$MC){
      if(algo.par$only.experienced){
        Q.path.train <- (1-algo.par$hybrid.Q.a.MC)*Q.model.old + algo.par$hybrid.Q.a.MC*(Q.path)
      } else {
        Q.path.train <- t(sapply(1:length(action),FUN=function(x){
          #restore.point("y_train.intern")
          res <- Q.model.old[x,]
          res[action[x]] <- (1-algo.par$hybrid.Q.a.MC)*Q.model.old[x,action[x]] + algo.par$hybrid.Q.a.MC*(Q.path[x])
          return(res)
        }))
      }
      if(algo.par$hybrid.Q){
        y_train <- Weigh.History(y.pred,Q.path.train, minibatch.no, algo.par, mem.length=length(algo.var$memory), type, MC.factor=algo.var$MC.factor)
      } else if (algo.par$MC){
        y_train <- Q.path.train
      } else { # should never be reached
        stop("Something is wrong with the parameters hybrid.Q or MC")
      }
    } else {
        y_train <- y.pred
    }

    if(algo.par$curio.beta>0){
      y_train <- y_train+R.phi
    }

    if(algo.par$block.expl.surp!=0 || (algo.par$block.expl.multi!=0 && (algo.par$expl.path.multi.start.frac.surp!=0||algo.par$expl.path.multi.end.frac.surp!=0))){ #We want surprise scaling
      if(!algo.par$only.experienced){
        stop("Surprise factoring only implemented with the option \'only experienced\'")
      }
      surprise.score <- (Q.model.old-y_train)^2
      model.surp <- Update.Surprise.Model.QLearningPersExpPath(game.object, model.surp, model.par.surp, x_train, action, surprise.score) #Train new model
    }

    #Setup necessary precision
    if(!is.null(model.par$enforce.increasing.precision)&&model.par$enforce.increasing.precision==TRUE){
      prec.repeat <- TRUE
    } else {
      prec.repeat <- FALSE
    }

    if(type=="init"){
      model.par$epochs <- algo.par$mem.init.epochs
    }

    #Main Part -> Training of model
    ## We want stability and e.g. NN might produce NA values
    #good.vals <- (complete.cases(x_train)&complete.cases(y_train))
    #x_train <- x_train[good.vals,]
    #if(algo.par$only.experienced){
    #  y_train <- y_train[good.vals]
    #} else {
    #  y_train <- y_train[good.vals,]
    #}
    #action <- action[good.vals]

    #restore.point("before model.train")
    if(algo.par$only.experienced){
      model.train <- model.par$train(model, model.par, x_train, y_train, action)
    } else {
      model.train <- model.par$train(model, model.par, x_train, y_train)
    }

    model <- model.train$model
    fit.obj <- model.train$fit.obj
    #restore.point("before.pre.training")
    #If model is not trained enough, repeat Training until ready
    if(is.null(model.par$single.train) || !model.par$single.train){
      if(type=="replay"){
        if(prec.repeat && is.null(algo.var$cur.loss)){
          algo.var$cur.loss <- mean(fit.obj$metrics$loss)
        } else if (prec.repeat) {
          counter <- 0
          while(mean(fit.obj$metrics$loss)>algo.var$cur.loss){
            counter <- counter+1
            writeLines(paste0("Loss was still ",round(mean(fit.obj$metrics$loss),5), " but ",round(algo.var$cur.loss,5), " needed","\n",collapse=""))
            if(algo.par$only.experienced){
              model.train <- model.par$train(model, model.par, x_train, y_train, action)
            } else {
              model.train <- model.par$train(model, model.par, x_train, y_train)
            }
            model <- model.train$model
            fit.obj <- model.train$fit.obj
            if(counter>model.par$give.up.precision){
              break
            }
          }
        }
        if(prec.repeat){
          algo.var$cur.loss <- mean(fit.obj$metrics$loss)
        }
      } else {
        stop.cond <- FALSE
        while(!stop.cond){
          cur.loss <- mean(fit.obj$metrics$loss)
          writeLines(paste0("Loss is ",round(mean(fit.obj$metrics$loss),5), ", so we continue","\n",collapse=""))
          if(algo.par$only.experienced){
            model.train <- model.par$train(model, model.par, x_train, y_train, action)
          } else {
            model.train <- model.par$train(model, model.par, x_train, y_train)
          }
          model <- model.train$model
          fit.obj <- model.train$fit.obj
          if(mean(fit.obj$metrics$loss)>cur.loss*(1-algo.par$mem.expected.improve)){
            stop.cond <- TRUE
          }
        }
      }
    }
  }
  return(nlist(model, model.fam, model.surp, algo.var))
}

#Q-Learning with Discrete Choices
#' Sets up a model based on model parameters
#'
#' @param game.object Game Object as defined by \code{Get.Game.Object.<NAME>}.
#' @param model.par Model parameters. If \code{NULL}, the function \code{Get.Def.Par.QLearningPersExpPath()} is called.
#' @param model.par.surp Model parameters for surprise model. If \code{NULL}, we assume that we do not want to use a surprise model.
#' @param model.par.fam Model parameters for the model which tries to gauge familiarity with the specific state/action combination. If \code{NULL}, we assume that no familiarity model is needed.
#' @export
Setup.QLearningPersExpPath <- function(game.object, algo.par=NULL, model.par, model.par.surp=NULL, model.par.fam=NULL){
  restore.point("Setup.QLearningPersExpPath")
  if(is.null(algo.par)){
    algo.par <- Get.Def.Par.QLearningPersExpPath()
  }

  game.par <- game.object$game.par(game.object)

  if(is.null(model.par$name)){
    stop("name parameter of model.par missing!")
  }

  model.best <- model.par$setup(model.par, game.par)
  model.cur <- model.par$setup(model.par, game.par)

  if(algo.par$block.expl.fam>0||(algo.par$block.expl.multi>0 && (algo.par$expl.path.multi.start.frac.fam!=0||algo.par$expl.path.multi.end.frac.fam!=0))){ #Setup only relevant if used at all
    if(is.null(model.par.fam$name)){
      stop("name parameter of model.par.fam missing!")
    }
    model.fam <- model.par.fam$setup(model.par.fam, game.par)
  } else {
    model.fam <- NULL
  }

  if(algo.par$block.expl.surp>0||(algo.par$block.expl.multi>0 && (algo.par$expl.path.multi.start.frac.surp!=0||algo.par$expl.path.multi.end.frac.surp!=0))){ #Setup only relevant if used at all
    if(is.null(model.par.surp$name)){
      stop("name parameter of model.par.surp missing!")
    }
    model.surp <- model.par.fam$setup(model.par.surp, game.par)
  } else {
    model.surp <- NULL
  }

  evaluator <- nlist(model.best, model.cur, model.surp, model.fam)

  return(evaluator)
}

#' Set changeable model variables
#'
#' Returns a list with the following items \itemize{
#' \item \strong{epsilon} [only in case of epsilon.greedy] Specifies how often the Algorithm tries a random move. Initialized with \code{epsilon.start} of \code{model.par}.
#' \item \strong{memory} Memory of the Algorithm given past games. May be intitialised to give a starting point to learning.
#' }
#'
#' @param game.object A Game Object as defined by \code{Get.Game.Object.<Name>}. Necessary in the case of memory intitialisation.
#' @param model.par Parameters of QLearning specification. Have to be specified and should be identical to the model.par as given to \code{Setup.QLearningPersExpPath()}.
#' @param memory.init Which type of initialization should take place? It \code{NULL}, the option \code{none} is used. The following types are supported \itemize{
#' \item \strong{none} No initialization takes place. Memory is an empty list.
#' \item \strong{self.play} The other strategies play against themselves - to understand possible secret handshakes. The following \code{memory.param} are expected: \itemize{
#' \item \strong{no} How often should the other strategies play against themselves?
#' }
#' }
#' If combinations of different memories are needed, one can use the function \code{Extend.Memory.QLearningPersExpPath()}
#' @param memory.param Parameters necessary for the chosen \code{memory.init}.
#' @param model.par Parameters of model i.e. Neural Network. Currently only used for RNN.
#'@export
Initialise.QLearningPersExpPath <- function(game.object=NULL, algo.par, memory.init=NULL, memory.param = NULL, model.par=NULL){
  restore.point("Initialise.QLearningPersExpPath")
  if(is.null(memory.init)){
    memory.init <- "none"
  }

  algo.var <- list()
  algo.var$epsilon <- algo.par$epsilon.start
  algo.var$path.goal.var <- algo.par$expl.path.var.start
  algo.var$path.goal.shock <- algo.par$expl.path.shock.start
  algo.var$path.goal.surp <- algo.par$expl.path.surp.start
  algo.var$path.goal.fam <- algo.par$expl.path.fam.start
  algo.var$expl.path.var <- algo.par$expl.path.var.start.var
  algo.var$expl.path.shock <- algo.par$expl.path.shock.start.shock
  algo.var$expl.path.surp <- algo.par$expl.path.surp.start.surp
  algo.var$expl.path.fam <- algo.par$expl.path.fam.start.fam
  algo.var$expl.path.vs.var <- algo.par$expl.path.vs.start.var
  algo.var$expl.path.vs.shock <- algo.par$expl.path.vs.start.shock
  algo.var$expl.path.multi.var <- algo.par$expl.path.multi.start.var
  algo.var$expl.path.multi.shock <- algo.par$expl.path.multi.start.shock
  algo.var$expl.path.multi.surp <- algo.par$expl.path.multi.start.surp
  algo.var$expl.path.multi.fam <- algo.par$expl.path.multi.start.fam
  algo.var$MC.factor <- 1
  algo.var$experience.count <- 1
  algo.var$memory <- list() #items: state, action, next.state, reward

  #Curiosity
  if(algo.par$curio.beta>0){
    algo.var$phi.table <- Start.Phi.Table(game.object$feature.types(game.object))
  } else {
    algo.var$phi.table <- NULL
  }


  if (memory.init != "none") {
    algo.var <- Extend.Memory.QLearningPersExpPath(algo.var, algo.par=algo.par, game.object=game.object, memory.type=memory.init, memory.param=memory.param, model.par=model.par)
  }

  return(algo.var)
}

#' Extend Memory by specified experiences
#'
#' Returns modified algo.var, where memory has been extended as specified.
#'
#' @param algo.var A variable algorithm object, where to be modified variables are saved. Given by \code{Initialise.QLearningPersExpPath()}
#' @param game.object A game object as defined by \code{Get.Game.Object.<Name>}.
#' @param memory.init Which type of extension should take place? The following types are supported \itemize{
#' \item \strong{self.play} The other strategies play against themselves - to understand possible secret handshakes. If I am myself part of the other strategies, the "self" strategy is ignored. The following \code{memory.param} are expected: \itemize{
#' \item \strong{no} How often should the other strategies play against themselves?
#' }
#' \item \strong{solid.foundation} Not only self.play, but also a random initialisation with increasing defect probabilities.The following \code{memory.param} are expected: \itemize{
#' \item \strong{self.no} How often should the other strategies play against themselves?
#' \item \strong{rep.no} How often should a random strategy be played? The defection probability is linearly increased.
#' }
#' }
#' If combinations of different memories are needed, one can use the function multiple times.
#' @param memory.param Parameters necessary for the chosen \code{memory.type}.
#' @param model.par Parameters of model (i.e. Neural Network). Currently only used for RNNs mask value.
#'@export
Extend.Memory.QLearningPersExpPath <- function(algo.var, algo.par=NULL, game.object, memory.type, memory.param=NULL, model.par=NULL){
  restore.point("Extend.Memory.QLearningPersExpPath")

  if(memory.type == "self.play"){
    new.mem <- unlist(lapply(1:memory.param$no,FUN=function(x){
      if(!is.null(game.object$supports) && any(game.object$supports == "memory.self.play")){
        return(game.object$memory.self.play(game.object, algo.par))
      }
    }), recursive=FALSE)
  } else if (memory.type== "solid.foundation"){
    self.mem <- unlist(lapply(1:memory.param$self.no,FUN=function(x){
      if(!is.null(game.object$supports) && any(game.object$supports == "memory.self.play")){
        return(game.object$memory.self.play(game.object, algo.par))
      }
    }), recursive=FALSE)
    def.arr <- seq(0,1,length.out = memory.param$rep.no)
    rand.mem <- unlist(lapply(def.arr,FUN=function(x){
      if(!is.null(game.object$supports) && any(game.object$supports == "memory.random.play")){
        algo.par$def.prob <- x
        return(game.object$memory.random.play(game.object, algo.par))
      }
    }), recursive=FALSE)
    new.mem <- c(self.mem, rand.mem)
  } else {
    stop(paste0("memory.type ",memory.type," not supported."))
  }
  if(algo.par$use.rnn){
    new.mem <- lapply(new.mem,FUN=function(x){
      if(any(is.na(x$state))) x$state[is.na(x$state)] <- model.par$mask.value
      if(any(is.na(x$next.state))) x$next.state[is.na(x$next.state)] <- model.par$mask.value
      return(x)
    })
  }
  algo.var <- Update.Memory.QLearningPersExpPath(algo.var, new.mem, game.object, algo.par)

  return(algo.var)
}

#' Add historic Q-Values and Curiosity to memory
#'
#' @export
Update.Memory.QLearningPersExpPath <- function(algo.var, new.mem, game.object, algo.par, best.effort=NULL){
  restore.point("Update.Memory.QLearning")

  if(is.null(new.mem)||length(new.mem)==0){
    return(algo.var)
  }

  #Relevant Portion for Hybrid Q and MC Q
  if(algo.par$hybrid.Q || algo.par$MC){
    #Identify start/end points
    start.candidates <- sapply(new.mem,FUN=function(x){x$start})
    if(is.null(unlist(start.candidates))){
      starts <- 1
    } else {
      starts <-  which(start.candidates)
    }
    ends <- which(sapply(new.mem,FUN=function(x){x$done}))
    no.paths <- length(starts)

    for(x in 1:no.paths){
      #restore.point("inside.update.memory.qlearningpersExpPath")
      if (algo.par$hybrid.Q.apply=="sensible" && !is.null(best.effort)){
        if(all(best.effort)){
          Q.path.on <- rep(TRUE,ends[x]-starts[x]+1)
        } else {
          deviation.point <- max(which(best.effort == FALSE))
          Q.path.on <- rep(TRUE,ends[x]-starts[x]+1)
          Q.path.on[1:deviation.point] <- FALSE
        }
      } else if (algo.par$hybrid.Q.apply=="always"){
        Q.path.on <- rep(TRUE,ends[x]-starts[x]+1)
      } else if (is.null(best.effort)){
        Q.path.on <- rep(FALSE,ends[x]-starts[x]+1)
      }
      for(i in ends[x]:starts[x]){
        if(i == ends[x]){
          if(Q.path.on[i-starts[x]+1]){
            new.mem[[i]]$Q.path <- new.mem[[i]]$reward
          } else {
            new.mem[[i]]$Q.path <- NA
          }
        } else {
          if(Q.path.on[i-starts[x]+1]){
            new.mem[[i]]$Q.path <- new.mem[[i]]$reward + algo.par$gamma * new.mem[[i+1]]$Q.path
          } else {
            new.mem[[i]]$Q.path <- NA
          }
        }
      }
    }
  }

  #Add Experience count to gauge age of experience
  if(!is.null(algo.var$experience.count)){
    new.mem <- lapply(new.mem,FUN=function(x){
      x$experience.count <- algo.var$experience.count
      return(x)
    })
  }

  #Add Curiosity
  if(algo.par$curio.beta>0){
    for(i in 1:length(new.mem)){
      rho <- Calc.Rho(phi.table=algo.var$phi.table,phi=new.mem[[i]]$next.state)
      if(algo.par$N.type=="naive"){
        t <- sum(algo.var$phi.table[[1]]$count)
        algo.var$phi.table <- Update.Phi.Table(phi.table=algo.var$phi.table,phi=new.mem[[i]]$next.state,phi.cont.digits=algo.par$phi.cont.digits)
        R.phi <- t*rho
      } else {
        restore.point("r.Phi")
        algo.var$phi.table <- Update.Phi.Table(phi.table=algo.var$phi.table,phi=new.mem[[i]]$next.state,phi.cont.digits=algo.par$phi.cont.digits)
        rho.next <- Calc.Rho(phi.table=algo.var$phi.table,phi=new.mem[[i]]$next.state)
        N.phi <- rho*(1-rho.next)/(rho.next-rho)
        R.phi <- algo.par$curio.beta/sqrt(N.phi)
      }
      new.mem[[i]]$R.phi <- min(R.phi,algo.par$curio.cap)
    }
  }

  algo.var$memory <- c(algo.var$memory,new.mem)

  return(algo.var)
}

#' Train model of Q learning
#'
#' @param model.par.surp If \code{NULL}, no suprise model is generated/updated.
#' @param model.par.fam If \code{NULL}, no familiarity model is generated/updated.
#' @export
Replay.QLearningPersExpPath <- function(evaluator, model.par, model.par.surp=NULL,model.par.fam=NULL, algo.par, algo.var, game.object){
  restore.point("Replay.QLearningPersExpPath")

  algo.var$experience.count <- algo.var$experience.count + 1 #With each Replay our experience increases
  train.res <- Train.On.Memory.QLearningPersExpPath(model=evaluator$model.cur, model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par=model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.var=algo.var, algo.par=algo.par, game.object=game.object, type="replay")
  evaluator <- Update.Evaluator.QLearningPersExpPath(evaluator, new.model=train.res$model, new.model.surp = train.res$model.surp, new.model.fam=train.res$model.fam, which.update="cur", model.par)
  algo.var <- train.res$algo.var

  return(nlist(evaluator, algo.var))
}

#'
#'
#'@export
Change.Algo.Var.QLearningPersExpPath <- function(algo.par, algo.var, cur.block, blocks){
  restore.point("Change.Algo.Var.QLearningPersExpPath")

  if(algo.par$action.policy=="epsilon.greedy" && algo.var$epsilon > algo.par$epsilon.min && cur.block!=blocks){
    if(algo.par$epsilon.decay.type=="linear"){
      algo.var$epsilon <- seq(algo.par$epsilon.start,algo.par$epsilon.min,length.out=blocks)[cur.block+1]
    } else if(algo.par$epsilon.decay.type=="exponential"){
      decay.par <- (algo.par$epsilon.min/algo.par$epsilon.start)^(algo.par$replay.intensive/blocks)
      algo.var$epsilon <- decay.par*algo.var$epsilon
    } else {
      algo.var$epsilon <- algo.par$epsilon.decay*algo.var$epsilon
    }
  } else if (algo.par$action.policy=="exploration.path") {

    exploit.data.length <- max(length(algo.var$analysis$score.best.block),length(algo.var$analysis$score.cur.block), na.rm=TRUE)

    #Var data points
    if(!is.null(algo.par$block.expl.var)&&algo.par$block.expl.var!=0){
      if(exploit.data.length<algo.par$expl.path.var.data.base){
        rel.best.effort.var <-  mean(c(algo.var$analysis$score.best.block,algo.var$analysis$score.cur.block), na.rm=TRUE)
        rel.expl.var <- mean(c(algo.var$analysis$score.expl.block.var))
      } else {
        if(!is.null(algo.par$block.best)&&algo.par$block.best!=0){
          mean.best <- algo.var$analysis$score.best.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.var.data.base+1)]
        } else {
          mean.best <- NA
        }
        if(!is.null(algo.par$block.cur)&&algo.par$block.cur!=0){
          mean.cur <- algo.var$analysis$score.cur.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.var.data.base+1)]
        } else {
          mean.cur <- NA
        }

        rel.best.effort.var <-  mean(c(mean.best,mean.cur), na.rm=TRUE)
        rel.expl.var <- mean(c(algo.var$analysis$score.expl.block.var[length(algo.var$analysis$score.expl.block.var):(length(algo.var$analysis$score.expl.block.var)-algo.par$expl.path.var.data.base+1)]))
      }
    }

    #Shock data points
    if(!is.null(algo.par$block.expl.shock)&&algo.par$block.expl.shock!=0){
      if(exploit.data.length<algo.par$expl.path.shock.data.base){
        rel.best.effort.shock <-  mean(c(algo.var$analysis$score.best.block,algo.var$analysis$score.cur.block), na.rm=TRUE)
        rel.expl.shock <- mean(c(algo.var$analysis$score.expl.block.shock))
      } else {
        if(!is.null(algo.par$block.best)&&algo.par$block.best!=0){
          mean.best <- algo.var$analysis$score.best.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.shock.data.base+1)]
        } else {
          mean.best <- NA
        }
        if(!is.null(algo.par$block.cur)&&algo.par$block.cur!=0){
          mean.cur <- algo.var$analysis$score.cur.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.shock.data.base+1)]
        } else {
          mean.cur <- NA
        }

        rel.best.effort.shock <-  mean(c(mean.best,mean.cur), na.rm=TRUE)
        rel.expl.shock <- mean(c(algo.var$analysis$score.expl.block.shock[length(algo.var$analysis$score.expl.block.shock):(length(algo.var$analysis$score.expl.block.shock)-algo.par$expl.path.shock.data.base+1)]))
      }
    }

    #Surp data points
    if(!is.null(algo.par$block.expl.surp)&&algo.par$block.expl.surp!=0){
      if(exploit.data.length<algo.par$expl.path.surp.data.base){
        rel.best.effort.surp <-  mean(c(algo.var$analysis$score.best.block,algo.var$analysis$score.cur.block), na.rm=TRUE)
        rel.expl.surp <- mean(c(algo.var$analysis$score.expl.block.surp))
      } else {
        if(!is.null(algo.par$block.best)&&algo.par$block.best!=0){
          mean.best <- algo.var$analysis$score.best.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.surp.data.base+1)]
        } else {
          mean.best <- NA
        }
        if(!is.null(algo.par$block.cur)&&algo.par$block.cur!=0){
          mean.cur <- algo.var$analysis$score.cur.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.surp.data.base+1)]
        } else {
          mean.cur <- NA
        }

        rel.best.effort.surp <-  mean(c(mean.best,mean.cur), na.rm=TRUE)
        rel.expl.surp <- mean(c(algo.var$analysis$score.expl.block.surp[length(algo.var$analysis$score.expl.block.surp):(length(algo.var$analysis$score.expl.block.surp)-algo.par$expl.path.surp.data.base+1)]))
      }
    }

    #Familiarity data points
    if(!is.null(algo.par$block.expl.fam)&&algo.par$block.expl.fam!=0){
      if(exploit.data.length<algo.par$expl.path.fam.data.base){
        rel.best.effort.fam <-  mean(c(algo.var$analysis$score.best.block,algo.var$analysis$score.cur.block), na.rm=TRUE)
        rel.expl.fam <- mean(c(algo.var$analysis$score.expl.block.fam))
      } else {
        if(!is.null(algo.par$block.best)&&algo.par$block.best!=0){
          mean.best <- algo.var$analysis$score.best.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.fam.data.base+1)]
        } else {
          mean.best <- NA
        }
        if(!is.null(algo.par$block.cur)&&algo.par$block.cur!=0){
          mean.cur <- algo.var$analysis$score.cur.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.fam.data.base+1)]
        } else {
          mean.cur <- NA
        }

        rel.best.effort.fam <-  mean(c(mean.best,mean.cur), na.rm=TRUE)
        rel.expl.fam <- mean(c(algo.var$analysis$score.expl.block.fam[length(algo.var$analysis$score.expl.block.fam):(length(algo.var$analysis$score.expl.block.fam)-algo.par$expl.path.fam.data.base+1)]))
      }
    }

    #VarShock Combi data points
    if(!is.null(algo.par$block.expl.vs)&&algo.par$block.expl.vs!=0){
      #restore.point("varshock data")
      if(exploit.data.length<algo.par$expl.path.vs.data.base){
        rel.best.effort.vs <-  mean(c(algo.var$analysis$score.best.block,algo.var$analysis$score.cur.block), na.rm=TRUE)
        rel.expl.vs <- mean(c(algo.var$analysis$score.expl.block.vs))
        rel.sub.vs.var <- mean(algo.var$analysis$sub.block.vs.var)
        rel.sub.vs.shock <- mean(algo.var$analysis$sub.block.vs.shock)
      } else {
        if(!is.null(algo.par$block.best)&&algo.par$block.best!=0){
          mean.best <- algo.var$analysis$score.best.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.vs.data.base+1)]
        } else {
          mean.best <- NA
        }
        if(!is.null(algo.par$block.cur)&&algo.par$block.cur!=0){
          mean.cur <- algo.var$analysis$score.cur.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.vs.data.base+1)]
        } else {
          mean.cur <- NA
        }

        rel.best.effort.vs <-  mean(c(mean.best,mean.cur), na.rm=TRUE)
        rel.expl.vs <- mean(c(algo.var$analysis$score.expl.block.vs[length(algo.var$analysis$score.expl.block.vs):(length(algo.var$analysis$score.expl.block.vs)-algo.par$expl.path.vs.data.base+1)]))
        rel.sub.vs.var <- mean(algo.var$analysis$sub.block.vs.var[length(algo.var$analysis$score.expl.block.vs):(length(algo.var$analysis$score.expl.block.vs)-algo.par$expl.path.vs.data.base+1)])
        rel.sub.vs.shock <- mean(algo.var$analysis$sub.block.vs.shock[length(algo.var$analysis$score.expl.block.vs):(length(algo.var$analysis$score.expl.block.vs)-algo.par$expl.path.vs.data.base+1)])
      }
    }

    #Multi data points
    if(!is.null(algo.par$block.expl.multi)&&algo.par$block.expl.multi!=0){
      restore.point("multi data")

      #Aggregate cur and best
      aggr.vec <- sapply(1:exploit.data.length,FUN=function(x){
        mean(c(algo.var$analysis$score.best.block[x],algo.var$analysis$score.cur.block[x]), na.rm=TRUE)
      })

      #Smoothed vector of cur and best over db discounted data points
      aggr.vec.smoothed <- sapply(1:exploit.data.length, FUN=function(x){
        Weighted.Discount(aggr.vec[max(1,x-algo.par$expl.path.multi.best.db):x],discount=algo.par$expl.path.multi.best.disc)
      })

      #
      #
      # if(exploit.data.length<algo.par$expl.path.multi.data.base){
      #   rel.best.effort.multi <-  mean(c(algo.var$analysis$score.best.block,algo.var$analysis$score.cur.block), na.rm=TRUE)
      #   rel.expl.multi <- mean(c(algo.var$analysis$score.expl.block.multi))
      #   rel.sub.multi.var <- mean(algo.var$analysis$sub.block.multi.var)
      #   rel.sub.multi.shock <- mean(algo.var$analysis$sub.block.multi.shock)
      #   rel.sub.multi.surp <- mean(algo.var$analysis$sub.block.multi.surp)
      #   rel.sub.multi.fam <- mean(algo.var$analysis$sub.block.multi.fam)
      # } else {
      #   if(!is.null(algo.par$block.best)&&algo.par$block.best!=0){
      #     mean.best <- algo.var$analysis$score.best.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.multi.data.base+1)]
      #   } else {
      #     mean.best <- NA
      #   }
      #   if(!is.null(algo.par$block.cur)&&algo.par$block.cur!=0){
      #     mean.cur <- algo.var$analysis$score.cur.block[exploit.data.length:(exploit.data.length-algo.par$expl.path.multi.data.base+1)]
      #   } else {
      #     mean.cur <- NA
      #   }
      #
      #   rel.best.effort.multi <-  mean(c(mean.best,mean.cur), na.rm=TRUE)
      #   rel.expl.multi <- mean(c(algo.var$analysis$score.expl.block.multi[length(algo.var$analysis$score.expl.block.multi):(length(algo.var$analysis$score.expl.block.multi)-algo.par$expl.path.multi.data.base+1)]))
      #   rel.sub.multi.var <- mean(algo.var$analysis$sub.block.multi.var[length(algo.var$analysis$score.expl.block.multi):(length(algo.var$analysis$score.expl.block.multi)-algo.par$expl.path.multi.data.base+1)])
      #   rel.sub.multi.shock <- mean(algo.var$analysis$sub.block.multi.shock[length(algo.var$analysis$score.expl.block.multi):(length(algo.var$analysis$score.expl.block.multi)-algo.par$expl.path.multi.data.base+1)])
      #   rel.sub.multi.surp <- mean(algo.var$analysis$sub.block.multi.surp[length(algo.var$analysis$score.expl.block.multi):(length(algo.var$analysis$score.expl.block.multi)-algo.par$expl.path.multi.data.base+1)])
      #   rel.sub.multi.fam <- mean(algo.var$analysis$sub.block.multi.fam[length(algo.var$analysis$score.expl.block.multi):(length(algo.var$analysis$score.expl.block.multi)-algo.par$expl.path.multi.data.base+1)])
      # }
    }

    #Var Path
    if(!is.null(algo.par$block.expl.var)&&algo.par$block.expl.var!=0){
      if(algo.par$expl.path.var.decay.type=="linear"){
        algo.var$path.goal.var <- seq(algo.par$expl.path.var.start,algo.par$expl.path.var.end,length.out=blocks)[cur.block+1]
      } else if(algo.par$expl.path.var.decay.type=="exponential"){
        decay.par <- (algo.par$expl.path.var.end/algo.par$expl.path.var.start)^(algo.par$replay.intensive/blocks)
        algo.var$path.goal.var <- decay.par*algo.var$path.goal.var
      }
      if(rel.best.effort.var<0){
        path.goal.var <- rel.best.effort.var/algo.var$path.goal.var
      } else {
        path.goal.var <- rel.best.effort.var*algo.var$path.goal.var
      }
    }

    #Shock Path
    if(!is.null(algo.par$block.expl.shock)&&algo.par$block.expl.shock!=0){
      if(algo.par$expl.path.shock.decay.type=="linear"){
        algo.var$path.goal.shock <- seq(algo.par$expl.path.shock.start,algo.par$expl.path.shock.end,length.out=blocks)[cur.block+1]
      } else if(algo.par$expl.path.shock.decay.type=="exponential"){
        decay.par <- (algo.par$expl.path.shock.end/algo.par$expl.path.shock.start)^(algo.par$replay.intensive/blocks)
        algo.var$path.goal.shock <- decay.par*algo.var$path.goal.shock
      }
      if(rel.best.effort.shock<0){
        path.goal.shock <- rel.best.effort.shock/algo.var$path.goal.shock
      } else {
        path.goal.shock <- rel.best.effort.shock*algo.var$path.goal.shock
      }
    }

    #Surp Path
    if(!is.null(algo.par$block.expl.surp)&&algo.par$block.expl.surp!=0){
      if(algo.par$expl.path.surp.decay.type=="linear"){
        algo.var$path.goal.surp <- seq(algo.par$expl.path.surp.start,algo.par$expl.path.surp.end,length.out=blocks)[cur.block+1]
      } else if(algo.par$expl.path.surp.decay.type=="exponential"){
        decay.par <- (algo.par$expl.path.surp.end/algo.par$expl.path.surp.start)^(algo.par$replay.intensive/blocks)
        algo.var$path.goal.surp <- decay.par*algo.var$path.goal.surp
      }
      if(rel.best.effort.surp<0){
        path.goal.surp <- rel.best.effort.surp/algo.var$path.goal.surp
      } else {
        path.goal.surp <- rel.best.effort.surp*algo.var$path.goal.surp
      }
    }

    #fam Path
    if(!is.null(algo.par$block.expl.fam)&&algo.par$block.expl.fam!=0){
      if(algo.par$expl.path.fam.decay.type=="linear"){
        algo.var$path.goal.fam <- seq(algo.par$expl.path.fam.start,algo.par$expl.path.fam.end,length.out=blocks)[cur.block+1]
      } else if(algo.par$expl.path.fam.decay.type=="exponential"){
        decay.par <- (algo.par$expl.path.fam.end/algo.par$expl.path.fam.start)^(algo.par$replay.intensive/blocks)
        algo.var$path.goal.fam <- decay.par*algo.var$path.goal.fam
      }
      if(rel.best.effort.fam<0){
        path.goal.fam <- rel.best.effort.fam/algo.var$path.goal.fam
      } else {
        path.goal.fam <- rel.best.effort.fam*algo.var$path.goal.fam
      }
    }

    #VS Path
    if(!is.null(algo.par$block.expl.vs)&&algo.par$block.expl.vs!=0){
      if(algo.par$expl.path.vs.decay.type=="linear"){
        algo.var$path.goal.vs <- seq(algo.par$expl.path.vs.start,algo.par$expl.path.vs.end,length.out=blocks)[cur.block+1]
      } else if(algo.par$expl.path.vs.decay.type=="exponential"){
        decay.par <- (algo.par$expl.path.vs.end/algo.par$expl.path.vs.start)^(algo.par$replay.intensive/blocks)
        algo.var$path.goal.vs <- decay.par*algo.var$path.goal.vs
      }
      if(rel.best.effort.vs<0){
        path.goal.vs <- rel.best.effort.vs/algo.var$path.goal.vs
      } else {
        path.goal.vs <- rel.best.effort.vs*algo.var$path.goal.vs
      }
    }

    #Multi Path
    if(!is.null(algo.par$block.expl.multi)&&algo.par$block.expl.multi!=0){
      restore.point("multi.path")
      if(algo.par$expl.path.multi.decay.type=="linear"){
        algo.var$path.goal.multi <- seq(algo.par$expl.path.multi.start,algo.par$expl.path.multi.end,length.out=blocks)[1:cur.block]
      } else if(algo.par$expl.path.multi.decay.type=="exponential"){
        decay.par <- (algo.par$expl.path.multi.end/algo.par$expl.path.multi.start)^(algo.par$replay.intensive/blocks)
        algo.var$path.goal.multi <- algo.par$expl.path.multi.start*decay.par^(0:(cur.block-1))
      }
      path.goal.multi <- sapply(1:exploit.data.length,FUN=function(x){
        if(aggr.vec.smoothed[x]<0){
          return(aggr.vec.smoothed[x]/algo.var$path.goal.multi[x])
        } else {
          return(aggr.vec.smoothed[x]*algo.var$path.goal.multi[x])
        }
      })
    }

    #Recalibration Var
    if(!is.null(algo.par$block.expl.var)&&algo.par$block.expl.var!=0){
      if(rel.expl.var>rel.best.effort.var){ #We are doing something good here do not change anything
        print(paste0(c(" [Var] Best Effort: ", round(rel.best.effort.var,2), " and path goal is ", round(path.goal.var,2),". We have ", round(rel.expl.var,2), " which is better than best effort, so we do not change anything."),collapse=""))
      } else if(path.goal.var>rel.expl.var){ #We are worse than the Path
          print(paste0(c(" [Var] Best Effort: ", round(rel.best.effort.var,2), " so path goal is ", round(path.goal.var,2),". We have ", round(rel.expl.var,2), " so we decrease variance from ", round(algo.var$expl.path.var,3), " to ", round(algo.var$expl.path.var/algo.par$expl.path.var.momentum,3)),collapse=""))
          algo.var$expl.path.var <- algo.var$expl.path.var/algo.par$expl.path.var.momentum
      } else { #We are better than the path
          print(paste0(c(" [Var] Best Effort: ", round(rel.best.effort.var,2), " so path goal is ", round(path.goal.var,2),". We have ", round(rel.expl.var,2), " so we increase variance from ", round(algo.var$expl.path.var,3), " to ", round(algo.var$expl.path.var*algo.par$expl.path.var.momentum,3)),collapse=""))
          algo.var$expl.path.var <- algo.var$expl.path.var*algo.par$expl.path.var.momentum
      }
    }

    #Recalibration Shock
    if(!is.null(algo.par$block.expl.shock)&&algo.par$block.expl.shock!=0){
      if(rel.expl.shock>rel.best.effort.shock){ #We are doing something good here do not change anything
        print(paste0(c(" [Shock] Best Effort: ", round(rel.best.effort.shock,2), " and path goal is ", round(path.goal.shock,2),". We have ", round(rel.expl.shock,2), " which is better than best effort, so we do not change anything."),collapse=""))
      } else if(path.goal.shock>rel.expl.shock){ #We are worse than the Path
        print(paste0(c(" [Shock] Best Effort: ", round(rel.best.effort.shock,2), " so path goal is ", round(path.goal.shock,2),". We have ", round(rel.expl.shock,2), " so we decrease epsilon shock from ", round(algo.var$expl.path.shock,3), " to ", round(algo.var$expl.path.shock/algo.par$expl.path.shock.momentum,3)),collapse=""))
        algo.var$expl.path.shock <- algo.var$expl.path.shock/algo.par$expl.path.shock.momentum
      } else { #We are better than the path
        old.shock <- algo.var$expl.path.shock
        new.shock <- min(algo.var$expl.path.shock*algo.par$expl.path.shock.momentum,1)
        algo.var$expl.path.shock <- new.shock

        if(old.shock>=1){
            print(paste0(c(" [Shock] Best Effort: ", round(rel.best.effort.shock,2), " so path goal is ", round(path.goal.shock,2),". We have ", round(rel.expl.shock,2), " but increase in shock is not possible as already 1."),collapse=""))
        } else {
            print(paste0(c(" [Shock] Best Effort: ", round(rel.best.effort.shock,2), " so path goal is ", round(path.goal.shock,2),". We have ", round(rel.expl.shock,2), " so we increase epsilon shock from ", round(old.shock,3), " to ", round(new.shock,3)),collapse=""))
        }
      }
    }

    #Recalibration Surp
    if(!is.null(algo.par$block.expl.surp)&&algo.par$block.expl.surp!=0){
      if(rel.expl.surp>rel.best.effort.surp){ #We are doing something good here do not change anything
        print(paste0(c(" [Surp] Best Effort: ", round(rel.best.effort.surp,2), " and path goal is ", round(path.goal.surp,2),". We have ", round(rel.expl.surp,2), " which is better than best effort, so we do not change anything."),collapse=""))
      } else if(path.goal.surp>rel.expl.surp){ #We are worse than the Path
        print(paste0(c(" [Surp] Best Effort: ", round(rel.best.effort.surp,2), " so path goal is ", round(path.goal.surp,2),". We have ", round(rel.expl.surp,2), " so we decrease surprise factor from ", round(algo.var$expl.path.surp,3), " to ", round(algo.var$expl.path.surp/algo.par$expl.path.surp.momentum,3)),collapse=""))
        algo.var$expl.path.surp <- algo.var$expl.path.surp/algo.par$expl.path.surp.momentum
      } else { #We are better than the path
        print(paste0(c(" [Surp] Best Effort: ", round(rel.best.effort.surp,2), " so path goal is ", round(path.goal.surp,2),". We have ", round(rel.expl.surp,2), " so we increase surprise factor from ", round(algo.var$expl.path.surp,3), " to ", round(algo.var$expl.path.surp*algo.par$expl.path.surp.momentum,3)),collapse=""))
        algo.var$expl.path.surp <- algo.var$expl.path.surp*algo.par$expl.path.surp.momentum
      }
    }

    #Recalibration fam
    if(!is.null(algo.par$block.expl.fam)&&algo.par$block.expl.fam!=0){
      if(rel.expl.fam>rel.best.effort.fam){ #We are doing something good here do not change anything
        print(paste0(c(" [Fam] Best Effort: ", round(rel.best.effort.fam,2), " and path goal is ", round(path.goal.fam,2),". We have ", round(rel.expl.fam,2), " which is better than best effort, so we do not change anything."),collapse=""))
      } else if(path.goal.fam>rel.expl.fam){ #We are worse than the Path
        print(paste0(c(" [Fam] Best Effort: ", round(rel.best.effort.fam,2), " so path goal is ", round(path.goal.fam,2),". We have ", round(rel.expl.fam,2), " so we decrease fam factor from ", round(algo.var$expl.path.fam,3), " to ", round(algo.var$expl.path.fam/algo.par$expl.path.fam.momentum,3)),collapse=""))
        algo.var$expl.path.fam <- algo.var$expl.path.fam/algo.par$expl.path.fam.momentum
      } else { #We are better than the path
        print(paste0(c(" [Fam] Best Effort: ", round(rel.best.effort.fam,2), " so path goal is ", round(path.goal.fam,2),". We have ", round(rel.expl.fam,2), " so we increase fam factor from ", round(algo.var$expl.path.fam,3), " to ", round(algo.var$expl.path.fam*algo.par$expl.path.fam.momentum,3)),collapse=""))
        algo.var$expl.path.fam <- algo.var$expl.path.fam*algo.par$expl.path.fam.momentum
      }
    }

    #Recalibration VarShockCombi
    if(!is.null(algo.par$block.expl.vs)&&algo.par$block.expl.vs!=0){
      #restore.point("recalibrate.varshock")
      if(rel.expl.vs>rel.best.effort.vs){ #We are doing something good here do not change anything
        print(paste0(c(" [VS] Best Effort: ", round(rel.best.effort.vs,2), " and path goal is ", round(path.goal.vs,2),". We have ", round(rel.expl.vs,2), " which is better than best effort, so we do not change anything."),collapse=""))
      } else if(path.goal.vs>rel.expl.vs){ #We are worse than the Path

        old.vs.var <- algo.var$expl.path.vs.var
        old.vs.shock <- algo.var$expl.path.vs.shock

        target.shock <- max((1-cur.block/blocks)*(algo.par$expl.path.vs.start.shock.frac),algo.par$expl.path.vs.min.shock.frac)
        target.var <- 1-target.shock
        current.var <- rel.sub.vs.var/(rel.sub.vs.var+rel.sub.vs.shock)
        if(is.nan(current.var)) current.var <- 0
        current.shock <- 1-current.var
        diff.total <- abs(target.var-current.var) + abs(target.shock-current.shock)
        if(current.var>target.var){ #Var has to be reduced more severely
          w.var <- min(0.5+algo.par$expl.path.vs.strength*diff.total,1)
          w.shock <- 1-w.var
        } else {
          w.shock <- min(0.5 + algo.par$expl.path.vs.strength*diff.total,1)
          w.var <- 1-w.shock
        }

        new.vs.var <- old.vs.var/((algo.par$expl.path.vs.momentum-1)*w.var+1)
        new.vs.shock <- old.vs.shock/((algo.par$expl.path.vs.momentum-1)*w.shock+1)

        print(paste0(c(" [VS] Best Effort: ", round(rel.best.effort.vs,2), " so path goal is ", round(path.goal.vs,2),". We have ", round(rel.expl.vs,2), " so we decrease vs-epsilon shock from ", round(old.vs.shock,3), " to ", round(new.vs.shock,3)," and vs-var from ", round(old.vs.var,3), " to ", round(new.vs.var,3)," with a weight for var of ", round(w.var,2), " (>0.5 means more reduction of var)"),collapse=""))

        algo.var$expl.path.vs.var <- new.vs.var
        algo.var$expl.path.vs.shock <- new.vs.shock

      } else { #We are better than the path

        old.vs.var <- algo.var$expl.path.vs.var
        old.vs.shock <- algo.var$expl.path.vs.shock

        target.shock <- max((1-cur.block/blocks)*(algo.par$expl.path.vs.start.shock.frac),algo.par$expl.path.vs.min.shock.frac)
        target.var <- 1-target.shock
        current.var <- rel.sub.vs.var/(rel.sub.vs.var+rel.sub.vs.shock)
        if(is.nan(current.var)) current.var <- 0
        current.shock <- 1-current.var
        diff.total <- abs(target.var-current.var) + abs(target.shock-current.shock)
        if(current.var<target.var){ #Var has to be increased more severely
          w.var <- min(0.5+algo.par$expl.path.vs.strength*diff.total,1)
          w.shock <- 1-w.var
        } else {
          w.shock <- min(0.5 + algo.par$expl.path.vs.strength*diff.total,1)
          w.var <- 1-w.shock
        }

        new.vs.var <- old.vs.var*((algo.par$expl.path.vs.momentum-1)*w.var+1)
        new.vs.shock <- min(old.vs.shock*((algo.par$expl.path.vs.momentum-1)*w.shock+1),1)

        print(paste0(c(" [VS] Best Effort: ", round(rel.best.effort.vs,2), " so path goal is ", round(path.goal.vs,2),". We have ", round(rel.expl.vs,2), " so we increase vs-epsilon shock from ", round(old.vs.shock,3), " to ", round(new.vs.shock,3)," and vs-var from ", round(old.vs.var,3), " to ", round(new.vs.var,3)," with a weight for var of ", round(w.var,2), " (>0.5 means more increase of var)"),collapse=""))

        algo.var$expl.path.vs.var <- new.vs.var
        algo.var$expl.path.vs.shock <- new.vs.shock
      }
    }
  }

  #Recalibration Multi
  if(!is.null(algo.par$block.expl.multi)&&algo.par$block.expl.multi!=0){
    restore.point("change.path")

    old.multi.var <- algo.var$expl.path.multi.var
    old.multi.shock <- algo.var$expl.path.multi.shock
    old.multi.surp <- algo.var$expl.path.multi.surp
    old.multi.fam <- algo.var$expl.path.multi.fam

    #Target suboptimality fractions in respective round
    start.fracs <- c(algo.par$expl.path.multi.start.frac.var, algo.par$expl.path.multi.start.frac.shock,algo.par$expl.path.multi.start.frac.surp, algo.par$expl.path.multi.start.frac.fam)
    start.fracs <- start.fracs/sum(start.fracs) #norm for safety
    end.fracs <- c(algo.par$expl.path.multi.end.frac.var, algo.par$expl.path.multi.end.frac.shock,algo.par$expl.path.multi.end.frac.surp, algo.par$expl.path.multi.end.frac.fam)
    end.fracs <- end.fracs/sum(end.fracs)
    target.fracs <- t(sapply(1:cur.block,FUN=function(x){
      start.fracs+x/blocks*(end.fracs-start.fracs)
    }))

    #Observed suboptimality fractions
    total.sub <- algo.var$analysis$sub.block.multi.var + algo.var$analysis$sub.block.multi.shock + algo.var$analysis$sub.block.multi.surp + algo.var$analysis$sub.block.multi.fam
    fracs <- t(sapply(1:cur.block,FUN=function(x){
      cur.frac <- c(algo.var$analysis$sub.block.multi.var[x],algo.var$analysis$sub.block.multi.shock[x],algo.var$analysis$sub.block.multi.surp[x],algo.var$analysis$sub.block.multi.fam[x])/total.sub[x]
      if(total.sub[x]==0) cur.frac <- rep(0,4)
      return(cur.frac)
    }))

    #Transforming fractions to real values
    cur.abs.sub.var <- (aggr.vec.smoothed-algo.var$analysis$score.expl.block.multi)*fracs[,1]
    goal.abs.sub.var <- (aggr.vec.smoothed-path.goal.multi)*target.fracs[,1]
    err.vec.var <- goal.abs.sub.var - cur.abs.sub.var

    cur.abs.sub.shock <- (aggr.vec.smoothed-algo.var$analysis$score.expl.block.multi)*fracs[,2]
    goal.abs.sub.shock <- (aggr.vec.smoothed-path.goal.multi)*target.fracs[,2]
    err.vec.shock <- goal.abs.sub.shock - cur.abs.sub.shock

    cur.abs.sub.surp <- (aggr.vec.smoothed-algo.var$analysis$score.expl.block.multi)*fracs[,3]
    goal.abs.sub.surp <- (aggr.vec.smoothed-path.goal.multi)*target.fracs[,3]
    err.vec.surp <- goal.abs.sub.surp - cur.abs.sub.surp

    cur.abs.sub.fam <- (aggr.vec.smoothed-algo.var$analysis$score.expl.block.multi)*fracs[,4]
    goal.abs.sub.fam <- (aggr.vec.smoothed-path.goal.multi)*target.fracs[,4]
    err.vec.fam <- goal.abs.sub.fam - cur.abs.sub.fam

    pid.val.var <- PID.controller(err.vec=err.vec.var, Kp=algo.par$expl.path.multi.Kp.var, Ki=algo.par$expl.path.multi.Ki.var, Kd=algo.par$expl.path.multi.Kd.var, Kp.db=algo.par$expl.path.multi.Kp.db, Ki.db=algo.par$expl.path.multi.Ki.db, Kd.db=algo.par$expl.path.multi.Kd.db, Kp.disc=algo.par$expl.path.multi.Kp.disc, Kd.disc=algo.par$expl.path.multi.Kd.disc, Ki.disc=algo.par$expl.path.multi.Ki.disc)
    pid.val.shock <- PID.controller(err.vec=err.vec.shock, Kp=algo.par$expl.path.multi.Kp.shock, Ki=algo.par$expl.path.multi.Ki.shock, Kd=algo.par$expl.path.multi.Kd.shock, Kp.db=algo.par$expl.path.multi.Kp.db, Ki.db=algo.par$expl.path.multi.Ki.db, Kd.db=algo.par$expl.path.multi.Kd.db, Kp.disc=algo.par$expl.path.multi.Kp.disc, Kd.disc=algo.par$expl.path.multi.Kd.disc, Ki.disc=algo.par$expl.path.multi.Ki.disc)
    pid.val.surp <- PID.controller(err.vec=err.vec.surp, Kp=algo.par$expl.path.multi.Kp.surp, Ki=algo.par$expl.path.multi.Ki.surp, Kd=algo.par$expl.path.multi.Kd.surp, Kp.db=algo.par$expl.path.multi.Kp.db, Ki.db=algo.par$expl.path.multi.Ki.db, Kd.db=algo.par$expl.path.multi.Kd.db, Kp.disc=algo.par$expl.path.multi.Kp.disc, Kd.disc=algo.par$expl.path.multi.Kd.disc, Ki.disc=algo.par$expl.path.multi.Ki.disc)
    pid.val.fam <- PID.controller(err.vec=err.vec.fam, Kp=algo.par$expl.path.multi.Kp.fam, Ki=algo.par$expl.path.multi.Ki.fam, Kd=algo.par$expl.path.multi.Kd.fam, Kp.db=algo.par$expl.path.multi.Kp.db, Ki.db=algo.par$expl.path.multi.Ki.db, Kd.db=algo.par$expl.path.multi.Kd.db, Kp.disc=algo.par$expl.path.multi.Kp.disc, Kd.disc=algo.par$expl.path.multi.Kd.disc, Ki.disc=algo.par$expl.path.multi.Ki.disc)

    #new.multi.var <- max(old.multi.var*min(max((1+pid.val.var),0.5),1.5),0)
    #new.multi.shock <- min(max(old.multi.shock*min(max((1+pid.val.shock),0.5),1.5),0),1)
    #new.multi.surp <- max(old.multi.surp*min(max((1+pid.val.surp),0.5),1.5),0)
    #new.multi.fam <- max(old.multi.fam*min(max((1+pid.val.fam),0.5),1.5),0)
    new.multi.var <- max(old.multi.var+pid.val.var,0)
    new.multi.shock <- max(old.multi.shock+pid.val.shock,0)
    new.multi.surp <- max(old.multi.surp+pid.val.surp,0)
    new.multi.fam <- max(old.multi.fam+pid.val.fam,0)

    print(paste0(c(" [Multi] Best Effort: ", round(aggr.vec.smoothed[cur.block],2), " with path goal ", round(path.goal.multi[cur.block],2),". We have ", round(algo.var$analysis$score.expl.block.multi[cur.block],2),"."),collapse=""))
    print(paste0(c("  [Var] Changes: ", round(old.multi.var,2), " to ", round(new.multi.var,2),", as frac ", round(fracs[cur.block,1],3)," with goal ", round(target.fracs[cur.block,1],3), " and abs ",round(cur.abs.sub.var[cur.block],3)," with goal ",round(goal.abs.sub.var[cur.block],3)),collapse=""))
    print(paste0(c("  [Shock] Changes: ", round(old.multi.shock,2), " to ", round(new.multi.shock,2),", as frac ", round(fracs[cur.block,2],3)," with goal ", round(target.fracs[cur.block,2],3), " and abs ",round(cur.abs.sub.shock[cur.block],3)," with goal ",round(goal.abs.sub.shock[cur.block],3)),collapse=""))
    print(paste0(c("  [Surp] Changes: ", round(old.multi.surp,2), " to ", round(new.multi.surp,2),", as frac ", round(fracs[cur.block,3],3)," with goal ", round(target.fracs[cur.block,3],3), " and abs ",round(cur.abs.sub.surp[cur.block],3)," with goal ",round(goal.abs.sub.surp[cur.block],3)),collapse=""))
    print(paste0(c("  [Fam] Changes: ", round(old.multi.fam,2), " to ", round(new.multi.fam,2),", as frac ", round(fracs[cur.block,4],3)," with goal ", round(target.fracs[cur.block,4],3), " and abs ",round(cur.abs.sub.fam[cur.block],3)," with goal ",round(goal.abs.sub.fam[cur.block],3)),collapse=""))

    algo.var$expl.path.multi.var <- new.multi.var
    algo.var$expl.path.multi.shock <- new.multi.shock
    algo.var$expl.path.multi.surp <- new.multi.surp
    algo.var$expl.path.multi.fam <- new.multi.fam

    # if(rel.expl.multi>rel.best.effort.multi){ #We are doing something good here do not change anything
    #   print(paste0(c(" [Multi] Best Effort: ", round(rel.best.effort.multi,2), " and path goal is ", round(path.goal.multi,2),". We have ", round(rel.expl.multi,2), " which is better than best effort, so we do not change anything."),collapse=""))
    # } else { #We have to change the variables
    #   old.multi.var <- algo.var$expl.path.multi.var
    #   old.multi.shock <- algo.var$expl.path.multi.shock
    #   old.multi.surp <- algo.var$expl.path.multi.surp
    #   old.multi.fam <- algo.var$expl.path.multi.fam
    #
    #   #Target suboptimality fractions
    #   start.fracs <- c(algo.par$expl.path.multi.start.frac.var, algo.par$expl.path.multi.start.frac.shock,algo.par$expl.path.multi.start.frac.surp, algo.par$expl.path.multi.start.frac.fam)
    #   start.fracs <- start.fracs/sum(start.fracs) #norm for safety
    #   end.fracs <- c(algo.par$expl.path.multi.end.frac.var, algo.par$expl.path.multi.end.frac.shock,algo.par$expl.path.multi.end.frac.surp, algo.par$expl.path.multi.end.frac.fam)
    #   end.fracs <- end.fracs/sum(end.fracs)
    #   target.fracs <- start.fracs+cur.block/blocks*(end.fracs-start.fracs)
    #
    #   #Observed supoptimality fractions
    #   total.sub <- rel.sub.multi.var + rel.sub.multi.shock + rel.sub.multi.surp + rel.sub.multi.fam
    #   current.fracs <- c(rel.sub.multi.var,rel.sub.multi.shock,rel.sub.multi.surp,rel.sub.multi.fam)/total.sub
    #   if(total.sub==0) current.fracs <- rep(1/length(current.fracs),length(current.fracs))
    #
    #   #var
    #   if(target.fracs[1]<current.fracs[1]){ #We want to decrease the noise
    #     factor.var <- 1/algo.par$expl.path.multi.momentum.internal
    #   } else {
    #     factor.var <- algo.par$expl.path.multi.momentum.internal
    #   }
    #
    #   #shock
    #   if(target.fracs[2]<current.fracs[2]){ #We want to decrease the noise
    #     factor.shock <- 1/algo.par$expl.path.multi.momentum.internal
    #   } else {
    #     factor.shock <- algo.par$expl.path.multi.momentum.internal
    #   }
    #
    #   #surp
    #   if(target.fracs[3]<current.fracs[3]){ #We want to decrease the noise
    #     factor.surp <- 1/algo.par$expl.path.multi.momentum.internal
    #   } else {
    #     factor.surp <- algo.par$expl.path.multi.momentum.internal
    #   }
    #
    #   #fam
    #   if(target.fracs[4]<current.fracs[4]){ #We want to decrease the noise
    #     factor.fam <- 1/algo.par$expl.path.multi.momentum.internal
    #   } else {
    #     factor.fam <- algo.par$expl.path.multi.momentum.internal
    #   }
    #
    #   if(path.goal.multi>rel.expl.multi){ #We are worse than the Path
    #     #Worse than path means we want to increase our performance, which means decreasing the noise
    #     basic.multi.factor <- 1/algo.par$expl.path.multi.momentum
    #   } else { #We are better than the path
    #     basic.multi.factor <- algo.par$expl.path.multi.momentum
    #   }
    #
    #   new.multi.var <- old.multi.var*basic.multi.factor*factor.var
    #   new.multi.shock <- old.multi.shock*basic.multi.factor*factor.shock
    #   new.multi.surp <- old.multi.surp*basic.multi.factor*factor.surp
    #   new.multi.fam <- old.multi.fam*basic.multi.factor*factor.fam
    #
    #   print(paste0(c(" [Multi] Best Effort: ", round(rel.best.effort.multi,2), " with path goal ", round(path.goal.multi,2),". We have ", round(rel.expl.multi,2),"."),collapse=""))
    #   print(paste0(c("  [Var] Changes: ", round(old.multi.var,2), " to ", round(new.multi.var,2),", as frac ", round(current.fracs[1],3)," with goal ", round(target.fracs[1],3)),collapse=""))
    #   print(paste0(c("  [Shock] Changes: ", round(old.multi.shock,2), " to ", round(new.multi.shock,2),", as frac ", round(current.fracs[2],3)," with goal ", round(target.fracs[2],3)),collapse=""))
    #   print(paste0(c("  [Surp] Changes: ", round(old.multi.surp,2), " to ", round(new.multi.surp,2),", as frac ", round(current.fracs[3],3)," with goal ", round(target.fracs[3],3)),collapse=""))
    #   print(paste0(c("  [Fam] Changes: ", round(old.multi.fam,2), " to ", round(new.multi.fam,2),", as frac ", round(current.fracs[4],3)," with goal ", round(target.fracs[4],3)),collapse=""))
    #
    #   algo.var$expl.path.multi.var <- new.multi.var
    #   algo.var$expl.path.multi.shock <- new.multi.shock
    #   algo.var$expl.path.multi.surp <- new.multi.surp
    #   algo.var$expl.path.multi.fam <- new.multi.fam
    # }
  }

  algo.var$MC.factor <- algo.var$MC.factor*algo.par$hybrid.decay

  return(algo.var)
}

#' Determines which action to take
#'
#' @export
Act.QLearningPersExpPath <- function(state, model, model.surp=NULL, model.fam=NULL, model.par, model.par.surp=NULL, model.par.fam=NULL, algo.par, algo.var, game.object, eval.only=FALSE, best.guess.episode=FALSE, block.type=NULL){
  restore.point("Act.QLearningPersExpPath")

  Surprise.Formula.QLearningPersExpPath <- function(pred.values,surp.score,algo.par, algo.var){
    res <- pred.values + algo.var$expl.path.surp*surp.score
    return(res)
  }

  Familiarity.Formula.QLearningPersExpPath <- function(pred.values,fam.score,algo.par, algo.var){
    res <- pred.values + algo.var$expl.path.fam*(1-fam.score)
    return(res)
  }

  res <- list(action=NA, action.info=list(best.effort=NA, suboptimality=0, noise.type=NA))

  if(eval.only||best.guess.episode){
    act.values <- model.par$predict(model,model.par,state)
    res$action <- which.is.max(act.values)
    if(is.na(res$action)) res$action <- sample(1:game.object$game.par(game.object)$output.nodes,1)
    res$action.info$best.effort <- TRUE
    return(res)
  }

  if(algo.par$action.policy=="epsilon.greedy"){
    if(runif(1) <= algo.var$epsilon){
      game.par <- game.object$game.par(game.object)
      res$action <- sample(1:game.par$output.nodes,1)
      pred.values <- model.par$predict(model,model.par,state)
      if(res$action==which.is.max(pred.values)){
        res$action.info$best.effort <- TRUE
      } else {
        res$action.info$best.effort <- FALSE
        res$action.info$suboptimality <- max(pred.values)-pred.values[res$action]
      }
      return(res)
    } else {
      act.values <- model.par$predict(model,model.par,state)
      res$action <- which.is.max(act.values)
      if(is.na(res$action)) res$action <- sample(1:game.object$game.par(game.object)$output.nodes,1)
      res$action.info$best.effort <- TRUE
      return(res)
    }
  } else if (algo.par$action.policy == "exploration.path") {
    #restore.point("within.expl.path")
    if(block.type=="var"){
      pred.values <- model.par$predict(model,model.par,state)
      rands <- rnorm(length(pred.values),mean=0, sd=algo.var$expl.path.var)
      act.values <- pred.values + rands
      res$action <- which.is.max(act.values)
      if(is.na(res$action)) res$action <- sample(1:game.object$game.par(game.object)$output.nodes,1)
      if(res$action==which.is.max(pred.values)){
        res$action.info$best.effort <- TRUE
      } else {
        res$action.info$best.effort <- FALSE
        res$action.info$suboptimality <- max(pred.values)-pred.values[res$action]
      }
      return(res)
    } else if(block.type=="shock"){
      if(runif(1) <= algo.var$expl.path.shock){
        game.par <- game.object$game.par(game.object)
        res$action <- sample(1:game.par$output.nodes,1)
        pred.values <- model.par$predict(model,model.par,state)
        if(res$action!=which.is.max(pred.values)){
          res$action.info$best.effort <- FALSE
          res$action.info$suboptimality <- max(pred.values)-pred.values[res$action]
        } else {
          res$action.info$best.effort <- TRUE
        }
        return(res)
      } else {
        act.values <- model.par$predict(model,model.par,state)
        res$action <- which.is.max(act.values)
        if(is.na(res$action)) res$action <- sample(1:game.object$game.par(game.object)$output.nodes,1)
        res$action.info$best.effort <- TRUE
        return(res)
      }
    } else if(block.type=="surp"){
      if(!is.null(model.surp)){
        pred.values <- model.par$predict(model,model.par,state)
        surp.score <- model.par.surp$predict(model.surp,model.par.surp,state)
        act.values <- Surprise.Formula.QLearningPersExpPath(pred.values,surp.score,algo.par, algo.var)
        res$action <- which.is.max(act.values)
        if(res$action!=which.is.max(pred.values)){
          res$action.info$best.effort <- FALSE
          res$action.info$suboptimality <- max(pred.values)-pred.values[res$action]
        } else {
          res$action.info$best.effort <- TRUE
        }
        return(res)
      } else {
        stop("Surprise Model not usable in Action selection")
      }
    } else if(block.type=="fam"){
      if(!is.null(model.fam)){
        pred.values <- model.par$predict(model,model.par,state)
        fam.score <- model.par.fam$predict(model.fam,model.par.fam,state)
        act.values <- Familiarity.Formula.QLearningPersExpPath(pred.values,fam.score,algo.par, algo.var)
        res$action <- which.is.max(act.values)
        if(res$action!=which.is.max(pred.values)){
          res$action.info$best.effort <- FALSE
          res$action.info$suboptimality <- max(pred.values)-pred.values[res$action]
        } else {
          res$action.info$best.effort <- TRUE
        }
        return(res)
      } else {
        stop("Familiarity Model not usable in Action selection")
      }
    } else if(block.type=="vs"){
      pred.values <- model.par$predict(model,model.par,state)

      #Shock part
      if(runif(1) <= algo.var$expl.path.vs.shock){
        game.par <- game.object$game.par(game.object)
        res$action <- sample(1:game.par$output.nodes,1)

        if(res$action==which.is.max(pred.values)){
          res$action.info$best.effort <- TRUE
        } else {
          res$action.info$best.effort <- FALSE
          res$action.info$suboptimality <- max(pred.values)-pred.values[res$action]
          res$action.info$noise.type <- "shock"
        }
        return(res)
      } else {
        rands <- rnorm(length(pred.values),mean=0, sd=algo.var$expl.path.vs.var)
        act.values <- pred.values + rands
        res$action <- which.is.max(act.values)
        if(is.na(res$action)) res$action <- sample(1:game.object$game.par(game.object)$output.nodes,1)

        if(res$action==which.is.max(pred.values)){
          res$action.info$best.effort <- TRUE
        } else {
          res$action.info$best.effort <- FALSE
          res$action.info$suboptimality <- max(pred.values)-pred.values[res$action]
          res$action.info$noise.type <- "var"
        }
        return(res)
      }
    } else if(block.type=="multi"){
      pred.values <- model.par$predict(model,model.par,state)
      best.action <- which.is.max(pred.values)

      #Shock part
      if(runif(1) <= algo.var$expl.path.multi.shock){
        game.par <- game.object$game.par(game.object)
        res$action <- sample(1:game.par$output.nodes,1)

        if(res$action==which.is.max(pred.values)){
          res$action.info$best.effort <- TRUE
        } else {
          res$action.info$best.effort <- FALSE
          res$action.info$suboptimality <- pred.values[best.action]-pred.values[res$action]
          res$action.info$noise.type <- "shock"
        }
        return(res)
      } else {
        rands <- rnorm(length(pred.values),mean=0, sd=algo.var$expl.path.multi.var)
        if(!is.null(model.fam)) {
          fam.noise <- algo.var$expl.path.multi.fam*(1-model.par.fam$predict(model.fam,model.par.fam,state))
        } else {
          fam.noise <- rep(0,length(pred.values))
        }
        if(!is.null(model.surp)) {
          surp.noise <- algo.var$expl.path.multi.surp*(model.par.surp$predict(model.surp,model.par.surp,state))
        } else {
          surp.noise <- rep(0,length(pred.values))
        }
        act.values <- pred.values + rands + fam.noise + surp.noise
        res$action <- which.is.max(act.values)
        if(is.na(res$action)) res$action <- sample(1:game.object$game.par(game.object)$output.nodes,1)
        if(res$action==which.is.max(pred.values)){
          res$action.info$best.effort <- TRUE
        } else {
          res$action.info$best.effort <- FALSE
          res$action.info$suboptimality <- pred.values[best.action]-pred.values[res$action]
          res$action.info$noise.type <- "multi"

          #Calculate Suoptimality Actions
          total.deciding.noise <- (rands[res$action]-rands[best.action]) + (fam.noise[res$action]-fam.noise[best.action]) + (surp.noise[res$action]-surp.noise[best.action])
          if(total.deciding.noise==0) total.deciding.noise <- 1 #If the which.is.max was the random decision
          res$action.info$noise.fractions <- c((rands[res$action]-rands[best.action])/total.deciding.noise, (fam.noise[res$action]-fam.noise[best.action])/total.deciding.noise, (surp.noise[res$action]-surp.noise[best.action])/total.deciding.noise)
          names(res$action.info$noise.fractions) <- c("var","fam","surp")
        }
        return(res)
      }
    } else {
      stop("block type not supported")
    }
  } else {
    stop("Wrong action policy specified in algo.par.")
  }
}

#' Play the game based on strategy
#'
#' @export
Play.On.Strategy.QLearningPersExpPath <- function(model, model.surp=NULL, model.fam=NULL, model.par, model.par.surp=NULL, model.par.fam=NULL, algo.par, algo.var, game.object, best.guess.episode=FALSE, eval.only=TRUE, block.type=NULL){
  restore.point("Play.On.Strategy.QLearningPersExpPath")
  state <- game.object$start.state(game.object)
  score <- 0
  new.mem <- c()
  best.effort <- c()
  suboptimality <- list()
  suboptimality$total <- 0
  start <- TRUE
  if(!is.null(block.type)&&block.type=="vs"){
    suboptimality$var <- 0
    suboptimality$shock <- 0
  }
  if(!is.null(block.type)&&block.type=="multi"){
    suboptimality$var <- 0
    suboptimality$shock <- 0
    suboptimality$surp <- 0
    suboptimality$fam <- 0
  }

  #if(algo.par$action.policy=="exploration.path"){
  #  algo.var$expl.path.var.cur <- runif(1,min=0,max=algo.var$expl.path.var)
  #}

  while(TRUE){
    restore.point("within.Train.QLearning.II")

    vis.state <- t(game.object$state.2.array(game.state=state, game.object=game.object)) # not a real state but what the algorithm sees. Could be a lot smaller than the real game state [but might depend on encoding]
    if(algo.par$use.rnn){
      if(any(is.na(vis.state))) vis.state[is.na(vis.state)] <- model.par$mask.value
    }
    action.full <- Act.QLearningPersExpPath(state=vis.state, model=model, model.surp=model.surp, model.fam=model.fam, model.par=model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par=algo.par, algo.var=algo.var, game.object=game.object, eval.only=eval.only, best.guess.episode=best.guess.episode, block.type=block.type)
    action <- action.full[["action"]]
    if(action>2){
      stop("heretmp")
    }
    best.effort <- c(best.effort,action.full$action.info$best.effort)
    suboptimality$total <- suboptimality$total+action.full$action.info$suboptimality

    #Deal with suboptimality juggling with vs
    if(!is.null(block.type)&&block.type=="vs"){
      if(!is.na(action.full$action.info$noise.type)){
        if(action.full$action.info$noise.type == "var"){
          suboptimality$var <- suboptimality$var + action.full$action.info$suboptimality
        } else if(action.full$action.info$noise.type == "shock"){
          suboptimality$shock <- suboptimality$shock + action.full$action.info$suboptimality
        } else {
          stop("unknown noise type")
        }
      }
    }

    #Deal with suboptimality juggling with multi
    if(!is.null(block.type)&&block.type=="multi"){
      if(!is.na(action.full$action.info$noise.type)){
        if(action.full$action.info$noise.type == "multi"){
          suboptimality$var <- suboptimality$var + action.full$action.info$suboptimality*action.full$action.info$noise.fractions["var"]
          suboptimality$surp <- suboptimality$surp + action.full$action.info$suboptimality*action.full$action.info$noise.fractions["surp"]
          suboptimality$fam <- suboptimality$fam + action.full$action.info$suboptimality*action.full$action.info$noise.fractions["fam"]
        } else if(action.full$action.info$noise.type == "shock"){
          suboptimality$shock <- suboptimality$shock + action.full$action.info$suboptimality
        } else {
          stop("unknown noise type")
        }
      }
    }

    next.state.full <- game.object$state.transition(game.state=state,action=action,game.object=game.object)

    next.state <- next.state.full$next.state
    vis.next.state <- t(game.object$state.2.array(game.state=next.state, game.object=game.object))
    reward <- next.state.full$reward
    if("discounted.reward" %in% game.object$supports){
      score <- game.object$discounted.reward(game.object, game.state=state, old.score=score, reward=reward)
    } else {
      score <- score+reward
    }
    done <- next.state.full$game.finished

    #Build memory cache
    if(algo.par$use.rnn){
      if(any(is.na(vis.next.state))) vis.next.state[is.na(vis.next.state)] <- model.par$mask.value
    }
    new.mem[[length(new.mem)+1]] <- list(state=vis.state, action=action, next.state=vis.next.state, reward=reward, done=done, start=start)
    start <- FALSE
    if(done){
      break
    }
    state <- next.state
  }
  res <- nlist(score,new.mem, best.effort, suboptimality)
}

#' Train multiple games
#'
#' @export
Play.Multiple.Games.QLearningPersExpPath <- function(episodes, model,model.surp=NULL, model.fam=NULL, model.par, model.par.surp=NULL, model.par.fam=NULL,algo.par,algo.var,game.object,type="best", eval.only=TRUE){
  #restore.point("Play.Multiple.Games")
  if(is.null(episodes)||is.na(episodes)||episodes==0){
    return(NULL)
  }
  if((type=="expl.var"||type=="expl.vs" || type=="expl.surp"|| type=="expl.fam" || type=="expl.multi") &&  algo.par$action.policy!='exploration.path'){
    return(NULL)
  }
  if(type=="best"){
    l.res <- lapply(1:episodes,FUN=function(x){
      return(Play.On.Strategy.QLearningPersExpPath(model, model.surp=model.surp, model.fam=model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object, best.guess.episode = TRUE, eval.only=eval.only))
    })
  } else if(type=="cur"){
    l.res <- lapply(1:episodes,FUN=function(x){
      return(Play.On.Strategy.QLearningPersExpPath(model, model.surp=model.surp, model.fam=model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object, best.guess.episode = TRUE, eval.only=eval.only))
    })
  } else if(type=="expl.var"){
    l.res <- lapply(1:episodes,FUN=function(x){
      return(Play.On.Strategy.QLearningPersExpPath(model, model.surp=model.surp, model.fam=model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object, best.guess.episode = FALSE, eval.only=eval.only, block.type="var"))
    })
  } else if(type=="expl.shock"){
    l.res <- lapply(1:episodes,FUN=function(x){
      return(Play.On.Strategy.QLearningPersExpPath(model, model.surp=model.surp, model.fam=model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object, best.guess.episode = FALSE, eval.only=eval.only, block.type="shock"))
    })
  } else if (type=="expl.surp"){
    l.res <- lapply(1:episodes,FUN=function(x){
      return(Play.On.Strategy.QLearningPersExpPath(model, model.surp=model.surp, model.fam=model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object, best.guess.episode = FALSE, eval.only=eval.only, block.type="surp"))
    })
  } else if (type=="expl.fam"){
    l.res <- lapply(1:episodes,FUN=function(x){
      return(Play.On.Strategy.QLearningPersExpPath(model, model.surp=model.surp, model.fam=model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object, best.guess.episode = FALSE, eval.only=eval.only, block.type="fam"))
    })
  } else if(type=="expl.vs"){
    l.res <- lapply(1:episodes,FUN=function(x){
      return(Play.On.Strategy.QLearningPersExpPath(model, model.surp=model.surp, model.fam=model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object, best.guess.episode = FALSE, eval.only=eval.only, block.type="vs"))
    })
  } else if(type=="expl.multi"){
    l.res <- lapply(1:episodes,FUN=function(x){
      return(Play.On.Strategy.QLearningPersExpPath(model, model.surp=model.surp, model.fam=model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object, best.guess.episode = FALSE, eval.only=eval.only, block.type="multi"))
    })
  }
  new.mem <- unlist(lapply(l.res,FUN=function(x){x$new.mem}), recursive=FALSE)
  score <- unlist(lapply(l.res,FUN=function(x){x$score}), recursive=FALSE)
  best.effort <- unlist(lapply(l.res,FUN=function(x){x$best.effort}), recursive=FALSE)
  suboptimality <- list()
  suboptimality$total <- unlist(lapply(l.res,FUN=function(x){x$suboptimality$total}), recursive=FALSE)
  suboptimality$var <- unlist(lapply(l.res,FUN=function(x){x$suboptimality$var}), recursive=FALSE)
  suboptimality$shock <- unlist(lapply(l.res,FUN=function(x){x$suboptimality$shock}), recursive=FALSE)
  suboptimality$surp <- unlist(lapply(l.res,FUN=function(x){x$suboptimality$surp}), recursive=FALSE)
  suboptimality$fam <- unlist(lapply(l.res,FUN=function(x){x$suboptimality$fam}), recursive=FALSE)
  return(nlist(new.mem,score,best.effort, suboptimality))
}

#' Train a model based on Q-Learning
#'
#' @export
Train.QLearningPersExpPath <- function(evaluator, model.par, algo.par, algo.var, model.par.surp=NULL, model.par.fam=NULL, game.object, blocks, eval.only=FALSE, eval.each.round=NULL, start.w.training=TRUE, out.file=NULL){
  restore.point("Train.QLearningPersExpPath")

  if(is.null(algo.var$analysis)){
    algo.var$analysis <- list()
    algo.var$analysis$score.expl.var <- NA
    algo.var$analysis$score.expl.block.var <- NA
    algo.var$analysis$score.expl.shock <- NA
    algo.var$analysis$score.expl.block.shock <- NA
    algo.var$analysis$score.expl.surp <- NA
    algo.var$analysis$score.expl.block.surp <- NA
    algo.var$analysis$score.expl.fam <- NA
    algo.var$analysis$score.expl.block.fam <- NA
    algo.var$analysis$score.best <- NA
    algo.var$analysis$score.best.block <- NA
    algo.var$analysis$score.cur <- NA
    algo.var$analysis$score.cur.block <- NA
    algo.var$analysis$score.expl.vs <- NA
    algo.var$analysis$score.expl.block.vs <- NA
    algo.var$analysis$score.expl.multi <- NA
    algo.var$analysis$score.expl.block.multi <- NA
    algo.var$analysis$sub.vs.var <- NA
    algo.var$analysis$sub.block.vs.var <- NA
    algo.var$analysis$sub.vs.shock <- NA
    algo.var$analysis$sub.block.vs.shock <- NA
    algo.var$analysis$sub.multi <- NA
    algo.var$analysis$sub.block.multi <- NA
    algo.var$analysis$sub.multi.var <- NA
    algo.var$analysis$sub.block.multi.var <- NA
    algo.var$analysis$sub.multi.shock <- NA
    algo.var$analysis$sub.block.multi.shock <- NA
    algo.var$analysis$sub.multi.surp <- NA
    algo.var$analysis$sub.block.multi.surp <- NA
    algo.var$analysis$sub.multi.fam <- NA
    algo.var$analysis$sub.block.multi.fam <- NA
    algo.var$analysis$sub.vs <- NA
    algo.var$analysis$sub.block.vs <- NA
    algo.var$analysis$sub.var <- NA
    algo.var$analysis$sub.block.var <- NA
    algo.var$analysis$sub.shock <- NA
    algo.var$analysis$sub.block.shock <- NA
    algo.var$analysis$sub.surp <- NA
    algo.var$analysis$sub.block.surp <- NA
    algo.var$analysis$sub.fam <- NA
    algo.var$analysis$sub.block.fam <- NA
    algo.var$analysis$time <- NA
    algo.var$analysis$external.eval <- NA
  }

  if(is.null(algo.var$path.goal.var)) algo.var$path.goal.var <- algo.par$expl.path.var.start
  if(is.null(algo.var$path.goal.shock)) algo.var$path.goal.shock <- algo.par$expl.path.shock.start
  if(is.null(algo.var$path.goal.surp)) algo.var$path.goal.surp <- algo.par$expl.path.surp.start
  if(is.null(algo.var$path.goal.fam)) algo.var$path.goal.fam <- algo.par$expl.path.fam.start
  if(is.null(algo.var$path.goal.vs)) algo.var$path.goal.vs <- algo.par$expl.path.vs.start
  if(is.null(algo.var$path.goal.multi)) algo.var$path.goal.multi <- algo.par$expl.path.multi.start

  #Toggle Surprise model
  if(algo.par$block.expl.surp==0 && (algo.par$block.expl.multi==0 || (algo.par$block.expl.multi>0 && (algo.par$expl.path.multi.start.frac.surp==0&&algo.par$expl.path.multi.end.frac.surp==0)))){
    model.par.surp <- NULL
  }
  if(algo.par$block.expl.fam==0 && (algo.par$block.expl.multi==0 || (algo.par$block.expl.multi>0 && (algo.par$expl.path.multi.start.frac.fam==0&&algo.par$expl.path.multi.end.frac.fam==0)))){
    model.par.fam <- NULL
  }

  if(length(algo.var$memory)>0 && start.w.training && !eval.only){
    replay.res <- Replay.QLearningPersExpPath(evaluator, model.par, model.par.surp = model.par.surp, model.par.fam=model.par.fam, algo.par, algo.var, game.object)
    evaluator <- Update.Evaluator.QLearningPersExpPath(evaluator=evaluator, new.model=replay.res$model, new.model.surp=replay.res$model.surp, new.model.fam=replay.res$model.fam, which.update="both", model.par=model.par)
    algo.var <- replay.res$algo.var
  }

  start.block <- max(length(algo.var$analysis$score.best.block),length(algo.var$analysis$score.cur.block))+1
  if(is.na(algo.var$analysis$score.best.block)&&is.na(algo.var$analysis$score.cur.block)){
    start.block <- 1
  }

  for(i in start.block:blocks){
    restore.point("within.Train.QLearningPersExpPath")

    #### Start Outside Info for self-play
    assign(game.object$basic.name.eval.model, evaluator$model.best, envir=.GlobalEnv)
    assign(game.object$basic.name.eval.model.par, model.par, envir=.GlobalEnv)
    ##### End Outside Info for self-play

    tic()
    best.results <- Play.Multiple.Games.QLearningPersExpPath(episodes=algo.par$block.best,model=evaluator$model.best,model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par,algo.var,game.object,type="best", eval.only=eval.only)
    cur.results <- Play.Multiple.Games.QLearningPersExpPath(episodes=algo.par$block.cur,model=evaluator$model.cur,model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par,algo.var,game.object,type="cur", eval.only=eval.only)
    expl.results.var <- Play.Multiple.Games.QLearningPersExpPath(episodes=algo.par$block.expl.var,model=evaluator$model.cur,model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par,algo.var,game.object,type="expl.var", eval.only=eval.only)
    expl.results.shock <- Play.Multiple.Games.QLearningPersExpPath(episodes=algo.par$block.expl.shock,model=evaluator$model.cur,model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par,algo.var,game.object,type="expl.shock", eval.only=eval.only)
    expl.results.surp <- Play.Multiple.Games.QLearningPersExpPath(episodes=algo.par$block.expl.surp,model=evaluator$model.cur,model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par,algo.var,game.object,type="expl.surp", eval.only=eval.only)
    expl.results.fam <- Play.Multiple.Games.QLearningPersExpPath(episodes=algo.par$block.expl.fam,model=evaluator$model.cur,model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par,algo.var,game.object,type="expl.fam", eval.only=eval.only)
    expl.results.vs <- Play.Multiple.Games.QLearningPersExpPath(episodes=algo.par$block.expl.vs,model=evaluator$model.cur,model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par,algo.var,game.object,type="expl.vs", eval.only=eval.only)
    expl.results.multi <- Play.Multiple.Games.QLearningPersExpPath(episodes=algo.par$block.expl.multi,model=evaluator$model.cur,model.surp=evaluator$model.surp, model.fam=evaluator$model.fam, model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par,algo.var,game.object,type="expl.multi", eval.only=eval.only)

    #Update memory
    algo.var <- Update.Memory.QLearningPersExpPath(algo.var, best.results$new.mem, game.object, algo.par, best.effort=best.results$best.effort)
    algo.var <- Update.Memory.QLearningPersExpPath(algo.var, cur.results$new.mem, game.object, algo.par, best.effort=cur.results$best.effort)
    algo.var <- Update.Memory.QLearningPersExpPath(algo.var, expl.results.var$new.mem, game.object, algo.par, best.effort=expl.results.var$best.effort)
    algo.var <- Update.Memory.QLearningPersExpPath(algo.var, expl.results.shock$new.mem, game.object, algo.par, best.effort=expl.results.shock$best.effort)
    algo.var <- Update.Memory.QLearningPersExpPath(algo.var, expl.results.surp$new.mem, game.object, algo.par, best.effort=expl.results.surp$best.effort)
    algo.var <- Update.Memory.QLearningPersExpPath(algo.var, expl.results.fam$new.mem, game.object, algo.par, best.effort=expl.results.fam$best.effort)
    algo.var <- Update.Memory.QLearningPersExpPath(algo.var, expl.results.vs$new.mem, game.object, algo.par, best.effort=expl.results.vs$best.effort)
    algo.var <- Update.Memory.QLearningPersExpPath(algo.var, expl.results.multi$new.mem, game.object, algo.par, best.effort=expl.results.multi$best.effort)


    #If whished, eval.each.round evaluates current model externally
    if(!is.null(eval.each.round)&&eval.each.round!=0){
      restore.point("eval.internal")
      if(is.null(game.object$external.eval)){
        stop("This game does not support External Evaluation!")
      }
      external.eval <- game.object$external.eval(game.object,eval.no=eval.each.round)
      if(is.na(algo.var$analysis$external.eval[1])){
        algo.var$analysis$external.eval <- external.eval
      } else {
        algo.var$analysis$external.eval <- c(algo.var$analysis$external.eval,external.eval)
      }
    }

    #Update scores
    if(!is.null(expl.results.var)){
      if(is.na(algo.var$analysis$score.expl.var[1])){
        algo.var$analysis$score.expl.var <- expl.results.var$score
        algo.var$analysis$score.expl.block.var <- mean(expl.results.var$score)
      } else {
        algo.var$analysis$score.expl.var <- c(algo.var$analysis$score.expl.var, expl.results.var$score)
        algo.var$analysis$score.expl.block.var <- c(algo.var$analysis$score.expl.block.var,mean(expl.results.var$score))
      }
    }

    if(!is.null(expl.results.shock)){
      if(is.na(algo.var$analysis$score.expl.shock[1])){
        algo.var$analysis$score.expl.shock <- expl.results.shock$score
        algo.var$analysis$score.expl.block.shock <- mean(expl.results.shock$score)
      } else {
        algo.var$analysis$score.expl.shock <- c(algo.var$analysis$score.expl.shock, expl.results.shock$score)
        algo.var$analysis$score.expl.block.shock <- c(algo.var$analysis$score.expl.block.shock,mean(expl.results.shock$score))
      }
    }

    if(!is.null(expl.results.surp)){
      if(is.na(algo.var$analysis$score.expl.surp[1])){
        algo.var$analysis$score.expl.surp <- expl.results.surp$score
        algo.var$analysis$score.expl.block.surp <- mean(expl.results.surp$score)
      } else {
        algo.var$analysis$score.expl.surp <- c(algo.var$analysis$score.expl.surp, expl.results.surp$score)
        algo.var$analysis$score.expl.block.surp <- c(algo.var$analysis$score.expl.block.surp,mean(expl.results.surp$score))
      }
    }

    if(!is.null(expl.results.fam)){
      if(is.na(algo.var$analysis$score.expl.fam[1])){
        algo.var$analysis$score.expl.fam <- expl.results.fam$score
        algo.var$analysis$score.expl.block.fam <- mean(expl.results.fam$score)
      } else {
        algo.var$analysis$score.expl.fam <- c(algo.var$analysis$score.expl.fam, expl.results.fam$score)
        algo.var$analysis$score.expl.block.fam <- c(algo.var$analysis$score.expl.block.fam,mean(expl.results.fam$score))
      }
    }

    if(!is.null(expl.results.vs)){
      if(is.na(algo.var$analysis$score.expl.vs[1])){
        algo.var$analysis$score.expl.vs <- expl.results.vs$score
        algo.var$analysis$score.expl.block.vs <- mean(expl.results.vs$score)
      } else {
        algo.var$analysis$score.expl.vs <- c(algo.var$analysis$score.expl.vs, expl.results.vs$score)
        algo.var$analysis$score.expl.block.vs <- c(algo.var$analysis$score.expl.block.vs,mean(expl.results.vs$score))
      }
    }

    if(!is.null(expl.results.multi)){
      if(is.na(algo.var$analysis$score.expl.multi[1])){
        algo.var$analysis$score.expl.multi <- expl.results.multi$score
        algo.var$analysis$score.expl.block.multi <- mean(expl.results.multi$score)
      } else {
        algo.var$analysis$score.expl.multi <- c(algo.var$analysis$score.expl.multi, expl.results.multi$score)
        algo.var$analysis$score.expl.block.multi <- c(algo.var$analysis$score.expl.block.multi,mean(expl.results.multi$score))
      }
    }

    if(!is.null(best.results)){
      if(is.na(algo.var$analysis$score.best[1])){
        algo.var$analysis$score.best <- best.results$score
        algo.var$analysis$score.best.block <- mean(best.results$score)
      } else {
        algo.var$analysis$score.best <- c(algo.var$analysis$score.best, best.results$score)
        algo.var$analysis$score.best.block <- c(algo.var$analysis$score.best.block,mean(best.results$score))
      }
    }

    if(!is.null(cur.results)){
      if(is.na(algo.var$analysis$score.cur[1])){
        algo.var$analysis$score.cur <- cur.results$score
        algo.var$analysis$score.cur.block <- mean(cur.results$score)
      } else {
        algo.var$analysis$score.cur <- c(algo.var$analysis$score.cur, cur.results$score)
        algo.var$analysis$score.cur.block <- c(algo.var$analysis$score.cur.block,mean(cur.results$score))
      }
    }

    #Update suboptimality
    if(!is.null(expl.results.var)){
      if(is.na(algo.var$analysis$sub.var[1])){
        algo.var$analysis$sub.var <- expl.results.var$suboptimality$total
        algo.var$analysis$sub.block.var <- mean(expl.results.var$suboptimality$total)
      } else {
        algo.var$analysis$sub.var <- c(algo.var$analysis$sub.var,expl.results.var$suboptimality$total)
        algo.var$analysis$sub.block.var <- c(algo.var$analysis$sub.block.var,mean(expl.results.var$suboptimality$total))
      }
    }

    if(!is.null(expl.results.shock)){
      if(is.na(algo.var$analysis$sub.shock[1])){
        algo.var$analysis$sub.shock <- expl.results.shock$suboptimality$total
        algo.var$analysis$sub.block.shock <- mean(expl.results.shock$suboptimality$total)
      } else {
        algo.var$analysis$sub.shock <- c(algo.var$analysis$sub.shock,expl.results.shock$suboptimality$total)
        algo.var$analysis$sub.block.shock <- c(algo.var$analysis$sub.block.shock,mean(expl.results.shock$suboptimality$total))
      }
    }

    if(!is.null(expl.results.surp)){
      if(is.na(algo.var$analysis$sub.surp[1])){
        algo.var$analysis$sub.surp <- expl.results.surp$suboptimality$total
        algo.var$analysis$sub.block.surp <- mean(expl.results.surp$suboptimality$total)
      } else {
        algo.var$analysis$sub.surp <- c(algo.var$analysis$sub.surp,expl.results.surp$suboptimality$total)
        algo.var$analysis$sub.block.surp <- c(algo.var$analysis$sub.block.surp,mean(expl.results.surp$suboptimality$total))
      }
    }

    if(!is.null(expl.results.fam)){
      if(is.na(algo.var$analysis$sub.fam[1])){
        algo.var$analysis$sub.fam <- expl.results.fam$suboptimality$total
        algo.var$analysis$sub.block.fam <- mean(expl.results.fam$suboptimality$total)
      } else {
        algo.var$analysis$sub.fam <- c(algo.var$analysis$sub.fam,expl.results.fam$suboptimality$total)
        algo.var$analysis$sub.block.fam <- c(algo.var$analysis$sub.block.fam,mean(expl.results.fam$suboptimality$total))
      }
    }

    if(!is.null(expl.results.vs)){
      if(is.na(algo.var$analysis$sub.vs[1])){
        algo.var$analysis$sub.vs <- expl.results.vs$suboptimality$total
        algo.var$analysis$sub.block.vs <- mean(expl.results.vs$suboptimality$total)
        algo.var$analysis$sub.vs.var <- expl.results.vs$suboptimality$var
        algo.var$analysis$sub.block.vs.var <- mean(expl.results.vs$suboptimality$var)
        algo.var$analysis$sub.vs.shock <- expl.results.vs$suboptimality$shock
        algo.var$analysis$sub.block.vs.shock <- mean(expl.results.vs$suboptimality$shock)
      } else {
        algo.var$analysis$sub.vs <- c(algo.var$analysis$sub.vs,expl.results.vs$suboptimality$total)
        algo.var$analysis$sub.block.vs <- c(algo.var$analysis$sub.block.vs,mean(expl.results.vs$suboptimality$total))
        algo.var$analysis$sub.vs.var <- c(algo.var$analysis$sub.vs.var,expl.results.vs$suboptimality$var)
        algo.var$analysis$sub.block.vs.var <- c(algo.var$analysis$sub.block.vs.var,mean(expl.results.vs$suboptimality$var))
        algo.var$analysis$sub.vs.shock <- c(algo.var$analysis$sub.vs.shock,expl.results.vs$suboptimality$shock)
        algo.var$analysis$sub.block.vs.shock <- c(algo.var$analysis$sub.block.vs.shock,mean(expl.results.vs$suboptimality$shock))
      }
    }

    if(!is.null(expl.results.multi)){
      if(is.na(algo.var$analysis$sub.multi[1])){
        algo.var$analysis$sub.multi <- expl.results.multi$suboptimality$total
        algo.var$analysis$sub.block.multi <- mean(expl.results.multi$suboptimality$total)
        algo.var$analysis$sub.multi.var <- expl.results.multi$suboptimality$var
        algo.var$analysis$sub.block.multi.var <- mean(expl.results.multi$suboptimality$var)
        algo.var$analysis$sub.multi.shock <- expl.results.multi$suboptimality$shock
        algo.var$analysis$sub.block.multi.shock <- mean(expl.results.multi$suboptimality$shock)
        algo.var$analysis$sub.multi.surp <- expl.results.multi$suboptimality$surp
        algo.var$analysis$sub.block.multi.surp <- mean(expl.results.multi$suboptimality$surp)
        algo.var$analysis$sub.multi.fam <- expl.results.multi$suboptimality$fam
        algo.var$analysis$sub.block.multi.fam <- mean(expl.results.multi$suboptimality$fam)
      } else {
        algo.var$analysis$sub.multi <- c(algo.var$analysis$sub.multi,expl.results.multi$suboptimality$total)
        algo.var$analysis$sub.block.multi <- c(algo.var$analysis$sub.block.multi,mean(expl.results.multi$suboptimality$total))
        algo.var$analysis$sub.multi.var <- c(algo.var$analysis$sub.multi.var,expl.results.multi$suboptimality$var)
        algo.var$analysis$sub.block.multi.var <- c(algo.var$analysis$sub.block.multi.var,mean(expl.results.multi$suboptimality$var))
        algo.var$analysis$sub.multi.shock <- c(algo.var$analysis$sub.multi.shock,expl.results.multi$suboptimality$shock)
        algo.var$analysis$sub.block.multi.shock <- c(algo.var$analysis$sub.block.multi.shock,mean(expl.results.multi$suboptimality$shock))
        algo.var$analysis$sub.multi.surp <- c(algo.var$analysis$sub.multi.surp,expl.results.multi$suboptimality$surp)
        algo.var$analysis$sub.block.multi.surp <- c(algo.var$analysis$sub.block.multi.surp,mean(expl.results.multi$suboptimality$surp))
        algo.var$analysis$sub.multi.fam <- c(algo.var$analysis$sub.multi.fam,expl.results.multi$suboptimality$fam)
        algo.var$analysis$sub.block.multi.fam <- c(algo.var$analysis$sub.block.multi.fam,mean(expl.results.multi$suboptimality$fam))
      }
    }

    #Build output message
    output.message <- c("block: ",i," of ",blocks," with the following scores:")
    if(!is.null(best.results)) output.message <- paste0(c(output.message, " best: ",round(mean(best.results$score),2)),collapse="")
    if(!is.null(cur.results)) output.message <- paste0(c(output.message, " cur: ",round(mean(cur.results$score),2)),collapse="")
    if(!is.null(expl.results.var)) output.message <- paste0(c(output.message, " expl.var: ",round(mean(expl.results.var$score),2)),collapse="")
    if(!is.null(expl.results.surp)) output.message <- paste0(c(output.message, " expl.surp: ",round(mean(expl.results.surp$score),2)),collapse="")
    if(!is.null(expl.results.fam)) output.message <- paste0(c(output.message, " expl.fam: ",round(mean(expl.results.fam$score),2)),collapse="")
    if(!is.null(expl.results.vs)) output.message <- paste0(c(output.message, " expl.vs: ",round(mean(expl.results.vs$score),2)),collapse="")
    if(!is.null(expl.results.multi)) output.message <- paste0(c(output.message, " expl.multi: ",round(mean(expl.results.multi$score),2)),collapse="")
    if(!is.null(expl.results.shock) && algo.par$action.policy=="exploration.path") output.message <- paste0(c(output.message, " expl.shock(",round(algo.var$expl.path.shock,3),"): ",round(mean(expl.results.shock$score),2)),collapse="")
    if(!is.null(expl.results.shock) && algo.par$action.policy=="epsilon.greedy") output.message <- paste0(c(output.message, " expl.shock(",round(algo.var$epsilon,3),"): ",round(mean(expl.results.shock$score),2)),collapse="")

    print(output.message)

    if(length(algo.var$memory)>algo.par$max.mem){
      algo.var$memory <- algo.var$memory[-c(sample(1:length(algo.var$memory),round(length(algo.var$memory)*algo.par$remove.memory)))]
    }

    if(!eval.only){
      #If Score is better with new model, change best model
      if(!is.null(cur.results) && !is.null(best.results)){
        if(mean(cur.results$score)>mean(best.results$score)){
          evaluator <- Update.Evaluator.QLearningPersExpPath(evaluator, new.model=evaluator$model.cur, which.update = "best", model.par=model.par)
          print("New best model chosen!")
        }
      } else {
        if(!is.null(cur.results)){
          evaluator <- Update.Evaluator.QLearningPersExpPath(evaluator, new.model=evaluator$model.cur, which.update = "best", model.par=model.par)
        }
      }

      #Train new cur.model
      replay.res <- Replay.QLearningPersExpPath(evaluator=evaluator, model.par=model.par, model.par.surp=model.par.surp, model.par.fam=model.par.fam, algo.par=algo.par, algo.var=algo.var, game.object=game.object)
      evaluator <- replay.res$evaluator
      algo.var <- replay.res$algo.var

      #End time measurement
      toc.res <- toc(quiet=TRUE)
      used.time <- toc.res$toc-toc.res$tic
      #Update time
      if(is.na(algo.var$analysis$time[1])){
        algo.var$analysis$time <- used.time
      } else {
        algo.var$analysis$time <- c(algo.var$analysis$time,used.time)
      }

      #Change Algovariables
      algo.var <- Change.Algo.Var.QLearningPersExpPath(algo.par, algo.var, cur.block=i, blocks=blocks)
    }

    #Save intermediate results in case of computer crash etc.
    if(is.null(out.file)){
      out.file <- "out.save"
    }

    #Only same types of models are supported as of right now
    if((!is.null(model.par.surp)&&model.par$name!=model.par.surp$name)||(!is.null(model.par.fam)&&model.par$name!=model.par.fam$name)){
      stop("Models of Surprise, Familiarity and the Main Model have to be currently of the same type (Neural Network, XGBoost,...)")
    }

    if(!(model.par$name %in% c("Neural.Network.Basic", "RNN.Basic"))){ #Basic scenario, we do not need to worry about pointers
      save(evaluator, algo.var,file=out.file)
    } else {
      if(model.par$name=="Neural.Network.Basic"){
        file.sub.name <- "NN"
      } else {
        file.sub.name <- "RNN"
      }
      if(!is.null(evaluator$model.cur)&&("keras.engine.training.Model" %in% class(evaluator$model.cur))){
        cur.file.path <- paste0(getwd(),"/",format(Sys.time(), "%Y-%m-%d %H")," cur.",file.sub.name,"_save")
        evaluator$model.cur.out.file <- cur.file.path
        save_model_hdf5(object=evaluator$model.cur, filepath=cur.file.path, overwrite = TRUE, include_optimizer = TRUE)
      }
      if(!is.null(evaluator$model.best)&&("keras.engine.training.Model" %in% class(evaluator$model.best))){
        best.file.path <- paste0(getwd(),"/",format(Sys.time(), "%Y-%m-%d %H")," best.",file.sub.name,"_save")
        evaluator$model.best.out.file <- best.file.path
        save_model_hdf5(object=evaluator$model.best, filepath=best.file.path, overwrite = TRUE, include_optimizer = TRUE)
      }
      if(!is.null(evaluator$model.surp)&&("keras.engine.training.Model" %in% class(evaluator$model.surp))){
        surp.file.path <- paste0(getwd(),"/",format(Sys.time(), "%Y-%m-%d %H")," surp.",file.sub.name,"_save")
        evaluator$model.surp.out.file <- surp.file.path
        save_model_hdf5(object=evaluator$model.surp, filepath=surp.file.path, overwrite = TRUE, include_optimizer = TRUE)
      }
      if(!is.null(evaluator$model.fam)&&("keras.engine.training.Model" %in% class(evaluator$model.fam))){
        fam.file.path <- paste0(getwd(),"/",format(Sys.time(), "%Y-%m-%d %H")," fam.",file.sub.name,"_save")
        evaluator$model.fam.out.file <- fam.file.path
        save_model_hdf5(object=evaluator$model.fam, filepath=fam.file.path, overwrite = TRUE, include_optimizer = TRUE)
      }
      save(evaluator, algo.var,file=out.file)
    }
  }

  return(list(evaluator=evaluator, algo.var=algo.var))
}


#' Q-values based on history of IPD
#'
#' May become deprecated if changes in IPD are made. Just used for easier analysis.
#'
#' @export
Q.on.hist.PD.QLearning <- function(model, model.par, game.object, hist.me, hist.other, hist.real.me="not given"){
  restore.point("Q.on.hist.PD.QLearning")

  if(length(hist.me)!=length(hist.other)){
    stop("both histories have to have the same ")
  }

  state <- list()
  state$round <- length(hist.me)+1
  state$me.last.see <- hist.me[length(hist.me)]
  state$other.last.see <- hist.other[length(hist.other)]
  state$game.finished <- FALSE

  #as seen
  hist.me.full <- rep(NA,game.object$game.pars$T.max)
  if(length(hist.me)>0) hist.me.full[1:length(hist.me)] <- hist.me

  hist.other.full <- rep(NA,game.object$game.pars$T.max)
  if(length(hist.other)>0) hist.other.full[1:length(hist.other)] <- hist.other

  state$history.see <- cbind(hist.me.full,hist.other.full)

  #real
  if(is.null(hist.real.me) || hist.real.me != "not given"){
    hist.real.me.full <- rep(NA,game.object$game.pars$T.max)
    if(length(hist.real.me)>0) hist.real.me.full[1:length(hist.real.me)] <- hist.real.me

    hist.real.other.full <- rep(NA,game.object$game.pars$T.max)

    state$history.real <- cbind(hist.real.me.full,hist.real.other.full)
  }

  x_train <- t(game.object$state.2.array(game.state=state, game.object=game.object))

  if(model.par$name=="RNN.Basic"){
    x_train[is.na(x_train)] <- model.par$mask.value
  }

  return(model.par$predict(model,model.par, x_train))
}

