#Function related to XGBoost

#' Default Parameters for XGBoost
#' 
#' Returns the 'model.par' necessary for \code{\link{Setup.QLearningPersExpPath}}, \code{\link{Initialise.QLearningPersExpPath}} and \code{\link{Train.QLearningPersExpPath}}
#' 
#' @param setting String which defines the setting of the default parameters. \itemize{
#'   \item "ThesisOpt" - Default setting. The same parameters are used as for the final results of the thesis of Martin Kies.
#'   \item "Legacy.v.0.1.6" - Uses the default settings of version 0.1.6 of this package
#' }
#'
#' @export
Get.Def.Par.XGBoost <- function(setting="ThesisOpt"){
  #Identifier
  name <- "Gradient.Boosting.XGBoost"

  #Functions
  setup <- Setup.XGBoost
  predict <- Predict.XGBoost
  train <- Train.XGBoost

  #Parameters
  if(is.null(setting)||setting=="ThesisOpt"){
    model.defs <- Get.Def.Par.XGBoost.Legacy.ThesisOpt()
  } else if(setting=="Legacy.v.0.1.6"){
    model.defs <- Get.Def.Par.XGBoost.Legacy.v.0.1.6()
  } else {
    stop("Get.Def.Par.XGBoost has no legal setting.")
  } 
  
  #Meta Parameters
  single.train <- TRUE

  model.def.par <- c(nlist(name,setup,predict,train, single.train),model.defs)

  return(model.def.par)
}

#' @export
Setup.XGBoost <- function(model.par, game.par){
  #initialize with 0
  x_train <- matrix(0,ncol=game.par$input.nodes+game.par$output.nodes,nrow=(game.par$input.nodes+game.par$output.nodes)*3)
  y_train <- matrix(0,nrow=(game.par$input.nodes+game.par$output.nodes)*3)

  model <- xgboost(x_train, y_train,nrounds=model.par$nrounds,max_depth=model.par$max_depth, eta=model.par$eta, gamma=model.par$gamma,colsample=model.par$colsample, subsample=model.par$subsample, min_child_weight=model.par$min_child_weight, verbose=0, nthread=model.par$nthread)

  return(model)
}

#' @export
Predict.XGBoost <- function(model, model.par, state, action=NULL){
  restore.point("Predict.XGBoost")
  no.action <- model$nfeatures - ncol(state)
  if(!is.null(action)){
    add.actions <- t(sapply(action,FUN=function(x){
      in.res <-rep(0,no.action)
      in.res[x] <- 1
      return(in.res)
    }))
    state <- cbind(state,add.actions)
    res <- predict(model,state)
  } else {
    states.l <- lapply(1:no.action,FUN=function(x){
      m <- matrix(0,nrow=nrow(state),ncol=no.action, byrow=TRUE)
      m[,x] <- 1
      res <- cbind(state,m)
      return(res)
    })
    states <- do.call(rbind,states.l)
    res <- matrix(predict(model,states),ncol=no.action)
  }
  return(res)
}

#' @export
Train.XGBoost <- function(model, model.par, x_train, y_train, action=NULL){
  restore.point("Train.XGBoost")
  no.action <- model$nfeatures - ncol(x_train)
  if(is.null(action)){
    x_train.XG.l <- lapply(1:no.action,FUN=function(x){
      m <- matrix(0,nrow=nrow(x_train),ncol=no.action, byrow=TRUE)
      m[,x] <- 1
      res <- cbind(x_train,m)
      return(res)
    })
    x_train.XG <- do.call(rbind,x_train.XG.l)

    y_train.XG.l <- lapply(1:no.action,FUN=function(x){
      return(as.matrix(y_train[,x]))
    })
    y_train.XG <- do.call(rbind,y_train.XG.l)
  } else { #only experience
    add.actions <- t(sapply(action,FUN=function(x){
      in.res <-rep(0,no.action)
      in.res[x] <- 1
      return(in.res)
    }))
    x_train.XG <- cbind(x_train,add.actions)
    y_train.XG <- y_train
  }

  #Remove NAs
  na.vals <- is.na(y_train.XG)
  x_train.XG <- x_train.XG[!na.vals,]
  y_train.XG <- y_train.XG[!na.vals]

  model <- xgboost(x_train.XG, y_train.XG,nrounds=model.par$nrounds,max_depth=model.par$max_depth, eta=model.par$eta, gamma=model.par$gamma,colsample=model.par$colsample, subsample=model.par$subsample, min_child_weight=model.par$min_child_weight, verbose=0, nthread=model.par$nthread)
  fit.obj <- NA
  return(nlist(model,fit.obj))
}
