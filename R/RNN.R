library(abind)
#Functions related to Recurrent RNNs

#' Define default Parameters of the RNN Function
#'
#' Returns a List which may be used as \code{model.par} of e.g. the function \code{Setup.QLearning()} with the following parameters:\itemize{
#' \item \strong{name} - Identifier of Model. Per Default \"RNN\".
#' \item \strong{setup} - Function which should be used to setup the RNN. Per Default \code{Setup.RNN}
#' \item \strong{predict} - Function which should be used to predict the RNN. Per Default \code{Predict.RNN}
#' \item \strong{train} - Function which should be used to train/calibrate the RNN. Per Default \code{Train.RNN}
#' \item \strong{hidden.nodes} - A Vector consisting of the number of Neurons in each hidden layer - e.g. c(25,10) to have two hidden layers with the first layer having 25 Neurons.
#' \item \strong{layer.type} - A vector consisting of the names of the type of the hidden layer. Supported are "lstm", "gru", "dense". If lstm or gru are used in a deep layer the sequence is returned.
#' \item \strong{activation.hidden} - A Vector defining the activation functions of the hidden layers, e.g. c(\"relu\",\"relu\"). Has to have the same number of items as \code{hidden.nodes}. Supported are e.g. relu, tanh, sigmoid and linear
#' \item \strong{activation.output}. Activiation function of the output layer. Supported are e.g. relu, tanh, sigmoid and linear.
#' \item \strong{loss}. Specifies the loss function, e.g. \'mse\'
#' \item \strong{optimizer}. Specifies the used optimizer. By Default Adam Optimization is used with a Learning rate of 0.001.
#' \item \strong{mask.value}. Which value should be used for masking?
#' \item \strong{epochs}. How many epochs should the RNN be trained?
#' \item \strong{batch.size}. Batch Size of RNN.
#' \item \strong{verbose}. Should the RNN give an output? 0 for no output, 1 for output for each epoch, 2 for aggregate output every other epoch.
#' }
#' 
#' @param setting Specefies the to be used setting. Currently the following settings are available: \itemize{
#'   \item "ThesisOpt" - Default setting. The same parameters are used as for the final results of the thesis of Martin Kies.
#'   \item "Legacy.v.0.1.6" - Uses the default settings of version 0.1.6 of this package
#' }
#' 
#' @export
Get.Def.Par.RNN <- function(setting="ThesisOpt"){
  #Identifier
  name <- "RNN.Basic"

  #Functions
  setup <- Setup.RNN
  predict <- Predict.RNN
  train <- Train.RNN

  #Parameters
  if(is.null(setting)||setting=="ThesisOpt"){
    model.defs <- Get.Def.Par.RNN.Legacy.ThesisOpt()
  } else if(setting=="Legacy.v.0.1.6"){
    model.defs <- Get.Def.Par.RNN.Legacy.v.0.1.6()
  } else {
    stop("Get.Def.Par.RNN has no legal setting.")
  } 

  model.def.par <- c(nlist(name,setup,predict,train),model.defs)

  return(model.def.par)
}

#' Setup a RNN
#'
#' Setup the RNN in keras to work with it. Returns a keras stile RNN
#' @param model.par Parameters of RNN e.g. given by \code{Get.Def.Par.RNN}
#' @param game.par Parameters of Game. Used are \itemize{
#' \item input.nodes - Number of Input Nodes
#' \item output.nodes - Number of Actions
#' }
#' @export
Setup.RNN <- function(model.par, game.par){
  restore.point("Setup.RNN")
  model <- keras_model_sequential()
  if(model.par$single.dimensional){
    input.nodes <- game.par$input.nodes.time
    output.nodes <- 1
  } else {
    stop("Not supported")
    input.nodes <- game.par$input.nodes.time
    output.nodes <- game.par$output.nodes
  }

  for(i in 1:length(model.par$hidden.nodes)){
    if(i==1 && !is.null(model.par$mask.value)){
      model %>% layer_masking(mask_value=model.par$mask.value, input_shape = list(NULL, input.nodes))
    }
    if(i==1 && !is.null(model.par$input.dropout) && model.par$input.dropout!=0){
      model %>% layer_dropout(rate=model.par$input.dropout)
    }

    sequence.relevant <- c("lstm","gru")
    if(length(model.par$layer.type)>i&&model.par$layer.type[i+1] %in% sequence.relevant){
      return.sequence <- TRUE
    } else {
      return.sequence <- FALSE
    }
    if(model.par$layer.type[i]=="lstm"){
      model %>%
        layer_lstm(units = model.par$hidden.nodes[i], return_sequences = return.sequence, dropout = model.par$dropout[i], recurrent_dropout = model.par$recurrent.dropout[i]) %>%
        layer_activation(activation = model.par$activation.hidden[i])
    } else if (model.par$layer.type[i]=="gru"){
      model %>%
        layer_gru(units = model.par$hidden.nodes[i], return_sequences = return.sequence, dropout = model.par$dropout[i], recurrent_dropout = model.par$recurrent.dropout[i]) %>%
        layer_activation(activation = model.par$activation.hidden[i])
    } else if (model.par$layer.type[i]=="dense"){
      model %>%
        layer_dense(units = model.par$hidden.nodes[i]) %>%
        layer_activation(activation = model.par$activation.hidden[i])
      if(!is.null(model.par$dropout[i]) && model.par$dropout[i]!=0){
        model %>% layer_dropout(rate=model.par$dropout[i])
      }
    } else {
      stop("Layer type not supported!")
    }
  }

  model %>%
    layer_dense(units = output.nodes) %>%
    layer_activation(activation = model.par$activation.output)

  model %>% keras::compile(
    loss = model.par$loss,
    optimizer = model.par$optimizer
  )
  return(model)
}

#' Evaluate Recurrent RNN
#'
#' Evaluate a model based on a game.state
#' @param model A trained RNN e.g. given by \code{Setup.RNN}.
#' @param state A game.state after being encoded by the game.object.
#' @export
Predict.RNN <- function(model, model.par, state, action=NULL){
  restore.point("Predict.RNN")

  #If only one single prediction necessary
  if(length(dim(state))==2){
    check <- array(model.par$mask.value, dim=c(1, ncol(state), nrow(state)))
    check[1,,] <- t(state)
    state <- check
  }

  col.counts <- apply(state[1,,],MARGIN=2,FUN=function(x){sum(x==model.par$mask.value)})
  action.space <- as.logical(col.counts-min(col.counts))
  start.action.space <- which(action.space)[1]
  no.action <- sum(action.space)
  no.data <- dim(state)[1]

  round <- sapply(1:dim(state)[1],FUN=function(x){
    return(sum(state[x,,start.action.space]!=model.par$mask.value)+1)
  })

  if(!is.null(action)){ #we only want specific actions
    for(i in 1:dim(state)[1]){
      if(round[i]>dim(state)[2]){
        #do nothing, as already last round - the action does not matter
      } else {
        action.long <- rep(0,sum(action.space))
        action.long[action[i]] <- 1
        state[i,round[i],action.space] <- action.long
      }
    }
    return(model %>% predict(state))
  } else { #all actions in form of a matrix
    states.l.list <- lapply(1:no.action,FUN=function(x){state})
    states.l <- abind(states.l.list, along=1)
    action.l <- unlist(lapply(1:no.action,FUN=function(x){rep(x,no.data)}))
    round.l <- unlist(lapply(1:no.action,FUN=function(x){round}))
    for(i in 1:dim(states.l)[1]){
      if(round.l[i]>dim(state)[2]){
        #do nothing, as already last round - the action does not matter
      } else {
        action.long <- rep(0,sum(action.space))
        action.long[action.l[i]] <- 1
        states.l[i,round.l[i],action.space] <- action.long
      }
    }
    return(matrix(predict(model,states.l),ncol=no.action))
  }
}

#' Train RNN
#'
#' Trains a RNN and prints some helpful statistics. Returns a trained model.
#' @param model A RNN e.g. given by \code{Setup.RNN}
#' @param model.par Parameters of RNN, e.g. given by \code{Get.Default.RNN}
#' @param x_train A 3D array in the form c(minibatch.size, max.length.sequence, outputs)
#' @param y_train In the case of !single.dimensional: A Matrix with as much columns as output parameters (e.g. action parameters). For each state (x_train)/action combination(column of y_train) this value determines the target. If single.dimensional this is a simple vector with the result and the function will build a sufficient x/y pair based on action.
#' @param action Necessary if not single dimensional - actions which led to y based on x.
#' @export
Train.RNN <- function(model, model.par, x_train, y_train, action){
  restore.point("Train.RNN")
  col.counts <- apply(x_train[1,,],MARGIN=2,FUN=function(x){sum(x==model.par$mask.value)})
  action.space <- as.logical(col.counts-min(col.counts))
  start.action.space <- which(action.space)[1]
  no.action <- sum(action.space)
  no.data <- dim(x_train)[1]

  round <- sapply(1:dim(x_train)[1],FUN=function(x){
    return(sum(x_train[x,,start.action.space]!=model.par$mask.value)+1)
  })

  if(!is.null(action)){ #we want only to train on experienced actions
    if(model.par$single.dimensional){
      x_train.NN <- x_train
      for(i in 1:no.data){
        if(round[i]>dim(x_train)[2]){
          #do nothing, as already last round - the action does not matter
        } else {
          action.long <- rep(0,sum(action.space))
          action.long[action[i]] <- 1
          x_train.NN[i,round[i],action.space] <- action.long
        }
      }
      y_train.NN <- y_train
    } else {
      x_train.NN <- x_train
      y_train.NN <- y_train
    }
  } else{ #Train on non-experienced actions
      stop("Training on non experienced actions currently not supported. Has to be tested first, but should work similarly to NN.Basic .")
  }

  fit.obj <- fit(model,x_train.NN, y_train.NN, epochs = model.par$epochs, verbose=model.par$verbose, batch_size = model.par$batch.size.train)
  if(!model.par$verbose==0){
    print(fit.obj)
    print("")
  }
  return(nlist(model,fit.obj))
}
