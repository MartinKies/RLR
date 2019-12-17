#Functions related to NeuralNetworks

#' Define default Parameters of the Neural Network Function
#'
#' Returns a List which may be used as \code{model.par} of e.g. the function \code{Setup.QLearning()} with the following parameters:\itemize{
#' \item \strong{name} - Identifier of Model. Per Default \"Neural.Network.Basic\".
#' \item \strong{setup} - Function which should be used to setup the Neural Network. Per Default \code{Setup.Neural.Network}
#' \item \strong{predict} - Function which should be used to predict the Neural Network. Per Default \code{Predict.Neural.Network}
#' \item \strong{train} - Function which should be used to train/calibrate the Neural Network. Per Default \code{Train.Neural.Network}
#' \item \strong{hidden.nodes} - A Vector consisting of the number of Neurons in each hidden layer - e.g. c(25,10) to have two hidden layers with the first layer having 25 Neurons.
#' \item \strong{activation.hidden} - A Vector defining the activation functions of the hidden layers, e.g. c(\"relu\",\"relu\"). Has to have the same number of items as \code{hidden.nodes}. Supported are e.g. relu, tanh, sigmoid and linear
#' \item \strong{activation.output} Activiation function of the output layer. Supported are e.g. relu, tanh, sigmoid and linear.
#' \item \strong{dropout} - A Vector consisting of the dropout rate of the hidden layers - e.g. c(0.2,0.2) if one wants both hidden layers to have a dropout of 20 Percent.
#' \item \strong{input.dropout} - Dropout of the input layer.
#' \item \strong{loss} Specifies the loss function, e.g. \'mse\'
#' \item \strong{optimizer}. Specifies the used optimizer. By Default Adam Optimization is used with a Learning rate of 0.001.
#' \item \strong{epochs} How many epochs should the Neural Network be trained?
#' \item \strong{batch.size} Batch Size of Neural Network.
#' \item \strong{verbose} Should the Neural Network give an output? 0 for no output, 1 for output for each epoch, 2 for aggregate output every other epoch.
#' }
#' @export
Get.Def.Par.Neural.Network <- function(setting=NULL){
  #Identifier
  name <- "Neural.Network.Basic"

  #Functions
  setup <- Setup.Neural.Network
  predict <- Predict.Neural.Network
  train <- Train.Neural.Network

  #Parameters
  if(is.null(setting)||setting=="ThesisBasic"){
    model.defs <- Get.Def.Par.NN.Legacy.Thesis.Basic()
  } else if(setting=="Legacy.v.0.1.6"){
    model.defs <- Get.Def.Par.NN.Legacy.v.0.1.6()
  } else {
    stop("Get.Def.Par.NN has no legal setting.")
  } 

  model.def.par <- c(nlist(name,setup,predict,train),model.defs)

  return(model.def.par)
}

#' Setup a Neural Network
#'
#' Setup the Neural Network in keras to work with it. Returns a keras stile Neural Network
#' @param model.par Parameters of Neural Network e.g. given by \code{Get.Def.Par.Neural.Network}
#' @param game.par Parameters of Game. Used are \itemize{
#' \item input.nodes - Number of Input Nodes
#' \item output.nodes - Number of Actions
#' }
#' @export
Setup.Neural.Network <- function(model.par, game.par){
  restore.point("Setup.Neural.Network")
  model <- keras_model_sequential()
  if(model.par$single.dimensional){
    input.nodes <- game.par$input.nodes + game.par$output.nodes
    output.nodes <- 1
  } else {
    input.nodes <- game.par$input.nodes
    output.nodes <- game.par$output.nodes
  }

  for(i in 1:length(model.par$hidden.nodes)){
    if(i==1){
      model %>%
        layer_dense(units = model.par$hidden.nodes[i], input_shape = input.nodes) %>%
        layer_activation(activation = model.par$activation.hidden[i])
      if(i==1 && !is.null(model.par$input.dropout) && model.par$input.dropout!=0){
        model %>% layer_dropout(rate=model.par$input.dropout)
      }
    } else {
      model %>%
        layer_dense(units = model.par$hidden.nodes[i], input_shape = model.par$hidden.nodes[i-1]) %>%
        layer_activation(activation = model.par$activation.hidden[i])
      if(!is.null(model.par$dropout[i]) && model.par$dropout[i]!=0){
        model %>% layer_dropout(rate=model.par$dropout[i])
      }
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

#' Evaluate Neural Network
#'
#' Evaluate a model based on a game.state
#' @param model A trained Neural Network e.g. given by \code{Setup.Neural.Network}.
#' @param state A game.state after being encoded by the game.object.
#' @param action Only the relevant action is displayed
#' @export
Predict.Neural.Network <- function(model, model.par, state, action=NULL){
  restore.point("Predict.Neural.Network")
  no.action <- model$input_shape[[2]] - ncol(state)

  if(!is.null(action)){ #we want only specific actions
    if(model.par$single.dimensional){ #it is expected, that the actions are part of the state space
      add.actions <- t(sapply(action,FUN=function(x){
        in.res <-rep(0,no.action)
        in.res[x] <- 1
        return(in.res)
      }))
      state <- cbind(state,add.actions)
      res <- predict(model,state)
    } else { #give result for all actions
      res <- predict(model,state)
      res <- res[action]
    }
  } else { #we want all actions
    if(model.par$single.dimensional){ #it is expected, that the actions are part of the state space
      states.l <- lapply(1:no.action,FUN=function(x){
        m <- matrix(0,nrow=nrow(state),ncol=no.action, byrow=TRUE)
        m[,x] <- 1
        res <- cbind(state,m)
        return(res)
      })
      states <- do.call(rbind,states.l)
      res <- matrix(predict(model,states),ncol=no.action)
    }  else {
      res <- predict(model,state)
      long.l <- lapply(1:ncol(res),FUN=function(x){return(res[,x])})
      res <- unlist(long.l)
    }
  }
  k_clear_session()
  
  return(res)
}

#' Train Neural Network
#'
#' Trains a neural network and prints some helpful statistics. Returns a trained model.
#' @param model A Neural Network e.g. given by \code{Setup.Neural.Network}
#' @param model.par Parameters of Neural Network, e.g. given by \code{Get.Default.Neural.Network}
#' @param x_train A Matrix with as much columns as input parameters in the encoding.
#' @param y_train A Matrix with as much columns as output parameters (e.g. action parameters). For each state (x_train)/action combination(column of y_train) this value determines the target.
#' @param action Which action has been taken to receive the respective reward?
#' @export
Train.Neural.Network <- function(model, model.par, x_train, y_train, action=NULL){
  restore.point("Train.Neural.Network")

  no.action <- model$input_shape[[2]] - ncol(x_train)

  if(!is.null(action)){ #we want only to train on experienced actions
    if(model.par$single.dimensional){
      add.actions <- t(sapply(action,FUN=function(x){
        in.res <-rep(0,no.action)
        in.res[x] <- 1
        return(in.res)
      }))
      x_train.NN <- cbind(x_train,add.actions)
      y_train.NN <- y_train
    } else {
      x_train.NN <- x_train
      y_train.NN <- y_train
    }
  } else{ #Train on non-experienced actions
    if(model.par$single.dimensional){ #it is expected, that the actions are part of the state space
      x_train.NN.l <- lapply(1:no.action,FUN=function(x){
        m <- matrix(0,nrow=nrow(x_train),ncol=no.action, byrow=TRUE)
        m[,x] <- 1
        res <- cbind(x_train,m)
        return(res)
      })
      x_train.NN <- do.call(rbind,x_train.NN.l)

      y_train.NN.l <- lapply(1:no.action,FUN=function(x){
        return(as.matrix(y_train[,x]))
      })
      y_train.NN <- do.call(rbind,y_train.NN.l)
    } else { #give returns for all actions
      stop("Combination training everything at once while providing only special actions is not implemented.")
    }
  }

  fit.obj <- fit(model,x_train.NN, y_train.NN, epochs = model.par$epochs, verbose=model.par$verbose, batch_size = model.par$batch.size.train)
  if(!model.par$verbose==0){
    print(fit.obj)
    print("")
  }
  return(nlist(model,fit.obj))
}
