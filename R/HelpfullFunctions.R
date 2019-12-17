#' @export
to.binary <- function(num,max.dig=9){
  res <- rep(0,max.dig)
  for(i in max.dig:0){
    mod <- num %% 2^i
    if(mod < num){
      res[max.dig-i] <- 1
      num <- num-2^i
    } else {
      if(num == 2^i){
        res[max.dig-i] <- 1
        break
      }
    }
  }
  return(res)
}

#' Calculates a sensible moving average based on smoothing parameter
#' @param smooth How much should be smoothed. This is the total value of used data points, so (smooth-1)/2 is smoothed in either direction.
#' @param show Either 'value' for the actual value or 'sd' for a running of the averaged Standarddeviation within the smoothing range
#' @export
smooth.average <- function(input,smooth=5,show="value"){
  l <- length(input)
  if(l==1) return(input)

  #Prepare smooth
  smooth <- round(smooth)
  if(smooth %% 2 == 0) smooth <- smooth + 1
  smooth <- max(min(l,smooth),1)

  smoothed <- rep(NA,l)
  smoothed <- sapply(1:l,FUN=function(x){
    left <- max(x-(smooth-1)/2,1)
    right <- min((x+(smooth-1)/2),l)
    if(show=="value"){
      res <- mean(input[left:right])
    } else if (show=="sd"){
      res <- sd(input[left:right])
    } else {
      stop("show parameter not supported.")
    }
    return(res)
  })
  return(smoothed)
}

#' Calculates a sensible moving average based on smoothing parameter
#' @param smooth How much should be smoothed. This is the total value of used data points, so (smooth-1)/2 is smoothed in either direction.
#' @param show Either 'value' for the actual value or 'sd' for a triangle smoothing of the averaged Standarddeviation within the smoothing range
#'
#' @export
smooth.triangle <- function(input,smooth=5, show="value"){
  restore.point("smooth.triangle")
  l <- length(input)
  if(l==1) return(input)

  if(show!="value"){
    input <- smooth.average(input,smooth,show)
  }

  #Prepare smooth
  smooth <- round(smooth)
  if(smooth %% 2 == 0) smooth <- smooth + 1
  smooth <- max(min(l,smooth),1)

  smoothed <- rep(NA,l)
  smoothed <- sapply(1:l,FUN=function(x){
    left <- max(x-(smooth-1)/2,1)
    right <- min((x+(smooth-1)/2),l)
    l.left <- length(left:x)-1
    l.right <- length(x:right)-1
    w.norm <- 1:((smooth-1)/2)
    w.left <- 0
    if(l.left>0) w.left <-  w.norm[(length(w.norm)-l.left+1):length(w.norm)]
    w.right <- 0
    if(l.right>0) w.right <- w.norm[length(w.norm):(length(w.norm)-l.right+1)]
    w.abs <- c(w.left,w.norm[length(w.norm)]+1,w.right)
    w.abs <- w.abs[w.abs!=0]
    w.abs[is.na(input[left:right])] <- 0
    w <- w.abs/sum(w.abs, na.rm=TRUE)
    res <- sum(input[left:right]*w, na.rm=TRUE)
    return(res)
  })
  return(smoothed)
}

#'PID controller, which generates ouptput based on the error
#'
#' @param err.vec A vector with the error terms (discrepancies of the result and the value)
#' @param Kp Factor measuring how strong we want to turn based on proportial gain
#' @param Ki Factor measuring how strong we want to turn based on integral gain
#' @param Kd Factor measuring how strong we want to turn based on derivative gain
#' @param Kp.db How many data points should be used to calculate the proportional effect? By default the standard procedure is used, e.g. only the last data point. If there is a lot of noise in the environment it might be prudent to use a higher number.
#' @param Kp.disc When calculating the proportional effect based on multiple data.points with Kp.db it might be sensible to weight more recent data points higher. This parameter determines the discount factor.
#' @param Ki.db How many data points should be used to calculate the integral? By default the standard procedure is used, e.g. all data points. If there are strong changes in the environment it might be prudent to use a lower number.
#' @param Ki.disc When calculating the integral based on multiple data.points with Kp.db it might be sensible to weight more recent data points higher. This parameter determines the discount factor.
#' @param Kd.db How many data points should be used to calculate the derivative? By default the standard procedure is used, e.g. the change between the last two data points. If there is a lot of noise one might want to use a higher number of points.
#' @param Kd.disc When calculating the derivative based on multiple data.points with Kd.db it might be sensible to weight more recent changes higher. This parameter determines the discount factor to determine the weights of former changes.
#' @export
PID.controller <- function(err.vec, Kp, Ki, Kd, Kp.db = 1, Kp.disc=0.75, Ki.db=Inf, Kd.db=1, Kd.disc=0.75, Ki.disc=0.75){
  restore.point("PID.controller")

  pid.proportional <- function(short.vec, Kp.disc){
    weights <- Ki.disc^((length(short.vec)-1):0)
    weights.normed <- weights/sum(weights)
    res <- sum(short.vec*weights.normed)
    return(res)
  }

  pid.integral <- function(short.vec, Ki.disc){
    #Here one might use a more sophisticated version, but this is fairly standard but with discounting
    weights <- Ki.disc^((length(short.vec)-1):0)
    res <- sum(short.vec*weights)
    return(res)
  }

  pid.derivative <- function(short.vec, Kd.disc){
    if(length(short.vec)==1){
      return(0)
    }
    derivs <- short.vec[-1]-short.vec[-length(short.vec)]
    weights <- Kd.disc^((length(short.vec)-2):0)
    weights.normed <- weights/sum(weights)
    res <- sum(derivs*weights.normed)
    return(res)
  }

  last <- length(err.vec)

  prop.resp <- Kp*pid.proportional(err.vec[max(1,last-Kp.db):last], Kp.disc)
  int.resp <- Ki*pid.integral(err.vec[max(1,last-Ki.db):last], Ki.disc)
  deriv.resp <- Kd*pid.derivative(err.vec[max(1,last-Kd.db):last],Kd.disc)

  pid.resp <- prop.resp + int.resp + deriv.resp

  return(pid.resp)
}

#' Calculates a weighted Mean
#' @export
Weighted.Discount <- function(short.vec, discount){
  weights <- discount^((length(short.vec)-1):0)
  weights.normed <- weights/sum(weights)
  res <- sum(short.vec*weights.normed)
  return(res)
}

#### Hyperparameter Optimization
library(sktools)
library(rpart)
library(rpart.plot)
library(XML)
library(ggplot2)

#' @export
expand.runif.grid <- function(param.list, n, param.type, sort=FALSE){
  restore.point("expand.runif.grid")
  if(sort){
    param.list <- param.list[order(names(param.list))]
    param.type <- param.type[order(names(param.type))]
  }

  res.list <- lapply(1:length(param.list),FUN=function(i){
    if(length(param.list[[i]])==1){
      return(rep(param.list[[i]],n))
    } else {
      if(param.type[i]=="numeric"){
        return(runif(n,param.list[[i]][1],param.list[[i]][2]))
      } else if (param.type[i]=="integer"){
        return(sample(ceiling(param.list[[i]][1]):floor(param.list[[i]][2]),n,replace=TRUE))
      } else if (param.type[i]=="factor"){
        return(sample(param.list[[i]],n,replace=TRUE))
      } else {
        stop("Wrong param.type")
      }
    }
  })
  df <- as.data.frame(res.list,stringsAsFactors=FALSE)
  colnames(df) <- names(param.list)
  return(df)
}

#' @export
my.split.expressions <- function(tree, param.list, param.type){
  letter2num <- function(x) {utf8ToInt(x) - utf8ToInt("a") + 1L}

  restore.point("my.split.expressions")
  param.list <- param.list[order(names(param.list))]
  param.type <- param.type[order(names(param.type))]

  frame <- tree$frame
  node <- as.numeric(row.names(frame))
  frame$depth <- rpart:::tree.depth(node)
  z <- labels(tree)
  max.depth <- max(frame$depth)
  frame$working.depth <- frame$depth
  for(i in 1:length(param.list)){
    if(param.type[[i]]=="numeric"||param.type[[i]]=="integer"){
      if(length(param.list[[i]])==1){
        frame$tmp.min <- param.list[[i]]
        frame$tmp.max <- param.list[[i]]
        colnames(frame)[colnames(frame)=="tmp.min"] <- paste0(names(param.list)[i],".min",collapse="")
        colnames(frame)[colnames(frame)=="tmp.max"] <- paste0(names(param.list)[i],".max",collapse="")
      } else {
        frame$tmp.min <- param.list[[i]][1]
        frame$tmp.max <- param.list[[i]][2]
        colnames(frame)[colnames(frame)=="tmp.min"] <- paste0(names(param.list)[i],".min",collapse="")
        colnames(frame)[colnames(frame)=="tmp.max"] <- paste0(names(param.list)[i],".max",collapse="")
      }
    } else if(param.type[[i]]=="factor"){
      if(length(param.list[[i]])==1){
        frame$tmp.true <- TRUE
        colnames(frame)[colnames(frame)=="tmp.true"] <- paste0(names(param.list)[i],".",param.list[[i]],collapse="")
      } else {
        for(j in 1:length(param.list[[i]])){
          frame$tmp.true <- TRUE
          colnames(frame)[colnames(frame)=="tmp.true"] <- paste0(names(param.list)[i],".",param.list[[i]][j],collapse="")
        }
      }
    } else {
      stop("unknown param.type")
    }
  }

  while(max(frame$working.depth)>0){
    for (i in 1:length(z)){
      if(frame$working.depth[i]==0){
        next
      } else if (frame$working.depth[i]==1){
        current.exp <- z[i]
      } else {
        #do nothing
      }
      expr <- gsub("[[:space:]]", "", current.exp)
      split1 <- unlist(strsplit(expr,"[><=]"))
      var.name <- split1[1]
      if(param.type[[var.name]]=="integer"||param.type[[var.name]]=="numeric"){
        var.value.raw <- split1[length(split1)]
        var.value <- as.numeric(var.value.raw)
      } else if(param.type[[var.name]]=="factor"){
        var.value.raw <- split1[length(split1)]
        var.value <- param.list[[var.name]][letter2num(var.value.raw)]
      } else {
        stop("Wrong param.type")
      }

      split2 <- substr(expr,nchar(var.name)+1,nchar(expr)-nchar(var.value.raw))
      restore.point("my.split.expressions.before.split2")
      if(split2=="<" || split2=="<="){
        frame[i,paste0(var.name,".max",collapse="")] <- var.value
      } else if (split2==">" || split2==">="){
        frame[i,paste0(var.name,".min",collapse="")] <- var.value
      } else if(split2=="="){
        #Generate List of all candidates
        frame[i,startsWith(colnames(frame),paste0(c(var.name,"."),collapse=""))] <- FALSE
        for(j in 1:length(var.value)){
          frame[i,paste0(var.name,".",var.value[j],collapse="")] <- TRUE
        }
      } else {
        stop("Unknown split seperator")
      }
    }
    frame$working.depth[frame$working.depth>0] <- frame$working.depth[frame$working.depth>0]-1
  }
  res <- frame[frame$var=="<leaf>",]
  return(res)
}

#' @export
hyperpar.tree <- function(param.list,param.type, method, start.n=5,best.splits=2,add.n=5,remove.n=4, repeat.no=10,tree.param.cp=0.01,tree.param.minsplit=3,tree.param.minbucket=1,tree.param.xval=10,tree.param.maxdepth=30, sort=FALSE){
  restore.point("hyperpar.tree")

  #For Safeguard
  if(add.n<remove.n){
    remove.n <- add.n
  }

  grid <- expand.runif.grid(param.list,n=start.n, param.type=param.type, sort=sort)
  grid$y <- apply(grid,1,method)

  #Here we have chosen the parameters based on crossvalidation, but due to time constraints we will directly present the results. Feel free to experiment, though!
  # Grow tree
  for(i in 1:repeat.no){
    cat(paste0(i," of ",repeat.no,"\n"))
    mod.tree <- rpart(y ~ ., data=grid, control=rpart.control(cp=tree.param.cp,minsplit=tree.param.minsplit, minbucket=tree.param.minbucket,xval=tree.param.xval,maxdepth = tree.param.maxdepth),  method="anova")
    df.splits <- my.split.expressions(mod.tree, param.list, param.type)
    df.splits$order <- order(df.splits$yval)

    use.splits <- sample(1:min(best.splits,nrow(df.splits)),add.n,replace=TRUE)

    for(j in 1:length(use.splits)){
      param.list.int <- lapply(1:length(param.list),FUN=function(x){
        if(param.type[[x]]=="integer"||param.type[[x]]=="numeric"){
          min <- df.splits[df.splits$order==use.splits[j],paste0(names(param.list)[x],".min",collapse="")]
          max <- df.splits[df.splits$order==use.splits[j],paste0(names(param.list)[x],".max",collapse="")]
          res <- c(min,max)
        } else if(param.type[[x]]=="factor"){
          rel.cols <- startsWith(colnames(df.splits),paste0(c(names(param.list[x]),"."),collapse=""))
          rel.params <- df.splits[use.splits[j],rel.cols]
          names(rel.params) <- colnames(df.splits[rel.cols])
          param.names <- substr(names(rel.params),nchar(names(param.list[x]))+2,nchar(names(rel.params)))
          res <- param.names[unlist(rel.params)]
        } else {
          stop("Wrong param.type")
        }
      })
      names(param.list.int) <- names(param.list)
      grid.add <- expand.runif.grid(param.list.int,n=1, param.type=param.type, sort=sort)
      grid.add$y <- apply(grid.add,1,method)
      grid <- rbind(grid,grid.add)
    }

    #remove old data
    grid <- grid[order(grid$y,decreasing=FALSE)[1:(nrow(grid)-remove.n)],]
  }
  return(grid)
}



