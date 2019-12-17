#' @export
Start.Phi.Table <- function(feature.types){
  #restore.point("start.phi.table")

  return(lapply(1:length(feature.types),FUN=function(x){
    characteristics <- NULL
    count <- NULL
    type <- feature.types[x]
    res <- list(characteristics=characteristics,count=count,type=type)
    return(res)
  }))
}

#' @export
Update.Phi.Table.Memory <- function(phi.table, memory, phi.cont.digits){
  #restore.point("update.phi.table")
  if(is.null(memory)){ #no new memory
    return(phi.table)
  }
  for(i in 1:length(memory)){
    phi.table <- Update.Phi.Table(phi.table,phi=memory[[i]]$next.state,phi.cont.digits=phi.cont.digits)
  }
  return(phi.table)
}

#' @export
Update.Phi.Table <- function(phi.table,phi,phi.cont.digits){
  #restore.point("update.phi.table")
  lapply(1:length(phi.table),FUN=function(x){
    if(phi.table[[x]]$type=="contin"){
      my.phi <- round(phi[[x]],phi.cont.digits)
    } else {
      my.phi <- phi[[x]]
    }
    found <- which(phi.table[[x]]$characteristics %in% my.phi)
    if(length(found)==0){ # new feature
      phi.table[[x]]$characteristics <- c(phi.table[[x]]$characteristics, my.phi)
      phi.table[[x]]$count <- c(phi.table[[x]]$count,0)
      found <- length(phi.table[[x]]$count)
    }
    #Now found is on the correct place
    phi.table[[x]]$count[found] <- phi.table[[x]]$count[found]+1
    return(phi.table[[x]])
  })
}

#' @export
Calc.Rho <- function(phi.table,phi){
  restore.point("calc.rho")
  Kt <- function(phi.table.i,phi.i,t){
    #restore.point("Kt")
    pos <- which(phi.table.i$characteristics %in% phi.i)
    if(length(pos)==0){ #new feature
      val <- 0
    } else {
      val <- phi.table.i$count[pos]
    }
    return((val + 1/2)/(t+1))
  }

  rho.i <- sapply(1:length(phi.table),FUN=function(x){
    #restore.point("inside.rho.i")
    Kt(phi.table.i=phi.table[[x]],phi.i=phi[[x]],t=sum(phi.table[[x]]$count))
  })

  return(prod(rho.i))
}

#' deprecated
#' @export
Calc.R.phi <- function(phi.table, phi, beta, phi.cont.digits=1, N.type="naive"){
  rho <- Calc.Rho(phi.table=phi.table,phi=phi)
  if(N.type=="naive"){
    t <- sum(phi.table[[1]]$count)
    return(t*rho)
  } else {
    phi.table.next <- Update.Phi.Table(phi.table,phi,phi.cont.digits=phi.cont.digits)
    rho.next <- Calc.Rho(phi.table=phi.table.next,phi=phi)
    N.phi <- rho*(1-rho.next)/(rho.next-rho)
    R.phi <- beta/sqrt(N.phi)
    return(R.phi)
  }
}
