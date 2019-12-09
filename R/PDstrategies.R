##########
# Testpool and Counter Strategies
##########

#' A testpool strategy
#'
#' @export
strat.b = function(obs, i, t, game, defection.counter=0, ...) {

  if (t == 1){
    return(list(a = "C", defection.counter = defection.counter))
  } 

  j = 3 - i

  if (obs$a[j]=="C"){ #I observe cooperation of opponent
    return(list(a = "C", defection.counter = defection.counter))
  } else {
    defection.counter <- defection.counter + 1
    
    if (defection.counter == 3){
      defection.counter <- 1
      return (list(a = "D", defection.counter = defection.counter))
    }
    
    if (runif(1) < 0.15) {
      return(list(a = "C" , defection.counter = defection.counter))
    } else {
      return(list(a = "D", defection.counter = defection.counter))
    }
  }
}

#' A testpool strategy
#'
#' @export
strat.g = function(obs,i,t,game,reset.state=TRUE, consec.defections=0, ...) {

  if (t==1){
    return(list(a="C",reset.state=reset.state,consec.defections=0))
  }
  
  j <- 3-i
  a.opponent <- obs$a[j]
  
  if(a.opponent=="C"){
    return(list(a="C",reset.state=TRUE, consec.defections=0))
  }
  
  if(a.opponent=="D" && reset.state && consec.defections<5){
    return(list(a="C",reset.state=FALSE, consec.defections=consec.defections+1))
  }
  
  if(a.opponent=="D" && !reset.state && consec.defections<5){
    return(list(a="D",reset.state=FALSE, consec.defections=consec.defections+1))
  }

  return(list(a="C",reset.state=TRUE, consec.defections=0))
}

#' A best answer strategy
#'
#' Answerstrat to 'strat.g'
#'
#' @export
counter.strat.g = function(obs,i,t,game,my.consec.def=0, ...) {

  if (t==1){
    return(list(a="D", my.consec.def=my.consec.def))
  }
   
  a.me.obs <- obs$a[i] 

  if(a.me.obs=="D" && my.consec.def<5){
    my.consec.def <- my.consec.def+1
    return(list(a="C", my.consec.def=my.consec.def))
  }else{ #Only if I have extremely bad luck and my cooperations are wrongfully perceived 5 times
    return(list(a="D", my.consec.def=0))
  }
}

#' A testpool strategy
#'
#' @export
strat.d <- function(obs, i, t, game, Cooperation.Count=0, Consecutive.Cooperations=0, Consecutive.Defections=0, ...){

  if (t==1) {
    return(list(a="C", Cooperation.Count=Cooperation.Count, Consecutive.Cooperations=Consecutive.Cooperations, Consecutive.Defections=Consecutive.Defections))
  }
  
  #Update gathered information
  
  #Basic variables
  j=3-i
  a.opponent <- obs$a[j]
  
  #Update number of cooperations
  if (a.opponent == 'C'){
    Cooperation.Count <- Cooperation.Count + 1
  }
  
  # Update consecutive defects/cooperations
  if(a.opponent == 'D'){
    Consecutive.Defections <- Consecutive.Defections+1
    Consecutive.Cooperations <- 0
  } else {
    Consecutive.Defections <- 0
    Consecutive.Cooperations <- Consecutive.Cooperations+1
  }
  
  # if one of the first 5 periods always cooperate
  if (t <= 5) {
    return(list(a="C", Cooperation.Count=Cooperation.Count, Consecutive.Cooperations=Consecutive.Cooperations, Consecutive.Defections=Consecutive.Defections))
  }
  
  # Defect if there are at least 3 consecutive defections
  if(Consecutive.Defections >= 3){
    return(list(a="D", Cooperation.Count=Cooperation.Count, Consecutive.Cooperations=Consecutive.Cooperations, Consecutive.Defections=Consecutive.Defections))
  }
  
  # Cooperate if there are at least 2 consecutive cooperations
  if(Consecutive.Cooperations >= 2){
    return(list(a="C", Cooperation.Count=Cooperation.Count, Consecutive.Cooperations=Consecutive.Cooperations, Consecutive.Defections=Consecutive.Defections))
  }
  
  #is the number of observed cooperatons more than 50%?
  if (Cooperation.Count*2 > t-1) {
      return(list(a="C", Cooperation.Count=Cooperation.Count, Consecutive.Cooperations=Consecutive.Cooperations, Consecutive.Defections=Consecutive.Defections))
  } else {
    return(list(a="D", Cooperation.Count=Cooperation.Count, Consecutive.Cooperations=Consecutive.Cooperations, Consecutive.Defections=Consecutive.Defections))
  }
}

#' A best answer strategy
#'
#' Answerstrat to strat.d
#'
#' @export
counter.strat.d <- function(obs, i, t, game, my.obs.coop=0, my.obs.def=0, ...){
  #Cooperate first period
  if (t == 1) {
    return(list(a="C",my.obs.coop=my.obs.coop,my.obs.def=my.obs.def))
  }

  #Increase counter of either C or D
  if (obs$a[i] == "C"){
    my.obs.coop <- my.obs.coop + 1
  } else {
    my.obs.def <- my.obs.def + 1
  }

  if(my.obs.coop<=my.obs.def+2){
    return(list(a="C",my.obs.coop=my.obs.coop,my.obs.def=my.obs.def))
  } else {
    return(list(a="D",my.obs.coop=my.obs.coop,my.obs.def=my.obs.def))
  }
}

#' A testpool strategy
#
#' @export
strat.e = function(obs, i, t, game, total.opp.def=0, ...) {
  
  #Start friendly
  if (t == 1) {
    return(list(a = "C", total.opp.def = total.opp.def))
  }
  
  #Basic variables
  j=3-i
  a.opponent <- obs$a[j]
  
  #Count number of defections
  if (a.opponent == "D"){
    total.opp.def <- total.opp.def + 1
  }
  
  #Defect, if total number of defections >= 4
  if (total.opp.def >= 4){
    return(list(a = "D", total.opp.def = total.opp.def))
  } else {
    return(list(a = "C", total.opp.def = total.opp.def))
  }
}

#' A best answer strategy
#'
#' Answerstrat to strat.e
#'
#' @export
counter.strat.e = function(obs, i, t, game, total.me.def=0, ...) {
  #Start friendly
  if (t == 1) {
    return(list(a = "C", total.me.def = total.me.def))
  }
  
  #Basic variables
  a.me <- obs$a[i]
  
  #Count number of defections
  if (a.me == "D"){
    total.me.def <- total.me.def + 1
  }
  
  #Cooperate, if total number of defections <= 3
  if (total.me.def <= 3){
    return(list(a = "C", total.me.def = total.me.def))
  } else {
    return(list(a = "D", total.me.def = total.me.def))
  }
}

#' A testpool strategy
#'
#' @export
strat.f = function(obs, i, t, game, status="Skeptic", content.count=NA, ...) {
  content.duration <- 3
  
  if (t==1){
    return(list(a= "C", status=status, content.count=content.duration))
  }
  
  j <- 3-i
  a.opponent <- obs$a[j]

  if(status=="Skeptic"){
    if(a.opponent=="C"){
      status <- "Content"
    } else {
      return(list(a= "D", status=status, content.count=content.duration))
    }  
  }
    

  content.count <- content.count - 1
  if(status=="Content" && content.count==0){
    status <- "Skeptic"
    content.count <- content.duration
  }
  
  return(list(a= "C", status=status, content.count=content.count))
}

#' A best answer strategy
#'
#' Answer to strat.f
#' @export
counter.strat.f = function(obs, i, t, game, count=0, ...) {

  if (t == 1) {
    return(list(a = "C", count = 2))
  }
  a.me.obs <- obs$a[i]

  if (count == 0){
    count <- 2
    return(list(a = "C", count = count))
  }

  if (count == 1){
    return(list(a = "D", count = count-1))
  }
  
  if (count == 2){
    if(a.me.obs == "D"){
      return(list(a = "C", count = 2))
    } else {
      return(list(a = "D", count = count-1))
    }
  }
}

#' A testpool strategy
#'
#' @export
strat.i = function(obs,i,t,game,loss.conf=1,con.def=0,mistrust=0, ...) {

  j <- 3-i
  a.opp <- obs$a[j]
  
  if(t==1){
    return(list(a="C",loss.conf=loss.conf,con.def=con.def,mistrust=mistrust))
  }
  
  if(t==2){
    if(a.opp=="C"){
      return(list(a="C",loss.conf=loss.conf,con.def=con.def,mistrust=mistrust))
    } else{
      con.def <- 1
      mistrust <- 1
      return(list(a="D",loss.conf=loss.conf,con.def=con.def,mistrust=mistrust))
    }
  }
  
  #Period 3 and later
  if(a.opp=="C"){
    con.def <- 0
    mistrust <- mistrust-1
    if(mistrust>2){
      return(list(a="D",loss.conf=loss.conf,con.def=con.def,mistrust=mistrust))
    } else {
      return(list(a="C",loss.conf=loss.conf,con.def=con.def,mistrust=mistrust))
    }
  } else { #Opponent defected
    if(mistrust>3){
      con.def <- 0
      return(list(a="D",loss.conf=loss.conf,con.def=con.def,mistrust=mistrust))
    } else {
      if(con.def>2-loss.conf){
        loss.conf <- loss.conf+1
        con.def <- 0
        mistrust <- mistrust+2
        return(list(a="D",loss.conf=loss.conf,con.def=con.def,mistrust=mistrust))
      } else {
        con.def <- con.def+1
        return(list(a="C",loss.conf=loss.conf,con.def=con.def,mistrust=mistrust))
      }
    }
  }
}

#' A best answer strat
#'
#' Answerstrat to strat.i
#'
#' @export
counter.strat.i <- function(obs,i,t,game,...) {
  
  if (t==1){
    return(list(a="C"))
  }
  
  a.obs.me <- obs$a[i]
  
  if (a.obs.me == "D"){
    return(list(a="C"))
  }else{
    return(list(a="D"))
  }
  
}

#' A testpool strategy
#'
#' @export
strat.h = function(obs,i,t,game, opp.consec.def=0, opp.total.def=0,...) {

  if (t==1){
    return(list(a="C", opp.consec.def=opp.consec.def, opp.total.def=opp.total.def))
  }
  
  j <- 3-i
  a.opp <- obs$a[j]
  
  if(a.opp=="C"){
    opp.consec.def <- 0
  } else {
    opp.consec.def <- opp.consec.def+1
    opp.total.def <- opp.total.def+1
  } 
  
  if(opp.consec.def>=3||opp.total.def>=10){
    return(list(a="D", opp.consec.def=opp.consec.def, opp.total.def=opp.total.def))
  } else {
    return(list(a="C", opp.consec.def=opp.consec.def, opp.total.def=opp.total.def))
  }
}

#' A best answer strategy
#'
#' Answer to strat.h
#' @export
counter.strat.h <- function(obs,i,t, me.total.def=0,...){
  if (t==1){
    return(list(a="D", me.total.def=me.total.def))
  }
  
  j <- 3-i
  a.me <- obs$a[i]
  
  if(a.me=="D"){
    me.total.def <- me.total.def+1
  }
  
  if(me.total.def>=10){
    return(list(a="D", me.total.def=me.total.def))
  }
  
  if(a.me=="D"||(me.total.def>=5&&me.total.def<10)){
    return(list(a="C", me.total.def=me.total.def))
  }
  
  return(list(a="D", me.total.def=me.total.def))
}

#' A testpool strategy
#'
#' @export
strat.c <- function(obs,i,t,game,P_i=0.5,P_a=0.5, threshold=0.25, Fratio=0.4, ...){
  #Fratio: Familiarity Ratio -> 1 means "nice guy" 0 means "Anti nice guy"

  j=3-i
  P_D <- game.object$game.pars$err.D.prob
  P_C <- game.object$game.pars$err.C.prob


  #Update beliefs
  if(t==1){
    #do nothing
  } else {
    #Update of P_a
    P_a_old <- P_a
    #The other guy would assume, that I am me, given what he knows about me, given that he is me
    if(P_i>=threshold){
      if(obs$a[j] == "C"){
        P_a <- P_a*(1-P_D)/(P_a*(1-P_D)+(1-P_a)*(P_C+Fratio*(1-P_D-P_C)))
      } else { # Case Defect
        P_a <- P_a*P_D/(P_a*(P_D)+(1-P_a)*(P_D + (1-Fratio)*(1-P_D-P_C)))
      }
    } else { # other guy thinks I am evil
      if(obs$a[j] == "C"){
        P_a <- P_a*P_C/(P_a*P_C + (1-P_a)*(P_C + (1-Fratio)*(1-P_D-P_C)))
      } else {
        P_a <- P_a*(1-P_C)/(P_a*(1-P_C)+(1-P_a)*(P_D + Fratio*(1-P_D-P_C)))
      }
    }
    #Update my view of myself based on his knowledge
    if(P_a_old>=threshold){
      if(obs$a[i] == "C"){
        P_i <- P_i*(1-P_D)/(P_i*(1-P_D)+(1-P_i)*(P_C+Fratio*(1-P_D-P_C)))
      } else { # Case Defect
        P_i <- P_i*P_D/(P_i*(P_D)+(1-P_i)*(P_D + (1-Fratio)*(1-P_D-P_C)))
      }
    } else { # other guy thinks I am evil so he should play D
      if(obs$a[i] == "C"){
        P_i <- P_i*P_C/(P_i*P_C + (1-P_i)*(P_C + (1-Fratio)*(1-P_D-P_C)))
      } else {
        P_i <- P_i*(1-P_C)/(P_i*(1-P_C)+(1-P_i)*(P_D + Fratio*(1-P_D-P_C)))
      }
    }
  }
  #If I think he is me, than cooperate
  if(P_a>=threshold){
    return(list(a="C",P_i=P_i,P_a=P_a, threshold=threshold, Fratio=Fratio))
  } else {
    return(list(a="D",P_i=P_i,P_a=P_a, threshold=threshold, Fratio=Fratio))
  }
}

#' A bast answer to a testpool strategy
#'
#' Answerstrat to 'strat.c'
#'
#' @export
counter.strat.c <- function(obs,i,t,game,P_i=0.5,P_a=0.5, threshold=0.25, Fratio=0.4, ...){
  #Fratio: Familiarity Ratio -> 1 means "nice guy" 0 means "Anti nice guy"

  j=3-i
  P_D <- game.object$game.pars$err.D.prob
  P_C <- game.object$game.pars$err.C.prob


  #Update beliefs
  if(t==1){
    #do nothing
  } else {
    #Update of P_a
    P_a_old <- P_a
    #The other guy would assume, that I am me, given what he knows about me, given that he is me
    if(P_i>=threshold){
      if(obs$a[j] == "C"){
        P_a <- P_a*(1-P_D)/(P_a*(1-P_D)+(1-P_a)*(P_C+Fratio*(1-P_D-P_C)))
      } else { # Case Defect
        P_a <- P_a*P_D/(P_a*(P_D)+(1-P_a)*(P_D + (1-Fratio)*(1-P_D-P_C)))
      }
    } else { # other guy thinks I am an opponent strategy
      if(obs$a[j] == "C"){
        P_a <- P_a*P_C/(P_a*P_C + (1-P_a)*(P_C + (1-Fratio)*(1-P_D-P_C)))
      } else {
        P_a <- P_a*(1-P_C)/(P_a*(1-P_C)+(1-P_a)*(P_D + Fratio*(1-P_D-P_C)))
      }
    }
    #Update my view of myself based on his knowledge
    if(P_a_old>=threshold){
      if(obs$a[i] == "C"){
        P_i <- P_i*(1-P_D)/(P_i*(1-P_D)+(1-P_i)*(P_C+Fratio*(1-P_D-P_C)))
      } else { # Case Defect
        P_i <- P_i*P_D/(P_i*(P_D)+(1-P_i)*(P_D + (1-Fratio)*(1-P_D-P_C)))
      }
    } else { # other guy thinks I am an opponent strategy
      if(obs$a[i] == "C"){
        P_i <- P_i*P_C/(P_i*P_C + (1-P_i)*(P_C + (1-Fratio)*(1-P_D-P_C)))
      } else {
        P_i <- P_i*(1-P_C)/(P_i*(1-P_C)+(1-P_i)*(P_D + Fratio*(1-P_D-P_C)))
      }
    }
  }
  #Up until this point we updated analogously to strat.c
  if(P_a > 0.9999999){
    P_a <- 0.99 #for numerical purposes to avoid float problems
  }
  if(P_i >= 0.85){
    return(list(a="D",P_i=P_i,P_a=P_a, threshold=threshold, Fratio=Fratio))
  } else if (P_a >= threshold){
    return(list(a="C",P_i=P_i,P_a=P_a, threshold=threshold, Fratio=Fratio))
  } else {
    return(list(a="D",P_i=P_i,P_a=P_a, threshold=threshold, Fratio=Fratio))
  }
}

#' A testpool strategy
#'
#' @export
strat.a <-function (obs,i,t,game,CooperationDiff=0, ...) {
  if (t==1){
    return(list(a="C",CooperationDiff=CooperationDiff))
  } else {
    
    j=3-i
    if (obs$a[j]=="C"){
      CooperationDiff <- CooperationDiff+1
    } else {
      CooperationDiff <- CooperationDiff-1
    }
    
    if (CooperationDiff >= 0){
      return(list(a="C",CooperationDiff=CooperationDiff))
    } else {
      return(list(a="D",CooperationDiff=CooperationDiff))
    }
    
  }
}

#' A best answer strategy
#'
#' Answerstrat to 'strat.a'
#'
#' @export
counter.strat.a <- function (obs,i,t,game,CooperationDiff=0, ...) {
  if (t==1){
    return(list(a="C",CooperationDiff=CooperationDiff))
  }
  if (obs$a[i] =="C"){
    CooperationDiff <- CooperationDiff + 1
  } else {
    CooperationDiff <- CooperationDiff -1
  }
  if (CooperationDiff >= 2){
    return(list(a="D",CooperationDiff=CooperationDiff))
  } else {
    return(list(a="C",CooperationDiff=CooperationDiff))
  }
}

###########
# Strategies to highlight effects
###########

#' Strategy highlighting differences in Exploration
#' states are "no.info", "is.nice" [after first 10 rounds], "is.mean" [if defect in first 10 rounds]
#' @export
compare.exploration <- function(obs, i, t, game, state="no.info", ...) {

  ignore.defect <- c(11,15,17,23,26,28,30,34,37,38,40,51,53,56,58)

  if (t == 1)
    return(list(a = "C", state = "is.nice"))

  j = 3 - i
  observ=obs$a[j]
  my.action <- NULL

  if(t<=10 && observ=="D"){
    state="is.mean"
  }

  #Opponent always played nice in the first 10 rounds
  if(state=="is.nice" && t %in% ignore.defect){
    my.action <- "C"
  }

  if(state=="is.nice" && !(t %in% ignore.defect)){
    my.action <- observ #Tit for tat outside of special rounds
  }

  if(state=="is.mean"){
    my.action <- "D"
  }

  return(list(a = my.action, state = state))
}

############
# Established strategies and counterparts
############

#' @export
grim.trigger <- function(obs, i, t, game, state="not.triggered", ...) {

  if (t == 1)
    return(list(a = "C", state = "not.triggered"))

  j = 3 - i
  observ=obs$a[j]
  my.action <- NULL

  if(observ=="D"){
    state="triggered"
  }

  if(state=="triggered"){
    my.action <- "D"
  } else {
    my.action <- "C"
  }

  return(list(a = my.action, state = state))
}

#' @export
counter.grim.trigger <- function(obs, i, t, game, state="not.triggered", ...) {
  debug.store("counter.grim.trigger",i,t)
  debug.restore("counter.grim.trigger",i=1,t=7)

  if (t == 1)
    return(list(a = "C", state = "not.triggered"))

  j = 3 - i
  observ.other =obs$a[j]
  observ.me <- obs$a[i]

  if(observ.me=="D"){
    state="triggered"
  }

  if(state=="triggered"){
    my.action <- "D"
  } else {
    my.action <- "C"
  }

  return(list(a = my.action, state = state))
}

#' @export
generous.tit.for.tat <- function(obs, i, t, game, c=0.1, ...) {
  if (t == 1)
    return(list(a = "C", c = c))

  j = 3 - i
  observ=obs$a[j]
  my.action <- NULL

  if(runif(1)<c){
    return(list(a = "C", c=c))
  } else {
    return(list(a = observ, c=c))
  }
}

#' @export
contrite.tit.for.tat.Axelrod <- function(obs, i, t, game, contrite=FALSE, hist.want=c(), hist.obs=c(),...) {
  debug.store("contrite.tit.for.tat",i,t)
  debug.restore("contrite.tit.for.tat",i=1,t=7)

  if (t == 1){
    hist.want[t] = "C"
    return(nlist(a = "C", contrite, hist.want, hist.obs))
  }

  j = 3 - i
  observ <- obs$a[j]
  my.action <- obs$a[i]
  hist.obs[t-1] <- my.action


  if(contrite && my.action=="C"){ #Apology successful
    contrite <- FALSE
    hist.want[t] <- "C"
    return(nlist(a = "C", contrite, hist.want, hist.obs))
  }

  if(hist.want[t-1]!=hist.obs[t-1]){ #Noise
    if(my.action=="D" && observ == "C"){ #bad noise - i want to apologise
      contrite <- TRUE
    }
  }

  #Play Tit.for.Tat
  hist.want[t] <- observ
  return(nlist(a =  observ, contrite, hist.want, hist.obs))
}

#' @export
contrite.tit.for.tat <- function(obs, i, t, game, status="content",...) {
  debug.store("contrite.tit.for.tat.alternative",i,t)
  debug.restore("contrite.tit.for.tat.alternative",i=1,t=7)

  old.status <- status

  if (t == 1){
    return(nlist(a = "C", status))
  }

  j = 3 - i
  his.action <- obs$a[j]
  my.action <- obs$a[i]

  if(his.action=="D" && my.action=="C"){
    if(old.status=="content"){
      status <- "provoked"
    }
  }

  if(his.action=="C"){
    if(old.status=="provoked"){
      status <- "content"
    }
  }

  if(his.action=="C"&&my.action=="D"){
    if(old.status=="content"){
      status <- "contrite"
    }
  }

  if(my.action=="C"){
    if(old.status=="contrite"){
      status <- "content"
    }
  }

  if(status=="content"||status=="contrite"){
    return(nlist(a = "C", status))
  } else { #status is provoked
    return(nlist(a = "D", status))
  }
}

#' @export
counter.contrite.tit.for.tat <- function(obs, i, t, game, status="content", ...) {
  debug.store("contrite.tit.for.tat.alternative",i,t)
  debug.restore("contrite.tit.for.tat.alternative",i=1,t=7)

  old.status <- status

  if (t == 1){
    return(nlist(a = "C", status))
  }

  j = 3 - i
  his.action <- obs$a[j]
  my.action <- obs$a[i]

  if(my.action=="D" && his.action=="C"){
    if(old.status=="content"){
      status <- "provoked"
    }
  }

  if(my.action=="C"){
    if(old.status=="provoked"){
      status <- "content"
    }
  }

  if(my.action=="C"&&his.action=="D"){
    if(old.status=="content"){
      status <- "contrite"
    }
  }

  if(his.action=="C"){
    if(old.status=="contrite"){
      status <- "content"
    }
  }

  if(status=="content"||status=="provoked"){
    return(nlist(a = "C", status))
  } else { #status is contrite
    return(nlist(a = "D", status))
  }
}



#' @export
pavlov <- function(obs, i, t, game, ...) {
  if (t == 1)
    return(list(a = "C"))

  j = 3 - i
  observ <- obs$a[j]
  my.action <- obs$a[i]

  if(observ=="C" && my.action == "C") return(list(a = "C"))
  if(observ=="C" && my.action == "D") return(list(a = "D"))
  if(observ=="D" && my.action == "C") return(list(a = "D"))
  if(observ=="D" && my.action == "D") return(list(a = "C"))
}

## == Net.nice & variants == ##

#' A variant to net.nice0
#'
#' @export
net.nice1 = function(obs,i,t, net.nice=0,k=1, ...) {
  if (t==1) {
    return(nlist(a="C",net.nice,k))
  }
  a = obs$a
  j = 3-i
  a.num = ifelse(a=="C",1,0)
  net.nice = net.nice + a.num[i]-a.num[j]
  if (net.nice <= k) {
    return(nlist(a="C",net.nice,k))
  }
  return(nlist(a="D",net.nice,k))
}

#' A prof strategy
#'
#' @export
net.nice0 = function(obs,i,t,net.nice=0,k=0,...) {
  if (t==1)
    return(nlist(a="C",net.nice, k=k))
  
  # Wie oft habe ich haeufiger C gespielt als mein Partner
  net.nice = net.nice + (obs$a[i]=="C") - (obs$a[[3-i]]=="C")
  if (net.nice <= k + ( runif(1) <(k-floor(k)) ) ) {
    return(nlist(a="C",net.nice, k=k))
  } else {
    return(nlist(a="D",net.nice, k=k))
  }
}


#' A variant to net.nice0
#'
#' @export
net.nice.minus1 = function(obs,i,t,net.nice=0,k=-1,...) {
  if (t==1)
    return(nlist(a="C",net.nice, k=k))
  
  net.nice = net.nice + (obs$a[i]=="C") - (obs$a[[3-i]]=="C")
  if (net.nice <= k + ( runif(1) <(k-floor(k)) ) ) {
    return(nlist(a="C",net.nice, k=k))
  } else {
    return(nlist(a="D",net.nice, k=k))
  }
}

#' A variant to net.nice0
#'
#' @export
net.nice.start1 = function(obs,i,t,net.nice=1,k=0,...) {
  if (t==1)
    return(nlist(a="C",net.nice, k=k))
  
  net.nice = net.nice + (obs$a[i]=="C") - (obs$a[[3-i]]=="C")
  if (net.nice <= k + ( runif(1) <(k-floor(k)) ) ) {
    return(nlist(a="C",net.nice, k=k))
  } else {
    return(nlist(a="D",net.nice, k=k))
  }
}