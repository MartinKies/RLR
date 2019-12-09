#' Default Parameters QLearningPersExpPath v.0.1.6
Get.Def.Par.QLearningPersExpPath.Legacy.v.0.1.6 <- function(){
  
  action.policy <- "exploration.path"
  
  #Relevant parameters of epsilon.greedy
  epsilon.start <- 1
  epsilon.decay <- 0.95
  epsilon.min <- 0.02
  epsilon.decay.type <- "exponential" 
  
  
  #Relevant parameters of 'exploration.path'
  ## Shaping the Path itself
  expl.path.var.start <- 0.02
  expl.path.var.end <- 0.98
  expl.path.var.decay.type <- "exponential" 
  expl.path.var.momentum <- 1.1
  expl.path.var.start.var <- 1
  expl.path.var.data.base <- 5 
  ##unconventional shock
  expl.path.shock.start <- 0.02
  expl.path.shock.end <- 0.98
  expl.path.shock.decay.type <- "exponential"
  expl.path.shock.momentum <- 1.1
  expl.path.shock.start.shock <- 1
  expl.path.shock.data.base <- 5
  ##Seeking Surprise
  expl.path.surp.start <- 0
  expl.path.surp.end <- 0.98
  expl.path.surp.decay.type <- "exponential"
  expl.path.surp.momentum <- 1.1
  expl.path.surp.start.surp <- 1
  expl.path.surp.data.base <- 10
  ##Seeking Unfamiliarity
  expl.path.fam.start <- 0
  expl.path.fam.end <- 0.98
  expl.path.fam.decay.type <- "exponential"
  expl.path.fam.momentum <- 1.1
  expl.path.fam.start.fam <- 1
  expl.path.fam.data.base <- 10 
  ##shock and var combo
  expl.path.vs.start <- 0.02
  expl.path.vs.end <- 0.98
  expl.path.vs.decay.type <- "exponential"
  expl.path.vs.momentum <- 1.1
  expl.path.vs.strength <- 2
  expl.path.vs.start.var <- 1
  expl.path.vs.start.shock <- 1
  expl.path.vs.data.base <- 5 
  expl.path.vs.start.shock.frac <- 0.2
  expl.path.vs.min.shock.frac <- 0.05
  #Multi
  expl.path.multi.start <- 0.02
  expl.path.multi.end <- 0.98
  expl.path.multi.decay.type <- "exponential" 
  expl.path.multi.best.db <- Inf
  expl.path.multi.best.disc <- 0.9
  expl.path.multi.Kp.var <- 0.2          
  expl.path.multi.Ki.var <- 0.1            
  expl.path.multi.Kd.var <- 0.1          
  expl.path.multi.Kp.shock <- 0.2          
  expl.path.multi.Ki.shock <- 0.1      
  expl.path.multi.Kd.shock <- 0.1    
  expl.path.multi.Kp.surp <- 0.2     
  expl.path.multi.Ki.surp <- 0.1     
  expl.path.multi.Kd.surp <- 0.1     
  expl.path.multi.Kp.fam <- 0.2      
  expl.path.multi.Ki.fam <- 0.1      
  expl.path.multi.Kd.fam <- 0.1      
  expl.path.multi.Kp.db <- Inf           
  expl.path.multi.Ki.db <- Inf          
  expl.path.multi.Kd.db <- Inf          
  expl.path.multi.Kp.disc <- 0.9        
  expl.path.multi.Ki.disc <- 0.99       
  expl.path.multi.Kd.disc <- 0.9 
  expl.path.multi.start.var <- 1
  expl.path.multi.start.shock <- 1
  expl.path.multi.start.surp <- 1
  expl.path.multi.start.fam <- 1
  expl.path.multi.start.frac.var <- 0.25
  expl.path.multi.start.frac.shock <- 0.25
  expl.path.multi.start.frac.surp <- 0.25
  expl.path.multi.start.frac.fam <- 0.25
  expl.path.multi.end.frac.var <- 0.25
  expl.path.multi.end.frac.shock <- 0.25
  expl.path.multi.end.frac.surp <- 0.25
  expl.path.multi.end.frac.fam <- 0.25
  
  #Potential additional Curiosity factor to a path
  curio.beta <- 0.0
  phi.cont.digits <- 2
  N.type <- "naive"
  curio.decay.memory <- 0.95
  curio.cap <- 5
  
  #Relevant parameters
  #Memory
  batch.size <- 1000
  force.last <- 0
  max.mem <- 10000
  remove.memory <- 0.1 
  mem.type <- "game.encoded" 
  mem.selection <- "all"
  mem.init.epochs <- 100
  mem.expected.improve <- 0.01
  
  # Incorporating new information and Updating Status
  gamma <- 0.95
  a <- 0.1
  replay.intensive <- 1
  block.best <- 5
  block.curr <- 5
  block.expl.var <- 30
  block.expl.shock <- 30
  block.expl.surp <- 30
  block.expl.fam <- 30
  block.expl.vs <- 30
  block.expl.multi <- 30
  
  #Fancy Arguments
  hybrid.Q <- TRUE
  hybrid.Q.a.MC <- 0.2
  hybrid.Q.weighting <- "linear"
  hybrid.Q.apply <- "always" 
  use.rnn <- FALSE 
  only.experienced <- TRUE
  hybrid.switch <- TRUE
  hybrid.decay <- 0.95
  Q.Learning <- FALSE
  MC <- FALSE
  
  # Output
  show.current.status <- 1
  
  q.param <- nlist(action.policy, epsilon.start, epsilon.decay, epsilon.min, epsilon.decay.type, expl.path.var.start, expl.path.var.end, expl.path.var.decay.type, expl.path.var.momentum, expl.path.var.start.var,expl.path.var.data.base, expl.path.shock.start, expl.path.shock.end, expl.path.shock.decay.type, expl.path.shock.momentum, expl.path.shock.start.shock,expl.path.shock.data.base,expl.path.surp.start, expl.path.surp.end, expl.path.surp.decay.type, expl.path.surp.momentum, expl.path.surp.start.surp, expl.path.surp.data.base, expl.path.fam.start, expl.path.fam.end, expl.path.fam.decay.type, expl.path.fam.momentum, expl.path.fam.start.fam, expl.path.fam.data.base, expl.path.vs.start, expl.path.vs.end, expl.path.vs.decay.type, expl.path.vs.momentum,expl.path.vs.strength, expl.path.vs.start.var, expl.path.vs.start.shock, expl.path.vs.data.base, expl.path.vs.start.shock.frac, expl.path.vs.min.shock.frac,expl.path.multi.start,expl.path.multi.end, expl.path.multi.decay.type,expl.path.multi.best.db, expl.path.multi.best.disc, expl.path.multi.Kp.var, expl.path.multi.Ki.var, expl.path.multi.Kd.var,expl.path.multi.Kp.shock, expl.path.multi.Ki.shock, expl.path.multi.Kd.shock,expl.path.multi.Kp.surp, expl.path.multi.Ki.surp, expl.path.multi.Kd.surp,expl.path.multi.Kp.fam, expl.path.multi.Ki.fam, expl.path.multi.Kd.fam, expl.path.multi.Kp.db, expl.path.multi.Ki.db, expl.path.multi.Kd.db, expl.path.multi.Kp.disc, expl.path.multi.Ki.disc, expl.path.multi.Kd.disc, expl.path.multi.start.var, expl.path.multi.start.shock, expl.path.multi.start.surp, expl.path.multi.start.fam, expl.path.multi.start.frac.var, expl.path.multi.start.frac.shock, expl.path.multi.start.frac.surp, expl.path.multi.start.frac.fam, expl.path.multi.end.frac.var, expl.path.multi.end.frac.shock, expl.path.multi.end.frac.surp, expl.path.multi.end.frac.fam, curio.beta,phi.cont.digits,N.type,curio.decay.memory,curio.cap,batch.size, max.mem, remove.memory, mem.type, mem.selection, mem.init.epochs, mem.expected.improve, gamma, a, replay.intensive, force.last, hybrid.Q, hybrid.Q.a.MC, hybrid.Q.weighting, hybrid.Q.apply, use.rnn, only.experienced, hybrid.switch, hybrid.decay, Q.Learning, MC, block.expl.var, block.expl.shock, block.expl.surp, block.expl.fam, block.expl.vs, block.curr, block.best, block.expl.multi)
  
  return(q.param)
}

#' Default Parameters QLearningPersExpPath of the thesis of Martin Kies
Get.Def.Par.QLearningPersExpPath.Legacy.ThesisOpt.XGB <- function(){
  action.policy <- "exploration.path"
  
  #Relevant parameters of epsilon.greedy
  epsilon.start <- 1
  epsilon.decay <- 0.9
  epsilon.min <- 0
  epsilon.decay.type <- "exponential" 
  
  
  #Relevant parameters of 'exploration.path'
  ## Shaping the Path itself
  expl.path.var.start <- 0.85
  expl.path.var.end <- 0.85
  expl.path.var.decay.type <- "exponential" 
  expl.path.var.momentum <- 1.1
  expl.path.var.start.var <- 0.1
  expl.path.var.data.base <- 5 
  ##unconventional shock
  expl.path.shock.start <- 0.85
  expl.path.shock.end <- 0.85
  expl.path.shock.decay.type <- "exponential"
  expl.path.shock.momentum <- 1.1
  expl.path.shock.start.shock <- 1
  expl.path.shock.data.base <- 5
  ##Seeking Surprise
  expl.path.surp.start <- 0.85
  expl.path.surp.end <- 0.85
  expl.path.surp.decay.type <- "exponential"
  expl.path.surp.momentum <- 1.1
  expl.path.surp.start.surp <- 1
  expl.path.surp.data.base <- 10
  ##Seeking Unfamiliarity
  expl.path.fam.start <- 0.85
  expl.path.fam.end <- 0.85
  expl.path.fam.decay.type <- "exponential"
  expl.path.fam.momentum <- 1.1
  expl.path.fam.start.fam <- 1
  expl.path.fam.data.base <- 10 
  ##shock and var combo
  expl.path.vs.start <- 0.85
  expl.path.vs.end <- 0.85
  expl.path.vs.decay.type <- "exponential"
  expl.path.vs.momentum <- 1.1
  expl.path.vs.strength <- 2
  expl.path.vs.start.var <- 1
  expl.path.vs.start.shock <- 1
  expl.path.vs.data.base <- 5 
  expl.path.vs.start.shock.frac <- 0.2
  expl.path.vs.min.shock.frac <- 0.01
  #Multi
  expl.path.multi.start <- 0.85
  expl.path.multi.end <- 0.85
  expl.path.multi.decay.type <- "linear" 
  expl.path.multi.best.db <- 100
  expl.path.multi.best.disc <- 0.98
  expl.path.multi.Kp.var <- 0.005          
  expl.path.multi.Ki.var <- 0.00001            
  expl.path.multi.Kd.var <- 0.000005          
  expl.path.multi.Kp.shock <- 0.005          
  expl.path.multi.Ki.shock <- 0.00001      
  expl.path.multi.Kd.shock <- 0.000005    
  expl.path.multi.Kp.surp <- 0.5     
  expl.path.multi.Ki.surp <- 0.2     
  expl.path.multi.Kd.surp <- 0.05     
  expl.path.multi.Kp.fam <- 0.5      
  expl.path.multi.Ki.fam <- 0.02      
  expl.path.multi.Kd.fam <- 0.01      
  expl.path.multi.Kp.db <- 5           
  expl.path.multi.Ki.db <- 100          
  expl.path.multi.Kd.db <- 5          
  expl.path.multi.Kp.disc <- 0.95        
  expl.path.multi.Ki.disc <- 0.99       
  expl.path.multi.Kd.disc <- 0.95 
  
  expl.path.multi.start.var <- 0.1
  expl.path.multi.start.shock <- 0.1
  expl.path.multi.start.surp <- 0
  expl.path.multi.start.fam <- 0
  expl.path.multi.start.frac.var <- 0.5
  expl.path.multi.start.frac.shock <- 0.5
  expl.path.multi.start.frac.surp <- 0
  expl.path.multi.start.frac.fam <- 0
  expl.path.multi.end.frac.var <- 0.5
  expl.path.multi.end.frac.shock <- 0.5
  expl.path.multi.end.frac.surp <- 0
  expl.path.multi.end.frac.fam <- 0
  
  #Potential additional Curiosity factor to a path
  curio.beta <- 0
  phi.cont.digits <- 2 #not used due to curio.beta = 0
  N.type <- "naive" #not used due to curio.beta = 0
  curio.decay.memory <- 0.95 #not used due to curio.beta = 0
  curio.cap <- 5 #not used due to curio.beta = 0
  
  #Relevant parameters
  #Memory
  batch.size <- 4*60*50 #4 (episodes per block) * 60 (periods per episode) * 50 (number of to be included blocks)
  force.last <- 4*60*1 #4 (episodes per block) * 60 (periods per episode) * 1 (number of to be included blocks)
  max.mem <- 4*60*100 #4 (episodes per block) * 60 (periods per episode) * 100 (number of to be included blocks)
  remove.memory <- 0.1 
  mem.type <- "game.encoded" 
  mem.selection <- "all"
  mem.init.epochs <- 100
  mem.expected.improve <- 0.01
  
  # Incorporating new information and Updating Status
  gamma <- 0.95
  a <- 0.25
  replay.intensive <- 1
  block.best <- 1 
  block.curr <- 1 
  block.expl.var <- 0
  block.expl.shock <- 0
  block.expl.surp <- 0
  block.expl.fam <- 0
  block.expl.vs <- 0
  block.expl.multi <- 2
  
  #Fancy Arguments
  hybrid.Q <- TRUE
  hybrid.Q.a.MC <- 0.25
  hybrid.Q.weighting <- "linear"
  hybrid.Q.apply <- "always" 
  use.rnn <- FALSE 
  only.experienced <- TRUE
  hybrid.switch <- TRUE
  hybrid.decay <- 0.9
  Q.Learning <- FALSE
  MC <- FALSE
  
  # Output
  show.current.status <- 1
  
  q.param <- nlist(action.policy, epsilon.start, epsilon.decay, epsilon.min, epsilon.decay.type, expl.path.var.start, expl.path.var.end, expl.path.var.decay.type, expl.path.var.momentum, expl.path.var.start.var,expl.path.var.data.base, expl.path.shock.start, expl.path.shock.end, expl.path.shock.decay.type, expl.path.shock.momentum, expl.path.shock.start.shock,expl.path.shock.data.base,expl.path.surp.start, expl.path.surp.end, expl.path.surp.decay.type, expl.path.surp.momentum, expl.path.surp.start.surp, expl.path.surp.data.base, expl.path.fam.start, expl.path.fam.end, expl.path.fam.decay.type, expl.path.fam.momentum, expl.path.fam.start.fam, expl.path.fam.data.base, expl.path.vs.start, expl.path.vs.end, expl.path.vs.decay.type, expl.path.vs.momentum,expl.path.vs.strength, expl.path.vs.start.var, expl.path.vs.start.shock, expl.path.vs.data.base, expl.path.vs.start.shock.frac, expl.path.vs.min.shock.frac,expl.path.multi.start,expl.path.multi.end, expl.path.multi.decay.type,expl.path.multi.best.db, expl.path.multi.best.disc, expl.path.multi.Kp.var, expl.path.multi.Ki.var, expl.path.multi.Kd.var,expl.path.multi.Kp.shock, expl.path.multi.Ki.shock, expl.path.multi.Kd.shock,expl.path.multi.Kp.surp, expl.path.multi.Ki.surp, expl.path.multi.Kd.surp,expl.path.multi.Kp.fam, expl.path.multi.Ki.fam, expl.path.multi.Kd.fam, expl.path.multi.Kp.db, expl.path.multi.Ki.db, expl.path.multi.Kd.db, expl.path.multi.Kp.disc, expl.path.multi.Ki.disc, expl.path.multi.Kd.disc, expl.path.multi.start.var, expl.path.multi.start.shock, expl.path.multi.start.surp, expl.path.multi.start.fam, expl.path.multi.start.frac.var, expl.path.multi.start.frac.shock, expl.path.multi.start.frac.surp, expl.path.multi.start.frac.fam, expl.path.multi.end.frac.var, expl.path.multi.end.frac.shock, expl.path.multi.end.frac.surp, expl.path.multi.end.frac.fam, curio.beta,phi.cont.digits,N.type,curio.decay.memory,curio.cap,batch.size, max.mem, remove.memory, mem.type, mem.selection, mem.init.epochs, mem.expected.improve, gamma, a, replay.intensive, force.last, hybrid.Q, hybrid.Q.a.MC, hybrid.Q.weighting, hybrid.Q.apply, use.rnn, only.experienced, hybrid.switch, hybrid.decay, Q.Learning, MC, block.expl.var, block.expl.shock, block.expl.surp, block.expl.fam, block.expl.vs, block.curr, block.best, block.expl.multi)
  
  return(q.param)
}

Get.Def.Par.QLearningPersExpPath.Legacy.ThesisOpt.RNN <- function(){
  action.policy <- "exploration.path"
  
  #Relevant parameters of epsilon.greedy (not used)
  epsilon.start <- 1
  epsilon.decay <- 0.9
  epsilon.min <- 0
  epsilon.decay.type <- "exponential" 
  
  
  #Relevant parameters of 'exploration.path'
  ## Shaping the Path itself (not used)
  expl.path.var.start <- 0.85
  expl.path.var.end <- 0.95
  expl.path.var.decay.type <- "linear" 
  expl.path.var.momentum <- 1.1
  expl.path.var.start.var <- 0.1
  expl.path.var.data.base <- 5 
  ##unconventional shock (not used)
  expl.path.shock.start <- 0.85
  expl.path.shock.end <- 0.95
  expl.path.shock.decay.type <- "linear"
  expl.path.shock.momentum <- 1.1
  expl.path.shock.start.shock <- 1
  expl.path.shock.data.base <- 5
  ##Seeking Surprise (not used)
  expl.path.surp.start <- 0.85
  expl.path.surp.end <- 0.85
  expl.path.surp.decay.type <- "linear"
  expl.path.surp.momentum <- 1.1
  expl.path.surp.start.surp <- 1
  expl.path.surp.data.base <- 10
  ##Seeking Unfamiliarity (not used)
  expl.path.fam.start <- 0.85
  expl.path.fam.end <- 0.95
  expl.path.fam.decay.type <- "linear"
  expl.path.fam.momentum <- 1.1
  expl.path.fam.start.fam <- 1
  expl.path.fam.data.base <- 10 
  ##shock and var combo (not used)
  expl.path.vs.start <- 0.85
  expl.path.vs.end <- 0.85
  expl.path.vs.decay.type <- "linear"
  expl.path.vs.momentum <- 1.1
  expl.path.vs.strength <- 2
  expl.path.vs.start.var <- 1
  expl.path.vs.start.shock <- 1
  expl.path.vs.data.base <- 5 
  expl.path.vs.start.shock.frac <- 0.2
  expl.path.vs.min.shock.frac <- 0.01
  #Multi
  expl.path.multi.start <- 0.85
  expl.path.multi.end <- 0.95
  expl.path.multi.decay.type <- "linear" 
  expl.path.multi.best.db <- 100
  expl.path.multi.best.disc <- 0.98
  expl.path.multi.Kp.var <- 0.005          
  expl.path.multi.Ki.var <- 0.00001            
  expl.path.multi.Kd.var <- 0.000005          
  expl.path.multi.Kp.shock <- 0.005          
  expl.path.multi.Ki.shock <- 0.00001      
  expl.path.multi.Kd.shock <- 0.000005    
  expl.path.multi.Kp.surp <- 0.5     
  expl.path.multi.Ki.surp <- 0.2     
  expl.path.multi.Kd.surp <- 0.05     
  expl.path.multi.Kp.fam <- 0.5      
  expl.path.multi.Ki.fam <- 0.02      
  expl.path.multi.Kd.fam <- 0.01      
  expl.path.multi.Kp.db <- 5           
  expl.path.multi.Ki.db <- 100          
  expl.path.multi.Kd.db <- 5          
  expl.path.multi.Kp.disc <- 0.95        
  expl.path.multi.Ki.disc <- 0.99       
  expl.path.multi.Kd.disc <- 0.95 
  
  expl.path.multi.start.var <- 0.1
  expl.path.multi.start.shock <- 0.1
  expl.path.multi.start.surp <- 0
  expl.path.multi.start.fam <- 0
  
  expl.path.multi.start.frac.var <- 0.5
  expl.path.multi.start.frac.shock <- 0.5
  expl.path.multi.start.frac.surp <- 0
  expl.path.multi.start.frac.fam <- 0
  expl.path.multi.end.frac.var <- 0.5
  expl.path.multi.end.frac.shock <- 0.5
  expl.path.multi.end.frac.surp <- 0
  expl.path.multi.end.frac.fam <- 0
  
  #Potential additional Curiosity factor to a path
  curio.beta <- 0
  phi.cont.digits <- 2 #not used due to curio.beta = 0
  N.type <- "naive" #not used due to curio.beta = 0
  curio.decay.memory <- 0.95 #not used due to curio.beta = 0
  curio.cap <- 5 #not used due to curio.beta = 0
  
  #Relevant parameters
  #Memory
  batch.size <- 4*60*5 #4 (episodes per block) * 60 (periods per episode) * 5 (number of to be included blocks)
  force.last <- 4*60*1 #4 (episodes per block) * 60 (periods per episode) * 1 (number of to be included blocks)
  max.mem <- 4*60*100 #4 (episodes per block) * 60 (periods per episode) * 100 (number of to be included blocks)
  remove.memory <- 0.1 
  mem.type <- "game.encoded" 
  mem.selection <- "all"
  mem.init.epochs <- 100
  mem.expected.improve <- 0.01
  
  # Incorporating new information and Updating Status
  gamma <- 0.95
  a <- 0.1
  replay.intensive <- 1
  block.best <- 1 
  block.curr <- 1 
  block.expl.var <- 0
  block.expl.shock <- 0
  block.expl.surp <- 0
  block.expl.fam <- 0
  block.expl.vs <- 0
  block.expl.multi <- 2
  
  #Fancy Arguments
  hybrid.Q <- TRUE
  hybrid.Q.a.MC <- 0.1
  hybrid.Q.weighting <- "linear"
  hybrid.Q.apply <- "always" 
  use.rnn <- TRUE 
  only.experienced <- TRUE
  hybrid.switch <- TRUE
  hybrid.decay <- 0.98
  Q.Learning <- FALSE
  MC <- FALSE
  
  # Output
  show.current.status <- 1
  
  q.param <- nlist(action.policy, epsilon.start, epsilon.decay, epsilon.min, epsilon.decay.type, expl.path.var.start, expl.path.var.end, expl.path.var.decay.type, expl.path.var.momentum, expl.path.var.start.var,expl.path.var.data.base, expl.path.shock.start, expl.path.shock.end, expl.path.shock.decay.type, expl.path.shock.momentum, expl.path.shock.start.shock,expl.path.shock.data.base,expl.path.surp.start, expl.path.surp.end, expl.path.surp.decay.type, expl.path.surp.momentum, expl.path.surp.start.surp, expl.path.surp.data.base, expl.path.fam.start, expl.path.fam.end, expl.path.fam.decay.type, expl.path.fam.momentum, expl.path.fam.start.fam, expl.path.fam.data.base, expl.path.vs.start, expl.path.vs.end, expl.path.vs.decay.type, expl.path.vs.momentum,expl.path.vs.strength, expl.path.vs.start.var, expl.path.vs.start.shock, expl.path.vs.data.base, expl.path.vs.start.shock.frac, expl.path.vs.min.shock.frac,expl.path.multi.start,expl.path.multi.end, expl.path.multi.decay.type,expl.path.multi.best.db, expl.path.multi.best.disc, expl.path.multi.Kp.var, expl.path.multi.Ki.var, expl.path.multi.Kd.var,expl.path.multi.Kp.shock, expl.path.multi.Ki.shock, expl.path.multi.Kd.shock,expl.path.multi.Kp.surp, expl.path.multi.Ki.surp, expl.path.multi.Kd.surp,expl.path.multi.Kp.fam, expl.path.multi.Ki.fam, expl.path.multi.Kd.fam, expl.path.multi.Kp.db, expl.path.multi.Ki.db, expl.path.multi.Kd.db, expl.path.multi.Kp.disc, expl.path.multi.Ki.disc, expl.path.multi.Kd.disc, expl.path.multi.start.var, expl.path.multi.start.shock, expl.path.multi.start.surp, expl.path.multi.start.fam, expl.path.multi.start.frac.var, expl.path.multi.start.frac.shock, expl.path.multi.start.frac.surp, expl.path.multi.start.frac.fam, expl.path.multi.end.frac.var, expl.path.multi.end.frac.shock, expl.path.multi.end.frac.surp, expl.path.multi.end.frac.fam, curio.beta,phi.cont.digits,N.type,curio.decay.memory,curio.cap,batch.size, max.mem, remove.memory, mem.type, mem.selection, mem.init.epochs, mem.expected.improve, gamma, a, replay.intensive, force.last, hybrid.Q, hybrid.Q.a.MC, hybrid.Q.weighting, hybrid.Q.apply, use.rnn, only.experienced, hybrid.switch, hybrid.decay, Q.Learning, MC, block.expl.var, block.expl.shock, block.expl.surp, block.expl.fam, block.expl.vs, block.curr, block.best, block.expl.multi)
  
  return(q.param)
}

Get.Def.Par.XGBoost.Legacy.ThesisOpt <- function(){
  nrounds <- 50
  max_depth <- 5
  eta <- 0.3
  gamma <- 0.1
  colsample <- 0.95
  subsample <- 0.9
  min_child_weight <- 1
  nthread <- detectCores()-2
  model.defs <- nlist(nrounds, max_depth, eta, gamma, colsample, subsample, min_child_weight, nthread)
  return(model.defs)
}

Get.Def.Par.XGBoost.Legacy.v.0.1.6 <- function(){
  nrounds <- 400
  max_depth <- 45
  eta <- 0.1
  gamma <- 0.8
  colsample <- 0.4
  subsample <- 0.4
  min_child_weight <- 10
  nthread <- detectCores()-1
  model.defs <- nlist(nrounds, max_depth, eta, gamma, colsample, subsample, min_child_weight, nthread)
  return(model.defs)
}

Get.Game.Param.PD.Legacy.BattleOfStrategies2013.Baseline <- function(){
  uCC <- 1
  uCD <- -1
  uDC <- 2
  uDD <- 0
  err.D.prob <- 0.15
  err.C.prob <- 0
  delta <- 0.95
  T <- 60
  T.max <- 60
  intermed <- 0
  direct.rewards <- TRUE
  game.pars <- nlist(uCC, uCD, uDC, uDD, err.D.prob, err.C.prob, delta, T, T.max, intermed, direct.rewards)
  return(game.pars)
}

Get.Game.Param.PD.Legacy.BattleOfStrategies2019 <- function(){
  uCC <- 1
  uCD <- -1
  uDC <- 2
  uDD <- 0
  err.D.prob <- 0.25
  err.C.prob <- 0.25
  delta <- 0.985
  T <- 60
  T.max <- 60
  intermed <- 0
  direct.rewards <- TRUE
  game.pars <- nlist(uCC, uCD, uDC, uDD, err.D.prob, err.C.prob, delta, T, T.max, intermed, direct.rewards)
  return(game.pars)
}

Get.Def.Par.RNN.Legacy.v.0.1.6 <- function(){
  #Struktural Parameters
  hidden.nodes <- c(16,8)
  layer.type <- c("lstm","dense")
  activation.hidden <- c("relu","relu")
  activation.output <- c("linear")
  loss <- "mse"
  optimizer <- optimizer_adam(lr=0.001)
  single.dimensional <- FALSE #Only one output neuron. Actions are part of Statespace
  mask.value <- -99
  dropout <- c(0.2,0.2) #Dropouts for the hidden layers. 0 deactivates.
  recurrent.dropout <- c(0.2,NULL) #given for each layer, but every non TimeSeries layer ignores.0 deactivates.
  input.dropout <- c(0.2) #Should there be a input dropout? NULL and 0 deactivates.
  
  #Training parameters
  epochs <- 50
  batch.size.train <- 32
  verbose <- 0
  enforce.increasing.precision <- TRUE
  give.up.precision <- 5
  
  model.defs <- nlist(hidden.nodes,layer.type,activation.hidden,activation.output,loss,optimizer,mask.value,dropout, recurrent.dropout, input.dropout, epochs, batch.size.train, verbose, enforce.increasing.precision, give.up.precision, single.dimensional)

  return(model.defs)
}

Get.Def.Par.RNN.Legacy.ThesisOpt <- function(){
  #Struktural Parameters
  hidden.nodes <- c(128,64)
  layer.type <- c("lstm","dense")
  activation.hidden <- c("sigmoid","sigmoid")
  activation.output <- c("linear")
  loss <- "mse"
  optimizer <- optimizer_adam(lr=0.001)
  single.dimensional <- TRUE #Only one output neuron. Actions are part of Statespace
  mask.value <- -99
  dropout <- c(0,0) #Dropouts for the hidden layers. 0 deactivates.
  recurrent.dropout <- c(0) #given for each layer, but every non TimeSeries layer ignores.0 deactivates.
  input.dropout <- c(0) #Should there be a input dropout? NULL and 0 deactivates.
  
  #Training parameters
  epochs <- 5
  batch.size.train <- 600
  verbose <- 0
  enforce.increasing.precision <- TRUE
  give.up.precision <- 10
  
  model.defs <- nlist(hidden.nodes,layer.type,activation.hidden,activation.output,loss,optimizer,mask.value,dropout, recurrent.dropout, input.dropout, epochs, batch.size.train, verbose, enforce.increasing.precision, give.up.precision, single.dimensional)
  
  return(model.defs)
}