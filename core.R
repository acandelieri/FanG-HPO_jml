library(DiceKriging)
library(optimParallel)
library(foreach)
library(doParallel)
source("AGP.R")
source("Pareto.R")


moEI.hvi <- function( x, combo.GPs, ParetoSet, ParetoFront, y.ref=NA, dgt=4 ) {

  source("Pareto.R")
  source("AGP.R")
  
  x <- matrix(x,nrow=1) # <- it comes from 'optim'
  stopifnot( is.data.frame(ParetoSet) && is.data.frame(ParetoFront) )
  
  # cat("[moEI.hvi]: computing the reference point...\n")
  if( length(y.ref)==1 && is.na(y.ref) )
    y.ref <- getReferencePoint( ParetoFront )
  
  # cat("[moEI.hvi]: computing y(x)...\n")
  y <- NULL
  y.sd <- NULL
  for( m in 1:length(combo.GPs) ) {
    preds <- predict( combo.GPs[[m]], data.frame(X=x), "UK", checkNames=F )
    y <- cbind( y, preds$mean )
    y.sd <- cbind( y.sd, preds$sd )
  }
  y <- round( y, dgt )
  y.sd <- round( y.sd, dgt )
  
  # # cat("[moEI.hvi]: check for dominance relation...\n")
  # if( is.dominated( y=y, PF=as.matrix(ParetoFront) ) ) {
  #   # cat("[moEI.hvi]: y(x) IS DOMINATED!\n")
  #   res <- 0 # TODO: va bene "cassare" oppure è meglio tenere i valori negativi?
  # } else {
    # cat("[moEI.hvi]: predicting with combined GP...\n")
    Mu <- NULL
    Sd <- NULL
    for( m in 1:length(combo.GPs) ) {
      preds <- predict( combo.GPs[[m]], ParetoSet, "UK", checkNames=F )
      Mu <- cbind( Mu, preds$mean )
      Sd <- cbind( Sd, preds$sd )
    }
      
      
    # TODO: check!
    # cat("> computing HVI(y(x))...\n")
    # Hy <- ehvi.box( y, y.ref, y, y.sd )
    Hy <- HV( y, y.ref )
    # cat("> computing EHVI.wfg recursively...\n")
    PF_ <- limitParetoFront( PF=as.matrix(ParetoFront), y=y )
    res <- EHVI.wfg( PF=PF_, ref.pt=y.ref, Mu=Mu, Sd=Sd )
    res <- Hy - res
  
  # } # endif is dominated
  
  # if( is.infinite(res) | is.na(res) ) {
  #   print( Hy )
  #   print( res )
  #   print( P_ )
  #   print( ref.pt )
  #   print( Mu )
  #   print( Sd )
  #   stop("STOPPED IN moEI.hvi!!!!!!")
  # }
  
  return( res )
}


FanG_BO.run <- function( search.space,
                         sources,
                         n.objectives,
                         sources.costs,
                         initial.X, # list of matrices, 1,...,S
                         initial.Y, # list of marices, 1,...,S
                         initially.paid,
                         budget,
                         kernel="matern3_2",
                         noisy=T,
                         dgt=4,
                         seed,
                         y.ref=NA,
                         check.for.setting=T,
                         ...) {
  
  # All the preliminary checks ---------------------------------------------------
  stopifnot( n.objectives>=2 )
  stopifnot( is.matrix(search.space) & ncol(search.space)==2 & names(search.space)==c('min','max') )
  stopifnot( is.list(sources) & is.numeric(sources.costs) & length(sources)==length(sources.costs) )
  #TODO check on the # of initial points on each source!
  # stopifnot( .... )
  
  # if( check.for.setting ) {
  #   cat("\n[ Summary of the experiment]\n")
  #   cat("- Search Space dimensionality, d=",nrow(search.space),"\n",sep="")
  #   for( i in 1:nrow(search.space) )
  #     cat("   x",i," in [",search.space[i,'min'],",",search.space[i,'max'],"]\n",sep="")
  #   cat("- Number of objectives, M=",n.objectives,"\n",sep="")
  #   cat("  * All the objectives will be minimized\n")
  #   cat("- Number of information sources, S=",length(sources),"\n",sep="")
  #   cat("- Initial random evaluations on sources:\n")
  #   for( i in 1:length(sources.n0) )
  #     cat("   source #",i,": ",sources.n0[i]," evaluations\n",sep="")
  #   reply <- ""
  #   cat("\n")
  #   while( reply!="y" && reply!="n" ) {
  #     reply <- tolower(readline("> Is the setting ok? [y/n]"))
  #   }
  #   if( reply=="n" )
  #     stop("\nInterrupted by the user.",call.=F )
  # }
  # ------------------------------------------------------------------------------
  
  # Initialization --------------------------------------------------------------- 
  set.seed(seed)
  d <- nrow(search.space)
  S <- length(sources)
  M <- n.objectives
  
  X <- initial.X
  Y <- initial.Y
  paid <- initially.paid
  
  
  # store optimization process' information into a suitable data structure 
  process.data <- NULL
  for( s in 1:S )
    process.data <- rbind( process.data, data.frame( iteration=0,
                                                     source=s,
                                                     X=X[[s]],
                                                     Y=Y[[s]],
                                                     cost=sources.costs[s],
                                                     EHVI=NA,
                                                     NU.m=matrix(NA,1,M),
                                                     DISCR.s=matrix(NA,1,S),
                                                     models.update.time=0,
                                                     x.next.time=0,
                                                     s.next.time=0,
                                                     query.time=rep(NA,nrow(X[[s]])) ) )
  
  
  
  # initialize GPs collection
  
  update.elapsed <- Sys.time()
  
  # parallel implementation (on objectives)
  if(tolower(.Platform$OS.type) == "windows"){
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl) }
  else {
    registerDoParallel(detectCores()-1)
  }
  GPs <- list(S)
  for( s in 1:S ) {
    cat("[core.R]> Fitting GPs for source #",s,"...\n",sep="")
    GPs[[s]] <- foreach( m = 1:M, .packages='DiceKriging' ) %dopar% {
      gp <- km( design=data.frame( X=X[[s]] ),
                response=Y[[s]][,m],
                covtype=kernel,
                nugget.estim=noisy,
                control=list(trace=F) )
    }
  }

  # combined GPs
  cat("\n[core.R]> Combining GPs over sources...",sep="")
  cat("\n[core.R]> (* Parallel execution)\n",sep="")
  combo.GPs <- foreach( m = 1:M ) %dopar% {
    source("AGP.R")
    to.combine <- list()
    for( s in 1:S ) {
      to.combine[[s]] <- GPs[[s]][[m]]
    }
    # the argument 'm' is the multiplier for the GP's standard deviation (is not for objectives)
    res <- augmented.gp( GPs=to.combine, m=1, covtype=kernel )
  }

  if(tolower(.Platform$OS.type) == "windows") {
    registerDoParallel(NULL); stopCluster(cl)
  } else {
    stopImplicitCluster()
  }
  
  
    
  update.elapsed <- round(as.numeric(difftime(Sys.time(),update.elapsed,units="secs")),3)
  

  rm(X); rm(Y) # remove useless data from memory
    
  stored.combo.GPs <- list( combo.GPs )
  
  paid <- initially.paid
  cat("\n[core.R]> Paid for initialization:",paid,"\n")
  cat("[core.R]> Starting sequential optimization...\n")
  iter.count <- 0
  
  s.next <- NULL
  X.next <- NULL
  # ------------------------------------------------------------------------------
  

  # Sequential Optimization Process ----------------------------------------------
  while( paid<budget ) {
    
    cat("[core.R]> Remaining budget:",budget-paid,"\n")
    
    # update the GPs related to the last " next queried source-location pair" 
    
    if( !is.null(s.next) && !is.null(X.next) ) {
      
      update.elapsed <- Sys.time()
      
      cat("[core.R]> Updating model for source s =",s.next,"...\n")
      X <- process.data[which(process.data$source==s.next),(2+1):(2+d)]
      Y <- process.data[which(process.data$source==s.next),(2+d+1):(2+d+M)]
      for( m in 1:M ) {
        cat("   * GP fitted for ",m,"/",M," objectives...\n",sep="")
        GPs[[s.next]][[m]] <- km( design=data.frame( X ),
                                  response=Y[,m],
                                  covtype=kernel,
                                  nugget.estim=noisy,
                                  control=list(trace=F) )
      }
      
      
      # parallel implementation
      if(tolower(.Platform$OS.type) == "windows") {
        cl <- makeCluster(detectCores()-1)
        registerDoParallel(cl)
      } else {
        registerDoParallel(detectCores()-1)
      }
      
      # combined GPs
      cat("[core.R]> Combining GPs over sources...\n",sep="")
      cat("[core.R]> (* Parallel execution)\n",sep="")
      combo.GPs <- list(M)
      combo.GPs <- foreach( m = 1:M ) %dopar% {
        source("AGP.R")
        to.combine <- list()
        for( s in 1:S ) {
          to.combine[[s]] <- GPs[[s]][[m]]
        }
        # the argument 'm' is the multiplier for the GP's standard deviation (is not for objectives)
        res <- augmented.gp( GPs=to.combine, m=1, covtype=kernel )
      }
      
      if(tolower(.Platform$OS.type) == "windows") {
        registerDoParallel(NULL); stopCluster(cl)
      } else {
        stopImplicitCluster()
      }
            
      
      update.elapsed <- round(as.numeric(difftime(Sys.time(),update.elapsed,units="secs")),3)
      
      stored.combo.GPs[[length(stored.combo.GPs)+1]] <- combo.GPs
      
    }
    

    # comboGPs-based approximated PF
    combo.PF <- process.data[,(2+d+1):(2+d+M)]
    ixs <- getParetoIxs(combo.PF)
    combo.PF <- combo.PF[ixs,,drop=F]
    combo.PS <- process.data[ixs,(2+1):(2+d),drop=F]
    Mu <- NULL
    Sd <- NULL
    for( m in 1:M ) {
      preds <- predict( combo.GPs[[m]], combo.PS, "UK", checkNames=F )
      Mu <- cbind(Mu,preds$mean)
      Sd <- cbind(Mu,preds$sd)
    }
    
    
    # TODO: parametrize!
    cat("[core.R]> Optimizing the acquisition function...\n")
    
    ehvi.elapsed <- Sys.time()
    
    x0 <- lhs::maximinLHS(1,d)
    for( i in 1:d )
      x0[i] <- search.space[i,'min'] + x0[i] * (search.space[i,'max']-search.space[i,'min'])
    

    if(tolower(.Platform$OS.type) != "windows"){
      cl <- makeCluster(spec=detectCores(), type="FORK", outfile="")
    } else
      cl <- makeCluster(spec=detectCores(), outfile="")
    setDefaultCluster(cl=cl)
    
    # # SOLUTION #1    
    # mo.acquisition <- moEI.hvi
    # res <- optimParallel( par=as.numeric(x0), fn=momisbo.acquisition, gr=NULL, method="L-BFGS-B",
    #                       lower=search.space[,'min'], upper=search.space[,'max'], control=list(fnscale=-1), # to maximize the acquisition!
    #                       mo.acquisition=mo.acquisition, combo.GPs=combo.GPs, ParetoSet=combo.PS, ParetoFront=combo.PF, y.ref=y.ref, GPs=GPs, sources.costs=sources.costs )
    # 
    # setDefaultCluster(cl=NULL); stopCluster(cl)
    # 
    # print( res$par )
    # print( res$value )
    # 
    # acq.values <- numeric(S)
    # for( s in 1:S ) {
    #   acq.values[s] <- do.call( momisbo.acquisition, list( x=res$par,
    #                                                        mo.acquisition=mo.acquisition,
    #                                                        combo.GPs=combo.GPs,
    #                                                        ParetoSet=combo.PS,
    #                                                        ParetoFront=combo.PF,
    #                                                        y.ref=y.ref,
    #                                                        GPs=list(GPs[[s]]),
    #                                                        sources.costs=sources.costs[s] )
    #                             )
    # }
    # cat("$$$ All the acquisition values:\n")
    # print( acq.values )
    # s.next <- which( acq.values==res$value )
    # if( length( s.next )>1 ) {
    #   cat("[core.R] *** given the same value of acquisition function, the cheapest source is selected ***\n")
    #   s.next <- s.next[ which.min(sources.costs[s.next]) ]
    # }
    # X.next <- matrix( round(res$par,dgt), 1 )
    # 
    # cat("[core.R]> Next source-location to query:\n")
    # cat("   - s.next:",s.next,"\n")
    # cat("   - X.next:[",X.next,"]\n")
    
    
    # SOLUTION #2 
    res <- optimParallel( par=as.numeric(x0), fn=moEI.hvi, gr=NULL, method="L-BFGS-B",
                          lower=search.space[,'min'], upper=search.space[,'max'], control=list(fnscale=-1), # to maximize the acquisition!
                          combo.GPs=combo.GPs, ParetoSet=combo.PS, ParetoFront=combo.PF, y.ref=y.ref, dgt=dgt )
    
    ehvi.elapsed <- round(as.numeric(difftime(Sys.time(),ehvi.elapsed,units="secs")),3)
    
    setDefaultCluster(cl=NULL); stopCluster(cl)
    
    print( res$par ) 
    print( res$value )
    
    
    s.next.elapsed <- Sys.time()
    
    x <- matrix( round(res$par,dgt), 1 )
    discr.s <- numeric( S ) # discrepancy for each source s=1,...,S
    for( s in 1:S ) {
      discrepancy <- numeric(M) # discrepancy on single objective m=1,...,M
      for( m in 1:M ) {
        tmp <- predict( combo.GPs[[m]], data.frame(X=x), "UK", checkNames=F )
        mu.comboGP.m <- tmp$mean
        tmp <- predict( GPs[[s]][[m]], data.frame(X=x), "UK", checkNames=F )
        mu.GP.sm <- tmp$mean
        discrepancy[m] <- abs( mu.comboGP.m - mu.GP.sm )
      }
      discr.s[s] <- sum( discrepancy ) 
    }
    
    # print( sources.costs )
    # print( order(sources.costs,decreasing=T) )
    # print( discr.s )
    # print( order(discr.s,decreasing=T) )
    # score <- (order(sources.costs,decreasing=T) + order(discr.s,decreasing=T))/2
    # print( score ) 
    # ixs <- which( score == max(score) )
    # print( ixs )
    # if( length(ixs)>1 ) {
    #   ixs <- order(discr.s,decreasing=T)[ixs]
    #   ixs <- ixs[length(ixs)]
    # }
    # print( ixs )
    
    # print( discr.s*sources.costs )
    # ixs <- which.min( discr.s*sources.costs )
    
    # print( discr.s )
    # ixs <- which.min( discr.s )
    
    
    
    nu <- matrix(0,S,M)
    for( m in 1:M ) {
      combo.X <- combo.GPs[[m]]@X
      for( s in 1:S ) {
        X <- GPs[[s]][[m]]@X
        for( k1 in 1:nrow(combo.X) ) {
          for( k2 in 1:nrow(X) ) {
            if( identical(combo.X[k1,],X[k2,]) )
              nu[s,m] <- nu[s,m]+1
          }
        }
      }
    }
    
    cat("$$$ discrepancies:",discr.s,"\n")
    cat("$$$ nu:\n")
    print(nu)
    
    
    if( S==1 || apply(nu,1,max)[1] < apply(nu,1,max)[-1] ) {
      s.next <- 1
      cat("$$$ FORCED!!!\n")
    } else {
      cat("$$$ select depending on:",discr.s * sources.costs,"\n")
      s.next <- which.min( discr.s * sources.costs )
    }
    
    s.next.elapsed <- round(as.numeric(difftime(Sys.time(),s.next.elapsed,units="secs")),3)
    
    
    X.next <- x
    colnames(X.next) <- colnames(initial.X[[1]])
    
    
    cat("[core.R]> Next source-location to query:\n")
    cat("   - source:",s.next,"\n")
    cat("   - location:",X.next,"\n")
    
    
    
    # Query and observe
    
    query.elapsed <- Sys.time()
    
    cat("[core.R]> Query and observe...\n")
    Y.next <- do.call( sources[[s.next]], list(x=X.next) )
    Y.next <- t(round(Y.next,dgt))
    rownames(Y.next) <- NULL
    
    query.elapsed <- round(as.numeric(difftime(Sys.time(),query.elapsed,units="secs")),3)
    
    
    iter.count <- iter.count+1
    
    print( data.frame( iteration=iter.count,
                source=s.next,
                X=X.next,
                Y=Y.next,
                cost=sources.costs[s.next],
                EHVI=round(res$value,dgt),
                NU.m=if(S>1){t(apply(nu,1,max))}else{nu},
                DISCR.s=t(discr.s),
                models.update.time=update.elapsed,
                x.next.time=ehvi.elapsed,
                s.next.time=s.next.elapsed,
                query.time=query.elapsed ) )
    
    process.data <- rbind( process.data, data.frame( iteration=iter.count,
                                                     source=s.next,
                                                     X=X.next,
                                                     Y=Y.next,
                                                     cost=sources.costs[s.next],
                                                     EHVI=round(res$value,dgt),
                                                     NU.m=if(S>1){t(apply(nu,1,max))}else{nu},
                                                     DISCR.s=t(discr.s),
                                                     models.update.time=update.elapsed,
                                                     x.next.time=ehvi.elapsed,
                                                     s.next.time=s.next.elapsed,
                                                     query.time=query.elapsed ) )
    
    
    paid <- paid + sources.costs[s.next]
    
    cat("\n")
    
  }
  
  # ------------------------------------------------------------------------------
  return( list(process.data, stored.combo.GPs) )
}
