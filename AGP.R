# version 1.0
# Date: 2021-07-22
# by: Antonio Candelieri

library(DiceKriging)

#  Function for measuring the discrepancy between 2 GPs
#  - x: points where measure the discrepancy
#  - gp: first GP
#  - gp_: second GP
gp.discrepancy <- function( x, gp, gp_ ) {
  p <- predict( gp, newdata=data.frame(x), type="UK", checkNames=F )
  p_ <- predict( gp_, newdata=data.frame(x), type="UK", checkNames=F)
  gp.discrepancy <- abs(p$mean-p_$mean)
}


# function to generate the Augmented GP (AGP)
# - GPs: a list of GPs, one for each information source
# - m: technical parameter of the MISO-AGP algorithm (see [1] )
augmented.gp <- function( GPs, m=1, covtype="gauss" ) {
  
  D.aug <- data.frame(source=rep(1,nrow(GPs[[1]]@X)),
                      GPs[[1]]@X,
                      y=GPs[[1]]@y,
                      stringsAsFactors=F) # design of the GP associated to the hifi source
  
  if( length(GPs)>1 ) {
    
    # Generate the augmeted design
    for( s in 2:length(GPs) ) {
      D.s <- GPs[[s]]@X
      d <- gp.discrepancy(D.s,GPs[[1]],GPs[[s]])
      p0 <- predict( GPs[[1]], newdata=data.frame(D.s), "UK", checkNames=F )
      ixs <- which(d<m*p0$sd)
      
      if( length(ixs)>0 ) {
        pts <- D.s[ixs,]
        if( !is.matrix( pts ) ) {
          pts <- matrix(pts,nrow=length(ixs))
          pts <- as.data.frame( pts )
          colnames(pts) <- colnames(GPs[[s]]@X)
        }
        
        # print(data.frame( source=rep(s,length(ixs)),
        #                   pts,
        #                   y=GPs[[s]]@y[ixs],
        #                   stringsAsFactors=F) )
        
        D.aug <- rbind(D.aug, data.frame( source=rep(s,length(ixs)),
                                          pts,
                                          y=GPs[[s]]@y[ixs],
                                          stringsAsFactors=F) )
      }
    }
    
    # Fitting the augmented GP
    GP.aug <- km( design=data.frame(D.aug[,2:(ncol(D.aug)-1)]),
                  response=D.aug$y,
                  covtype=covtype,
                  nugget.estim=T,
                  control=list(trace=F))
    
  } else {
    GP.aug <- GPs[[1]]
  }
  
  return( GP.aug )
}


# MISO-AGP acquisition function:
# - GPs: list of GPs, one for each information source
# - GP.aug: augmented GP
# - costs: source-specific costs, one for each source
# - min.problem: TRUE=minimization, FALSE=maximization
# - beta: parameter to deal with the exploration-exploitaion trade-off in GP.CB
misoagp.acquisition <- function( x, GPs, GP.aug, costs, f.best, min.problem = T, beta=1 ) {
  
  if( length(x)>1 )
    x <- t(x)
  p.aug <- predict( GP.aug, newdata=data.frame(x), type="UK", checkNames=F )
  ds <- numeric(length(GPs))
  for( i in 1:length(ds) )
    ds[i] <- gp.discrepancy( x, GPs[[i]], GP.aug )
  
  if( min.problem )
    cb <- p.aug$mean - sqrt(beta)*p.aug$sd # lower confidence bound
  else
    cb <- p.aug$mean + sqrt(beta)*p.aug$sd # upper confidence bound

  if( min.problem )
    optimistic.improvement <- f.best - cb
  else
    optimistic.improvement <- cb - f.best
  
  # penalizing the optimistic improvement
  utility.values <- optimistic.improvement/( costs * (1+ds) )
  
  return( utility.values ) # one for each source
}


# References
# [1] Candelieri, A., & Archetti, F. (2021). Sparsifying to optimize
#     over multiple information sources: an augmented Gaussian process
#     based algorithm. Structural and Multidisciplinary Optimization,
#     1-17.