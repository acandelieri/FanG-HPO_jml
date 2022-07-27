#---

getParetoIxs <- function( Y ) {
  non.dominated <- rep(T,nrow(Y))
  for( i in 1:nrow(Y) ) {
    if( non.dominated[i] ) {
      for( j in 1:nrow(Y) ) {
        if( i!=j ) {
          if( length(which(Y[i,]==Y[j,]))==length(Y[i,]) ) {
            non.dominated[j] <- F
          } else {
            if( length(which(Y[i,]<=Y[j,]))==length(Y[i,]) && length(which(Y[i,]<Y[j,]))>=1 ) {
              non.dominated[j] <- F
            }
          }
        }
      }
    }
  }
  return( which(non.dominated==T) )
}

HV <- function( P, ref ) {

  stopifnot( ncol(P)==2 & length(ref)==2 )

  if( max(P[,1]) > ref[1] || max(P[,2]) > ref[2] )
    stop( paste("! Reference point is not the worst point!") )

  P <- P[order(P[,1]),,drop=F]
  if( P[1,2] < ref[2] )
    P <- rbind( c(P[1,1],ref[2]), P )
  if( P[nrow(P),1] < ref[1] )
    P <- rbind( P, c(ref[1],P[nrow(P),2]) )
  HV <- 0
  for( i in 2:nrow(P) )
    HV <- HV + (P[(i-1),2]-P[i,2]) * (ref[1]-P[i,1])
  return(HV)
}

# HVI <- function( P.pre, P.next, ref ) {
#   HV1 <- HV( P.pre, ref )
#   HV2 <- HV( P.next, ref )
#   return( HV2-HV1 )
# }

is.dominated <- function( y, PF ) {
  stopifnot( is.matrix(PF) )
  stopifnot( is.matrix(y) )
  stopifnot( ncol(PF)==ncol(y) )
  stopifnot( nrow(y)==1 )
  i <- 1
  dominant.found <- F
  while( i<=nrow(PF) && !dominant.found ) {
    dominant.found = ( all(PF[i,]<=y[1,]) && length(which(PF[i,]<y[1,]))>=1 )
    i <- i+1
  }
  return( dominant.found )
}

# HVI.singlePoint <- function( P, csi, ref ) {
#   
#   stopifnot( ncol(P)==2 & length(csi)==2 & length(ref)==2 )
#   
#   HVI <- 0
#   if( !is.dominated( csi, P ) ) {
#     HV0 <- HV(P,ref)
#     Y <- rbind( csi, P )
#     ixs <- getParetoIxs( Y )
#     P1 <- Y[ixs,]
#     HV1 <- HV(P1)
#     HVI <- HV1 - HV0
#   }
#   
# }

getReferencePoint <- function( PFs ) {
  stopifnot( is.list(PFs) )
  pts <- PFs[[1]]
  for( i in 2:length(PFs) )
    pts <- rbind(pts,PFs[[i]])
  return( apply(pts,1,max) )
}

limitParetoFront <- function( PF, y ) {
  stopifnot( is.matrix(PF) )
  stopifnot( ncol(PF)==length(y) )
  for( i in 1:nrow(PF) ) {
    PF[i,] <- apply( rbind(PF[i,],y), 2, max )
  }
  return(PF)
}

ehvi.box <- function( l, u, mu, sd ) {
  res <- 1
  for( m in 1:length(l) ) {
    if( sd[m]<10^-16 )
      res <- 0
    else
      res <- res * ( ( (l[m]-mu[m]) * pnorm( (l[m]-mu[m])/sd[m] ) + sd[m] * dnorm( (l[m]-mu[m])/sd[m] ) )
                     - ( (u[m]-mu[m]) * pnorm( (u[m]-mu[m])/sd[m] ) + sd[m] * dnorm( (u[m]-mu[m])/sd[m] ))  )
  }
  return(res)
}

EHVI.wfg <- function( PF, ref.pt, Mu, Sd ) {
  stopifnot( is.matrix(PF) )
  stopifnot( is.matrix(Mu) )
  stopifnot( is.matrix(Sd) )
  stopifnot( nrow(PF)==nrow(Mu) && nrow(PF)==nrow(Sd) )

  value <- ehvi.box( l=PF[1,], u=ref.pt, mu=Mu[1,], sd=Sd[1,] )

  if( nrow(PF)>1 ) {
    PF_ <- limitParetoFront( PF[-1,,drop=F], PF[1,] )
    Mu_ <- Mu[-1,,drop=F]
    Sd_ <- Sd[-1,,drop=F]
    ixs <- getParetoIxs( PF_ )
    PF_ <- PF_[ixs,,drop=F]
    Mu_ <- Mu_[ixs,,drop=F]
    Sd_ <- Sd_[ixs,,drop=F]
    value <- value - EHVI.wfg( PF_, ref.pt, Mu_, Sd_ )
  }

  # if( is.infinite(value) | is.na(value) ) {
  #   print( value )
  #   print( PF )
  #   print( ref.pt )
  #   print( Mu )
  #   print( Sd )
  #   stop("STOPPED IN EHVI.wfg!!!!!!")
  # }
  
  return( value )
}

# HV.wfg <- function( PF, ref.pt ) {
#   stopifnot( is.matrix(PF) )
#   
#   value <- prod(ref.pt-PF[1,])
#   
#   if( nrow(PF)>1 ) {
#     PF_ <- limitParetoFront( PF[-1,,drop=F], PF[1,] )
#     ixs <- getParetoIxs( PF_ )
#     PF_ <- PF_[ixs,,drop=F]
#     value <- value - HV.wfg( PF_, ref.pt )
#   }
#   
#   return( value )
# }