rm(list=ls()); graphics.off(); cat("\014")

set.seed (42)

library(reticulate)
if( .Platform$OS.type=="unix") {
  use_python(python="/home/antonio/anaconda3/envs/py3.8/bin/python",required=T)
} else {
  use_python(python="C:/Users/Antonio Candelieri/Documents/.conda/envs/py3.8",required=T)
}
conda_python("py3.8")

source("core.R")
source("AGP.R")



source.1 <- function( x ) {
  
  hidden.layers.n <- x[1]
  h1.size <- x[2]
  h2.size <- x[3]
  h3.size <- x[4]
  h4.size <- x[5]
  alpha <- x[6]
  learning_rate_init <- x[7]
  beta_1 <- x[8]
  beta_2 <- x[9]
  tol <- x[10]
  
  # stopifnot( is.integer(hidden.layers.n) )
  
  hidden.layers.n = round(hidden.layers.n)
  h1.size=round(2^h1.size)
  h2.size=round(2^h2.size)
  h3.size=round(2^h3.size)
  h4.size=round(2^h4.size)
  alpha=round(10^alpha,6)
  learning_rate_init=round(10^learning_rate_init,6)
  beta_1=round(10^beta_1,3)
  beta_2=round(10^beta_2,3)
  tol=round(10^tol,5)
  
  py_run_string("dataset_name = 'ADULT_full'")
  py_run_string("sensitive_features = ['sex.Female',
                         'race.White', 'race.Asian.Pac.Islander',
                         'race.Amer.Indian.Eskimo', 'race.Other']")
  py_run_string("target = 'income.leq.50k'")

  # setting MLP's hyperparameters
  
  py$hidden_layer_sizes <- as.integer(h1.size)
  if( hidden.layers.n>1 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h2.size) )
  if( hidden.layers.n>2 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h3.size) )
  if( hidden.layers.n>3 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h4.size) )
  py$alpha <- alpha
  py$learning_rate_init <- learning_rate_init
  py$beta_1 <- beta_1
  py$beta_2 <- beta_2
  py$tol <- tol
  
  py_run_file("kfold_stratified.py")
  
  MCE <- 1-round(mean(unlist(py$res$accuracy)),4)
  DSP <- matrix(round(unlist(py$res$dsp),4),ncol=length(py$sensitive_features),byrow=T)
  DSP <- round(max(apply(DSP,1,mean)),4)
  
  return( c(MCE,DSP) )
  
}
  
source.2 <- function( x ) {
  
  hidden.layers.n <- x[1]
  h1.size <- x[2]
  h2.size <- x[3]
  h3.size <- x[4]
  h4.size <- x[5]
  alpha <- x[6]
  learning_rate_init <- x[7]
  beta_1 <- x[8]
  beta_2 <- x[9]
  tol <- x[10]
  
  hidden.layers.n = round(hidden.layers.n)
  h1.size=round(2^h1.size)
  h2.size=round(2^h2.size)
  h3.size=round(2^h3.size)
  h4.size=round(2^h4.size)
  alpha=round(10^alpha,6)
  learning_rate_init=round(10^learning_rate_init,6)
  beta_1=round(10^beta_1,3)
  beta_2=round(10^beta_2,3)
  tol=round(10^tol,5)
  
  py_run_string("dataset_name = 'ADULT_redux'")
  py_run_string("sensitive_features = ['sex.Female',
                         'race.White', 'race.Asian.Pac.Islander',
                         'race.Amer.Indian.Eskimo', 'race.Other']")
  py_run_string("target = 'income.leq.50k'")
  
  # setting MLP's hyperparameters
  
  py$hidden_layer_sizes <- as.integer(h1.size)
  if( hidden.layers.n>1 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h2.size) )
  if( hidden.layers.n>2 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h3.size) )
  if( hidden.layers.n>3 )
    py$hidden_layer_sizes <- c( py$hidden_layer_sizes, as.integer(h4.size) )
  py$alpha <- alpha
  py$learning_rate_init <- learning_rate_init
  py$beta_1 <- beta_1
  py$beta_2 <- beta_2
  py$tol <- tol
  
  py_run_file("kfold_stratified.py")
  
  MCE <- 1-round(mean(unlist(py$res$accuracy)),4)
  DSP <- matrix(round(unlist(py$res$dsp),4),ncol=length(py$sensitive_features),byrow=T)
  DSP <- round(max(apply(DSP,1,mean)),4)
  
  return( c(MCE,DSP) )
  
}  

sources <- list( source.1, source.2 )


d <- 10 # MLP's hyperparameters
M <- 2 # number of objectives
Omega <- matrix( NA, d, 2 )
colnames(Omega) <- c("min","max")
# number of hidden layers (integer)
Omega[1,'min'] <- 1; Omega[1,'max'] <- 4 
# number of units in each hidden layer (log2)
for( i in 2:5 ) {
  Omega[i,'min'] <- log(2,2); Omega[i,'max'] <- log(32,2)
}
# alpha (log10)
Omega[6,'min'] <- log(10^-6,10); Omega[6,'max'] <- log(10^-1,10)
# learnig rate init (log10)
Omega[7,'min'] <- log(10^-6,10); Omega[7,'max'] <- log(10^-1,10) 
# beta1 (log10)
Omega[8,'min'] <- log(10^-3,10); Omega[8,'max'] <- log(0.99,10) 
# beta2 (log10)
Omega[9,'min'] <- log(10^-3,10); Omega[9,'max'] <- log(0.99,10) 
# tol (log10)
Omega[10,'min'] <- log(10^-5,10); Omega[10,'max'] <- log(10^-2,10) 


# initial design from BOTorch experiments (source "FULL")
files.1 <- sort(list.files("only_FanG-HPO_results",pattern="_ADULT_MLP_" ))
files.2 <- sort(list.files("extras_BOTorch",pattern="obj_ADULT_MLP_", ))

stopifnot( length(files.1)==length(files.2) )
n.runs <- length(files.1)

for( run in 1:n.runs ) {
  
  cat("\014")
  
  X <- list()
  Y <- list()

  cat("> Reading file '",files.1[run],"'...\n",sep="")
  tmp <- readRDS( paste0("only_FanG-HPO_results/",files.1[run]))
  tmp <- tmp[[1]]
  ixs <- which(tmp$iteration==0 & tmp$source==1)
  X[[1]] <- tmp[ixs,2+(1:d)]
  ixs <- which(tmp$iteration==0 & tmp$source==2)
  X[[2]] <- tmp[ixs,2+(1:d)]
  Y[[2]] <- round(tmp[ixs,2+d+(1:M)],4)
  colnames(X[[1]]) <- NULL
  colnames(X[[2]]) <- NULL
  colnames(Y[[2]]) <- NULL
  
  cat("> Reading file '",files.2[run],"'...\n",sep="")
  tmp <- read.table( paste0("extras_BOTorch/",files.2[run]),sep=",",header=T)
  tmp <- as.matrix(tmp)
  Y[[1]] <- round(tmp,4)
  colnames(Y[[1]]) <- NULL

  
  stopifnot( nrow(X[[1]])==nrow(X[[2]]) )
  
  
  sources.costs <- c(2,1) # from preliminary analysis
  botorch.initially.paid <- nrow(X[[1]])*sources.costs[1]
  
  ixs <- sample(1:nrow(X[[1]]),13,replace=F)
  X[[1]] <- X[[1]][ixs,]
  Y[[1]] <- Y[[1]][ixs,]
  ixs <- sample(1:nrow(X[[2]]),14,replace=F)
  X[[2]] <- X[[2]][ixs,]
  Y[[2]] <- Y[[2]][ixs,]
  colnames(Y[[2]]) <- colnames(Y[[1]])
  initially.paid <- nrow(X[[1]])*sources.costs[1] + nrow(X[[2]])*sources.costs[2]
  # stopifnot( botorch.initially.paid==initially.paid )
  
  run.info <- FanG_BO.run( search.space=Omega,
                           sources=sources,
                           n.objectives=2,
                           sources.costs=sources.costs, #sources.costs=c(100,1),
                           initial.X=X,
                           initial.Y=Y,
                           initially.paid=initially.paid,
                           budget=20*d,
                           kernel="matern3_2",
                           noisy=T,
                           dgt=4,
                           seed=1+round(run/5),
                           y.ref=c(1,1),
                           check.for.setting=F,
                           p=NA)
  
  cat("> Saving results...")
  if( !dir.exists("results") )
    dir.create("results")
  saveRDS( object=run.info, file=paste0("results/results_ADULT_MLP_file_",run,".RDS") )
  cat(" done!\n")
}
