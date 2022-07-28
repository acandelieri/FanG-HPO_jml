rm(list=ls()); graphics.off(); cat("\014")

set.seed (42)

cat("> Configuring Python and reticulate...\n")
library(reticulate)
if(.Platform$OS.type=="unix") {
  # use_python(python="/home/antonio/anaconda3/envs/py3.8/bin/python",required=T)
  use_python(python="/home/ac21041/.conda/envs/autogluonFairBO/bin/python",required=T)
  autogluonFairBO_results_folder = "/home/ac21041/Desktop/autogluon-0.3.1/FanG-HPO_AutogluonFairBO_runs/MLP_HPO_COMPAS_results"
  conda_python("autogluonFairBO")
} else {
  use_python(python="C:/Users/Public/anaconda3",required=T)
  autogluonFairBO_results_folder = "G:/Il mio Drive/AutogluonFairBO_results/MLP_HPO_COMPAS_results"
  conda_python("py3.8")
}

source("core.R")
# source("AGP.R") # already loaded into 'core.R'


# 10-FCVAccuracy and DSP computed on the FULL dataset
source.1 = function( x ) {
  
  hidden.layers.n = x[1]
  h1.size = x[2]
  h2.size = x[3]
  h3.size = x[4]
  h4.size = x[5]
  alpha = x[6]
  learning_rate_init = x[7]
  beta_1 = x[8]
  beta_2 = x[9]
  tol = x[10]
  
  # stopifnot( is.integer(hidden.layers.n) )
  
  hidden.layers.n = round(hidden.layers.n)
  h1.size=round(h1.size)
  h2.size=round(h2.size)
  h3.size=round(h3.size)
  h4.size=round(h4.size)
  alpha=round(10^alpha,6)
  learning_rate_init=round(10^learning_rate_init,6)
  beta_1=round(10^beta_1,3)
  beta_2=round(10^beta_2,3)
  tol=round(10^tol,5)
  
  py_run_string("dataset_name = 'COMPAS_full'")
  py_run_string("sensitive_features = ['sex.Female', 'race.African.American',
                         'race.Asian','race.Caucasian', 'race.Hispanic', 'race.Native.American']")
  py_run_string("target = 'two_year_recid'")
  
  # setting MLP's hyperparameters
  
  py$hidden_layer_sizes = as.integer(h1.size)
  if( hidden.layers.n>1 )
    py$hidden_layer_sizes = c( py$hidden_layer_sizes, as.integer(h2.size) )
  if( hidden.layers.n>2 )
    py$hidden_layer_sizes = c( py$hidden_layer_sizes, as.integer(h3.size) )
  if( hidden.layers.n>3 )
    py$hidden_layer_sizes = c( py$hidden_layer_sizes, as.integer(h4.size) )
  py$alpha = alpha
  py$learning_rate_init = learning_rate_init
  py$beta_1 = beta_1
  py$beta_2 = beta_2
  py$tol = tol
  
  py_run_file("kfold_stratified_MLP.py")
  
  MCE = 1-round(mean(unlist(py$res$accuracy)),4)
  DSP = matrix(round(unlist(py$res$dsp),4),ncol=length(py$sensitive_features),byrow=T)
  DSP = round(max(apply(DSP,1,mean)),4)
  
  return( c(MCE,DSP) )
  
}

# 10-FCVAccuracy and DSP computed on the REDUX dataset  
source.2 = function( x ) {
  
  hidden.layers.n = x[1]
  h1.size = x[2]
  h2.size = x[3]
  h3.size = x[4]
  h4.size = x[5]
  alpha = x[6]
  learning_rate_init = x[7]
  beta_1 = x[8]
  beta_2 = x[9]
  tol = x[10]
  
  hidden.layers.n = round(hidden.layers.n)
  h1.size=round(h1.size)
  h2.size=round(h2.size)
  h3.size=round(h3.size)
  h4.size=round(h4.size)
  alpha=round(10^alpha,6)
  learning_rate_init=round(10^learning_rate_init,6)
  beta_1=round(10^beta_1,3)
  beta_2=round(10^beta_2,3)
  tol=round(10^tol,5)
  
  py_run_string("dataset_name = 'COMPAS_redux'")
  py_run_string("sensitive_features = ['sex.Female', 'race.African.American',
                         'race.Asian','race.Caucasian', 'race.Hispanic', 'race.Native.American']")
  py_run_string("target = 'two_year_recid'")
  
  # setting MLP's hyperparameters
  
  py$hidden_layer_sizes = as.integer(h1.size)
  if( hidden.layers.n>1 )
    py$hidden_layer_sizes = c( py$hidden_layer_sizes, as.integer(h2.size) )
  if( hidden.layers.n>2 )
    py$hidden_layer_sizes = c( py$hidden_layer_sizes, as.integer(h3.size) )
  if( hidden.layers.n>3 )
    py$hidden_layer_sizes = c( py$hidden_layer_sizes, as.integer(h4.size) )
  py$alpha = alpha
  py$learning_rate_init = learning_rate_init
  py$beta_1 = beta_1
  py$beta_2 = beta_2
  py$tol = tol
  
  py_run_file("kfold_stratified_MLP.py")
  
  MCE = 1-round(mean(unlist(py$res$accuracy)),4)
  DSP = matrix(round(unlist(py$res$dsp),4),ncol=length(py$sensitive_features),byrow=T)
  DSP = round(max(apply(DSP,1,mean)),4)
  
  return( c(MCE,DSP) )
  
}  

sources = list( source.1, source.2 )



d = 10 # number of MLP's hyperparameters
M = 2 # number of objectives

# hyperparameters' search space
Omega = matrix( NA, d, 2 )
colnames(Omega) = c("min","max")

# number of hidden layers (integer)
Omega[1,'min'] = 1; Omega[1,'max'] = 4 

# number of units in each hidden layer (integer)
for( i in 2:5 ) {
  Omega[i,'min'] = 2; Omega[i,'max'] = 32
}

# alpha (log10)
Omega[6,'min'] = log(10^-6,10); Omega[6,'max'] = log(10^-1,10)

# learnig rate init (log10)
Omega[7,'min'] = log(10^-6,10); Omega[7,'max'] = log(10^-1,10) 

# beta1 (log10)
Omega[8,'min'] = log(10^-3,10); Omega[8,'max'] = log(0.99,10) 

# beta2 (log10)
Omega[9,'min'] = log(10^-3,10); Omega[9,'max'] = log(0.99,10) 

# tol (log10)
Omega[10,'min'] = log(10^-5,10); Omega[10,'max'] = log(10^-2,10) 


sources.costs = c(1,0.5) # from preliminary analysis



# initial design from AutoGluon - FairBO experiments (source "FULL" only!)
files = list.files(autogluonFairBO_results_folder)

n.runs = length(files)

for( f in files ) {
  
  run = as.integer(strsplit(gsub(".csv","",f,fixed=T),"_")[[1]][2])
  
  X = list()
  Y = list()
  
  cat("> Reading results from Autogluon-FairnessBO: file '",f,"'...\n",sep="")
  
  tmp = read.table( paste0(autogluonFairBO_results_folder,"/",f),sep=",",header=T)
  total.cost = sum(tmp$fold)/10 # total cost incurred by autogluon-FairBO: this will be the budget for FanG-HPO
  
  cat("> Selecting the first",2*(d+1),"initial configurations to initialize FanG-HPO...\n")
  tmp$run = tmp$run-min(tmp$run)+1
  tmp = tmp[which(tmp$run<=2*(d+1)),]
  tmp = unique(tmp[,2+(1:nrow(Omega))])
  
  X.tmp = tmp$n_layers
  X.tmp = cbind(X.tmp,tmp$layer_1)
  X.tmp = cbind(X.tmp,tmp$layer_2)
  X.tmp = cbind(X.tmp,tmp$layer_3)
  X.tmp = cbind(X.tmp,tmp$layer_4)
  X.tmp = cbind(X.tmp,log10(tmp$alpha))
  X.tmp = cbind(X.tmp,log10(tmp$learning_rate_init))
  X.tmp = cbind(X.tmp,log10(tmp$beta_1))
  X.tmp = cbind(X.tmp,log10(tmp$beta_2))
  X.tmp = cbind(X.tmp,log10(tmp$tol))
  
  Y.tmp = NULL
  for( i in 1:nrow(X.tmp) ) {
    res = source.1(X.tmp[i,])
    Y.tmp =  rbind(Y.tmp,res)
  }
  
  ixs = sample(1:nrow(X.tmp),d+1,replace=F)
  X[[1]] = X.tmp[ixs,]
  X[[2]] = X.tmp[-ixs,]
  Y[[1]] = Y.tmp[ixs,]
  Y[[2]] = Y.tmp[-ixs,]
  initially.paid = nrow(X[[1]])*sources.costs[1] + nrow(X[[2]])*sources.costs[2] 
  
  
  run.info = FanG_BO.run( search.space=Omega,
                          sources=sources,
                          n.objectives=2,
                          sources.costs=sources.costs, #sources.costs=c(100,1),
                          initial.X=X,
                          initial.Y=Y,
                          initially.paid=initially.paid,
                          budget=total.cost,
                          kernel="matern3_2",
                          noisy=T,
                          dgt=4,
                          seed=1+round(run/length(files)),
                          y.ref=c(1,1),
                          check.for.setting=F,
                          p=NA )
  
  cat("> Saving results...")
  if( !dir.exists("results") )
    dir.create("results")
  saveRDS( object=run.info, file=paste0("results/results_COMPAS_MLP_file_",run,".RDS") )
  cat(" done!\n")
  
  cat("\014")
}
