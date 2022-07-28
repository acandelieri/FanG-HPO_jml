rm(list=ls()); graphics.off(); cat("\014")

set.seed (42)

cat("> Configuring Python and reticulate...\n")
library(reticulate)
if(.Platform$OS.type=="unix") {
  # use_python(python="/home/antonio/anaconda3/envs/py3.8/bin/python",required=T)
  use_python(python="/home/ac21041/.conda/envs/autogluonFairBO/bin/python",required=T)
  autogluonFairBO_results_folder = "/home/ac21041/Desktop/autogluon-0.3.1/FanG-HPO_AutogluonFairBO_runs/XGB_HPO_ADULT_results"
  conda_python("autogluonFairBO")
} else {
  use_python(python="C:/Users/Public/anaconda3",required=T)
  autogluonFairBO_results_folder = "G:/Il mio Drive/AutogluonFairBO_results/XGB_HPO_ADULT_results"
  conda_python("py3.8")
}

source("core.R")
# source("AGP.R") # already loaded into 'core.R'


# 10-FCV Accuracy and DSP computed on the FULL dataset
source.1 = function( x ) {
  
  n.estimators = x[1]
  learning.rate = x[2]
  gamma = x[3]
  reg.alpha = x[4]
  reg.lambda = x[5]
  subsample = x[6]
  max.depth = x[7]
  
  n.estimators = round(n.estimators)
  learning.rate = round(10^learning.rate,2)
  gamma = round(gamma,1)
  reg.alpha = round(10^reg.alpha,3)
  reg.lambda = round(10^reg.lambda,3)
  subsample = round(subsample,2)
  max.depth = round(max.depth)
  
  py_run_string("dataset_name = 'ADULT_full'")
  py_run_string("sensitive_features = ['sex.Female',
                         'race.White', 'race.Asian.Pac.Islander',
                         'race.Amer.Indian.Eskimo', 'race.Other']")
  py_run_string("target = 'income.leq.50k'")  
  # setting XGBoost's hyperparameters
  
  py$n_estimators = as.integer(n.estimators)
  py$learning_rate = learning.rate
  py$gamma = gamma
  py$reg_alpha = reg.alpha
  py$reg_lambda = reg.lambda
  py$subsample = subsample
  py$max_depth = as.integer(max.depth)
  
  py_run_file("kfold_stratified_XGB.py")
  
  MCE = 1-round(mean(unlist(py$res$accuracy)),4)
  DSP = matrix(round(unlist(py$res$dsp),4),ncol=length(py$sensitive_features),byrow=T)
  DSP = max(apply(DSP,1,mean))
  
  return( c(MCE,DSP) )
  
}

# 10-FCV Accuracy and DSP computed on the REDUX dataset  
source.2 = function( x ) {
  
  n.estimators = x[1]
  learning.rate = x[2]
  gamma = x[3]
  reg.alpha = x[4]
  reg.lambda = x[5]
  subsample = x[6]
  max.depth = x[7]
  
  n.estimators = round(n.estimators)
  learning.rate = round(10^learning.rate,2)
  gamma = round(gamma,1)
  reg.alpha = round(10^reg.alpha,3)
  reg.lambda = round(10^reg.lambda,3)
  subsample = round(subsample,2)
  max.depth = round(max.depth)
  
  py_run_string("dataset_name = 'ADULT_redux'")
  py_run_string("sensitive_features = ['sex.Female',
                         'race.White', 'race.Asian.Pac.Islander',
                         'race.Amer.Indian.Eskimo', 'race.Other']")
  py_run_string("target = 'income.leq.50k'")
  
  # setting XGBoost's hyperparameters
  
  py$n_estimators = as.integer(n.estimators)
  py$learning_rate = learning.rate
  py$gamma = gamma
  py$reg_alpha = reg.alpha
  py$reg_lambda = reg.lambda
  py$subsample = subsample
  py$max_depth = as.integer(max.depth)
  
  py_run_file("kfold_stratified_XGB.py")
  
  MCE = 1-round(mean(unlist(py$res$accuracy)),4)
  DSP = matrix(round(unlist(py$res$dsp),4),ncol=length(py$sensitive_features),byrow=T)
  DSP = max(apply(DSP,1,mean))
  
  return( c(MCE,DSP) )
  
}  

sources = list( source.1, source.2 )



d = 7 # XGboost's hyperparameters
M = 2 # number of objectives

Omega = matrix( NA, d, 2 )
colnames(Omega) = c("min","max")

# number of estimators (integer)
Omega[1,'min'] = 2; Omega[1,'max'] = 256

# learning rate (log10)
Omega[2,'min'] = log(10^-2,10); Omega[2,'max'] = log(10,10)

# gamma (double)
Omega[3,'min'] = 0.0; Omega[3,'max'] = 0.1 

# reg.alpha (log10)
Omega[4,'min'] = log(10^-3,10); Omega[4,'max'] = log(10^3,10)

# reg.lambda (log10)
Omega[5,'min'] = log(10^-3,10); Omega[5,'max'] = log(10^3,10) 

# subsample (double)
Omega[6,'min'] = 0.01; Omega[6,'max'] = 1.0 

# max.depth (integer)
Omega[7,'min'] = 1; Omega[7,'max'] = 16 



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
  
  X.tmp = tmp$n_estimators
  X.tmp = cbind(X.tmp,log10(tmp$learning_rate))
  X.tmp = cbind(X.tmp,tmp$gamma)
  X.tmp = cbind(X.tmp,log10(tmp$reg_alpha))
  X.tmp = cbind(X.tmp,log10(tmp$reg_lambda))
  X.tmp = cbind(X.tmp,tmp$subsample)
  X.tmp = cbind(X.tmp,tmp$max_depth)

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
  saveRDS( object=run.info, file=paste0("results/results_ADULT_XGB_file_",run,".RDS") )
  cat(" done!\n")
  
  cat("\014")
}
