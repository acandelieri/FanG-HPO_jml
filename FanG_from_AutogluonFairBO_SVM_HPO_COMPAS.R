rm(list=ls()); graphics.off(); cat("\014")

set.seed (42)

cat("> Configuring Python and reticulate...\n")
library(reticulate)
if(.Platform$OS.type=="unix") {
  # use_python(python="/home/antonio/anaconda3/envs/py3.8/bin/python",required=T)
  use_python(python="/home/ac21041/.conda/envs/autogluonFairBO/bin/python",required=T)
  autogluonFairBO_results_folder = "/home/ac21041/Desktop/autogluon-0.3.1/FanG-HPO_AutogluonFairBO_runs/SVM_HPO_COMPAS_results"
  conda_python("autogluonFairBO")
} else {
  use_python(python="C:/Users/Public/anaconda3",required=T)
  autogluonFairBO_results_folder = "G:/Il mio Drive/AutogluonFairBO_results/SVM_HPO_COMPAS_results"
  conda_python("py3.8")
}

source("core.R")
# source("AGP.R") # already loaded into 'core.R'


# 10-FCV Accuracy and DSP computed on the FULL dataset
source.1 = function( x ) {
  
  svmC = 10^x[1]
  gamma = 10^x[2]
  
  py_run_string("dataset_name = 'COMPAS_full'")
  py_run_string("sensitive_features = ['sex.Female', 'race.African.American',
                         'race.Asian','race.Caucasian', 'race.Hispanic', 'race.Native.American']")
  py_run_string("target = 'two_year_recid'")
  
  # setting SVM classifier's hyperparameters
  
  py$svmC = svmC
  py$gamma = gamma
  
  py_run_file("kfold_stratified_SVM.py")
  
  MCE = 1-round(mean(unlist(py$res$accuracy)),4)
  DSP = matrix(round(unlist(py$res$dsp),4),ncol=length(py$sensitive_features),byrow=T)
  DSP = round(max(apply(DSP,1,mean)),4)
  
  return( c(MCE,DSP) )
  
}

# 10-FCV Accuracy and DSP computed on the REDUX dataset  
source.2 = function( x ) {
  
  svmC = 10^x[1]
  gamma = 10^x[2]
  
  py_run_string("dataset_name = 'COMPAS_redux'")
  py_run_string("sensitive_features = ['sex.Female', 'race.African.American',
                         'race.Asian','race.Caucasian', 'race.Hispanic', 'race.Native.American']")
  py_run_string("target = 'two_year_recid'")
  
  # setting SVM classifier's hyperparameters
  
  py$svmC = svmC
  py$gamma = gamma
  
  py_run_file("kfold_stratified_SVM.py")
  
  MCE = 1-round(mean(unlist(py$res$accuracy)),4)
  DSP = matrix(round(unlist(py$res$dsp),4),ncol=length(py$sensitive_features),byrow=T)
  DSP = round(max(apply(DSP,1,mean)),4)
  
  return( c(MCE,DSP) )
  
}  

sources = list( source.1, source.2 )



d = 2 # number of SVM classifier's hyperparameters
M = 2 # number of objectives

# hyperparameters' search space
Omega = matrix( NA, d, 2 )
colnames(Omega) = c("min","max")

# C (log10)
Omega[1,'min'] = log(10^-3,10); Omega[1,'max'] = log(10^3,10)

# gamma (log10)
Omega[2,'min'] = log(10^-3,10); Omega[2,'max'] = log(10^3,10)


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
  
  X.tmp = round(log(tmp$C,10),3)
  X.tmp = cbind(X.tmp,round(log(tmp$gamma,10),3))
  

  Y.tmp = NULL
  for( i in 1:nrow(X.tmp) ) {
    cat(i,"/",nrow(X.tmp),"... ",sep="")
    if( i <= round(nrow(X.tmp)/2) ) {
      # from source #1
      res = source.1(X.tmp[i,])
    } else {
      # from source #2
      res = source.1(X.tmp[i,])
    }
    Y.tmp =  rbind(Y.tmp,res)
  }
  cat("\n")
  
  ixs = 1:round(nrow(X.tmp)/2)
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
  saveRDS( object=run.info, file=paste0("results/results_COMPAS_SVM_file_",run,".RDS") )
  cat(" done!\n")
  
  cat("\014")
}
