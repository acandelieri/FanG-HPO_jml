rm(list=ls()); graphics.off(); cat("\014")

cat("> Configuring Python and reticulate...\n")
library(reticulate)
if(.Platform$OS.type=="unix") {
  # use_python(python="/home/antonio/anaconda3/envs/py3.8/bin/python",required=T)
  use_python(python="/home/ac21041/.conda/envs/autogluonFairBO/bin/python",required=T)
  conda_python("autogluonFairBO")
} else {
  use_python(python="C:/Users/Public/anaconda3",required=T)
  conda_python("py3.8")
}


source_python("fairness.py")
library(fairml)

# dataset.name = "adult"
# dataset.name = "compas"
# dataset.name = "germancredit"
dataset.name = "lawschooladmissions"

# model = "zlrm"
model = "fgrrm"

seed = 5

set.seed(seed)

cat("> Loading data...\n")
if( dataset.name=="adult") {
  # data(adult)
  # # short-hand variable names.
  # adult
  # r = adult[, "income"]
  # s = adult[, c("sex", "race")]
  # p = adult[, setdiff(names(adult), c("income", "sex", "race"))]
  adult = read.delim2("data/ADULT_full.txt",sep=",",header=T)
  for( h in 1:ncol(adult) )
    adult[,h] = as.numeric(adult[,h])
  # short-hand variable names.
  r = adult[, "income.leq.50k"]
  s = adult[, c("sex.Female", "race.White", "race.Asian.Pac.Islander", "race.Amer.Indian.Eskimo", "race.Other")]
  p = adult[, setdiff(names(adult), c("income.leq.50k", "sex.Female", "race.White", "race.Asian.Pac.Islander", "race.Amer.Indian.Eskimo", "race.Other"))]
} else {
  if( dataset.name=="compas") {
    # data(compas)
    # # short-hand variable names.
    compas = read.delim2("data/COMPAS_full.txt",sep=",",header=T)
    for( h in 1:ncol(compas) )
      compas[,h] = as.numeric(compas[,h])
    r = compas[, "two_year_recid"]
    s = compas[, c("sex.Female", "race.African.American", "race.Asian", "race.Caucasian",
                   "race.Hispanic", "race.Native.American")]
    p = compas[, setdiff(names(compas), c("two_year_recid", "sex.Female", "race.African.American", "race.Asian", "race.Caucasian",
                                          "race.Hispanic", "race.Native.American"))]
  } else {
    if( dataset.name=="germancredit") {
      # data(german.credit)
      # short-hand variable names.
      germancredit = read.delim2("data/GERMANCREDIT_full.txt",sep=",",header=T)
      for( h in 1:ncol(germancredit) )
        germancredit[,h] = as.numeric(germancredit[,h])
      r = germancredit[, c("has.bad.credit.risk")]
      s = germancredit[, c("Gender.Female"),drop=F]
      p = germancredit[, setdiff(names(germancredit), c("has.bad.credit.risk", "Gender.Female"))]
    } else {
      if( dataset.name=="lawschooladmissions") {
        # data(law.school.admissions)
        # short-hand variable names.
        lawschooladmissions = read.delim2("data/LAWSCHOOLADMISSIONS_full.txt",sep=",",header=T)
        for( h in 1:ncol(lawschooladmissions) )
          lawschooladmissions[,h] = as.numeric(lawschooladmissions[,h])
        r = lawschooladmissions[, c("has.passed.bar.exam")]
        s = lawschooladmissions[, c("gender.female","race1.asian","race1.black","race1.hisp","race1.other")]
        p = lawschooladmissions[, setdiff(names(lawschooladmissions), c("has.passed.bar.exam","gender.female","race1.asian","race1.black","race1.hisp","race1.other"))]
      } else {
        stop("ERROR: dataset.name =",dataset.name,"is not allowed!")  
      }
    }
  }  
}

# # running 10 FCV ----------------------------------------------------
# library(parallel)
# cl = makeCluster(n.processors)
# elapsed = Sys.time()
# 
# m = fairml.cv( response = r, predictors = p,
#                sensitive = s, unfairness = 0.10, model = model,
#                method = "k-fold", k = 10, runs = 1, cluster = cl )
# elapsed = difftime(Sys.time(),elapsed,units="secs")
# cat("Elapsed:",elapsed,"[secs]\n")
# stopCluster(cl)
# print(m)
# # -------------------------------------------------------------------


cat("> Executing fairml algorithm...\n")
N = length(r)
ixs = sample(1:N,N,replace=F )
r = r[ixs]
p = p[ixs,,drop=F]
s = s[ixs,,drop=F]


folds = cut(1:N,breaks=10,labels=FALSE)

DSP.all = matrix(NA,10,ncol(s))
misclassified = numeric(10)
for( k in 1:10 ) {
  cat(".")
  ixs = which(folds == k)
  if( model=="zlrm" ) {
    m = zlrm( response=factor(r[-ixs]), predictors=p[-ixs,,drop=F],
               sensitive=s[-ixs,,drop=F], unfairness=0.10  )
    preds = predict(m, new.predictors=p[ixs,,drop=F], type="class" )
  } else {
    if( model=="fgrrm" ) {
      m = fgrrm( response=as.factor(r[-ixs]), predictors=p[-ixs,,drop=F],
                  sensitive=s[-ixs,,drop=F], unfairness=0.10  )
      preds = predict(m, new.predictors=p[ixs,,drop=F], new.sensitive=s[ixs,,drop=F],type="class" )
    } else {
      stop("ERROR:", model,"is not valid for fairml!")
    }  
  }
  
  for( j in 1:ncol(DSP.all) )
    DSP.all[k,j] = statistical_parity_difference( matrix(as.numeric(preds),ncol=1), as.matrix(s[ixs,j,drop=F]) )
  
  misclassified[k] = length(which( preds!=r[ixs] ))
  
}
cat("\n")

solution = data.frame( MCE=round(sum(misclassified)/length(r),4),
                        DSP=max( apply( DSP.all, 2, mean ) ) )

print(solution) 

if( !dir.exists("results_fairml") )
  dir.create("results_fairml")
saveRDS( solution, paste0("results_fairml/fairml_",dataset.name,"_",model,"_seed_",seed,".RDS") )
