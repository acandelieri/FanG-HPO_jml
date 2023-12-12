
*****************************************************************************************
This file is part of the R project named "FanG-HPO_jml".

FanG-HPO_jml is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

FanG-HPO_jml is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

For information on GNU General Public License, see <http://www.gnu.org/licenses/>.
*****************************************************************************************

Authors:
Antonio Candelieri, Andrea Ponti, and Francesco Archetti
Univesity of Milano-Bicocca, Italy
2023


*****************************************************************************************
The R project FanG-HPO_jml aims at replicating the experiments reported in the paper
"Fair and Green Hyperparameter Optimization via Multi-objective and Multiple Information
Source Bayesian Optimization" (by Antonio Candelieri, Andrea Ponti, Francesco Archetti),
Machine Learning, Springer

The project is organized as follows:
 - the 'data' folder contains all the datasets, with suffixes 'full' and 'reduced' representing
   the ground-truth and the cheap sources, respectively;
 - the 'AGP.R' file refers to the functions for fitting the Augmented Gaussian Process (AGP)
   model and also using it for making predictions;
 - the 'core.R' file contains the functions implementing (a) the proposed approach and (b) the
   computation of the Expected Hyper Volume Improvement (EHVI) acquisition function;
 - the 'Pareto.R' file contains all the functions for computing the approximated Pareto front,
   the associated Hyper Volume, etc;
 - the 'run_fairML.R' is used to run the experiments with the 'fair-by-design' Machine Learning
   algorithms (from the R package 'fairML');
 - the following Python files are used to compute 'fairness' and 'accuracy' (on k-fold cross
   validation). They are Python files because they are used by the Python-based 'competitors'
   Autogluon and BoTorch-MOMF. Instead of re-implementing the computation in R, the Python
   files are recalled from the R code, to guarantee homogenity in the computation:
   - fairness.py
   - kfold_stratified_MLP.py
   - kfold_stratified_RF.py
   - kfold_stratified_XGB.py
   - kfold_stratified_SVM.py
   
 - Finally, a separate .R file is provided for running FanG-HPO on a specific
   'dataset - ML algo' pair. The name of each file is defined as follows:
   
   FanG_from_AutogluonFairBO_<MLalgo>_<dataset>.R
   
   with <MLalgo> in {'MLP','RF','SVM','XGB'}, and <dataset> in {'ADULT',
   'COMPAS','GERMANCREDIT','LAWSCHOOLADMISSIONS'}
   


*****************************************************************************************
IMPORTANT NOTES:
*****************************************************************************************

1) The experiments are performed by starting from the initial designs of AutogluonFairBO,
in order to guarantee a fair comparison between the diffferent methods. For more detailed information, please refere to the paper.


2) AutogluonFairBO results must be downloaded from:

https://drive.google.com/drive/folders/1qxSU2iuyvf1BZFfkDyrYPLSueyFPc3J3

and local pathways to the folders must be updated in all the files

FanG_from_AutogluonFairBO_<MLalgo>_<dataset>.R

before running them


3) This R project refers only to the code needed to run FanG-HPO. The code for running
experments with AutogluonFairBO and BoTorchMOMF is also freely available but in two
separated repositories:

Autogluon-FairBO:
https://drive.google.com/drive/folders/1-2PYP6uS-r8Oe70ZwSxplJr6kaWPdDCM?usp=sharing
(for installation and configuration, please use the official documentation of
autogluon).

BoTorch-MOMF:
https://github.com/andreaponti5/FanG-HPO-MOMF.git
(for installation and configuration, please use the official documentation of
BoTorch).
*****************************************************************************************
   



