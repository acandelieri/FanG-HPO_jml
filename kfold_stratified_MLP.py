import pandas as pd
import time

from sklearn.metrics import accuracy_score
from sklearn.model_selection import StratifiedKFold
from sklearn.neural_network import MLPClassifier

from fairness import statistical_parity_difference

import numpy as np

# -------------- set on the R side ---------------------------------------
# dataset_name = 'COMPAS_full'
# sensitive_features = ['sex.Female',
#                       'race.African.American', 'race.Asian',
#                       'race.Caucasian', 'race.Hispanic', 'race.Native.American']
# target = 'two_year_recid'


dataset = pd.read_csv('data/' + dataset_name + '.txt', sep=',', header=0)
features = list(set(dataset.columns) - {target})

X = dataset[features]
y = dataset[target]

# -------------- set on the R side ---------------------------------------
# # Hyperparameters
# hidden_layer_sizes = [16, 8, 4, 2]
# alpha = 0.1
# learning_rate_init = 0.01
# beta_1 = 0.99
# beta_2 = 0.99
# tol = 0.01
# -------------- set on the R side ---------------------------------------


# Performance Metrics
res = {'accuracy': [], 'dsp': [], 'train_time': []}

kf = StratifiedKFold(n_splits=10)
for train_idx, test_idx in kf.split(X, y):

  # Divide train/test
  X_train, y_train = X.loc[train_idx], y.loc[train_idx]
  X_test, y_test = X.loc[test_idx], y.loc[test_idx]
  start = time.time()

  # Train the classifier and predict on test set
  classifier = MLPClassifier(hidden_layer_sizes=hidden_layer_sizes, alpha=alpha,
                             learning_rate_init=learning_rate_init, beta_1=beta_1,
                             beta_2=beta_2, tol=tol).fit(X_train, y_train)
                               
  res['train_time'].append(time.time() - start)
    
  y_pred = classifier.predict(X_test)
    
  # Compute accuracy and DSP
  res['accuracy'].append(accuracy_score(y_test, y_pred))
  fold_dsp = []
  for feature in sensitive_features:
    f = X_test[feature].to_numpy()
    fold_dsp.append(statistical_parity_difference(y_pred, f))
  res['dsp'].append(fold_dsp)

