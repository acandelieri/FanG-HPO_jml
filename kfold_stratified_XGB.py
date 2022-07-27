import pandas as pd
import time
import xgboost as xgb

from sklearn.metrics import accuracy_score
from sklearn.model_selection import StratifiedKFold

from fairness import statistical_parity_difference

import numpy as np

# -------------- set on the R side ---------------------------------------
# dataset_name
# sensitive_features
# target
# ------------------------------------------------------------------------

dataset = pd.read_csv('data/' + dataset_name + '.txt', sep=',', header=0)
features = list(set(dataset.columns) - {target})

X = dataset[features]
y = dataset[target]

# -------------- set on the R side ---------------------------------------
# # Hyperparameters
# n_estimators {1,...,256} logarithmic (base 2)
# learning_rate [0.01,1] logarithmic (base 10)
# gamma [0,0.1] linear
# reg_alpha [10^-3, 10^3] logarithmic (base 10)
# reg_lambda [10^-3, 10^3] logarithmic base(10)
# subsample [0.01,1] linear
# max_depth {1,...,16} linear
# ------------------------------------------------------------------------


# Performance Metrics
res = {'accuracy': [], 'dsp': [], 'train_time': []}

kf = StratifiedKFold(n_splits=10)
for train_idx, test_idx in kf.split(X, y):

  # Divide train/test
  X_train, y_train = X.loc[train_idx], y.loc[train_idx]
  X_test, y_test = X.loc[test_idx], y.loc[test_idx]
  start = time.time()

  # Train the classifier and predict on test set
  classifier = xgb.XGBClassifier( n_jobs=8,
      objective="binary:logistic",random_state=1,use_label_encoder=False,
      n_estimators=n_estimators,learning_rate=learning_rate,
      gamma=gamma, reg_alpha=reg_alpha, reg_lambda=reg_lambda,
      subsample=subsample, max_depth=max_depth ).fit(X_train, y_train)
    

  res['train_time'].append(time.time() - start)
  y_pred = classifier.predict(X_test)

  # Compute accuracy and DSP
  res['accuracy'].append(accuracy_score(y_test, y_pred))
  fold_dsp = []
  for feature in sensitive_features:
      f = X_test[feature].to_numpy()
      fold_dsp.append(statistical_parity_difference(y_pred, f))
  res['dsp'].append(fold_dsp)
