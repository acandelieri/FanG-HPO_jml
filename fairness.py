import numpy as np


def statistical_parity_difference(y_pred, sensitive_features):
    prob_y_given_s = []
    N = y_pred.shape[0]
    prob_y = np.where(y_pred == 1)[0].shape[0] / N
    for value in [0, 1]:
        idx = np.where(sensitive_features == value)[0]
        if idx.shape[0] == 0:
            return 0
        prob_s = idx.shape[0] / N
        prob_y_s = np.where((y_pred == 1) | (sensitive_features == value))[0].shape[0] / N
        prob_y_given_s.append((prob_y + prob_s - prob_y_s) / prob_s)
    return abs(prob_y_given_s[0] - prob_y_given_s[1])
