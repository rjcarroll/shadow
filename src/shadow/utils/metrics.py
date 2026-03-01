"""Shared evaluation metrics."""
import numpy as np


def log_loss_binary(y_true: np.ndarray, y_pred: np.ndarray) -> float:
    """Binary log-loss (negative mean log-likelihood)."""
    eps = np.finfo(float).eps
    y_pred = np.clip(y_pred, eps, 1 - eps)
    return -np.mean(
        y_true * np.log(y_pred) + (1 - y_true) * np.log(1 - y_pred)
    )


def prl(y_true: np.ndarray, y_pred: np.ndarray) -> float:
    """Proportional reduction in loss relative to the null (intercept-only) model."""
    null_pred = np.full_like(y_pred, y_true.mean())
    null_ll = log_loss_binary(y_true, null_pred)
    model_ll = log_loss_binary(y_true, y_pred)
    return (null_ll - model_ll) / null_ll
