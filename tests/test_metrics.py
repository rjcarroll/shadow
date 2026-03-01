"""Tests for shared metrics utilities."""
import numpy as np
import pytest
from shadow.utils.metrics import log_loss_binary, prl


def test_log_loss_perfect():
    y = np.array([1.0, 0.0, 1.0])
    p = np.array([1.0, 0.0, 1.0])
    assert log_loss_binary(y, p) == pytest.approx(0.0, abs=1e-6)


def test_log_loss_null():
    y = np.array([1.0, 0.0, 1.0, 0.0])
    base_rate = y.mean()
    p = np.full_like(y, base_rate)
    ll = log_loss_binary(y, p)
    assert ll > 0


def test_prl_null_model_is_zero():
    y = np.array([1.0, 0.0, 1.0, 0.0])
    p = np.full_like(y, y.mean())
    assert prl(y, p) == pytest.approx(0.0, abs=1e-6)


def test_prl_better_than_null_is_positive():
    y = np.array([1.0, 0.0, 1.0, 0.0])
    p = np.array([0.9, 0.1, 0.9, 0.1])
    assert prl(y, p) > 0
