"""Shared plotting helpers."""
import matplotlib.pyplot as plt


STYLE = {
    "figure.dpi": 150,
    "font.family": "serif",
    "axes.spines.top": False,
    "axes.spines.right": False,
}


def apply_style() -> None:
    plt.rcParams.update(STYLE)
