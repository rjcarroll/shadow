"""Shared plotting helpers."""
import matplotlib.pyplot as plt


STYLE = {
    "figure.dpi": 150,
    "font.family": "sans-serif",
    "axes.spines.top": False,
    "axes.spines.right": False,
}

# Grayscale palette for PSRM submission
GOV = "#000000"       # black  — government-side / primary series
OPP = "#888888"       # gray   — opposition-side / secondary series
ACCENT = "#555555"    # mid-gray — tertiary / reference
IS_CLR = "#000000"    # in-sample bars
OOS_CLR = "#aaaaaa"   # out-of-sample bars
SHADE = "#cccccc"     # war-year shading, histogram fill, etc.


def apply_style() -> None:
    plt.rcParams.update(STYLE)
