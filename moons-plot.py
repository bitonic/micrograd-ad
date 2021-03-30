# See <https://github.com/karpathy/micrograd/blob/c911406e5ace8742e5841a7e0df113ecb5d54685/demo.ipynb>
import pandas
import matplotlib.pyplot as plt
import glob
import numpy as np
import os
from matplotlib.animation import FuncAnimation

moons = pandas.read_csv("output/test-moons.cv", sep=",", header=None).values
X = moons[:, 0:2]
y = moons[:, 2]

plt.scatter(X[:,0], X[:,1], c=y, s=6, cmap="Spectral")
plt.savefig('output/moons.svg', dpi=200, transparent=True)

