# See <https://commons.wikimedia.org/wiki/File:Saddle_Point_between_maxima.svg>
import numpy as np
from numpy import pi, sin, cos
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt

def run(num_points, out):
  fig = plt.figure()
  ax = fig.add_subplot(111, projection='3d')
  plot_args = {'rstride': 1, 'cstride': 1, 'cmap':"Spectral",
               'linewidth': 0.4, 'antialiased': True,
               'vmin': -1.5, 'vmax': 1.5}


  x, y = np.mgrid[-6:2:31j, -2:6:31j]*pi

  def f(x, y):
    return 0.5*cos(x/2) + sin(y/4)

  z = f(x, y)

  def plot_points(x, y):
    ax.plot(
      x, y / 2, f(x, y / 2),
      marker=".", mfc="white", mec="black", mew=0.5, markersize=10,
      color="black", linewidth=0.5
    )

  alpha = 2

  points = np.array([
    (-9.42477796076938,3.141592653589793),
    (-10.42477796076938,3.8486994347763406),
    (-11.302360522659752,4.420436439971677),
    (-11.89312422747638,4.869473384679192),
    (-12.22342592418437,5.2155893225844725),
    (-12.394059210240133,5.479330824328197),
    (-12.480108365688444,5.678944475192803),
    (-12.523226118506464,5.8294308264864565),
    (-12.54479669332144,5.9426263090633045),
    (-12.555583444649068,6.0276632360240265),
  ])

  plot_points(points[:num_points, 0], points[:num_points, 1])

  ax.plot_surface(x, y/2, z, **plot_args)
  ax.view_init(azim=-60, elev=45)
  ax.set_xlim(-6*pi, 2*pi)
  ax.set_ylim(-pi, 3*pi)
  ax.set_zlim(-2, 2)
  plt.xticks([-6*pi, -2*pi, 2*pi],
             [r"$-6\pi$", r"$-2\pi$", r"$2\pi$"])
  plt.yticks([-pi, pi, 3*pi],
             [r"$-2\pi$", r"$2\pi$", r"$6\pi$"])
  ax.set_zticks([-2, 0, 2])
  ax.set_zticklabels([r"$-2$", r"$0$", r"$2$"])
  ax.w_xaxis.set_pane_color((1.0, 1.0, 1.0, 0.0))
  ax.w_yaxis.set_pane_color((1.0, 1.0, 1.0, 0.0))
  ax.w_zaxis.set_pane_color((1.0, 1.0, 1.0, 0.0))
  ax.set_xlabel(r"$x$", fontsize=18)
  ax.set_ylabel(r"$y$", fontsize=18)
  ax.set_zlabel(r"$z$", fontsize=18)
  plt.savefig(out, bbox_inches="tight", transparent=True)

run(1, "output/gradient-example-1.svg")
run(2, "output/gradient-example-2.svg")
run(6, "output/gradient-example-3.svg")

