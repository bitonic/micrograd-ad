# See <https://github.com/karpathy/micrograd/blob/c911406e5ace8742e5841a7e0df113ecb5d54685/demo.ipynb>
import pandas
import matplotlib.pyplot as plt
import glob
import numpy as np
import os
import matplotlib.animation as animation

moons = pandas.read_csv("output/test-moons.cv", sep=",", header=None).values
X = moons[:, 0:2]
y = moons[:, 2]

def read_epoch(epoch):
  # We rely on the CSV for each epoch to go col-by-col, ascending
  # both in the column and the row
  contour_data = pandas.read_csv(f"output/{epoch:03d}-contour.csv", sep=",", header=None).values
  xx = np.unique(contour_data[:, 0])
  yy = np.unique(contour_data[:, 1])
  Z = contour_data[:, 2]
  # Z = np.array([s > 0 for s in Z])
  Z = Z.reshape((len(xx), len(yy))).transpose()
  with open(f"output/{epoch:03d}-loss", "r") as loss_file:
    loss = float(loss_file.read())
  return (loss, xx, yy, Z)

epochs = 0
while True:
  if not os.path.isfile(f"output/{epochs:03d}-contour.csv"):
    break
  epochs += 1

fig, [moons_ax, loss_ax] = plt.subplots(2, 1, gridspec_kw={'height_ratios': [2, 1]})

def plot_moons():
  moons_ax.scatter(X[:,0], X[:,1], c=y, s=6, cmap="Spectral")

def plot_contour(xx, yy, Z):
  moons_ax.contourf(
    xx, yy, Z, cmap="Spectral", alpha=.8,
    levels=np.arange(-11, 11, 0.25)
  )
  moons_ax.contourf(
    xx, yy, Z, colors=["black"],
    levels=np.array([-0.08, 0.08])
  )

loss, xx, yy, Z = read_epoch(0)
plot_contour(xx, yy, Z)
plot_moons()

loss_ax.set_xlim(0, epochs)
loss_ax.set_ylim(0, 0.5)
loss_x = [0]
loss_y = [loss]
loss_line, = loss_ax.plot(loss_x, loss_y, lw=2)

def update(epoch):
  print(f"Plotting epoch {epoch:03d}")
  moons_ax.clear()
  loss, xx, yy, Z = read_epoch(epoch)
  plot_contour(xx, yy, Z)
  plot_moons()
  loss_x.append(epoch)
  loss_y.append(loss)
  loss_line.set_data(loss_x, loss_y)
  loss_ax.set_xlabel(f"epoch: {epoch:03d}, loss: {loss:.4f}")
  return loss_line,

anim = animation.FuncAnimation(fig, update, frames=range(epochs), interval=100)
writer = animation.FFMpegWriter()
anim.save("output/animation.mp4", dpi=200, writer=writer)
# anim.save("output/animation.gif", dpi=200, savefig_kwargs={"transparent": True})
# plt.show()
