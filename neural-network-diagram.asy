/* Adapted from <https://commons.wikimedia.org/wiki/File:Colored_neural_network.svg> */
size(0, 10cm);
defaultpen(fontsize(14pt));
import graph;

int dx = 4, dy = 3;

void interSphere(pair[] c1, pair[] c2, real delta1, real delta2) {
  pen whitepen = defaultpen + white + 4*linewidth(defaultpen);
  for(int i = 0; i < c1.length; ++i) {
    for(int j = 0; j < c2.length; ++j) {
      pair delta = unit(c2[j]-c1[i]);
      draw(c1[i]+(delta*delta1) -- c2[j]-(delta*delta2), whitepen);
      draw(c1[i]+(delta*delta1) -- c2[j]-(delta*delta2), Arrow);
    }
  }
}

void interSphereLast(pair[] c1, pair[] c2, real delta1, real delta2) {
  pen whitepen = defaultpen + white + 4*linewidth(defaultpen);
  for(int i = 0; i < c1.length; ++i) {
    pair delta = unit(c2[i]-c1[i]);
    draw(c1[i]+(delta*delta1) -- c2[i]-(delta*delta2), whitepen);
    draw(c1[i]+(delta*delta1) -- c2[i]-(delta*delta2), Arrow);
  }
}

void inputCirc(pair[] c0, pen p = defaultpen) {
    for(int i = 0; i < c0.length; ++i) {
        draw(Circle(c0[i], 0.5), p);
    }
}

void neuronCirc(pair[] c0, pen p = defaultpen) {
    for(int i = 0; i < c0.length; ++i) {
        draw(Circle(c0[i], 1), p);
    }
}

pair[] c0 = {(0, 0), (0, dy), (0, 2*dy)};
pair[] c1 = {(dx, -1.5), (dx, -1.5 + dy), (dx, -1.5 + 2*dy), (dx, -1.5+3*dy)};
pair[] c2 = {(2*dx, -1.5), (2*dx, -1.5 + dy), (2*dx, -1.5 + 2*dy), (2*dx, -1.5+3*dy)};
pair[] c3 = {(3*dx, -1.5 + dy), (3*dx, -1.5 + 2*dy)};
pair[] c4 = {(3.8*dx, -1.5 + dy), (3.8*dx, -1.5 + 2*dy)};

pen redPen = defaultpen + heavyred;
pen bluePen = defaultpen + heavyblue;
pen greenPen = defaultpen + deepgreen;
inputCirc(c0, defaultpen);
neuronCirc(c1, bluePen);
neuronCirc(c2, bluePen);
neuronCirc(c3, bluePen);
inputCirc(c4, defaultpen);

interSphere(c0, c1, 0.7, 1.1);
interSphere(c1, c2, 1.1, 1.1);
interSphere(c2, c3, 1.1, 1.1);
interSphereLast(c3, c4, 1.1, 0.7);

/* label("Input", (0, 2*dy + 1), N, redPen); */
/* label("Hidden", (dx, -1.5 + 3dy + 1), N, bluePen); */
/* label("Output", (2*dx, -1.5 + 2dy + 1), N, greenPen); */
for (int i = 0; i < c0.length; ++i) {
  label(format("$x_%d$", c0.length - i), c0[i]);
}
for (int i = 0; i < c1.length; ++i) {
  label(format("$\mathsf{n}_{1%d}$", c1.length - i), c1[i]);
}
for (int i = 0; i < c2.length; ++i) {
  label(format("$\mathsf{n}_{2%d}$", c2.length - i), c2[i]);
}
for (int i = 0; i < c3.length; ++i) {
  label(format("$\mathsf{n}_{3%d}$", c3.length - i), c3[i]);
}
for (int i = 0; i < c4.length; ++i) {
  label(format("$y_%d$", c4.length - i), c4[i]);
}

real rbox = 2*dx + 1.025;
draw((rbox,-3)--(rbox, 3), invisible);

