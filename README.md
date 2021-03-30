# micrograd-ad

A port of the example in [`micrograd`](https://github.com/karpathy/micrograd/) using Haskell
and [`ad`](https://github.com/ekmett/ad). Described in detail in my [blog post](https://mazzo.li/posts/haskell-backprop-short.html).

![animation](https://user-images.githubusercontent.com/556090/113144987-cadf2900-9225-11eb-9487-f8e252a51d40.gif)

The neural-network implementation is in [`MLP.hs`](MLP.hs), and the demo in
[`micrograd-ad.hs`](micrograd-ad.hs). The rest is code to
generate the graphics.

## To run

Install [nix](https://nixos.org/). Then, issue `make` in the root of the repository.
The data will be generated and plotted, with the output in the `output` directory.
