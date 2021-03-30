.PHONY: all
all: generate-data backprop-plot-anim moons-plot gradient-plot neural-network-diagram

.PHONY: haskell-repl
haskell-repl:
	nix-shell -A micrograd-ad.env --run "runhaskell Setup.hs configure && runhaskell Setup.hs repl"

dist/build/micrograd-ad/micrograd-ad: micrograd-ad.hs micrograd-ad.cabal
	nix-shell -A micrograd-ad.env --run "runhaskell Setup.hs configure && runhaskell Setup.hs build"

.PHONY: generate-data
generate-data: dist/build/micrograd-ad/micrograd-ad
	nix-shell -A micrograd-ad.env --run "dist/build/micrograd-ad/micrograd-ad"

.PHONY: backprop-plot-anim
backprop-plot-anim:
	nix-shell -A python-shell --run "python backprop-plot-anim.py"

.PHONY: moons-plot
moons-plot:
	nix-shell -A python-shell --run "python moons-plot.py"

.PHONY: gradient-plot
gradient-plot:
	nix-shell -A python-shell --run "python gradient-plot.py"

.PHONY: neural-network-diagram
neural-network-diagram:
	nix-shell -A asymptote-shell --run "asy -V -f svg -o output/neural-network-diagram.svg neural-network-diagram"

.PHONY: clean
clean:
	nix-shell -A micrograd-ad.env --run "runhaskell Setup.hs clean"
	rm -rf output
