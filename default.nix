{
  pinnedNixpkgsCommit ? "34f85de51bbc74595e63b22ee089adbb31f7c7a2", # nixos-20.09
  pinnedNixpkgsUrl ? "https://github.com/NixOS/nixpkgs/archive/${pinnedNixpkgsCommit}.tar.gz",
  pkgs ? import (fetchTarball pinnedNixpkgsUrl) {},
}:
let
  # Newwer matplotlib does not render the wiki plot that I modified
  # correctly (the point does not show).
  matplotlib_2 = with pkgs; with pkgs.python38Packages; callPackage ./matplotlib_2.nix {
    stdenv = if stdenv.isDarwin then pkgs.clangStdenv else pkgs.stdenv;
    inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa;
  };
  micrograd-ad = pkgs.haskellPackages.callCabal2nix
    "micrograd-ad"
    (builtins.filterSource
      (path: type: builtins.elem
        (builtins.baseNameOf path)
        [ "micrograd-ad.cabal" "Setup.hs" "micrograd-ad.hs" "MLP.hs" "LICENSE" ])
      ./.)
    {};
in {
  inherit micrograd-ad;
  python-shell = pkgs.mkShell {
    buildInputs = with pkgs; [
      matplotlib_2
      python38Packages.pandas
      python38Packages.numpy
    ];
  };
  asymptote-shell = pkgs.mkShell {
    buildInputs = with pkgs; [
      asymptote
      texlive.combined.scheme-medium
    ];
  };
}
