{
  pinnedNixpkgsCommit ? "cee3ca37c68dad981396365bf4b1d0c807ac4eb4", # nixos-20.09
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
      ffmpeg
    ];
  };
  asymptote-shell = pkgs.mkShell {
    buildInputs = with pkgs; [
      asymptote
      texlive.combined.scheme-medium
    ];
  };
}
