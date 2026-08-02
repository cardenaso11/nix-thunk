# Takes the same arguments as lib.nix, and for the same reason: the defaults
# work out the system for themselves, which a flake is not allowed to do, so
# flake.nix passes a package set it built for one.
let defaultInputs = import ./defaultInputs.nix; in
{
  haskell-nix ? defaultInputs.haskell-nix {},
  pkgs ? defaultInputs.pkgs { inherit haskell-nix; },
}:

let
  nix-thunk = import ./lib.nix { inherit haskell-nix pkgs; };
  project = (nix-thunk.perGhc {}).project;
in
project.shellFor {
  packages = ps: [ ps.nix-thunk ];

  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
    hlint = "latest";
  };
}
