# This file takes the same arguments as lib.nix. The defaults compute the system
# themselves, and a flake cannot do that. So flake.nix builds a package set for
# one system and passes that set here.
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
