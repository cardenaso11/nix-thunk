{
  inputs = {
    # dep/style.hs is a git submodule; this makes nix fetch it automatically
    # when the flake is fetched over git (Nix 2.27+; on older Nix, add
    # ?submodules=1 to the flake URL).
    self.submodules = true;

    # Everything else nix-thunk depends on is a nix-thunk under dep/, and a
    # packed thunk is itself a flake, so it is an input like any other.
    haskell-nix.url = ./dep/haskell.nix;

    # Whatever haskell.nix builds against, so that only one nixpkgs is in play.
    # This resolves through the thunk, which exposes upstream's inputs under
    # upstream's own names.
    nixpkgs.follows = "haskell-nix/nixpkgs";
  };

  outputs = inputs@{ self, ... }:
    let inherit (inputs) nixpkgs;
        eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;

        # `default.nix` is the stable entry point and takes no `system`, so the
        # flake hands it a package set built for one. It works the rest out for
        # itself, including `gitignoreSource` from dep/gitignore.nix.
        nixThunkFor = system: import ./default.nix {
          pkgs = inputs.haskell-nix.legacyPackages.${system};
        };
    in {
      lib = eachSystem (system:
        let nix-thunk = nixThunkFor system;
        in {
          inherit (nix-thunk) thunkSource mapSubdirectories;
        }
      );

      packages = eachSystem (system:
        let nix-thunk = (nixThunkFor system).command;
        in {
          inherit nix-thunk;
          default = nix-thunk;
        }
      );
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nixcache.reflex-frp.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" # reflex-frp
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
}
