<div align="center">

# nix-thunk

### Nix dependencies you can hack on.

Pin Git repositories without vendoring them. Check them out in place when you
need to contribute a fix, then pack them back up without changing your Nix code.

[![Built with Nix](https://img.shields.io/static/v1?logo=nixos&logoColor=white&label=&message=Built%20with%20Nix&color=41439a)](https://nixos.org)
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org)
[![Hackage](https://img.shields.io/hackage/v/nix-thunk.svg)](https://hackage.haskell.org/package/nix-thunk)
[![CI](https://github.com/obsidiansystems/nix-thunk/actions/workflows/haskell.yml/badge.svg)](https://github.com/obsidiansystems/nix-thunk/actions/workflows/haskell.yml)
[![Obsidian Systems](https://img.shields.io/badge/Obsidian-Systems-white)](https://obsidian.systems)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](./LICENSE)

</div>

```console
$ nix-thunk create https://github.com/example/some-dependency dep/some-dependency
$ git add dep/some-dependency

# Later, when the dependency needs a fix:
$ nix-thunk unpack dep/some-dependency
$ cd dep/some-dependency     # a normal Git checkout, ready to edit and push
```

`nix-thunk` represents each dependency as a small **thunk** directory. A packed
thunk records an exact Git revision and content hash, but it does not contain a
checkout. Nix can fetch it reproducibly when needed. Unpack the same directory
and it becomes a normal Git repository that your existing Nix expressions keep
using in place.

## Why nix-thunk?

Source dependencies often force an awkward choice: keep them reproducible, or
keep them easy to modify. `nix-thunk` is designed to give you both.

- **Pinned and reproducible.** Every packed thunk records the repository,
  revision, and Nix content hash needed to fetch the same source again.
- **Small in Git.** Commit a few pointer files instead of vendoring an entire
  repository or requiring every contributor to clone every dependency.
- **Made for upstream work.** `unpack` turns a dependency into an ordinary Git
  checkout. Edit it, commit it, push a branch, and open a pull request using the
  tools you already know.
- **Transparent to Nix.** The same path works while the dependency is packed or
  unpacked, so local source changes are immediately visible to your builds.
- **Fast routine updates.** Move a packed dependency to its latest upstream
  revision without cloning it first.
- **Friendly to teams and CI.** Packed thunks are reviewable, deterministic,
  and do not rely on mutable developer checkouts.

Unlike a Git submodule, a thunk does not need to be initialized or cloned before
Nix can use it. Unlike a plain fetch expression, it can become an editable
checkout in exactly the same directory.

## Quick start

### Install

Install the packaged release from nixpkgs:

```bash
nix-env -f '<nixpkgs>' -iA haskellPackages.nix-thunk
```

Or install the latest version from this repository:

```bash
nix-env -f https://github.com/obsidiansystems/nix-thunk/archive/master.tar.gz \
  -iA command
```

You can also build it without installing it globally:

```bash
nix-build https://github.com/obsidiansystems/nix-thunk/archive/master.tar.gz \
  -A command
./result/bin/nix-thunk --help
```

### Create a dependency

Create a packed thunk directly from a Git URI. The destination is optional; if
you omit it, `nix-thunk` derives a directory name from the URI.

```bash
nix-thunk create https://github.com/example/some-dependency.git \
  dep/some-dependency
```

If you already have a Git checkout, turn it into a thunk with `pack`:

```bash
nix-thunk pack dep/some-dependency
```

### Work on a dependency

Unpack a thunk when you need to inspect or modify its source:

```bash
nix-thunk unpack dep/some-dependency
cd dep/some-dependency

# Edit, test, commit, and push as usual.

cd ../..
nix-thunk pack dep/some-dependency
```

Packing records the checkout's current commit and upstream repository. By
default, `nix-thunk` protects you from packing uncommitted or unpushed work.

### Update a dependency

Update a packed thunk to the latest revision on its tracked branch without
cloning the repository:

```bash
nix-thunk update dep/some-dependency
```

You can also select a branch or exact revision:

```bash
nix-thunk update --branch main dep/some-dependency
nix-thunk update --rev COMMIT dep/some-dependency
```

### Use a local worktree

If you already have a separate clone, create a Git worktree for the thunk rather
than cloning it again:

```bash
nix-thunk worktree dep/some-dependency ~/code/some-dependency
```

## Nix integration

Import this repository's [`default.nix`](./default.nix) to get the stable Nix
interface:

- **`command`** builds the `nix-thunk` executable.
- **`thunkSource`** resolves a packed or unpacked thunk to its source tree.
- **`mapSubdirectories`** applies a function to every dependency directory.

For example, if this repository is itself available at `./nix-thunk`:

```nix
let
  nix-thunk = import ./nix-thunk {};
  some-dependency = nix-thunk.thunkSource ./dep/some-dependency;
in
  import some-dependency
```

The expression does not change when you run `nix-thunk unpack` and start editing
the dependency locally.

Resolve every thunk under one directory at once:

```nix
let
  pkgs = import <nixpkgs> {};
  nix-thunk = import ./nix-thunk {};
  sources = nix-thunk.mapSubdirectories nix-thunk.thunkSource ./dep;
in {
  some-package = pkgs.callPackage sources.some-package {};
  another-package = pkgs.callPackage sources.another-package {};
}
```

The resolved path is a normal Nix source path, so subdirectories work as usual:

```nix
imports = [ "${nix-thunk.thunkSource ./dep/some-project}/nix/module.nix" ];
```

## How thunks work

A packed thunk contains generated Nix loaders and a JSON pointer describing its
source. `nix-thunk` selects the appropriate loader format, asks Nix for the
source hash, and writes the pointer files. When unpacked, those files are
replaced by a Git checkout at the pinned revision.

The file format is a compatibility protocol. New versions of `nix-thunk` can
read historical thunk formats, while newly written thunks use the latest format.
This lets repositories update the tool independently from their existing
dependency pointers.

## Thunks as flake inputs

A packed thunk is also a flake, so you can use it as an input directly:

```nix
inputs.mythunk.url = "path:./dep/mythunk";
```

The generated `flake.nix` forwards the outputs of the repository that the thunk
points at. It also exposes that repository's own inputs. Each input keeps its
own name, and the flake pins each input to the revision that upstream locked. So
`follows` works in both directions:

```nix
# Use whatever the thunk uses
inputs.someinput.follows = "mythunk/nixpkgs";

# Make the thunk use yours
inputs.mythunk.inputs.nixpkgs.follows = "nixpkgs";
```

Note two limits. First, `mythunk.outPath` is the thunk directory, and not the
fetched source, because Nix always takes a flake's `outPath` from its own source
tree. Use the named outputs instead. Second, upstream can declare an input as a
relative path that leaves its repository. Such an input cannot have a reference
of its own, so the flake exposes it as an alias. You can read the alias, and you
cannot override it.

When the repository is not a flake, the generated flake has no inputs to expose,
and it provides the fetched source as `src`. `src` is a fetched tree, and not a
bare path, so `src` holds both `src.outPath` and `src.narHash`.

`nix-thunk` usually writes the `flake.lock` from upstream's own lock, and it
does not run `nix flake lock`. So a pack does not fetch the repositories that
upstream depends on, and a pack does not fail when one of those repositories is
unreachable. The result matches the lock that Nix writes itself, so `nix flake
lock` on a packed thunk changes nothing. When `nix-thunk` cannot restate an
input, it asks Nix to resolve that input, and the pack then takes as long as it
did before.

`nix-thunk` cannot restate an input in two cases. Upstream declared the input as
a path outside its own repository, or upstream gave the input a name that Nix
does not accept in a `follows`. A flake can declare an input called `hls-1.10`,
as haskell.nix does, and nothing can refer to that name. So the flake exposes
such an input under the nearest name that Nix accepts (`hls-1_10`). It leaves
the input for upstream's own lock to resolve.

You can turn all of this off. `create`, `pack` and `update` take `--no-flake`,
and they then write the newest format that carries no flake files. The format
belongs to the thunk, so `unpack` keeps the format that it finds. A pack without
the flag moves a thunk to the newest format.

The flake interface survives `nix-thunk unpack` and `nix-thunk worktree`, so a
project can keep building while you develop a dependency. An unpacked thunk of a
flake repository is that repository's own flake, and it exposes the same outputs
and the same input names. For a repository that is not a flake, `nix-thunk`
writes a generated `flake.nix` and `flake.lock` into the checkout. They provide
the same `src` output in the same shape. `nix-thunk` hides those files through
git's `info/exclude`, so git still reports the checkout as clean. A pack removes
them again. `nix-thunk` writes nothing when the repository already holds a
`flake.nix` or a `flake.lock`. It also leaves a `flake.nix` that you write
yourself in place.

## Binary cache

Builds from this repository are published to the Reflex binary cache. For a
one-off installation, trusted users can enable it on the command line:

```bash
nix-env -f https://github.com/obsidiansystems/nix-thunk/archive/master.tar.gz \
  -iA command \
  --option extra-substituters https://nixcache.reflex-frp.org \
  --option extra-trusted-public-keys \
    'ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI='
```

For persistent NixOS configuration, add the cache alongside your existing
substituters and trusted keys:

```nix
nix.settings.substituters = [ "https://nixcache.reflex-frp.org" ];
nix.settings.trusted-public-keys = [
  "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
];
```

Then run `sudo nixos-rebuild switch`. Multi-user Nix only accepts these settings
from trusted users. Installations from nixpkgs can normally use the standard
nixpkgs cache instead.

## Building from source

`nix-thunk` must be built through this repository's Nix expressions, not with a
standalone Cabal build. The build embeds knowledge of a pinned, known-good
nixpkgs used by historical thunk formats.

```bash
nix-build release.nix -A command --no-out-link
```

For a development environment with GHC, Cabal, Haskell Language Server, and
HLint:

```bash
nix-shell
```

## Contributing

Pull requests are welcome. For substantial changes, please open an issue first
so the design and thunk-format compatibility can be discussed. See
[`CONTRIBUTING.md`](./CONTRIBUTING.md) for the project conventions.

## About Obsidian Systems

`nix-thunk` is built and maintained by
**[Obsidian Systems](https://obsidian.systems)**. We provide frontier engineering
for high-assurance systems and have long contributed to open-source Nix and
Haskell tooling, including [Obelisk](https://github.com/obsidiansystems/obelisk)
and [Reflex](https://reflex-frp.org/).

If your team needs help designing reproducible developer environments, taming
complex Nix builds, or shipping reliable Haskell systems, we would love to hear
from you.

- Website: <https://obsidian.systems>
- Blog: <https://blog.obsidian.systems>
- GitHub: <https://github.com/obsidiansystems>

## License

`nix-thunk` is released under the [BSD-3-Clause License](./LICENSE), copyright
2020 Obsidian Systems LLC.
