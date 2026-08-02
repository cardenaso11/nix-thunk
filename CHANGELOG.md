# Revision history for nix-thunk

# Unreleased

* Packed thunks are now flakes. Alongside `default.nix` and `thunk.nix`, a
  packed thunk carries a generated `flake.nix` and `flake.lock`, so a thunk can
  be used as a flake input:

  ```nix
  inputs.mythunk.url = "path:./dep/mythunk";
  ```

  The thunk's flake forwards the outputs of the repository it points at, and
  exposes that repository's own inputs under their own names, each pinned to
  the revision upstream locked. Both directions of `follows` therefore work:

  ```nix
  inputs.someinput.follows = "mythunk/nixpkgs";        # read
  inputs.mythunk.inputs.nixpkgs.follows = "nixpkgs";   # override
  ```

  When the repository is not a flake, the generated flake exposes the fetched
  source as `src` instead.

  The flake interface survives `unpack` and `worktree`, so a project keeps
  building while a dependency is checked out. For a repository that is not a
  flake, the same files are written into the checkout and hidden through git's
  `info/exclude`; `pack` removes them again, nothing is written if the
  repository has a `flake.nix` or `flake.lock` of its own, and a `flake.nix` you
  wrote yourself is left alone.

  This introduces the `github-v9` and `git-v10` thunk specs. They fetch exactly
  as `github-v8` and `git-v9` do, and differ only in carrying the flake files.
  Please update all your thunks: `nix-thunk unpack $path; nix-thunk pack $path`.

  **Breaking:** the two extra files mean a packed thunk no longer matches the
  `thunkSource` in older, vendored copies of this repository's `default.nix`,
  which reject any file they do not recognise. Projects pinning an older
  nix-thunk for their Nix code need to update it before updating their thunks.

* `create`, `pack` and `update` accept `--no-flake`, which writes a thunk in
  the newest format that carries no flake files. Such a thunk cannot be used as
  a flake input, so this is for projects that do not want the interface. The
  format is a property of the thunk rather than of the tool: `unpack` keeps
  whichever format the thunk it is unpacking already had, and packing without
  the flag brings a thunk up to the newest one.

* Packing no longer fetches the repositories a flake dependency points at. The
  generated `flake.lock` is written from upstream's own lock, which already
  records every reference and hash it needs, rather than by running
  `nix flake lock` over inputs Nix would have to resolve one at a time. Packing
  a thunk of a repository with a large dependency graph no longer downloads
  that graph, and no longer fails when one of its pins is unreachable.

  Nix is still asked when an input could not be restated, and then packing
  costs what it always did. That happens when upstream declares an input as a
  path outside its own repository, and when it gives one a name that Nix will
  not accept in a `follows`: haskell.nix, for instance, has inputs called
  things like `hls-1.10`, which a flake may declare but nothing may refer to.

* The attribute cache moved out of the thunk directory, into
  `$XDG_CACHE_HOME/nix-thunk`. A packed thunk is now also read as a flake
  input, so everything inside it is hashed into a consumer's `flake.lock`, and
  building an attribute of a thunk would otherwise have changed the thunk. A
  leftover `.attr-cache` directory is still accepted, so existing thunks keep
  reading; nothing writes to it any more, and it can be deleted.

* Drop GHC older than 9.12. `Nix.Thunk.Flake` uses `MultilineStrings`, which
  requires 9.12. Also bumps the `haskell.nix` pin.

* Add a `--version` option to the `nix-thunk` executable.

## 0.7.3.0

* Add `runMonadNixThunk` to make it easier to run the actions defined in the library.

## 0.7.2.2

* Support GHC 9.12

## 0.7.2.1
* Loosen version bounds
* Fix issue where thunk branches aren't updated when the user specifies --branch on nix-thunk update
* Swap default.nix and lib.nix

## 0.7.2.0
* Support retrieving revs that aren't on the default branch when a branch isn't specified.  To use this functionality, update your thunks.

## 0.7.1.0
* Allow specifying `--rev` when doing `update` to update to a specific revision.

## 0.7.0.1

* Support GHC 9.6

## 0.7.0.0

* Caching now works
* [#42](https://github.com/obsidiansystems/nix-thunk/pull/42) Thunk read errors are now presented in a more informative manner.
* [#43](https://github.com/obsidiansystems/nix-thunk/pull/43) `nix-thunk` will now ensure that any `git` processes invoked during its execution have a clean configuration.
  This prevents `nix-thunk` crashing when e.g. the user's configuration `git` is valid only in a version newer than what `nix-thunk` links against, and works towards making thunks more reproducible by ensuring that thunk URIs are resolvable independently of the user's environment.

## 0.6.1.0

* [#36](https://github.com/obsidiansystems/nix-thunk/pull/36) Expose the internals of the `nix-thunk` library.

## 0.6.0.0

* [#34](https://github.com/obsidiansystems/nix-thunk/pull/34) Fix an
  issue where thunks could not be fetched without `nix-thunk` (or one of
  its dependents, e.g. `obelisk`) being installed. Please update all
  your thunks to use the new v8 thunk spec.

  Updating your thunk can be done by running `nix-thunk unpack $path; nix-thunk pack $path`.

* [#35](https://github.com/obsidiansystems/nix-thunk/pull/35) Determine remote using git-config when `branch.<name>.merge` option is set
  (Fixes [obelisk#792](https://github.com/obsidiansystems/obelisk/issues/792).)

## 0.5.1.0

* Bump to cli-nix 0.2.0.0; This ensures that `nix-prefetch-git` can always be found.

## 0.5.0.0

* Fix a critical bug where v6 thunks can not be used to fetch non-GitHub repositories. Please update all your thunks to use the new v7 thunk spec.
  Updating your thunk can be done by running `nix-thunk unpack $path; nix-thunk pack $path`.
* Building a functional `nix-thunk` _must_ be done using the included Nix derivation.

## 0.4.0.0

* The default thunk specification ("v6") now uses a pinned version of nixpkgs, rather than the magic `<nixpkgs>`, for fetching thunks. This ensures that thunks can be fetched even in an environment where `NIX_PATH` is unset.

## 0.3.0.0

* Fix readThunk when thunk is checked out [#4](https://github.com/obsidiansystems/nix-thunk/pull/4)
* Fix removal of .git from default destination [#10](https://github.com/obsidiansystems/nix-thunk/pull/10)

## 0.2.0.3

* Default to GHC 8.8.4 and update dependency bounds

## 0.2.0.2
* Add support for GHC 8.8.4.

## 0.2.0.1
* Fix parsing of --rev arguments

## 0.2.0.0
* Add nix-thunk create.  This caused some minor breakage to the Haskell library API, but not the Nix or command line interfaces.

## 0.1.0.0
* Initial release.  Extracted the Nix part of this code from https://github.com/obsidiansystems/reflex-platform and the Haskell part from https://github.com/obsidiansystems/obelisk
