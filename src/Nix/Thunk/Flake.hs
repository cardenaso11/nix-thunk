-- | Generating the @flake.nix@ and @flake.lock@ of a packed thunk.
--
-- A packed thunk is itself a flake which forwards the outputs of the repository
-- it points at. For a consumer to be able to write
--
-- > inputs.someinput.follows = "mythunk/nixpkgs";       # read
-- > inputs.mythunk.inputs.nixpkgs.follows = "nixpkgs";  # override
--
-- the thunk's flake has to expose upstream's inputs under upstream's own names,
-- which is what "Nix.Thunk.Flake.Flatten" works out and the two renderers write
-- down.
--
-- Everything under here is pure. Fetching upstream, writing the files and
-- having Nix check the result is left to "Nix.Thunk.Internal".
--
-- Reading order, which is also the dependency order:
--
-- * "Nix.Thunk.Flake.Name": what an input may be called, and the rule Nix
--   imposes on names it has to parse.
-- * "Nix.Thunk.Flake.Ref": where a tree comes from, and the two forms of that
--   a lock records.
-- * "Nix.Thunk.Flake.Upstream": the lock we read, and how to walk it.
-- * "Nix.Thunk.Flake.Flatten": the translation from upstream's graph to ours.
-- * "Nix.Thunk.Flake.Render": the @flake.nix@ we write.
-- * "Nix.Thunk.Flake.Lockfile": the @flake.lock@ we write.
--
-- This module re-exports all of them, so a caller can import it alone.
module Nix.Thunk.Flake
  ( module Nix.Thunk.Flake.Name
  , module Nix.Thunk.Flake.Ref
  , module Nix.Thunk.Flake.Upstream
  , module Nix.Thunk.Flake.Flatten
  , module Nix.Thunk.Flake.Render
  , module Nix.Thunk.Flake.Lockfile
  ) where

import Nix.Thunk.Flake.Flatten
import Nix.Thunk.Flake.Lockfile
import Nix.Thunk.Flake.Name
import Nix.Thunk.Flake.Ref
import Nix.Thunk.Flake.Render
import Nix.Thunk.Flake.Upstream
