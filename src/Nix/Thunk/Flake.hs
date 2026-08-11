-- | This module generates the @flake.nix@ and @flake.lock@ of a packed thunk.
--
-- A packed thunk is also a flake. It forwards the outputs of the repository
-- that it points at. A consumer must be able to write this:
--
-- > inputs.someinput.follows = "mythunk/nixpkgs";       # read
-- > inputs.mythunk.inputs.nixpkgs.follows = "nixpkgs";  # override
--
-- So the thunk's flake exposes upstream's inputs under upstream's own names.
--
-- Every function in these modules is pure. "Nix.Thunk.Internal" does the impure
-- work:
--
-- * It fetches upstream.
-- * It writes the files.
-- * It asks Nix to check the result.
--
-- The modules, in dependency order:
--
-- * "Nix.Thunk.Flake.Name" defines the names that an input can take, and the
--   rule that Nix imposes on a name that it parses.
-- * "Nix.Thunk.Flake.Ref" defines the origin of a tree, and the two forms that
--   a lock records it in.
-- * "Nix.Thunk.Flake.Upstream" defines the lock that we read, and the way to
--   walk it.
-- * "Nix.Thunk.Flake.Flatten" translates upstream's graph into ours.
-- * "Nix.Thunk.Flake.Render" renders the @flake.nix@ that we write.
-- * "Nix.Thunk.Flake.Lockfile" renders the @flake.lock@ that we write.
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
