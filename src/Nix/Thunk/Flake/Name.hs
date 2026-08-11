-- | This module defines the names that an input can take, and the rule that Nix
-- imposes on them.
--
-- An 'InputName' is any name that a flake gave an input, and Nix accepts any
-- name here. A 'FlakeId' is an 'InputName' that Nix also accepts where it
-- /parses/ a name. A @follows@ target is one such place. The
-- @--override-input@ argument is another such place. An override inside a
-- @flake.nix@ is an attribute name. Nix accepts any name in that place.
--
-- A name that we read from upstream's lock is an 'InputName'. Upstream chose
-- it, and an override must match it exactly. A name that the generated flake
-- binds is a 'FlakeId'. It exists so a @follows@ can point at it.
module Nix.Thunk.Flake.Name where

import Data.Aeson qualified as Aeson
import Data.Char (isAlpha, isAscii, isDigit)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The name of an input, either as an edge label in a lock or as a root-level
-- input of the flake that we generate.
--
-- Nix accepts any attribute name here, so this type carries no constraint.
newtype InputName = InputName {unInputName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey)

-- | An 'InputName' that matches @[a-zA-Z][a-zA-Z0-9_-]*@. Nix accepts this form
-- wherever it parses a name.
--
-- Nix does not impose this rule where a flake /declares/ a name. So upstream
-- can have an input that nobody can name. haskell.nix has fourteen such inputs,
-- and one is @hls-1.10@.
newtype FlakeId = FlakeId {flakeIdName :: InputName}
  deriving stock (Eq, Ord, Show)

-- | A @follows@ target: a sequence of input names that we walk from the root
-- node.
--
-- A path that we read from upstream's lock holds 'InputName's, and we take each
-- name as we find it. A path that we write holds 'FlakeId's, because Nix parses
-- it back as identifiers.
newtype FollowsPath name = FollowsPath {unFollowsPath :: [name]}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON)

-- | Anything that names an input, for the code that needs only its text.
--
-- Rendering and JSON encoding do not depend on the side of the boundary that a
-- name came from.
class IsInputName name where
  inputName :: name -> InputName

instance IsInputName InputName where
  inputName = id

instance IsInputName FlakeId where
  inputName = flakeIdName

--------------------------------------------------------------------------------
-- Flake identifiers
--------------------------------------------------------------------------------

-- | The name unchanged as a 'FlakeId' when Nix accepts it, and 'Nothing' when
-- Nix does not.
--
-- Use this function for a name that a @follows@ must reach with upstream's own
-- spelling. We cannot repair such a name, so we drop it.
flakeId :: InputName -> Maybe FlakeId
flakeId name
  | isFlakeId name = Just $ FlakeId name
  | otherwise = Nothing

-- | A name that Nix accepts, with the smallest change that this function can
-- make.
--
-- Use this function for a name that we choose. For those names, a form that
-- Nix can parse matters more than upstream's exact spelling. Never use this
-- function for a name that must match upstream. Use 'flakeId' for that name.
--
-- Example: @hls-1.10@ becomes @hls-1_10@. A name that starts with any character
-- other than a letter also gets the prefix @input-@.
--
-- This function can create a collision that upstream did not have. @a.b@ and
-- @a_b@ both become @a_b@. 'freshInputName' then renames the second one to
-- @a_b_2@, a name that upstream never used.
toFlakeId :: InputName -> FlakeId
toFlakeId name = fromMaybe (FlakeId sanitised) $ flakeId name
  where
    text = unInputName name
    sanitised = InputName $ leadingLetter <> T.map keepOrReplace text
    leadingLetter = if maybe False (isFlakeIdStart . fst) (T.uncons text) then "" else "input-"
    keepOrReplace c = if isFlakeIdChar c then c else '_'

-- | The rule of 'FlakeId' as a predicate. 'flakeId' applies the same rule, and
-- it also returns the name as a 'FlakeId'.
isFlakeId :: InputName -> Bool
isFlakeId name = case T.uncons $ unInputName name of
  Just (leading, rest) -> isFlakeIdStart leading && T.all isFlakeIdChar rest
  Nothing -> False

-- | Nix's rule is @[a-zA-Z]@, so the 'isAscii' guard matters. 'isAlpha' accepts
-- @é@, and Nix's parser rejects it.
isFlakeIdStart :: Char -> Bool
isFlakeIdStart c =
  and
    [ isAscii c
    , isAlpha c
    ]

isFlakeIdChar :: Char -> Bool
isFlakeIdChar c =
  or
    [ isFlakeIdStart c
    , and
        [ isAscii c
        , isDigit c
        ]
    , c == '_'
    , c == '-'
    ]

-- | Adds a suffix to a name until no other input holds that name. A name with a
-- suffix is still an identifier, so the result is always a 'FlakeId'.
freshInputName :: Set FlakeId -> FlakeId -> FlakeId
freshInputName taken base = go (0 :: Int)
  where
    go n
      | candidate `Set.member` taken = go (n + 1)
      | otherwise = candidate
      where
        candidate
          | n == 0 = base
          | otherwise = FlakeId $ InputName $ nameText base <> "_" <> T.pack (show $ n + 1)

-- | Some repositories are not flakes. A thunk of such a repository has one
-- input, and this is the name of that input.
sourceOnlyInputName :: FlakeId
sourceOnlyInputName = toFlakeId $ InputName "src"

--------------------------------------------------------------------------------
-- Names as text
--------------------------------------------------------------------------------

-- | A name as text, for a place that quotes or encodes it.
nameText :: IsInputName name => name -> Text
nameText = unInputName . inputName
