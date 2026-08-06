-- | The names an input can be bound to, and the rule Nix imposes on them.
--
-- Two types, and the boundary between them runs through the whole package. An
-- 'InputName' is whatever a flake called something, and Nix accepts anything
-- as one. A 'FlakeId' is an 'InputName' that Nix will also accept where it
-- /parses/ one, which a @follows@ and the attribute path of an override both
-- are.
--
-- Names read out of upstream's lock are 'InputName's, because upstream chose
-- them and an override has to match them exactly. Names the generated flake
-- binds are 'FlakeId's, because they exist to be followed.
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

-- | The name an input is bound to, either as an edge label within a lock or as
-- a root-level input of the flake being generated.
--
-- Whatever a flake called something. Nix accepts any attribute name here, so
-- this is the unconstrained end: see 'FlakeId' for the other.
newtype InputName = InputName {unInputName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey)

-- | An 'InputName' that Nix will accept where it parses one, which is to say
-- one matching @[a-zA-Z][a-zA-Z0-9_-]*@.
--
-- A refinement rather than a sibling: every 'FlakeId' is an 'InputName', and
-- 'flakeIdName' gets it back for free.
--
-- Nix does not require this of a name being /declared/, which is how upstream
-- comes to have inputs nobody can refer to: haskell.nix has fourteen,
-- including @hls-1.10@.
newtype FlakeId = FlakeId {flakeIdName :: InputName}
  deriving stock (Eq, Ord, Show)

-- | A @follows@ target: a sequence of input names walked from the root node.
--
-- Parameterised by the kind of name, because the two directions differ. A path
-- read out of upstream's lock is 'InputName's, taken as we find them. A path
-- we write is 'FlakeId's, since Nix will parse it back as identifiers, and
-- building one out of anything else is the mistake this is here to prevent.
newtype FollowsPath name = FollowsPath {unFollowsPath :: [name]}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON)

-- | Anything that names an input, for the code that only needs the text of it.
--
-- Rendering and JSON encoding are the same whichever side of the boundary a
-- name came from, and this is what lets them say so, rather than either
-- unwrapping at every call or taking 'InputName' and losing the distinction
-- everywhere upstream of them.
class IsInputName name where
  inputName :: name -> InputName

instance IsInputName InputName where
  inputName = id

instance IsInputName FlakeId where
  inputName = flakeIdName

--------------------------------------------------------------------------------
-- Flake identifiers
--------------------------------------------------------------------------------

-- | The name as it stands, when Nix will accept it, and 'Nothing' when it will
-- not.
--
-- The only way to obtain a 'FlakeId' from a name somebody else chose. Where a
-- name has to be reproduced exactly, as an override attribute path does, this
-- failing is the end of the matter and the edge gets dropped.
flakeId :: InputName -> Maybe FlakeId
flakeId name
  | isFlakeId name = Just $ FlakeId name
  | otherwise = Nothing

-- | The nearest name to this one that Nix will accept.
--
-- For names that are ours to choose, where being able to say the name matters
-- more than matching upstream's spelling of it. Never for a name that has to
-- be matched: see 'flakeId'.
--
-- @hls-1.10@ comes out as @hls-1_10@. A name starting with something other
-- than a letter is prefixed rather than repaired, since the first character is
-- the one position with a rule of its own; the substitution then runs over the
-- whole string, first character included, which is safe because the prefix has
-- already taken care of the start.
--
-- This can manufacture a collision upstream did not have: @a.b@ and @a_b@ both
-- arrive at @a_b@, and 'freshInputName' gives the loser @a_b_2@, a name
-- upstream never used. That is the one point where exposing upstream's names
-- degrades quietly rather than visibly.
toFlakeId :: InputName -> FlakeId
toFlakeId name = fromMaybe (FlakeId sanitised) $ flakeId name
  where
    text = unInputName name
    sanitised = InputName $ leadingLetter <> T.map keepOrReplace text
    leadingLetter = if maybe False (isFlakeIdStart . fst) (T.uncons text) then "" else "input-"
    keepOrReplace c = if isFlakeIdChar c then c else '_'

-- | The rule itself, for a caller that only needs the answer. 'flakeId' is
-- this plus the evidence.
isFlakeId :: InputName -> Bool
isFlakeId name = case T.uncons $ unInputName name of
  Just (leading, rest) -> isFlakeIdStart leading && T.all isFlakeIdChar rest
  Nothing -> False

-- | Nix's rule is @[a-zA-Z]@, so the 'isAscii' guard is doing work: 'isAlpha'
-- is true of @é@ and Nix's parser is not.
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

-- | Suffix a name until it is one nobody has taken. A suffixed identifier is
-- still an identifier, so this cannot leave the refinement.
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

-- | The name a thunk of a repository that is not a flake gives its one input.
sourceOnlyInputName :: FlakeId
sourceOnlyInputName = toFlakeId $ InputName "src"

--------------------------------------------------------------------------------
-- Names as text
--------------------------------------------------------------------------------

-- | A name as text, for the places that are about to quote or encode it. This
-- and its two callers are all 'IsInputName' earns: a handful of leaves that do
-- not care which side of the boundary a name came from, so that everything
-- above them can.
nameText :: IsInputName name => name -> Text
nameText = unInputName . inputName
