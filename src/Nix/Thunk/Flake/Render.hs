-- | Turning a 'FlattenedFlake' into @flake.nix@ source, and the two trivial
-- flakes that need no flattening at all.
module Nix.Thunk.Flake.Render where

import Data.Foldable (fold, toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Nix.Thunk.Flake.Flatten
import Nix.Thunk.Flake.Name
import Nix.Thunk.Flake.Ref

-- | Render the @flake.nix@ of a packed thunk whose upstream is itself a flake.
renderFlakeNix :: FlattenedFlake -> Text
renderFlakeNix flake =
  T.unlines $
    fold @[]
      [
        [ "# DO NOT HAND-EDIT THIS FILE"
        , "{"
        , "  inputs = {"
        ]
      , renderRefEntry
          flake.flattenedFlake_sourceName
          -- The thunk's source is always a flake here, so no `flake = false`.
          (unFetchableRef flake.flattenedFlake_sourceRefs.nodeRefs_original)
          flake.flattenedFlake_sourceEdges
      , fold $ Map.mapWithKey renderInputEntry flake.flattenedFlake_inputs
      ,
        [ "  };"
        , "  outputs = inputs: inputs."
            <> renderName flake.flattenedFlake_sourceName
            <> ".outputs;"
        , "}"
        ]
      ]

renderInputEntry :: FlakeId -> FlattenedInput -> [Text]
renderInputEntry name input = case input.flattenedInput_ref of
  -- An input carrying a `follows` may not also carry a flake reference, so an
  -- alias is emitted on its own.
  Left followsPath -> renderAliasEntry name followsPath
  Right refs ->
    renderRefEntry
      name
      (withFlakeAttr input.flattenedInput_isFlake refs.nodeRefs_original)
      input.flattenedInput_edges

-- | Nix takes an input to be a flake unless told otherwise, so only the
-- negative case is worth recording.
--
-- Takes a 'FetchableRef' and gives back a plain 'FlakeRef', because @flake@ is
-- not a fetchable attribute: it says how to read the tree rather than where to
-- get it. A flake input carries it and a lock entry does not, which is why the
-- lock records it as a field of the node instead.
withFlakeAttr :: Bool -> FetchableRef -> FlakeRef
withFlakeAttr isFlake (FetchableRef flakeRef)
  | isFlake = flakeRef
  | otherwise = FlakeRef $ Map.insert (AttrName "flake") (FlakeRefValue_Bool False) $ unFlakeRef flakeRef

renderRefEntry :: IsInputName name => name -> FlakeRef -> Map InputName FlakeId -> [Text]
renderRefEntry name flakeRef inputEdges =
  fold @[]
    [ ["    " <> renderName name <> " = {"]
    , toList $ Map.mapWithKey renderAttr $ unFlakeRef flakeRef
    , renderEdges inputEdges
    , ["    };"]
    ]

renderAliasEntry :: IsInputName name => name -> FollowsPath FlakeId -> [Text]
renderAliasEntry name followsPath =
  [ "    " <> renderName name <> " = {"
  , "      follows = " <> nixString (renderFollowsPath followsPath) <> ";"
  , "    };"
  ]

renderEdges :: Map InputName FlakeId -> [Text]
renderEdges inputEdges
  | Map.null inputEdges = []
  | otherwise =
      fold @[]
        [ ["      inputs = {"]
        , toList $ Map.mapWithKey renderEdge inputEdges
        , ["      };"]
        ]

-- | One override. The two names come from opposite sides of the boundary, and
-- this is the line where they meet: upstream's on the left, since that is what
-- the override has to name, and ours on the right, since that is what Nix will
-- follow.
renderEdge :: (IsInputName from, IsInputName to) => from -> to -> Text
renderEdge name target =
  "        "
    <> renderName name
    <> " = { follows = "
    <> renderName target
    <> "; };"

renderAttr :: AttrName -> FlakeRefValue -> Text
renderAttr name value = "      " <> unAttrName name <> " = " <> renderRefValue value <> ";"

-- | A reference as a standalone attribute set, for handing to
-- @builtins.fetchTree@. Fetching the repository the same way the generated
-- flake will also proves, at pack time, that the reference resolves.
renderFlakeRefExpr :: FlakeRef -> Text
renderFlakeRefExpr flakeRef = "{ " <> fold (Map.mapWithKey attr $ unFlakeRef flakeRef) <> "}"
  where
    attr name value = unAttrName name <> " = " <> renderRefValue value <> "; "

renderFollowsPath :: IsInputName name => FollowsPath name -> Text
renderFollowsPath = T.intercalate "/" . fmap nameText . unFollowsPath

-- | A name as Nix source. Takes either kind, since quoting an identifier and
-- quoting a name upstream chose are the same operation.
renderName :: IsInputName name => name -> Text
renderName = nixString . nameText

renderRefValue :: FlakeRefValue -> Text
renderRefValue = \case
  FlakeRefValue_String s -> nixString s
  FlakeRefValue_Bool b -> if b then "true" else "false"
  FlakeRefValue_Int n -> T.pack $ show n

nixString :: Text -> Text
nixString t = "\"" <> T.concatMap escapeNixChar t <> "\""

escapeNixChar :: Char -> Text
escapeNixChar = \case
  '"' -> "\\\""
  '\\' -> "\\\\"
  '$' -> "\\$"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c -> T.singleton c

-- | The @flake.nix@ of a packed thunk whose upstream is not a flake. There are
-- no inputs to expose, so this exposes the fetched source and nothing else. The
-- content is the same for every such thunk.
--
-- The source is declared as an input rather than fetched by reusing
-- @.\/thunk.nix@: that loader reaches @(import nixpkgs {}).fetchgit@ on its
-- common path, and importing nixpkgs needs @builtins.currentSystem@, which
-- does not exist under the pure evaluation a flake is read with.
renderSourceOnlyFlakeNix :: FlakeRef -> Text
renderSourceOnlyFlakeNix srcRef =
  T.unlines $
    fold @[]
      [
        [ "# DO NOT HAND-EDIT THIS FILE"
        , "{"
        , "  description = \"nix-thunk packed thunk\";"
        , "  inputs = {"
        ]
      , renderRefEntry
          sourceOnlyInputName
          (withFlakeAttr False $ fetchableRef srcRef)
          Map.empty
      ,
        [ "  };"
        , "  outputs = { self, src }: { inherit src; };"
        , "}"
        ]
      ]

-- | The @flake.nix@ written into an unpacked checkout of a repository that is
-- not itself a flake, so that a consumer using the thunk as a flake input is
-- not broken by unpacking it. The checkout is the source, so unlike
-- 'renderSourceOnlyFlakeNix' there is nothing left to fetch.
--
-- @src@ is @sourceInfo@ rather than @outPath@ because that is what the packed
-- thunk's @src@ is: there it is a @flake = false@ input, and Nix binds such an
-- input to the fetched tree's attributes. A consumer reading @src.outPath@ or
-- @src.narHash@ has to keep working across a pack or an unpack, which is the
-- whole point of writing this file at all.
--
-- Whatever this says, @default.nix@ has to recognise byte for byte, so that
-- @thunkSource@ can keep it out of an unpacked dependency's source. Change one
-- and change the other.
unpackedSourceFlakeNix :: Text
unpackedSourceFlakeNix =
  """
  # DO NOT HAND-EDIT THIS FILE
  {
    description = "nix-thunk unpacked thunk";
    outputs = { self }: { src = self.sourceInfo; };
  }
  """
