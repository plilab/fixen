{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compact inferred hash tries. Keys are stored only in the indexes; leaves
-- hold just the remaining fields. All readers share a field-wise traversal.
module Fixen.CodeGen.Cpp.Database (database, comparison, conclusion, scanRows, discreteFields) where

import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Fixen.CodeGen.Cpp.Common
import Fixen.CodeGen.Cpp.Syntax
import Fixen.IR.AST qualified as A
import Fixen.IR.RelationRepresentation
import Prettyprinter

database :: RelationRepresentation -> Gen [Doc ()]
database layouts = do
  if Map.null layouts then unsupported "C++ generation requires at least one relation." else pure ()
  records <- mapM record (Map.toList layouts)
  stores <- mapM (storageType True . storageLayout) (Map.elems layouts)
  operations <- concat <$> mapM relationOperations (Map.toList layouts)
  pure $
    [pretty hashRuntime]
      ++ records
      ++ [ "using Fact =" <+> pretty (template "std::variant" (Map.keys layouts)) <> semi
         , block "struct Database" [pretty t <+> pretty (factsField n) <> "{};" | (n, t) <- zip (Map.keys layouts) stores] <> semi
         ]
      ++ operations
      ++ [pretty runtime]
  where
    record (n, layout) = do
      _ <- identifier n
      ts <- mapM (lowerType . snd) (_factTypes (_factRepresentation layout))
      pure $ block ("struct" <+> pretty n) [pretty t <+> pretty ("arg" <> T.show i) <> semi | (i, t) <- zip [0 :: Int ..] ts] <> semi

-- | The extraction map maps storage positions to original argument positions.
discreteFields :: RelationRepresentationInfo -> [(Int, A.Type)]
discreteFields layout =
  [(i, t) | (i, (Match, _, t)) <- zip (IM.elems (_extractionMap db)) (_databaseTypes db)]
  where
    db = _databaseRepresentation layout

type StoredField = (Int, QueryType, A.Type)

-- Like the Haskell layout: a terminal discrete field is a set, and ordered
-- suffixes contain no discrete keys. Only a root singleton needs optionality;
-- under a map, absence is represented by the missing key itself.
data Storage
  = Presence
  | KeySet Int A.Type
  | KeyMap Int A.Type Storage
  | Singleton [StoredField]
  | Antichain [StoredField]

storageLayout :: RelationRepresentationInfo -> Storage
storageLayout layout = build [(i, q, t) | (i, (q, _, t)) <- zip (IM.elems (_extractionMap db)) (_databaseTypes db)]
  where
    db = _databaseRepresentation layout
    build [] = Presence
    build [(i, Match, t)] = KeySet i t
    build ((i, Match, t) : rest) = KeyMap i t (build rest)
    build fields@((_, LatticeMeet {}, _) : _) = Singleton fields
    build fields = Antichain fields

storageType :: Bool -> Storage -> Gen Text
storageType _ Presence = pure "bool"
storageType _ (KeySet _ t) = do
  key <- lowerType t
  pure (template "std::unordered_set" [key, template "fx_Hash" [key]])
storageType _ (KeyMap _ t rest) = do
  key <- lowerType t
  value <- storageType False rest
  pure (template "std::unordered_map" [key, value, template "fx_Hash" [key]])
storageType root (Singleton fields) = do
  value <- packedType fields
  pure (if root then template "std::optional" [value] else value)
storageType _ (Antichain fields) = template "std::vector" . pure <$> packedType fields

packedType :: [StoredField] -> Gen Text
packedType fields = do
  types <- mapM (\(_, _, t) -> lowerType t) fields
  pure (case types of [t] -> t; _ -> template "std::tuple" types)

packFields :: [StoredField] -> Expr -> Gen Expr
packFields [(i, _, _)] value = pure (field value i)
packFields fields value = do
  t <- packedType fields
  pure (Construct t [field value i | (i, _, _) <- fields])

unpackFields :: [StoredField] -> Expr -> Map Int Expr
unpackFields [(i, _, _)] value = Map.singleton i value
unpackFields fields value = Map.fromList [(i, call (template "std::get" [T.show position]) [value]) | (position, (i, _, _)) <- zip [0 :: Int ..] fields]

-- | Bound keys use find (never operator[], which would mutate a query).
-- Unbound keys enumerate that level; later bound keys still use lookups.
-- The callback receives references to fields in original argument order,
-- avoiding reconstruction/copying of whole facts during internal operations.
scanRows :: Text -> RelationRepresentationInfo -> Expr -> Map Int Expr -> (Map Int Expr -> Gen [Stmt]) -> Gen [Stmt]
scanRows prefix layout storage bound continuation = descend 0 storage Map.empty (storageLayout layout)
  where
    descend :: Int -> Expr -> Map Int Expr -> Storage -> Gen [Stmt]
    descend depth index values (KeyMap i _ rest) = do
      let local = prefix <> "_index" <> T.show depth
      case Map.lookup i bound of
        Just key -> do
          let entry = Unary "*" (Name local)
          body <- descend (depth + 1) (Member entry "second") (Map.insert i (Member entry "first") values) rest
          pure [Scope [Declare "const auto" local (Call (Member index "find") [key]), If (Binary "!=" (Name local) (Call (Member index "end") [])) body]]
        Nothing -> do
          body <- descend (depth + 1) (Member (Name local) "second") (Map.insert i (Member (Name local) "first") values) rest
          pure [For ("const auto& " <> local) index body]
    descend depth index values (KeySet i _) = do
      let local = prefix <> "_index" <> T.show depth
      case Map.lookup i bound of
        Just key -> do
          body <- continuation (Map.insert i (Unary "*" (Name local)) values)
          pure [Scope [Declare "const auto" local (Call (Member index "find") [key]), If (Binary "!=" (Name local) (Call (Member index "end") [])) body]]
        Nothing -> do
          body <- continuation (Map.insert i (Name local) values)
          pure [For ("const auto& " <> local) index body]
    descend _ index values Presence = do
      body <- continuation values
      pure [If index body]
    descend depth index values (Singleton fields) = do
      let value = if depth == 0 then Unary "*" index else index
      body <- continuation (Map.union values (unpackFields fields value))
      pure (if depth == 0 then [If (Call (Member index "has_value") []) body] else body)
    descend _ index values (Antichain fields) = do
      let row = prefix <> "_row"
      body <- continuation (Map.union values (unpackFields fields (Name row)))
      pure [For ("const auto& " <> row) index body]

comparison :: QueryType -> Expr -> Expr -> Gen Expr
comparison Match a b = pure (Binary "==" a b)
comparison (Meet leq _) a b = (`call` [a, b]) <$> operation leq
comparison (LatticeMeet leq _ _) a b = (`call` [a, b]) <$> operation leq

conclusion :: RelationRepresentation -> Map Text Expr -> A.Conclusion -> Gen Expr
conclusion layouts names c = do
  let n = A.simpleIdentifier (A.relationLikeName c)
      fields = _factTypes (_factRepresentation (layouts Map.! n))
  args <- sequence (zipWith (lowerExpr names . Just . snd) fields (A.relationLikeArgs c))
  pure (Construct n args)

relationOperations :: (Text, RelationRepresentationInfo) -> Gen [Doc ()]
relationOperations (n, layout) = do
  comparisons <- sequence [comparison q (field (Name "a") i) (field (Name "b") i) | (i, (q, _)) <- numbered]
  let storage = Member (Name "db") (factsField n)
      bound = Map.fromList [(i, field (Name "f") i) | (i, _) <- discreteFields layout]
  entailment <- scanRows "fx_scan" layout storage bound $ \old -> do
    conditions <- sequence [comparison q (field (Name "f") i) (old Map.! i) | (i, (q, _)) <- numbered]
    pure [If (andExpr conditions) [Return (Name "true")]]
  contour <-
    if any (isLattice . fst . snd) numbered
      then scanRows "fx_scan" layout storage bound $ \old -> mergeFields old numbered []
      else pure []
  insertBody <- insertFields storage (storageLayout layout)
  let typed arg = "const " <> n <> "& " <> arg
      leq = function "inline bool" "fx_leq" [typed "a", typed "b"] [Return (andExpr comparisons)]
      entails =
        function
          "inline bool"
          "fx_entails"
          ["const Database& db", typed "f"]
          (entailment ++ [Return (Name "false")])
      insertion =
        block ("inline void fx_insert(Database& db, [[maybe_unused]]" <+> pretty (typed "f") <> ")") insertBody
      merge =
        function "inline std::vector<Fact>" "fx_contour" [typed "f", "const Database& db"] $
          [Declare "std::vector<Fact>" "result" (Construct "std::vector<Fact>" [Name "f"])]
            ++ contour
            ++ [Return (Name "result")]
  pure [leq, entails, insertion, merge]
  where
    numbered = zip [0 ..] (_factTypes (_factRepresentation layout))
    insertFields index Presence = pure [expr index <+> "= true;"]
    insertFields index (KeySet i _) = pure [stmt (Statement (Call (Member index "insert") [field (Name "f") i]))]
    insertFields index (KeyMap i _ (Singleton fields)) = do
      value <- packFields fields (Name "f")
      -- operator[] would unnecessarily require a default-constructible
      -- lattice value. A key and its complete value are installed together.
      pure [stmt (Statement (Call (Member index "insert_or_assign") [field (Name "f") i, value]))]
    insertFields index (KeyMap i _ rest) = insertFields (Index index (field (Name "f") i)) rest
    insertFields index (Singleton fields) = do
      value <- packFields fields (Name "f")
      pure [expr index <+> equals <+> expr value <> semi]
    insertFields index (Antichain fields) = do
      value <- packFields fields (Name "f")
      let old = unpackFields fields (Name "old")
      conditions <- sequence [comparison q (old Map.! i) (field (Name "f") i) | (i, q, _) <- fields]
      pure
        [ "auto& rows =" <+> expr index <> semi
        , "rows.erase(std::remove_if(rows.begin(), rows.end(), [&](const auto& old) { return" <+> expr (andExpr conditions) <> "; }), rows.end());"
        , stmt (Statement (call "rows.push_back" [value]))
        ]
    isLattice LatticeMeet {} = True
    isLattice _ = False
    mergeFields _ [] values = pure [Statement (call "result.emplace_back" [Construct n (reverse values)])]
    mergeFields old ((i, (q, _)) : rest) values = do
      let incoming = field (Name "f") i
          stored = old Map.! i
          joined = "fx_joined" <> T.show i
      case q of
        Match -> do
          body <- mergeFields old rest (incoming : values)
          pure [If (Binary "==" incoming stored) body]
        Meet {} -> do
          f <- operation (refinementOperation q)
          body <- mergeFields old rest (Name joined : values)
          pure [For ("const auto& " <> joined) (call f [stored, incoming]) body]
        LatticeMeet _ join _ -> do
          f <- operation join
          body <- mergeFields old rest (Name joined : values)
          pure (Declare "const auto" joined (call f [stored, incoming]) : body)

-- No specializations are added to namespace std for standard-library types.
-- Foreign scalar keys customize std::hash; composite keys recurse through it.
hashRuntime :: Text
hashRuntime =
  """
  template<class T> struct fx_Hash {
    std::size_t operator()(const T& value) const { return std::hash<T>{}(value); }
  };
  inline void fx_hashCombine(std::size_t& seed, std::size_t value) {
    seed ^= value + std::size_t{0x9e3779b9} + (seed << 6) + (seed >> 2);
  }
  template<class T, class Allocator> struct fx_Hash<std::vector<T, Allocator>> {
    std::size_t operator()(const std::vector<T, Allocator>& values) const {
      std::size_t seed = values.size();
      for (const auto& value : values) fx_hashCombine(seed, fx_Hash<T>{}(value));
      return seed;
    }
  };
  template<class T, std::size_t N> struct fx_Hash<std::array<T, N>> {
    std::size_t operator()(const std::array<T, N>& values) const {
      std::size_t seed = N;
      for (const auto& value : values) fx_hashCombine(seed, fx_Hash<T>{}(value));
      return seed;
    }
  };
  template<class... Ts> struct fx_Hash<std::tuple<Ts...>> {
    std::size_t operator()(const std::tuple<Ts...>& values) const {
      std::size_t seed = 0;
      std::apply([&](const auto&... value) {
        (fx_hashCombine(seed, fx_Hash<std::decay_t<decltype(value)>>{}(value)), ...);
      }, values);
      return seed;
    }
  };
  """

runtime :: Text
runtime =
  """
  inline bool factLeq(const Fact& a, const Fact& b) {
    return std::visit([&](const auto& value) {
      using T = std::decay_t<decltype(value)>;
      const auto* other = std::get_if<T>(&b);
      return other && fx_leq(value, *other);
    }, a);
  }
  inline bool entails(const Database& db, const Fact& fact) {
    return std::visit([&](const auto& value) { return fx_entails(db, value); }, fact);
  }
  inline void insertToDb(Database& db, const Fact& fact) {
    std::visit([&](const auto& value) { fx_insert(db, value); }, fact);
  }
  inline std::vector<Fact> mergeContour(const Fact& fact, const Database& db) {
    return std::visit([&](const auto& value) { return fx_contour(value, db); }, fact);
  }
  inline std::vector<Fact> maximalContour(const std::vector<Fact>& facts) {
    std::vector<Fact> result;
    for (const auto& fact : facts) {
      if (std::any_of(result.begin(), result.end(), [&](const auto& old) { return factLeq(fact, old); })) continue;
      result.erase(std::remove_if(result.begin(), result.end(), [&](const auto& old) { return factLeq(old, fact); }), result.end());
      result.push_back(fact);
    }
    return result;
  }
  """
