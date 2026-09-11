{-# LANGUAGE OverloadedStrings #-}

module Fixen.CodeGen.Cpp.Common where

import Control.Monad (unless)
import Data.Char (isAlphaNum, isAscii, isLetter, ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Fixen.CodeGen.Common (CodeGenState, isOp)
import Fixen.CodeGen.Cpp.Syntax qualified as C
import Fixen.IR.AST
import Fixen.Monad
import Numeric (showOct)

type Gen a = FixenPass CodeGenState a

unsupported :: Text -> Gen a
unsupported message = failErr (Just "C++ backend") (T.unpack message) [] []

-- Reject names that cannot be exported faithfully rather than silently
-- mangling two distinct Fixen names to the same C++ name.
identifier :: Text -> Gen Text
identifier n = do
  unless (valid && not (n `elem` reserved) && not ("fx_" `T.isPrefixOf` n) && not ("__" `T.isInfixOf` n)) $
    unsupported ("Not a supported C++ identifier: " <> n <> ". Use an ASCII identifier that is not a C++ keyword or prefixed fx_.")
  pure n
  where
    valid = case T.uncons n of
      Just (c, rest) -> isAscii c && (isLetter c || c == '_') && T.all (\x -> isAscii x && (isAlphaNum x || x == '_')) rest
      Nothing -> False
    reserved = T.words "alignas alignof and and_eq asm atomic_cancel atomic_commit atomic_noexcept auto bitand bitor bool break case catch char char8_t char16_t char32_t class compl concept const consteval constexpr constinit const_cast continue co_await co_return co_yield decltype default delete do double dynamic_cast else enum explicit export extern false float for friend goto if inline int long mutable namespace new noexcept not not_eq nullptr operator or or_eq private protected public reflexpr register reinterpret_cast requires return short signed sizeof static static_assert static_cast struct switch synchronized template this thread_local throw true try typedef typeid typename union unsigned using virtual void volatile wchar_t while xor xor_eq"

hostName :: Identifier -> Gen Text
hostName n = do
  let name = fullIdentifier n
      global = "::" `T.isPrefixOf` name
      parts = T.splitOn "::" (if global then T.drop 2 name else name)
  result <- T.intercalate "::" <$> mapM identifier parts
  pure ((if global then "::" else "") <> result)

lowerType :: Type -> Gen Text
lowerType t = case t of
  TypeCpp _ value -> pure value
  TypeName _ n -> hostName n
  _ -> unsupported "Haskell type syntax passed to the C++ generator; use the C++ frontend."

template :: Text -> [Text] -> Text
template n xs = n <> "<" <> T.intercalate ", " xs <> ">"

-- | Native literals/operators retain C++ semantics; the host compiler checks
-- conversions against the relation field type. Only rule variables are renamed.
lowerExpr :: Map Text C.Expr -> Maybe Type -> Expr -> Gen C.Expr
lowerExpr names _expected source = case source of
  ExprCpp _ form children -> do
    args <- mapM (lowerExpr names Nothing) children
    case (form, args) of
      (CppLiteral value, []) -> pure (C.Name value)
      (CppCall, f : xs) -> pure (C.Call f xs)
      (CppMember member, [value]) -> pure (C.Member value member)
      (CppIndex, [value, index]) -> pure (C.Index value index)
      (CppUnary op, [value]) -> pure (C.Unary op value)
      (CppBinary op, [a, b]) -> pure (C.Binary op a b)
      (CppConditional, [c, a, b]) -> pure (C.Conditional c a b)
      (CppConstruct t, values) -> pure (C.Construct t values)
      (CppCast t, [value]) -> pure (C.call (template "static_cast" [t]) [value])
      _ -> unsupported "Invalid C++ expression tree."
  ExprVar _ n -> case Map.lookup (fullIdentifier n) names of
    Just e -> pure e
    Nothing -> C.Name <$> hostName n
  _ -> unsupported "Haskell expression syntax passed to the C++ generator; use the C++ frontend."

operation :: Identifier -> Gen Text
operation n = case lookup (fullIdentifier n) builtinOperations of
  Just op -> pure op
  Nothing | isOp n -> unsupported ("No C++ mapping for operator " <> fullIdentifier n <> "; use a named host function.")
  Nothing -> hostName n

builtinOperations :: [(Text, Text)]
builtinOperations =
  [(op, "std::" <> fn <> "<>{}") | (op, fn) <- [("+", "plus"), ("-", "minus"), ("*", "multiplies"), ("==", "equal_to"), ("<", "less"), ("<=", "less_equal"), (">", "greater"), (">=", "greater_equal"), ("&&", "logical_and"), ("||", "logical_or")]]
    ++ [("!=", "std::not_equal_to<>{}"), ("/", "std::divides<>{}"), ("%", "std::modulus<>{}")]

stringLiteral :: Text -> Text
stringLiteral s = "\"" <> T.concatMap escape s <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | ord c < 32 || ord c == 127 = "\\" <> T.justifyRight 3 '0' (T.pack (showOct (ord c) ""))
    escape c = T.singleton c

utf8Length :: Text -> Int
utf8Length = T.foldl' (\n c -> n + if ord c < 0x80 then 1 else if ord c < 0x800 then 2 else if ord c < 0x10000 then 3 else 4) 0

field :: C.Expr -> Int -> C.Expr
field value i = C.Member value ("arg" <> T.show i)

ruleType :: NodeId -> Text
ruleType i = "fx_Rule" <> T.show i

factsField :: Text -> Text
factsField = ("fx_facts" <>)
