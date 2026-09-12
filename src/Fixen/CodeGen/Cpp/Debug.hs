{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell-style trace events and value rendering, emitted only in debug
-- builds. Foreign types opt in through their normal stream insertion operator.
module Fixen.CodeGen.Cpp.Debug (definitions) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Fixen.CodeGen.Common (CodeGenOptions (..))
import Fixen.CodeGen.Cpp.Common (stringLiteral)
import Fixen.CodeGen.Cpp.Syntax (block)
import Fixen.IR.RelationRepresentation
import Prettyprinter

definitions :: CodeGenOptions -> Int -> RelationRepresentation -> [Doc ()]
definitions options count layouts
  | not (codeGenDebug options) = []
  | otherwise =
      [ "inline constexpr bool fx_debugColors =" <+> pretty (boolean (debugColor options)) <> semi
      , "inline constexpr bool fx_debugPhased =" <+> pretty (boolean (count > 1)) <> semi
      , pretty printers
      ]
        ++ [factPrinter n (length (_factTypes (_factRepresentation layout))) | (n, layout) <- Map.toList layouts]
        ++ [pretty events]
  where
    boolean True = "true" :: Text
    boolean False = "false"
    factPrinter n arity =
      block ("inline void fx_debugFact(std::ostream& out, [[maybe_unused]] const" <+> pretty n <> "& value)") $
        ["out <<" <+> pretty (stringLiteral n) <> semi]
          ++ [ "out << ' '; fx_DebugValue<std::decay_t<decltype(value.arg" <> pretty (T.show i) <> ")>>::write(out, value.arg" <> pretty (T.show i) <> ", 11);"
             | i <- [0 .. arity - 1]
             ]

printers :: Text
printers =
  """
  template<class T, class = void> struct fx_DebugStreamable : std::false_type {};
  template<class T> struct fx_DebugStreamable<T, std::void_t<decltype(std::declval<std::ostream&>() << std::declval<const T&>())>> : std::true_type {};
  template<class T> struct fx_DebugValue {
    static void write(std::ostream& out, const T& value, [[maybe_unused]] int precedence = 0) {
      if constexpr (fx_DebugStreamable<T>::value) {
        if constexpr (std::is_arithmetic_v<T>) {
          bool parens = false;
          if constexpr (std::is_signed_v<T>) parens = precedence > 6 && value < 0;
          if constexpr (std::is_floating_point_v<T>) parens = precedence > 6 && std::signbit(value);
          if (parens) out << '(';
          out << value;
          if (parens) out << ')';
        } else {
          out << value;
        }
      } else {
        static_assert(fx_DebugStreamable<T>::value,
          "Fixen debug traces require operator<<(std::ostream&, const T&) for custom field types; define it in a cpp block or header.");
      }
    }
  };
  template<> struct fx_DebugValue<bool> {
    static void write(std::ostream& out, bool value, int = 0) { out << (value ? "True" : "False"); }
  };
  // Decode UTF-8 and use Haskell Show-style escapes, including \\& to end a
  // decimal escape before a digit. Invalid UTF-8 bytes are escaped individually.
  inline void fx_debugQuoted(std::ostream& out, const std::string& value, char quote) {
    static const char* controls[] = {"NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "a", "b", "t", "n", "v", "f", "r", "SO", "SI", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US"};
    out << quote;
    for (std::size_t i = 0; i < value.size();) {
      const auto first = static_cast<unsigned char>(value[i]);
      std::uint32_t code = first;
      std::size_t width = first < 0x80 ? 1 : first >= 0xc2 && first <= 0xdf ? 2 : first >= 0xe0 && first <= 0xef ? 3 : first >= 0xf0 && first <= 0xf4 ? 4 : 1;
      if (width > 1) {
        code = first & (0x7f >> width);
        bool valid = i + width <= value.size();
        for (std::size_t j = 1; valid && j < width; ++j) {
          const auto byte = static_cast<unsigned char>(value[i + j]);
          valid = (byte & 0xc0) == 0x80;
          code = (code << 6) | (byte & 0x3f);
        }
        const auto minimum = width == 2 ? 0x80u : width == 3 ? 0x800u : 0x10000u;
        if (!valid || code < minimum || code > 0x10ffff || (code >= 0xd800 && code <= 0xdfff)) { code = first; width = 1; }
      }
      i += width;
      if (code == static_cast<unsigned char>(quote) || code == '\\\\') out << '\\\\' << static_cast<char>(code);
      else if (code < 32) {
        out << '\\\\' << controls[code];
        if (code == 14 && i < value.size() && value[i] == 'H') out << "\\\\&";
      } else if (code == 127) out << "\\\\DEL";
      else if (code > 127) {
        out << '\\\\' << code;
        if (i < value.size() && value[i] >= '0' && value[i] <= '9') out << "\\\\&";
      } else out << static_cast<char>(code);
    }
    out << quote;
  }
  template<> struct fx_DebugValue<std::string> {
    static void write(std::ostream& out, const std::string& value, int = 0) { fx_debugQuoted(out, value, '"'); }
  };
  template<> struct fx_DebugValue<char> {
    static void write(std::ostream& out, char value, int = 0) { fx_debugQuoted(out, std::string(1, value), '\\''); }
  };
  template<class T, class Allocator> struct fx_DebugValue<std::vector<T, Allocator>> {
    static void write(std::ostream& out, const std::vector<T, Allocator>& values, int = 0) {
      out << '[';
      bool first = true;
      for (const auto& value : values) {
        if (!first) out << ',';
        first = false;
        fx_DebugValue<T>::write(out, value);
      }
      out << ']';
    }
  };
  template<class T, std::size_t N> struct fx_DebugValue<std::array<T, N>> {
    static void write(std::ostream& out, const std::array<T, N>& values, int = 0) {
      out << '[';
      bool first = true;
      for (const auto& value : values) {
        if (!first) out << ',';
        first = false;
        fx_DebugValue<T>::write(out, value);
      }
      out << ']';
    }
  };
  template<class... Ts> struct fx_DebugValue<std::tuple<Ts...>> {
    static void write(std::ostream& out, const std::tuple<Ts...>& values, int = 0) {
      out << '(';
      [[maybe_unused]] std::size_t position = 0;
      std::apply([&](const auto&... value) {
        (((position++ ? static_cast<void>(out << ',') : static_cast<void>(0)),
          fx_DebugValue<std::decay_t<decltype(value)>>::write(out, value)), ...);
      }, values);
      out << ')';
    }
  };
  // Haskell's Show [Char] uses string syntax rather than a list of chars.
  template<class Allocator> struct fx_DebugValue<std::vector<char, Allocator>> {
    static void write(std::ostream& out, const std::vector<char, Allocator>& values, int = 0) {
      fx_debugQuoted(out, std::string(values.begin(), values.end()), '"');
    }
  };
  inline void fx_debugPaint(std::ostream& out, const char* color, const std::string& text) {
    if (fx_debugColors) out << color;
    out << text;
    if (fx_debugColors) out << "\\033[0m";
  }
  """

events :: Text
events =
  """
  inline void fx_debugFact(std::ostream& out, const Fact& fact) {
    std::visit([&](const auto& value) { fx_debugFact(out, value); }, fact);
  }
  inline void fx_debugSolverPrefix(std::ostream& out, std::size_t source, std::size_t target) {
    std::string prefix = "[Fixen] [Solver]";
    if (fx_debugPhased) prefix += " [Phase" + std::to_string(source) + " -> Phase" + std::to_string(target) + "]";
    fx_debugPaint(out, "\\033[33m", prefix);
  }
  inline void fx_debugRejected(const Fact& fact, std::size_t source, std::size_t target) {
    std::ostringstream out;
    fx_debugSolverPrefix(out, source, target);
    fx_debugPaint(out, "\\033[31m", " Subsumed");
    out << " candidate ";
    fx_debugFact(out, fact);
    std::cerr << out.str() << '\\n';
  }
  inline void fx_debugAccepted(const Fact& fact, const std::vector<Fact>& inserted, std::size_t source, std::size_t target) {
    std::ostringstream out;
    fx_debugSolverPrefix(out, source, target);
    fx_debugPaint(out, "\\033[32m", " Processed");
    out << " candidate ";
    fx_debugFact(out, fact);
    out << '\\n';
    fx_debugSolverPrefix(out, source, target);
    fx_debugPaint(out, "\\033[32m", " Inserted Facts");
    out << ": [";
    bool first = true;
    for (const auto& value : inserted) {
      if (!first) out << ',';
      first = false;
      fx_debugFact(out, value);
    }
    out << ']';
    std::cerr << out.str() << '\\n';
  }
  inline void fx_debugActivation(const Fact& premise, std::size_t phase, const char* rule, fx_Candidate& candidate) {
    // Cache every conclusion so debug printing never causes reevaluation.
    candidate.preview.emplace();
    fx_evaluate(candidate.instance, [&](const Fact& fact) { candidate.preview->push_back(fact); });
    std::ostringstream out;
    std::string prefix = "[Fixen] [Step] ";
    if (fx_debugPhased) prefix += "[Phase " + std::to_string(phase) + "] ";
    fx_debugPaint(out, "\\033[33m", prefix);
    fx_debugPaint(out, "\\033[32m", "Premise ");
    fx_debugFact(out, premise);
    out << ", ";
    fx_debugPaint(out, "\\033[32m", "Rule ");
    fx_debugPaint(out, "\\033[31m", rule);
    if (candidate.preview->size() == 1) {
      out << " activated, candidate: ";
      fx_debugFact(out, candidate.preview->front());
    } else {
      out << " activated, candidates: [";
      bool first = true;
      for (const auto& fact : *candidate.preview) {
        if (!first) out << ',';
        first = false;
        fx_debugFact(out, fact);
      }
      out << ']';
    }
    std::cerr << out.str() << '\\n';
  }
  """
